#' Session progress and commitment evaluation.
#'
#' Pure functions over a check-in log. A goal asks for a number of sessions
#' within a period. Each session may be anchored to its own weekday and time, or
#' left free to happen on any day; the interesting questions are what is still
#' owed, whether the period can still be salvaged, and where that leaves each
#' standing reward commitment.

#' Count logged sessions per requirement within one period.
#'
#' Sessions are counted as distinct local dates rather than rows. Telegram
#' expires callback query ids quickly, so a tap often cannot be acknowledged and
#' its buttons stay on screen, inviting a second tap. Counting dates makes a
#' repeat tap idempotent, and encodes the domain rule that one session of a
#' given type counts once per day.
#'
#' @param checkin_log Data frame as returned by `read_log()`.
#' @param goal_id Goal identifier.
#' @param key Period key to count within.
#' @return Named integer vector of session counts, keyed by requirement id.
session_counts <- function(checkin_log, goal_id, key) {
  if (nrow(checkin_log) == 0) return(integer())

  relevant <- checkin_log[checkin_log$goal_id == goal_id &
                            checkin_log$period_key == key &
                            checkin_log$outcome == "session", , drop = FALSE]
  if (nrow(relevant) == 0) return(integer())

  vapply(split(relevant$local_date, relevant$requirement_id),
         function(dates) length(unique(dates)), integer(1))
}

#' Sessions logged for one requirement, never more than it asks for.
logged_for_requirement <- function(requirement, counts) {
  logged <- unname(counts[requirement$id])
  if (length(logged) == 0 || is.na(logged)) return(0L)
  min(as.integer(logged), requirement$sessions_per_period)
}

#' Total sessions completed in a period, across every requirement.
#'
#' Capped per requirement, so logging the Monday session twice cannot stand in
#' for a session that never happened.
completed_sessions <- function(goal, counts) {
  sum(vapply(goal$requirements, logged_for_requirement, integer(1),
             counts = counts))
}

#' Total sessions a period asks for.
required_sessions <- function(goal) {
  sum(vapply(goal$requirements, `[[`, integer(1), "sessions_per_period"))
}

#' Progress against each requirement.
#'
#' @param goal A validated goal.
#' @param counts Session counts from `session_counts()`.
#' @param local_time Optional current time. When supplied, requirements are
#'   annotated with how many days remain until their day or deadline.
#' @return A list of lists describing each requirement's standing.
requirement_progress <- function(goal, counts, local_time = NULL) {
  lapply(goal$requirements, function(requirement) {
    logged <- logged_for_requirement(requirement, counts)

    list(
      id = requirement$id,
      label = requirement$label,
      required = requirement$sessions_per_period,
      logged = logged,
      remaining = max(0L, requirement$sessions_per_period - logged),
      intention = requirement$implementation_intention,
      coping_plan = requirement$coping_plan,
      on_day = requirement$on_day,
      by_day = requirement$by_day,
      days_until_due = if (is.null(local_time)) {
        NA_integer_
      } else {
        days_until_requirement_deadline(requirement, local_time, goal$id)
      }
    )
  })
}

#' Position of a weekday within an ISO week, where Monday is 1 and Sunday is 7.
iso_weekday_position <- function(wday) {
  if (wday == 0L) 7L else as.integer(wday)
}

#' Days remaining until a requirement's day or deadline within the current week.
#'
#' A requirement anchored with `on_day` is due on that day; one carrying `by_day`
#' must merely happen by then. Returns 1 when today is the day, a negative or
#' zero value when it has passed, and NA when neither is set.
days_until_requirement_deadline <- function(requirement, local_time,
                                           goal_id = "<unknown>") {
  anchor <- requirement$on_day %||% requirement$by_day
  if (is.null(anchor)) return(NA_integer_)

  target <- iso_weekday_position(weekday_number(anchor, goal_id))
  today <- iso_weekday_position(local_time$wday)
  target - today + 1L
}

#' Is a requirement out of time, or down to its last day?
is_requirement_urgent <- function(item) {
  item$remaining > 0L && !is.na(item$days_until_due) && item$days_until_due <= 1L
}

#' Requirements anchored to today's weekday.
sessions_on_day <- function(goal, local_time) {
  Filter(function(requirement) {
    !is.null(requirement$on_day) &&
      local_time$wday == weekday_number(requirement$on_day, goal$id)
  }, goal$requirements)
}

#' Anchored sessions still to come later this week and not yet logged.
#'
#' Used to say whether a weekly target is still reachable after a miss.
remaining_scheduled_sessions <- function(goal, counts, local_time) {
  length(anchored_sessions(goal, counts, local_time, when = "later"))
}

#' Anchored sessions whose day has passed without being logged.
#'
#' These are what a catch-up prompt offers, since a session that has not come
#' round yet will get a prompt of its own on its day.
overdue_sessions <- function(goal, counts, local_time) {
  anchored_sessions(goal, counts, local_time, when = "earlier")
}

#' Outstanding anchored sessions, either side of today.
#'
#' @param when Either "earlier" or "later", relative to today's weekday.
anchored_sessions <- function(goal, counts, local_time, when) {
  today <- iso_weekday_position(local_time$wday)

  Filter(function(requirement) {
    if (is.null(requirement$on_day)) return(FALSE)
    if (logged_for_requirement(requirement, counts) >=
          requirement$sessions_per_period) {
      return(FALSE)
    }
    position <- iso_weekday_position(weekday_number(requirement$on_day, goal$id))
    if (when == "earlier") position < today else position > today
  }, goal$requirements)
}

#' Total sessions still owed across all requirements.
total_remaining <- function(progress) {
  sum(vapply(progress, `[[`, integer(1), "remaining"))
}

#' Days remaining in the current period, counting today.
#'
#' Weeks are ISO weeks, running Monday to Sunday.
days_left_in_period <- function(period, local_time) {
  local_date <- as.Date(local_time)
  switch(period,
    day = 1L,
    week = if (local_time$wday == 0L) 1L else 8L - local_time$wday,
    month = {
      first_of_month <- as.Date(format(local_date, "%Y-%m-01"))
      first_of_next <- seq(first_of_month, by = "month", length.out = 2L)[2L]
      as.integer(first_of_next - local_date)
    },
    year = {
      next_year <- as.integer(format(local_date, "%Y")) + 1L
      as.integer(as.Date(paste0(next_year, "-01-01")) - local_date)
    },
    stop("Unsupported period '", period, "'.", call. = FALSE)
  )
}

#' Can the remaining sessions no longer absorb a skipped day?
#'
#' True when there are at least as many sessions owed as days left, which is the
#' moment a reminder carries real information rather than being noise.
is_at_risk <- function(remaining, days_left) {
  remaining > 0L && remaining >= days_left
}

#' Period keys covered by a commitment's window.
#'
#' Keys earlier than the goal's start date are dropped, so a commitment that
#' spans a calendar month does not judge weeks from before the goal existed.
commitment_period_keys <- function(goal, commitment, local_time) {
  window <- commitment$window
  starts <- switch(window$kind,
    range = seq(parse_date(window$from, "Window start"),
                parse_date(window$to, "Window end"), by = goal$period),
    month = period_starts_in_month(local_time),
    stop("Unsupported commitment window '", window$kind, "'.", call. = FALSE)
  )

  if (!is.null(goal$starts)) {
    starts <- starts[starts >= parse_date(goal$starts, "Goal start")]
  }
  if (length(starts) == 0) return(character())

  # Indexed rather than iterated so the Date class survives into period_key().
  vapply(seq_along(starts), function(index) {
    period_key(goal$period, as.POSIXlt(starts[index], tz = "UTC"))
  }, character(1))
}

#' Mondays of the calendar month that the current week belongs to.
#'
#' A week belongs to the month containing its Monday, which keeps every week in
#' exactly one month even when it straddles the boundary. The month is taken from
#' this week's Monday rather than from today, so a week spanning two months does
#' not switch which month it is judged in halfway through.
period_starts_in_month <- function(local_time) {
  this_monday <- as.Date(local_time) -
    (iso_weekday_position(local_time$wday) - 1L)

  first_of_month <- as.Date(format(this_monday, "%Y-%m-01"))
  last_of_month <- seq(first_of_month, by = "month", length.out = 2L)[2L] - 1L

  days <- seq(first_of_month, last_of_month, by = "day")
  days[format(days, "%u") == "1"]
}

#' Evaluate one reward commitment.
#'
#' Only periods that have finished are judged, so a week in progress is never
#' counted against you.
#'
#' @return A list describing the commitment, or NULL when its window is empty.
evaluate_commitment <- function(goal, commitment, checkin_log, local_time) {
  keys <- commitment_period_keys(goal, commitment, local_time)
  if (length(keys) == 0) return(NULL)

  current <- period_key(goal$period, local_time)

  # These key formats are zero padded, so lexical order matches chronological
  # order and no date parsing is needed to find the finished periods.
  finished <- keys[keys < current]
  shortfalls <- sum(vapply(finished, function(key) {
    completed <- completed_sessions(goal, session_counts(checkin_log, goal$id,
                                                        key))
    completed < commitment$min_sessions
  }, logical(1)))

  list(
    id = commitment$id,
    label = commitment$label %||% commitment$id,
    min_sessions = commitment$min_sessions,
    total_periods = length(keys),
    finished_periods = length(finished),
    shortfall_periods = shortfalls,
    is_active = current >= keys[1] && current <= keys[length(keys)],
    is_complete = current > keys[length(keys)],
    is_lost = is_commitment_lost(commitment, shortfalls),
    best_achievable = best_commitment_tier(commitment, shortfalls)
  )
}

#' Evaluate every commitment attached to a goal.
evaluate_commitments <- function(goal, checkin_log, local_time) {
  if (length(goal$commitments) == 0) return(list())

  evaluated <- lapply(goal$commitments, function(commitment) {
    evaluate_commitment(goal, commitment, checkin_log, local_time)
  })
  Filter(Negate(is.null), evaluated)
}

#' The best outcome still reachable given the shortfalls so far.
#'
#' Tiers are validated as ascending, so the first tier that still admits the
#' current shortfall count is the best one available.
best_commitment_tier <- function(commitment, shortfalls) {
  for (tier in commitment$tiers) {
    if (shortfalls <= as.numeric(tier$max_shortfalls)) return(tier$outcome)
  }
  commitment$otherwise
}

#' Have the shortfalls put every tier out of reach?
#'
#' Distinguishes a reward still worth chasing from one already gone, which the
#' outcome text alone cannot express.
is_commitment_lost <- function(commitment, shortfalls) {
  tolerances <- vapply(commitment$tiers, function(tier) {
    as.numeric(tier$max_shortfalls)
  }, numeric(1))
  shortfalls > max(tolerances)
}
