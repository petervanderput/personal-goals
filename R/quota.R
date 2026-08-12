#' Quota progress and reward block evaluation.
#'
#' Pure functions over a check-in log. A quota goal asks for a number of
#' sessions within a period on any day, so progress is a count rather than a
#' done/missed flag, and the interesting questions are how many sessions remain
#' and whether the remaining days can still absorb them.

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

#' Progress against each requirement of a quota goal.
#'
#' @param goal A validated goal.
#' @param counts Session counts from `session_counts()`.
#' @param local_time Optional current time. When supplied, requirements carrying
#'   a `by_day` are annotated with how many days remain until that day.
#' @return A list of lists with id, label, required, logged, remaining,
#'   intention and days_until_due.
requirement_progress <- function(goal, counts, local_time = NULL) {
  lapply(goal$requirements, function(requirement) {
    logged <- unname(counts[requirement$id])
    if (length(logged) == 0 || is.na(logged)) logged <- 0L

    list(
      id = requirement$id,
      label = requirement$label,
      required = requirement$sessions_per_period,
      logged = logged,
      remaining = max(0L, requirement$sessions_per_period - logged),
      intention = requirement$implementation_intention,
      coping_plan = requirement$coping_plan,
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

#' Days remaining until a requirement's own deadline within the current week.
#'
#' Some sessions cannot happen on an arbitrary day, so a requirement may name
#' the weekday it must be done by. Returns 1 when today is the deadline, a
#' negative or zero value when it has passed, and NA when none is set.
days_until_requirement_deadline <- function(requirement, local_time,
                                           goal_id = "<unknown>") {
  if (is.null(requirement$by_day)) return(NA_integer_)

  target <- iso_weekday_position(weekday_number(requirement$by_day, goal_id))
  today <- iso_weekday_position(local_time$wday)
  target - today + 1L
}

#' Is a requirement out of time, or down to its last day?
is_requirement_urgent <- function(item) {
  item$remaining > 0L && !is.na(item$days_until_due) && item$days_until_due <= 1L
}

#' Total sessions still owed across all requirements.
total_remaining <- function(progress) {
  sum(vapply(progress, `[[`, integer(1), "remaining"))
}

#' Has every requirement been met for the period?
is_period_satisfied <- function(goal, counts) {
  total_remaining(requirement_progress(goal, counts)) == 0L
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

#' Period keys covered by a goal's reward block.
block_period_keys <- function(goal) {
  block <- goal$block
  start <- as.Date(block$starts)
  if (is.na(start)) {
    stop("Block of goal '", goal$id, "' has an unparseable start date.",
         call. = FALSE)
  }

  starts <- seq(start, by = goal$period, length.out = as.integer(block$periods))
  vapply(starts, function(date) {
    period_key(goal$period, as.POSIXlt(date, tz = "UTC"))
  }, character(1))
}

#' Evaluate progress through a reward block.
#'
#' Only periods that have finished are judged, so a week in progress is never
#' counted as missed.
#'
#' @return A list describing the block, or NULL when the goal has no block.
evaluate_block <- function(goal, checkin_log, local_time) {
  if (is.null(goal$block)) return(NULL)

  keys <- block_period_keys(goal)
  current <- period_key(goal$period, local_time)

  # These key formats are zero padded, so lexical order matches chronological
  # order and no date parsing is needed to find the finished periods.
  finished <- keys[keys < current]
  missed <- sum(!vapply(finished, function(key) {
    is_period_satisfied(goal, session_counts(checkin_log, goal$id, key))
  }, logical(1)))

  list(
    total_periods = length(keys),
    finished_periods = length(finished),
    remaining_periods = sum(keys >= current),
    missed_periods = missed,
    is_active = current >= keys[1] && current <= keys[length(keys)],
    is_complete = current > keys[length(keys)],
    best_achievable = best_achievable_tier(goal, missed)
  )
}

#' The best outcome still reachable given the misses so far.
#'
#' Tiers are validated as ascending, so the first tier that still admits the
#' current miss count is the best one available.
best_achievable_tier <- function(goal, missed) {
  for (tier in goal$block$tiers) {
    if (missed <= as.numeric(tier$max_missed)) return(tier$outcome)
  }
  goal$block$otherwise
}
