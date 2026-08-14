#' The view model behind the dashboard.
#'
#' Pure functions from a check-in log to a description of what happened, with no
#' HTML anywhere. Every count the dashboard shows is derived from the same
#' per-day statuses, so the tally card and the charts can never disagree.

MONTH_NAMES <- c("January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December")

# Statuses a single day can carry. Only the first two are wins; `moved` is a
# session done on another day, which is neither a win for this day nor a miss.
DAY_STATUSES <- c("done", "makeup", "moved", "missed", "today", "upcoming",
                  "rest")

#' Weekday abbreviation of a date, independent of locale.
#'
#' `format(date, "%a")` would follow the runner's locale, which differs between a
#' Windows laptop and a CI container.
weekday_abbreviation <- function(date) {
  WEEKDAY_ABBREVIATIONS[as.integer(format(date, "%w")) + 1L]
}

#' Day of the month as a plain integer, with no padding.
day_of_month <- function(date) as.integer(format(date, "%d"))

#' Three-letter month name. Built from a constant for the same reason as the
#' weekday: `%b` follows the locale.
month_abbreviation <- function(date) {
  substr(MONTH_NAMES[as.integer(format(date, "%m"))], 1L, 3L)
}

#' Human label for the month a date falls in, such as "August 2026".
month_label <- function(date) {
  paste(MONTH_NAMES[as.integer(format(date, "%m"))], format(date, "%Y"))
}

#' A date as "17 Aug".
short_date <- function(date) {
  sprintf("%d %s", day_of_month(date), month_abbreviation(date))
}

#' Compact label for a week, such as "17-23 Aug" or "31 Aug - 6 Sep".
week_label <- function(monday) {
  sunday <- monday + 6L

  if (format(monday, "%m") == format(sunday, "%m")) {
    sprintf("%d-%d %s", day_of_month(monday), day_of_month(sunday),
            month_abbreviation(monday))
  } else {
    sprintf("%s - %s", short_date(monday), short_date(sunday))
  }
}

#' The requirement scheduled on a given weekday, or NULL.
requirement_on_weekday <- function(goal, weekday) {
  for (requirement in goal$requirements) {
    if (identical(requirement$on_day, weekday)) return(requirement)
  }
  NULL
}

#' Dates each requirement was logged on within one period.
#'
#' @return A named list of character vectors of dates, keyed by requirement id.
logged_dates_by_requirement <- function(checkin_log, goal_id, key) {
  if (nrow(checkin_log) == 0) return(list())

  relevant <- checkin_log[checkin_log$goal_id == goal_id &
                            checkin_log$period_key == key &
                            checkin_log$outcome == "session", , drop = FALSE]
  if (nrow(relevant) == 0) return(list())

  lapply(split(relevant$local_date, relevant$requirement_id), unique)
}

#' Status of a single day within a week.
#'
#' A session credited on a day it was not scheduled for is a makeup; a scheduled
#' day whose session happened elsewhere in the week is `moved`, which keeps a
#' recovered day from being shown as a failure.
day_status <- function(date, goal, logged_dates, today) {
  weekday <- weekday_abbreviation(date)
  stamp <- format(date, "%Y-%m-%d")

  credited <- Filter(function(requirement) {
    stamp %in% (logged_dates[[requirement$id]] %||% character())
  }, goal$requirements)

  if (length(credited) > 0) {
    on_its_day <- any(vapply(credited, function(requirement) {
      identical(requirement$on_day, weekday)
    }, logical(1)))
    return(list(
      status = if (on_its_day) "done" else "makeup",
      label = paste(vapply(credited, `[[`, character(1), "label"),
                    collapse = ", ")
    ))
  }

  scheduled <- requirement_on_weekday(goal, weekday)
  if (is.null(scheduled)) return(list(status = "rest", label = NULL))

  elsewhere <- length(logged_dates[[scheduled$id]] %||% character()) > 0
  status <- if (elsewhere) {
    "moved"
  } else if (date < today) {
    "missed"
  } else if (date == today) {
    "today"
  } else {
    "upcoming"
  }
  list(status = status, label = scheduled$label)
}

#' Everything the dashboard needs about one week.
week_model <- function(goal, checkin_log, monday, local_time) {
  key <- period_key(goal$period, as.POSIXlt(monday, tz = "UTC"))
  logged_dates <- logged_dates_by_requirement(checkin_log, goal$id, key)
  today <- as.Date(local_time)

  days <- lapply(0:6, function(offset) {
    date <- monday + offset
    status <- day_status(date, goal, logged_dates, today)

    list(
      date = format(date, "%Y-%m-%d"),
      weekday = weekday_abbreviation(date),
      status = status$status,
      label = status$label
    )
  })

  completed <- completed_sessions(goal, session_counts(checkin_log, goal$id,
                                                      key))
  required <- required_sessions(goal)

  list(
    key = key,
    label = week_label(monday),
    # Axis ticks have roughly 40px each on a phone, so the run chart labels its
    # weeks by the date they start rather than by the full label.
    tick = as.character(day_of_month(monday)),
    starts = format(monday, "%Y-%m-%d"),
    is_current = key == period_key(goal$period, local_time),
    is_future = monday > today,
    completed = completed,
    required = required,
    remaining = max(0L, required - completed),
    days = days
  )
}

#' Roll a set of weeks up into one horizon, judged against a session minimum.
#'
#' @param weeks Week models, in order.
#' @param min_sessions Sessions a week needs to count as met.
horizon_model <- function(weeks, min_sessions, label) {
  status_of <- function(week) {
    if (week$is_future) return("upcoming")
    if (week$is_current) return("today")
    if (week$completed >= min_sessions) "done" else "missed"
  }
  statuses <- vapply(weeks, status_of, character(1))

  list(
    label = label,
    min_sessions = min_sessions,
    total = length(weeks),
    met = sum(statuses == "done"),
    short = sum(statuses == "missed"),
    left = sum(statuses %in% c("upcoming", "today")),
    weeks = lapply(seq_along(weeks), function(index) {
      list(key = weeks[[index]]$key, label = weeks[[index]]$label,
           tick = weeks[[index]]$tick,
           completed = weeks[[index]]$completed,
           required = weeks[[index]]$required,
           status = statuses[index])
    })
  )
}

#' Lifetime counts of days trained, skipped and made up.
#'
#' Derived from the day statuses rather than recounted, so the card always agrees
#' with the charts above it.
day_tally <- function(weeks) {
  statuses <- unlist(lapply(weeks, function(week) {
    vapply(week$days, `[[`, character(1), "status")
  }), use.names = FALSE)

  list(
    boxed = sum(statuses %in% c("done", "makeup")),
    skipped = sum(statuses == "missed"),
    makeups = sum(statuses == "makeup")
  )
}

#' Build the whole view model.
#'
#' @param goal A validated quota goal with anchored sessions.
#' @param checkin_log Check-in log as read from disk.
#' @param local_time Current local time as POSIXlt.
#' @param dashboard The `dashboard` block from the definitions file.
dashboard_model <- function(goal, checkin_log, local_time, dashboard = NULL) {
  run_commitment <- commitment_of_kind(goal, "range")
  month_commitment <- commitment_of_kind(goal, "month")
  if (is.null(run_commitment) || is.null(month_commitment)) {
    stop("Goal '", goal$id, "' needs both a range and a month commitment for ",
         "the dashboard to have horizons to report.", call. = FALSE)
  }

  run_mondays <- commitment_period_starts(goal, run_commitment, local_time)
  weeks <- lapply(seq_along(run_mondays), function(index) {
    week_model(goal, checkin_log, run_mondays[index], local_time)
  })
  names(weeks) <- vapply(weeks, `[[`, character(1), "key")

  month_keys <- commitment_period_keys(goal, month_commitment, local_time)
  month_weeks <- weeks[intersect(month_keys, names(weeks))]

  current_key <- period_key(goal$period, local_time)
  # Before the run starts nothing is current, and after it ends the last week is
  # the one worth opening on.
  selected <- if (current_key %in% names(weeks)) {
    current_key
  } else if (current_key < names(weeks)[1]) {
    names(weeks)[1]
  } else {
    names(weeks)[length(weeks)]
  }

  today <- as.Date(local_time)
  last_day <- run_mondays[length(run_mondays)] + 6L

  list(
    title = goal$title,
    # Deliberately to the day and no finer. The page is rebuilt every polling
    # cycle and committed to the repo, so a clock time would produce a commit
    # every half hour saying nothing. To the day, the page changes only when the
    # log does or when a day turns over, which are the only real changes.
    generated_at = sprintf("%s %s", weekday_abbreviation(today),
                           short_date(today)),
    selected_week = selected,
    weeks = weeks,
    tally = day_tally(weeks),
    month = horizon_model(month_weeks, month_commitment$min_sessions,
                          month_label(today)),
    run = horizon_model(weeks, run_commitment$min_sessions,
                        sprintf("%s to %s", short_date(run_mondays[1]),
                                short_date(last_day))),
    commitments = evaluate_commitments(goal, checkin_log, local_time),
    period = goal$period,
    url = dashboard$url
  )
}
