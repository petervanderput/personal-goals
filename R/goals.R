#' Goal definitions, scheduling rules, and reminder text.
#'
#' Everything in this file is a pure function of its inputs: no HTTP calls and
#' no file writes beyond reading the definitions file. The current time is
#' always passed in rather than read from the clock, so the scheduling rules
#' can be tested at any date.

library(yaml)

VALID_PERIODS <- c("day", "week", "month", "year")
WEEKDAY_ABBREVIATIONS <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

#' Load and validate goal definitions.
#'
#' @param path Path to the goals YAML file.
#' @return A list with `timezone` and a validated `goals` list.
load_goals <- function(path = "goals.yml") {
  if (!file.exists(path)) {
    stop("Goal definitions not found at '", path, "'.", call. = FALSE)
  }
  definitions <- read_yaml(path)

  timezone <- definitions$timezone
  if (is.null(timezone) || !nzchar(timezone)) {
    stop("goals.yml must set a `timezone` (e.g. America/Denver).", call. = FALSE)
  }
  # An unrecognised timezone makes R fall back to UTC with only a warning, which
  # would silently shift every reminder by hours. Refuse to run instead.
  if (!timezone %in% OlsonNames()) {
    stop("goals.yml timezone '", timezone, "' is not recognised on this system.",
         call. = FALSE)
  }
  if (length(definitions$goals) == 0) {
    stop("goals.yml contains no goals.", call. = FALSE)
  }

  goals <- lapply(definitions$goals, validate_goal)

  identifiers <- vapply(goals, `[[`, character(1), "id")
  duplicated_ids <- unique(identifiers[duplicated(identifiers)])
  if (length(duplicated_ids) > 0) {
    stop("Duplicate goal id(s): ", paste(duplicated_ids, collapse = ", "),
         call. = FALSE)
  }

  list(timezone = timezone, goals = goals)
}

#' Validate one goal definition, returning it unchanged if it passes.
validate_goal <- function(goal) {
  required <- c("id", "title", "period", "remind_at")
  missing_fields <- setdiff(required, names(goal))
  if (length(missing_fields) > 0) {
    stop("Goal '", goal$id %||% "<no id>", "' is missing required field(s): ",
         paste(missing_fields, collapse = ", "), call. = FALSE)
  }

  # The id travels inside Telegram's 64-byte callback payload, so keep it short
  # and free of the pipe character used as the payload separator.
  if (!grepl("^[a-z0-9_-]{1,24}$", goal$id)) {
    stop("Goal id '", goal$id, "' must be 1-24 characters of a-z, 0-9, _ or -.",
         call. = FALSE)
  }
  if (!goal$period %in% VALID_PERIODS) {
    stop("Goal '", goal$id, "' has period '", goal$period, "'; expected one of ",
         paste(VALID_PERIODS, collapse = ", "), ".", call. = FALSE)
  }
  parse_time_of_day(goal$remind_at, goal$id)

  goal
}

#' Convert an "HH:MM" string to minutes after local midnight.
parse_time_of_day <- function(value, goal_id = "<unknown>") {
  if (!is.character(value) || !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", value)) {
    stop("Goal '", goal_id, "' has remind_at '", value,
         "'; expected 24-hour HH:MM.", call. = FALSE)
  }
  parts <- as.integer(strsplit(value, ":", fixed = TRUE)[[1]])
  parts[1] * 60L + parts[2]
}

#' Current local time as a POSIXlt in the configured timezone.
local_time_now <- function(timezone, now = Sys.time()) {
  as.POSIXlt(now, tz = timezone)
}

#' Minutes elapsed since local midnight.
minutes_since_midnight <- function(local_time) {
  local_time$hour * 60L + local_time$min
}

#' Identifier for the period a goal is being tracked over.
#'
#' Used to make reminders and check-ins idempotent: one reminder per goal per
#' period, however many times the scheduler happens to fire.
period_key <- function(period, local_time) {
  local_date <- as.Date(local_time)
  switch(period,
    day   = format(local_date, "%Y-%m-%d"),
    week  = format(local_date, "%G-W%V"),
    month = format(local_date, "%Y-%m"),
    year  = format(local_date, "%Y"),
    stop("Unsupported period '", period, "'.", call. = FALSE)
  )
}

#' Is this goal scheduled to be reminded on this local date?
#'
#' Weekly, monthly and yearly goals fire on an anchor day given by `remind_on`,
#' defaulting to Monday, the 1st, and January 1st respectively.
is_scheduled_today <- function(goal, local_time) {
  switch(goal$period,
    day = TRUE,
    week = {
      anchor <- goal$remind_on %||% "Mon"
      target <- match(anchor, WEEKDAY_ABBREVIATIONS)
      if (is.na(target)) {
        stop("Goal '", goal$id, "' has remind_on '", anchor,
             "'; expected one of ", paste(WEEKDAY_ABBREVIATIONS, collapse = ", "),
             ".", call. = FALSE)
      }
      local_time$wday == (target - 1L)
    },
    month = local_time$mday == as.integer(goal$remind_on %||% 1L),
    year  = format(as.Date(local_time), "%m-%d") == (goal$remind_on %||% "01-01")
  )
}

#' Select goals whose reminder is due and not yet sent.
#'
#' @param goals Validated goal list.
#' @param local_time Current local time as POSIXlt.
#' @param sent_keys Character vector of "goal_id|period_key" already sent.
#' @return List of goals, each with `period_key` attached.
due_goals <- function(goals, local_time, sent_keys = character()) {
  due <- list()
  for (goal in goals) {
    if (!is_scheduled_today(goal, local_time)) next
    if (minutes_since_midnight(local_time) < parse_time_of_day(goal$remind_at)) next

    key <- period_key(goal$period, local_time)
    if (paste(goal$id, key, sep = "|") %in% sent_keys) next

    goal$period_key <- key
    due[[length(due) + 1L]] <- goal
  }
  due
}

#' Compose the reminder message for a goal.
#'
#' The implementation intention is quoted back verbatim, because it is the
#' if-then cue that carries the behavioural effect, not the goal title.
format_reminder <- function(goal) {
  lines <- c(goal$title)

  intention <- goal$implementation_intention
  if (!is.null(intention)) {
    lines <- c(lines, "", sprintf("Your plan: %s, %s, %s",
                                  intention$when, intention$where, intention$what))
  }
  if (!is.null(goal$target) && !is.null(goal$measure)) {
    lines <- c(lines, sprintf("Target: %s %s this %s",
                              goal$target, goal$measure, goal$period))
  }
  if (!is.null(goal$obstacle) && !is.null(goal$coping_plan)) {
    lines <- c(lines, sprintf("If %s, then %s", goal$obstacle, goal$coping_plan))
  }
  if (!is.null(goal$stake)) {
    lines <- c(lines, "", sprintf("On the line: %s", goal$stake))
  }

  paste(lines, collapse = "\n")
}
