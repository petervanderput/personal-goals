#' Goal definitions, validation, and calendar helpers.
#'
#' Two scheduling styles are supported:
#'
#'   fixed  A goal anchored to a known day and time, checked in as done or
#'          missed once per period. Best evidenced for habit formation, because
#'          a fixed cue is what an implementation intention hangs on.
#'   quota  A day-agnostic count of sessions per period. Reminders track what is
#'          left rather than naming a day.
#'
#' Everything here is a pure function of its inputs. The current time is always
#' passed in rather than read from the clock, so the rules can be tested at any
#' date.

library(yaml)

VALID_PERIODS <- c("day", "week", "month", "year")
VALID_SCHEDULES <- c("fixed", "quota")
VALID_CADENCES <- c("risk_only", "daily")
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

#' Validate one goal definition, returning it with defaults applied.
validate_goal <- function(goal) {
  missing_fields <- setdiff(c("id", "title", "period"), names(goal))
  if (length(missing_fields) > 0) {
    stop("Goal '", goal$id %||% "<no id>", "' is missing required field(s): ",
         paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  validate_identifier(goal$id, "Goal id")

  if (!goal$period %in% VALID_PERIODS) {
    stop("Goal '", goal$id, "' has period '", goal$period, "'; expected one of ",
         paste(VALID_PERIODS, collapse = ", "), ".", call. = FALSE)
  }

  goal$schedule <- goal$schedule %||% "fixed"
  if (!goal$schedule %in% VALID_SCHEDULES) {
    stop("Goal '", goal$id, "' has schedule '", goal$schedule,
         "'; expected one of ", paste(VALID_SCHEDULES, collapse = ", "), ".",
         call. = FALSE)
  }

  goal <- switch(goal$schedule,
    fixed = validate_fixed_goal(goal),
    quota = validate_quota_goal(goal)
  )
  if (!is.null(goal$block)) validate_block(goal)

  goal
}

#' Identifiers travel inside Telegram's 64-byte callback payload, so they must
#' be short and must not contain the payload separator.
validate_identifier <- function(value, label) {
  if (!is.character(value) || length(value) != 1L ||
      !grepl("^[a-z0-9_-]{1,24}$", value)) {
    stop(label, " '", value %||% "<null>",
         "' must be 1-24 characters of a-z, 0-9, _ or -.", call. = FALSE)
  }
  invisible(value)
}

#' A fixed goal fires on an anchor day at a set time.
validate_fixed_goal <- function(goal) {
  if (is.null(goal$remind_at)) {
    stop("Goal '", goal$id, "' has schedule 'fixed' and must set remind_at.",
         call. = FALSE)
  }
  parse_time_of_day(goal$remind_at, goal$id)
  goal
}

#' A quota goal counts sessions per period, on any day.
validate_quota_goal <- function(goal) {
  if (length(goal$requirements) == 0) {
    stop("Goal '", goal$id, "' has schedule 'quota' and must list at least one ",
         "requirement.", call. = FALSE)
  }

  goal$requirements <- lapply(goal$requirements, function(requirement) {
    absent <- setdiff(c("id", "sessions_per_period"), names(requirement))
    if (length(absent) > 0) {
      stop("A requirement of goal '", goal$id, "' is missing: ",
           paste(absent, collapse = ", "), call. = FALSE)
    }
    validate_identifier(requirement$id, "Requirement id")

    sessions <- requirement$sessions_per_period
    if (!is.numeric(sessions) || length(sessions) != 1L || sessions < 1) {
      stop("Requirement '", requirement$id, "' of goal '", goal$id,
           "' needs sessions_per_period to be a positive number.", call. = FALSE)
    }
    requirement$sessions_per_period <- as.integer(sessions)
    requirement$label <- requirement$label %||% requirement$id
    requirement
  })

  requirement_ids <- vapply(goal$requirements, `[[`, character(1), "id")
  if (anyDuplicated(requirement_ids) > 0) {
    stop("Goal '", goal$id, "' has duplicate requirement ids.", call. = FALSE)
  }

  nudge <- goal$nudge %||% list()
  if (is.null(nudge$at)) {
    stop("Goal '", goal$id, "' has schedule 'quota' and must set nudge.at.",
         call. = FALSE)
  }
  parse_time_of_day(nudge$at, goal$id)

  nudge$cadence <- nudge$cadence %||% "risk_only"
  if (!nudge$cadence %in% VALID_CADENCES) {
    stop("Goal '", goal$id, "' has nudge.cadence '", nudge$cadence,
         "'; expected one of ", paste(VALID_CADENCES, collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.null(nudge$kickoff_on) &&
      !nudge$kickoff_on %in% WEEKDAY_ABBREVIATIONS) {
    stop("Goal '", goal$id, "' has nudge.kickoff_on '", nudge$kickoff_on,
         "'; expected one of ", paste(WEEKDAY_ABBREVIATIONS, collapse = ", "),
         ".", call. = FALSE)
  }
  goal$nudge <- nudge

  goal
}

#' Validate a reward block: a fixed run of periods with outcome tiers.
validate_block <- function(goal) {
  block <- goal$block
  absent <- setdiff(c("periods", "starts", "tiers"), names(block))
  if (length(absent) > 0) {
    stop("Block of goal '", goal$id, "' is missing: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(block$periods) || block$periods < 1) {
    stop("Block of goal '", goal$id, "' needs periods to be a positive number.",
         call. = FALSE)
  }
  if (is.null(block$otherwise)) {
    stop("Block of goal '", goal$id, "' must set `otherwise`, the outcome when ",
         "no tier is met.", call. = FALSE)
  }

  thresholds <- vapply(block$tiers, function(tier) {
    if (is.null(tier$max_missed) || is.null(tier$outcome)) {
      stop("Every tier of goal '", goal$id,
           "' needs max_missed and outcome.", call. = FALSE)
    }
    as.numeric(tier$max_missed)
  }, numeric(1))

  # Tiers are matched in order, so an unsorted list would award a lower tier
  # than earned.
  if (any(diff(thresholds) <= 0)) {
    stop("Tiers of goal '", goal$id,
         "' must be listed by ascending max_missed.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Convert an "HH:MM" string to minutes after local midnight.
parse_time_of_day <- function(value, goal_id = "<unknown>") {
  if (!is.character(value) || !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", value)) {
    stop("Goal '", goal_id, "' has time '", value,
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

#' Identifier for the period a goal is tracked over.
#'
#' Used to make reminders and check-ins idempotent: one reminder per goal per
#' key, however many times the scheduler happens to fire. Weeks use the ISO
#' year-week so they stay unambiguous across year boundaries.
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

#' Is a fixed goal scheduled on this local date?
#'
#' Weekly, monthly and yearly goals fire on an anchor day given by `remind_on`,
#' defaulting to Monday, the 1st, and January 1st respectively.
is_scheduled_today <- function(goal, local_time) {
  switch(goal$period,
    day = TRUE,
    week = local_time$wday == weekday_number(goal$remind_on %||% "Mon", goal$id),
    month = local_time$mday == as.integer(goal$remind_on %||% 1L),
    year  = format(as.Date(local_time), "%m-%d") == (goal$remind_on %||% "01-01")
  )
}

#' Convert a weekday abbreviation to POSIXlt's wday numbering (Sunday = 0).
weekday_number <- function(abbreviation, goal_id = "<unknown>") {
  position <- match(abbreviation, WEEKDAY_ABBREVIATIONS)
  if (is.na(position)) {
    stop("Goal '", goal_id, "' has weekday '", abbreviation,
         "'; expected one of ", paste(WEEKDAY_ABBREVIATIONS, collapse = ", "),
         ".", call. = FALSE)
  }
  position - 1L
}
