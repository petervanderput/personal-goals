#' Goal definitions, validation, and calendar helpers.
#'
#' Two scheduling styles are supported:
#'
#'   fixed  A goal anchored to a known day and time, checked in as done or
#'          missed once per period. Best evidenced for habit formation, because
#'          a fixed cue is what an implementation intention hangs on.
#'   quota  A count of sessions per period, listed as requirements. A requirement
#'          may be anchored to its own weekday and time with `on_day`/`at`, which
#'          gives it a fixed cue while still being counted rather than judged
#'          pass/fail; one left unanchored can happen on any day.
#'
#' Everything here is a pure function of its inputs. The current time is always
#' passed in rather than read from the clock, so the rules can be tested at any
#' date.

library(yaml)

VALID_PERIODS <- c("day", "week", "month", "year")
VALID_SCHEDULES <- c("fixed", "quota")
VALID_CADENCES <- c("risk_only", "daily")
VALID_WINDOW_KINDS <- c("range", "month")
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

  list(timezone = timezone, goals = goals,
       dashboard = validate_dashboard(definitions$dashboard))
}

#' Validate the dashboard block, returning it unchanged.
#'
#' @param dashboard The `dashboard` block, or NULL when none is configured.
validate_dashboard <- function(dashboard) {
  if (is.null(dashboard)) return(NULL)

  url <- dashboard$url
  if (!is.character(url) || length(url) != 1L || !nzchar(url)) {
    stop("goals.yml dashboard must set a `url` for the published page.",
         call. = FALSE)
  }
  # A relative or scheme-less link is unusable inside a Telegram button, and the
  # failure would only show up when the digest is sent a week later.
  if (!grepl("^https://", url)) {
    stop("goals.yml dashboard url '", url, "' must start with https://.",
         call. = FALSE)
  }

  digest <- dashboard$digest
  if (!is.null(digest)) {
    if (is.null(digest$on_day) || is.null(digest$at)) {
      stop("goals.yml dashboard digest needs both on_day and at.", call. = FALSE)
    }
    weekday_number(digest$on_day, "dashboard digest")
    parse_time_of_day(digest$at, "dashboard digest")
  }
  dashboard
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
  if (!is.null(goal$starts)) {
    parse_date(goal$starts, paste0("Start date of goal '", goal$id, "'"))
  }
  if (!is.null(goal$missed_notice_at)) {
    parse_time_of_day(goal$missed_notice_at, goal$id)
    if (is.null(goal$missed_session_consequence)) {
      stop("Goal '", goal$id, "' sets missed_notice_at but no ",
           "missed_session_consequence to report.", call. = FALSE)
    }
  }

  if (length(goal$commitments) > 0) {
    # Commitments are judged on session counts, which only a quota goal has.
    if (goal$schedule != "quota") {
      stop("Goal '", goal$id, "' has commitments, which need schedule 'quota'.",
           call. = FALSE)
    }
    for (commitment in goal$commitments) validate_commitment(goal, commitment)

    commitment_ids <- vapply(goal$commitments, `[[`, character(1), "id")
    if (anyDuplicated(commitment_ids) > 0) {
      stop("Goal '", goal$id, "' has duplicate commitment ids.", call. = FALSE)
    }
  }

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

    # A session that cannot happen on an arbitrary day may name the weekday it
    # must be done by; weekday_number() raises on anything unrecognised.
    if (!is.null(requirement$by_day)) {
      weekday_number(requirement$by_day, goal$id)
    }
    # An anchored session happens on a known day, and therefore needs a time of
    # day for its reminder to fire at. That time is when to prompt, which is
    # usually earlier than the session itself.
    if (!is.null(requirement$on_day)) {
      weekday_number(requirement$on_day, goal$id)
      if (is.null(requirement$remind_at)) {
        stop("Requirement '", requirement$id, "' of goal '", goal$id,
             "' sets on_day and must also set remind_at.", call. = FALSE)
      }
      parse_time_of_day(requirement$remind_at, goal$id)
    }
    requirement
  })

  requirement_ids <- vapply(goal$requirements, `[[`, character(1), "id")
  if (anyDuplicated(requirement_ids) > 0) {
    stop("Goal '", goal$id, "' has duplicate requirement ids.", call. = FALSE)
  }

  # Catching up only means something for sessions tied to a day, since a free
  # session can already be logged whenever it happens.
  if (!is.null(goal$makeup)) {
    if (is.null(goal$makeup$at)) {
      stop("Goal '", goal$id, "' allows makeups and must give makeup.at.",
           call. = FALSE)
    }
    parse_time_of_day(goal$makeup$at, goal$id)

    anchored <- vapply(goal$requirements, function(requirement) {
      !is.null(requirement$on_day)
    }, logical(1))
    if (!any(anchored)) {
      stop("Goal '", goal$id, "' allows makeups but has no session anchored to ",
           "a day to make up.", call. = FALSE)
    }
  }

  # Every anchored session brings its own reminder time, so a goal-level nudge is
  # only needed when some session can land on any day.
  has_free_sessions <- any(vapply(goal$requirements, function(requirement) {
    is.null(requirement$on_day)
  }, logical(1)))

  if (is.null(goal$nudge)) {
    if (has_free_sessions) {
      stop("Goal '", goal$id, "' has sessions that are not anchored to a day, ",
           "so it must set nudge.at.", call. = FALSE)
    }
    return(goal)
  }

  nudge <- goal$nudge
  if (is.null(nudge$at)) {
    stop("Goal '", goal$id, "' sets a nudge and must give nudge.at.",
         call. = FALSE)
  }
  parse_time_of_day(nudge$at, goal$id)

  nudge$cadence <- nudge$cadence %||% "risk_only"
  if (!nudge$cadence %in% VALID_CADENCES) {
    stop("Goal '", goal$id, "' has nudge.cadence '", nudge$cadence,
         "'; expected one of ", paste(VALID_CADENCES, collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.null(nudge$kickoff_on)) {
    weekday_number(nudge$kickoff_on, goal$id)
  }
  goal$nudge <- nudge

  goal
}

#' Validate one reward commitment.
#'
#' A commitment judges a window of periods against a minimum number of sessions
#' and awards the best tier whose tolerance for short periods still holds.
validate_commitment <- function(goal, commitment) {
  absent <- setdiff(c("id", "window", "min_sessions", "tiers", "otherwise"),
                    names(commitment))
  if (length(absent) > 0) {
    stop("A commitment of goal '", goal$id, "' is missing: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  validate_identifier(commitment$id, "Commitment id")
  validate_commitment_window(goal, commitment)

  minimum <- commitment$min_sessions
  if (!is.numeric(minimum) || length(minimum) != 1L || minimum < 1) {
    stop("Commitment '", commitment$id, "' of goal '", goal$id,
         "' needs min_sessions to be a positive number.", call. = FALSE)
  }
  # A minimum above what the schedule offers could never be met, which would
  # quietly guarantee failure rather than motivate anything.
  if (minimum > required_sessions(goal)) {
    stop("Commitment '", commitment$id, "' of goal '", goal$id, "' asks for ",
         minimum, " sessions per ", goal$period, " but the schedule only has ",
         required_sessions(goal), ".", call. = FALSE)
  }

  thresholds <- vapply(commitment$tiers, function(tier) {
    if (is.null(tier$max_shortfalls) || is.null(tier$outcome)) {
      stop("Every tier of commitment '", commitment$id, "' of goal '", goal$id,
           "' needs max_shortfalls and outcome.", call. = FALSE)
    }
    as.numeric(tier$max_shortfalls)
  }, numeric(1))

  # Tiers are matched in order, so an unsorted list would award a lower tier
  # than earned.
  if (length(thresholds) > 1L && any(diff(thresholds) <= 0)) {
    stop("Tiers of commitment '", commitment$id, "' of goal '", goal$id,
         "' must be listed by ascending max_shortfalls.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate the window a commitment is judged over.
validate_commitment_window <- function(goal, commitment) {
  window <- commitment$window
  if (!is.list(window) || is.null(window$kind)) {
    stop("Commitment '", commitment$id, "' of goal '", goal$id,
         "' needs a window with a kind.", call. = FALSE)
  }
  if (!window$kind %in% VALID_WINDOW_KINDS) {
    stop("Commitment '", commitment$id, "' of goal '", goal$id,
         "' has window kind '", window$kind, "'; expected one of ",
         paste(VALID_WINDOW_KINDS, collapse = ", "), ".", call. = FALSE)
  }
  if (window$kind == "range") {
    label <- paste0("Window of commitment '", commitment$id, "'")
    from <- parse_date(window$from, paste(label, "from"))
    to <- parse_date(window$to, paste(label, "to"))
    if (to < from) {
      stop(label, " ends before it starts.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Parse a YYYY-MM-DD date, refusing anything else.
#'
#' An explicit format is given because `as.Date()` throws on unrecognised input
#' rather than returning NA, which would bypass this check with a cryptic error.
parse_date <- function(value, label) {
  parsed <- suppressWarnings(as.Date(as.character(value), format = "%Y-%m-%d"))
  if (length(parsed) != 1L || is.na(parsed)) {
    stop(label, " '", value %||% "<null>",
         "' must be a date in YYYY-MM-DD form.", call. = FALSE)
  }
  parsed
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
