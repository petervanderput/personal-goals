#' Deciding what to say and when to say it.
#'
#' Planning is separated from sending: these functions take the clock and the
#' logs and return the messages that ought to go out, which makes the timing
#' rules testable without touching the network.

#' Plan every reminder that is due now and not already sent.
#'
#' @param definitions Output of `load_goals()`.
#' @param local_time Current local time as POSIXlt.
#' @param sent_keys Character vector of "goal_id|reminder_key" already sent.
#' @param checkin_log Data frame of recorded check-ins.
#' @return A list of plans, each with goal_id, reminder_key, text and buttons.
plan_reminders <- function(definitions, local_time, sent_keys = character(),
                           checkin_log = NULL) {
  plans <- list()
  for (goal in definitions$goals) {
    plan <- switch(goal$schedule,
      fixed = plan_fixed_reminder(goal, local_time, sent_keys),
      quota = plan_quota_reminder(goal, local_time, sent_keys, checkin_log)
    )
    if (!is.null(plan)) plans[[length(plans) + 1L]] <- plan
  }
  plans
}

#' Plan the reminder for a goal anchored to a day and time.
plan_fixed_reminder <- function(goal, local_time, sent_keys) {
  if (!is_scheduled_today(goal, local_time)) return(NULL)
  if (minutes_since_midnight(local_time) < parse_time_of_day(goal$remind_at)) {
    return(NULL)
  }

  key <- period_key(goal$period, local_time)
  if (paste(goal$id, key, sep = "|") %in% sent_keys) return(NULL)

  list(
    goal_id = goal$id,
    reminder_key = key,
    text = format_fixed_reminder(goal),
    buttons = inline_keyboard(
      labels = c("Done", "Missed"),
      payloads = c(
        build_callback_payload(goal$id, key, NO_REQUIREMENT, "done"),
        build_callback_payload(goal$id, key, NO_REQUIREMENT, "missed")
      )
    )
  )
}

#' Plan the reminder for a day-agnostic quota goal.
#'
#' At most one message per day, because the decision of whether to nudge is a
#' daily one. Under the default `risk_only` cadence a message is sent only on
#' the kickoff day or once skipping a day would make the period impossible,
#' which keeps reminders informative instead of habituating.
plan_quota_reminder <- function(goal, local_time, sent_keys, checkin_log) {
  if (minutes_since_midnight(local_time) < parse_time_of_day(goal$nudge$at)) {
    return(NULL)
  }

  reminder_key <- format(as.Date(local_time), "%Y-%m-%d")
  if (paste(goal$id, reminder_key, sep = "|") %in% sent_keys) return(NULL)

  key <- period_key(goal$period, local_time)
  counts <- if (is.null(checkin_log)) integer() else {
    session_counts(checkin_log, goal$id, key)
  }
  progress <- requirement_progress(goal, counts)
  remaining <- total_remaining(progress)
  if (remaining == 0L) return(NULL)

  days_left <- days_left_in_period(goal$period, local_time)
  at_risk <- is_at_risk(remaining, days_left)
  is_kickoff <- !is.null(goal$nudge$kickoff_on) &&
    local_time$wday == weekday_number(goal$nudge$kickoff_on, goal$id)

  if (!(goal$nudge$cadence == "daily" || at_risk || is_kickoff)) return(NULL)

  outstanding <- Filter(function(item) item$remaining > 0L, progress)

  list(
    goal_id = goal$id,
    reminder_key = reminder_key,
    text = format_quota_reminder(goal, progress, days_left, at_risk, is_kickoff,
                                 checkin_log, local_time),
    buttons = inline_keyboard(
      labels = vapply(outstanding, `[[`, character(1), "label"),
      payloads = vapply(outstanding, function(item) {
        build_callback_payload(goal$id, key, item$id, "session")
      }, character(1))
    )
  )
}

#' Compose the reminder for a fixed goal.
#'
#' The implementation intention is quoted back verbatim, because the if-then cue
#' is what carries the behavioural effect, not the goal title.
format_fixed_reminder <- function(goal) {
  lines <- c(goal$title)

  intention <- goal$implementation_intention
  if (!is.null(intention)) {
    lines <- c(lines, "", sprintf("Your plan: %s, %s, %s",
                                  intention$when, intention$where,
                                  intention$what))
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

#' Compose the reminder for a quota goal.
format_quota_reminder <- function(goal, progress, days_left, at_risk, is_kickoff,
                                  checkin_log, local_time) {
  headline <- if (is_kickoff) {
    sprintf("New %s: %s", goal$period, goal$title)
  } else if (at_risk) {
    sprintf("%s: no room left to skip", goal$title)
  } else {
    goal$title
  }
  lines <- c(headline, "")

  for (item in progress) {
    lines <- c(lines, sprintf("%s: %d of %d", item$label, item$logged,
                              item$required))
  }

  lines <- c(lines, "", sprintf("%d session(s) left, %d day(s) to fit them in.",
                                total_remaining(progress), days_left))
  if (at_risk) {
    lines <- c(lines, "Training today is the only way to keep the week intact.")
  }

  intention <- goal$implementation_intention
  if (!is.null(intention) && (is_kickoff || at_risk)) {
    lines <- c(lines, "", sprintf("Your plan: %s, %s, %s",
                                  intention$when, intention$where,
                                  intention$what))
  }
  if (!is.null(goal$obstacle) && !is.null(goal$coping_plan) && at_risk) {
    lines <- c(lines, sprintf("If %s, then %s", goal$obstacle, goal$coping_plan))
  }

  block_line <- format_block_status(goal, checkin_log, local_time)
  if (!is.null(block_line)) lines <- c(lines, "", block_line)

  paste(lines, collapse = "\n")
}

#' One-line summary of reward block standing, or NULL when there is no block.
format_block_status <- function(goal, checkin_log, local_time) {
  if (is.null(checkin_log)) return(NULL)
  status <- evaluate_block(goal, checkin_log, local_time)
  if (is.null(status) || !status$is_active) return(NULL)

  sprintf("Block: %d of %d %ss done, %d missed. Still on for: %s",
          status$finished_periods, status$total_periods, goal$period,
          status$missed_periods, status$best_achievable)
}
