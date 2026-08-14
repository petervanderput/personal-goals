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
    plans <- c(plans, switch(goal$schedule,
      fixed = plan_fixed_reminder(goal, local_time, sent_keys),
      quota = plan_quota_reminders(goal, local_time, sent_keys, checkin_log)
    ))
  }
  plans
}

#' Has a goal's start date arrived?
#'
#' Goals may be configured ahead of time, and nudging before the start date would
#' log periods the goal was never meant to cover.
has_started <- function(goal, local_time) {
  if (is.null(goal$starts)) return(TRUE)
  as.Date(local_time) >= parse_date(goal$starts, "Goal start")
}

#' Plan the reminder for a goal anchored to a day and time.
#'
#' @return A list of zero or one plan.
plan_fixed_reminder <- function(goal, local_time, sent_keys) {
  if (!has_started(goal, local_time)) return(list())
  if (!is_scheduled_today(goal, local_time)) return(list())
  if (minutes_since_midnight(local_time) < parse_time_of_day(goal$remind_at)) {
    return(list())
  }

  key <- period_key(goal$period, local_time)
  if (paste(goal$id, key, sep = "|") %in% sent_keys) return(list())

  list(list(
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
  ))
}

#' Plan every message a quota goal owes today.
#'
#' Three kinds, in the order they occur through a day: a prompt for each session
#' anchored to today, a late notice for an anchored session that never got
#' logged, and a single nudge covering any sessions free to land on any day.
plan_quota_reminders <- function(goal, local_time, sent_keys, checkin_log) {
  if (!has_started(goal, local_time)) return(list())

  key <- period_key(goal$period, local_time)
  counts <- if (is.null(checkin_log)) integer() else {
    session_counts(checkin_log, goal$id, key)
  }

  c(
    plan_session_prompts(goal, local_time, sent_keys, counts, key, checkin_log),
    plan_missed_notices(goal, local_time, sent_keys, counts, key, checkin_log),
    plan_free_nudge(goal, local_time, sent_keys, counts, key, checkin_log)
  )
}

#' Prompt each session anchored to today, once its reminder time has passed.
plan_session_prompts <- function(goal, local_time, sent_keys, counts, key,
                                 checkin_log) {
  due <- Filter(function(requirement) {
    minutes_since_midnight(local_time) >=
      parse_time_of_day(requirement$remind_at, goal$id) &&
      logged_for_requirement(requirement, counts) <
        requirement$sessions_per_period
  }, sessions_on_day(goal, local_time))

  build_session_plans(goal, due, local_time, sent_keys, key, suffix = NULL,
                     text_of = function(requirement) {
                       format_session_prompt(goal, requirement, counts,
                                             checkin_log, local_time)
                     })
}

#' Report anchored sessions that today's cut-off passed without a check-in.
#'
#' The consequence lands the same night, so the notice has to arrive before bed
#' rather than at the end of the week.
plan_missed_notices <- function(goal, local_time, sent_keys, counts, key,
                                checkin_log) {
  if (is.null(goal$missed_notice_at)) return(list())
  if (minutes_since_midnight(local_time) <
        parse_time_of_day(goal$missed_notice_at, goal$id)) {
    return(list())
  }

  missed <- Filter(function(requirement) {
    logged_for_requirement(requirement, counts) <
      requirement$sessions_per_period
  }, sessions_on_day(goal, local_time))

  build_session_plans(goal, missed, local_time, sent_keys, key,
                      suffix = "missed",
                      text_of = function(requirement) {
                        format_missed_notice(goal, requirement, counts,
                                             checkin_log, local_time)
                      })
}

#' Turn a set of requirements into per-session plans with a logging button.
#'
#' Keys are scoped by date and requirement so two sessions on one day, or a
#' prompt and its later missed notice, never collide in the sent log.
build_session_plans <- function(goal, requirements, local_time, sent_keys, key,
                                suffix, text_of) {
  today <- format(as.Date(local_time), "%Y-%m-%d")

  plans <- lapply(requirements, function(requirement) {
    reminder_key <- paste(c(today, requirement$id, suffix), collapse = ":")
    if (paste(goal$id, reminder_key, sep = "|") %in% sent_keys) return(NULL)

    list(
      goal_id = goal$id,
      reminder_key = reminder_key,
      text = text_of(requirement),
      buttons = inline_keyboard(
        labels = if (is.null(suffix)) "Done" else "I did train",
        payloads = build_callback_payload(goal$id, key, requirement$id,
                                         "session")
      )
    )
  })
  Filter(Negate(is.null), plans)
}

#' Plan the single daily nudge for sessions not tied to a weekday.
#'
#' Under the default `risk_only` cadence a message is sent only on the kickoff
#' day or once skipping a day would make the period impossible, which keeps
#' reminders informative instead of habituating.
plan_free_nudge <- function(goal, local_time, sent_keys, counts, key,
                            checkin_log) {
  if (is.null(goal$nudge)) return(list())
  if (minutes_since_midnight(local_time) <
        parse_time_of_day(goal$nudge$at, goal$id)) {
    return(list())
  }

  reminder_key <- format(as.Date(local_time), "%Y-%m-%d")
  if (paste(goal$id, reminder_key, sep = "|") %in% sent_keys) return(list())

  # Anchored sessions have their own prompts, so the nudge speaks only for the
  # ones that still need a day chosen for them.
  free_only <- goal
  free_only$requirements <- Filter(function(requirement) {
    is.null(requirement$on_day)
  }, goal$requirements)
  if (length(free_only$requirements) == 0) return(list())

  progress <- requirement_progress(free_only, counts, local_time)
  remaining <- total_remaining(progress)
  if (remaining == 0L) return(list())

  days_left <- days_left_in_period(goal$period, local_time)
  at_risk <- is_at_risk(remaining, days_left)
  is_kickoff <- !is.null(goal$nudge$kickoff_on) &&
    local_time$wday == weekday_number(goal$nudge$kickoff_on, goal$id)
  has_urgent <- any(vapply(progress, is_requirement_urgent, logical(1)))

  if (!(goal$nudge$cadence == "daily" || at_risk || is_kickoff || has_urgent)) {
    return(list())
  }

  outstanding <- Filter(function(item) item$remaining > 0L, progress)

  list(list(
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
  ))
}

#' Compose the reminder for a fixed goal.
#'
#' The implementation intention is quoted back verbatim, because the if-then cue
#' is what carries the behavioural effect, not the goal title.
format_fixed_reminder <- function(goal) {
  lines <- c(goal$title)

  cue <- format_intention(goal$implementation_intention)
  if (!is.null(cue)) lines <- c(lines, "", paste("Your plan:", cue))
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

#' Compose the prompt for one anchored session.
#'
#' Leads with this session's own cue, because the specific when and where is what
#' drives follow-through, and follows with the standings the session feeds into.
format_session_prompt <- function(goal, requirement, counts, checkin_log,
                                  local_time) {
  lines <- c(sprintf("Today: %s", requirement$label))

  cue <- format_intention(requirement$implementation_intention)
  if (!is.null(cue)) lines <- c(lines, cue)

  lines <- c(lines, "", format_period_standing(goal, counts),
             format_commitment_lines(goal, checkin_log, local_time))

  coping <- requirement$coping_plan %||% goal$coping_plan
  if (!is.null(coping)) {
    lines <- c(lines, "", format_coping(goal$obstacle, coping))
  }
  if (!is.null(goal$missed_session_consequence)) {
    lines <- c(lines, sprintf("Skip it and: %s", goal$missed_session_consequence))
  }
  paste(lines, collapse = "\n")
}

#' Compose the late notice for an anchored session that was not logged.
format_missed_notice <- function(goal, requirement, counts, checkin_log,
                                 local_time) {
  still_to_come <- remaining_scheduled_sessions(goal, counts, local_time)

  lines <- c(sprintf("Missed: %s", requirement$label), "",
             sprintf("Tonight: %s", goal$missed_session_consequence), "",
             format_period_standing(goal, counts),
             sprintf("%d session(s) still scheduled this %s.", still_to_come,
                     goal$period),
             format_commitment_lines(goal, checkin_log, local_time),
             "", "Trained and forgot to log it? Tap below.")
  paste(lines, collapse = "\n")
}

#' Compose the nudge for sessions that can land on any day.
format_quota_reminder <- function(goal, progress, days_left, at_risk, is_kickoff,
                                  checkin_log, local_time) {
  urgent <- Filter(is_requirement_urgent, progress)

  headline <- if (is_kickoff) {
    sprintf("New %s: %s", goal$period, goal$title)
  } else if (length(urgent) > 0) {
    sprintf("%s: %s is out of time", goal$title, urgent[[1]]$label)
  } else if (at_risk) {
    sprintf("%s: no room left to skip", goal$title)
  } else {
    goal$title
  }
  lines <- c(headline, "")

  for (item in progress) {
    lines <- c(lines, format_requirement_line(item))
  }

  lines <- c(lines, "", sprintf("%d session(s) left, %d day(s) to fit them in.",
                                total_remaining(progress), days_left))
  if (at_risk) {
    lines <- c(lines, "Training today is the only way to keep the week intact.")
  }

  if (!is.null(goal$coping_plan) &&
      (at_risk || is_kickoff || length(urgent) > 0)) {
    lines <- c(lines, "", format_coping(goal$obstacle, goal$coping_plan))
  }

  # A requirement whose own deadline is closing gets its specific fallback,
  # which the general coping plan cannot express.
  for (item in urgent) {
    if (!is.null(item$coping_plan)) {
      lines <- c(lines, sprintf("Fallback for %s: %s", item$label,
                                item$coping_plan))
    }
  }

  lines <- c(lines, "", format_commitment_lines(goal, checkin_log, local_time))
  paste(lines, collapse = "\n")
}

#' Sessions done against sessions asked for, in the current period.
format_period_standing <- function(goal, counts) {
  sprintf("This %s: %d of %d sessions.", goal$period,
          completed_sessions(goal, counts), required_sessions(goal))
}

#' Phrase a coping plan as an if-then when an obstacle is named.
format_coping <- function(obstacle, coping_plan) {
  if (is.null(obstacle)) return(sprintf("If it slips: %s", coping_plan))
  sprintf("If %s, then %s", obstacle, coping_plan)
}

#' One progress line for a requirement, with its deadline and cue.
format_requirement_line <- function(item) {
  line <- sprintf("%s: %d of %d", item$label, item$logged, item$required)

  deadline <- item$on_day %||% item$by_day
  if (!is.null(deadline) && item$remaining > 0L) {
    line <- if (!is.na(item$days_until_due) && item$days_until_due < 1L) {
      paste0(line, " (was due ", deadline, ")")
    } else {
      paste0(line, " (by ", deadline, ")")
    }
  }

  cue <- format_intention(item$intention)
  if (!is.null(cue) && item$remaining > 0L) line <- paste0(line, " - ", cue)

  line
}

#' Join the parts of an implementation intention that are present.
#'
#' Any of when, where and what may be omitted, so the phrase is assembled from
#' whatever the goal actually specifies rather than printing empty fragments.
format_intention <- function(intention) {
  if (is.null(intention)) return(NULL)

  parts <- unlist(intention[c("when", "where", "what")], use.names = FALSE)
  if (length(parts) == 0) return(NULL)
  paste(parts, collapse = ", ")
}

#' One line per commitment that is currently running.
#'
#' @return A character vector, empty when nothing is running or no log is given.
format_commitment_lines <- function(goal, checkin_log, local_time) {
  if (is.null(checkin_log)) return(character())

  running <- Filter(function(status) status$is_active,
                    evaluate_commitments(goal, checkin_log, local_time))
  vapply(running, format_commitment_line, character(1), period = goal$period)
}

#' Standing of one commitment: periods behind it, periods short, what is left.
format_commitment_line <- function(status, period) {
  outlook <- if (status$is_lost) "Gone, now" else "Still on for"

  sprintf("%s: %d of %d %ss in, %d short. %s: %s",
          status$label, status$finished_periods, status$total_periods, period,
          status$shortfall_periods, outlook, status$best_achievable)
}
