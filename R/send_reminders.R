#' Sending planned reminders.

#' Send every reminder the planner says is due.
#'
#' The reminder is logged only after the send succeeds, so a transient API
#' failure is retried on the next cycle instead of being silently skipped.
#'
#' @param config A `telegram_config` object.
#' @param definitions Output of `load_goals()`.
#' @param now Current time; injected for testability.
#' @return Number of reminders sent.
send_due_reminders <- function(config, definitions, now = Sys.time()) {
  local_time <- local_time_now(definitions$timezone, now)
  reminder_log <- read_log(REMINDER_LOG_PATH, REMINDER_LOG_COLUMNS)
  checkin_log <- read_log(CHECKIN_LOG_PATH, CHECKIN_LOG_COLUMNS)

  plans <- plan_reminders(definitions, local_time,
                          already_sent_keys(reminder_log), checkin_log)

  for (plan in plans) {
    send_message(config, text = plan$text, buttons = plan$buttons)
    append_log(REMINDER_LOG_PATH, REMINDER_LOG_COLUMNS, list(
      goal_id = plan$goal_id,
      reminder_key = plan$reminder_key,
      sent_at_utc = utc_timestamp(now)
    ))
    message("Sent reminder for '", plan$goal_id, "' (", plan$reminder_key, ")")
  }

  length(plans)
}
