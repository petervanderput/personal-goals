#' Sending due reminders.

#' Send a reminder for every goal that is due and not already reminded.
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
  due <- due_goals(definitions$goals, local_time, already_sent_keys(reminder_log))

  for (goal in due) {
    send_message(
      config,
      text = format_reminder(goal),
      buttons = inline_keyboard(
        labels = c("Done", "Missed"),
        payloads = c(
          build_callback_payload(goal$id, goal$period_key, "done"),
          build_callback_payload(goal$id, goal$period_key, "missed")
        )
      )
    )
    append_log(REMINDER_LOG_PATH, REMINDER_LOG_COLUMNS, list(
      goal_id = goal$id,
      period_key = goal$period_key,
      sent_at_utc = utc_timestamp(now)
    ))
    message("Sent reminder for '", goal$id, "' (", goal$period_key, ")")
  }

  length(due)
}
