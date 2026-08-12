#' Collecting check-in button taps.
#'
#' GitHub Actions is not a running server, so button taps are polled with
#' `getUpdates` rather than received on a webhook. Telegram queues updates for
#' 24 hours, and the stored offset marks which ones have been consumed.

#' Poll Telegram and record any check-in button taps.
#'
#' @param config A `telegram_config` object.
#' @param timezone Timezone used to stamp the local date of a session, which is
#'   what makes repeated taps count once.
#' @param now Current time; injected for testability.
#' @return Number of check-ins recorded.
collect_checkins <- function(config, timezone, now = Sys.time()) {
  offset <- read_update_offset()
  updates <- telegram_call(config, "getUpdates", list(
    offset = offset,
    timeout = 0,
    limit = 100,
    allowed_updates = list("callback_query")
  ))

  if (length(updates) == 0) return(0L)

  local_date <- format(as.Date(local_time_now(timezone, now)), "%Y-%m-%d")
  recorded <- 0L
  highest_update_id <- offset
  for (update in updates) {
    highest_update_id <- max(highest_update_id, update$update_id)

    callback <- update$callback_query
    if (is.null(callback)) {
      message("Skipping update ", update$update_id, ": not a button tap")
      next
    }
    checkin <- parse_callback_payload(callback$data)
    if (is.null(checkin)) next

    append_log(CHECKIN_LOG_PATH, CHECKIN_LOG_COLUMNS, list(
      goal_id = checkin$goal_id,
      period_key = checkin$period_key,
      requirement_id = checkin$requirement_id,
      outcome = checkin$outcome,
      local_date = local_date,
      recorded_at_utc = utc_timestamp(now)
    ))
    recorded <- recorded + 1L
    message("Recorded '", checkin$outcome, "' for '", checkin$goal_id,
            "' (", checkin$requirement_id, ")")

    # Acknowledgement is cosmetic and can legitimately fail: Telegram expires
    # callback query ids after roughly a minute, so a tap collected on a later
    # polling cycle cannot be answered. The check-in is already recorded, so a
    # failure here must not abort the run and strand the remaining updates.
    tryCatch(
      acknowledge_checkin(config, callback, checkin),
      error = function(condition) {
        warning("Could not acknowledge tap on update ", update$update_id, ": ",
                conditionMessage(condition), call. = FALSE)
      }
    )
  }

  # Advancing past every update we saw, including ones we skipped, prevents the
  # same malformed update being re-fetched forever.
  write_update_offset(highest_update_id + 1L)
  recorded
}

#' Clear the button spinner and mark the message as recorded.
#'
#' Omitting reply_markup on editMessageText removes the buttons, so a check-in
#' cannot be double-tapped.
acknowledge_checkin <- function(config, callback, checkin) {
  telegram_call(config, "answerCallbackQuery", list(
    callback_query_id = callback$id,
    text = paste("Recorded:", checkin$outcome)
  ))

  original_text <- callback$message$text
  if (is.null(original_text)) return(invisible(NULL))

  label <- if (checkin$requirement_id == NO_REQUIREMENT) {
    checkin$outcome
  } else {
    paste(checkin$outcome, checkin$requirement_id)
  }
  telegram_call(config, "editMessageText", list(
    chat_id = callback$message$chat$id,
    message_id = callback$message$message_id,
    text = paste0(original_text, "\n\n[recorded: ", label, "]")
  ))
  invisible(NULL)
}
