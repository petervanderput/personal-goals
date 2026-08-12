#' Report the chat ids that have interacted with the bot.
#'
#' Telegram refuses to deliver to a chat that has never messaged the bot, so
#' this reads the authoritative chat id from `getUpdates` rather than trusting
#' a hand-copied value.

source("R/config.R")
source("R/telegram.R")

config <- telegram_config()

updates <- telegram_call(config, "getUpdates", list(timeout = 0, limit = 100))

if (length(updates) == 0) {
  cat("No updates yet. Open Telegram, message @PVDPBot, then re-run.\n")
} else {
  cat("Found", length(updates), "update(s):\n")
  for (update in updates) {
    chat <- update$message$chat %||% update$callback_query$message$chat
    if (is.null(chat)) next
    cat(sprintf("  chat_id=%s  type=%s  name=%s %s  username=%s\n",
                chat$id, chat$type,
                chat$first_name %||% "", chat$last_name %||% "",
                chat$username %||% "-"))
  }
  cat("\nConfigured TELEGRAM_CHAT_ID:", config$chat_id, "\n")
}
