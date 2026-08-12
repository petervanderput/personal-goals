#' One-off connectivity check.
#'
#' Confirms the bot token and chat id work, and that inline buttons render,
#' before any scheduling or goal logic is layered on top.

source("R/config.R")
source("R/telegram.R")

config <- telegram_config()

identity <- telegram_call(config, "getMe")
cat("Bot identity: @", identity$username, " (", identity$first_name, ")\n", sep = "")

invisible(send_message(
  config,
  text = paste(
    "Connection test from your Personal Goals project.",
    "If you can see the two buttons below, the notification loop works.",
    sep = "\n\n"
  ),
  buttons = inline_keyboard(
    labels   = c("Done", "Missed"),
    payloads = c("test|done", "test|missed")
  )
))

cat("Test message sent to chat", config$chat_id, "\n")
