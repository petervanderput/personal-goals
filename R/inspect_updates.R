#' Dump every update still pending on Telegram's side.
#'
#' Diagnostic only. Uses offset 0 and no type filter, which returns the full
#' pending queue without confirming (and therefore without discarding) anything.

source("R/config.R")
source("R/telegram.R")
source("R/store.R")

config <- telegram_config()

cat("Stored offset:", read_update_offset(), "\n\n")

updates <- telegram_call(config, "getUpdates", list(offset = 0, timeout = 0,
                                                   limit = 100))
if (length(updates) == 0) {
  cat("Telegram has no pending updates.\n")
} else {
  cat("Pending updates:", length(updates), "\n")
  for (update in updates) {
    kind <- if (!is.null(update$callback_query)) {
      paste0("callback_query data='", update$callback_query$data, "'")
    } else if (!is.null(update$message)) {
      paste0("message text='", update$message$text %||% "", "'")
    } else {
      paste(names(update), collapse = ",")
    }
    cat(sprintf("  update_id=%s  %s\n", update$update_id, kind))
  }
}
