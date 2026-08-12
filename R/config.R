#' Configuration loading and validation.
#'
#' Credentials are read from the environment so the same scripts run locally
#' (via a gitignored .Renviron) and in CI (via repository secrets).

#' Build a validated Telegram configuration object.
#'
#' @param token Bot token issued by BotFather.
#' @param chat_id Numeric chat id of the recipient.
#' @return An object of class `telegram_config`.
telegram_config <- function(token = Sys.getenv("TELEGRAM_BOT_TOKEN"),
                            chat_id = Sys.getenv("TELEGRAM_CHAT_ID")) {
  if (!nzchar(token)) {
    stop("TELEGRAM_BOT_TOKEN is not set. Add it to .Renviron locally, or to ",
         "repository secrets in CI.", call. = FALSE)
  }
  if (!nzchar(chat_id)) {
    stop("TELEGRAM_CHAT_ID is not set. Add it to .Renviron locally, or to ",
         "repository secrets in CI.", call. = FALSE)
  }
  # Fail loudly on a malformed token rather than surfacing an opaque 404 from
  # the API, which is what Telegram returns for a bad token path segment.
  if (!grepl("^[0-9]+:[A-Za-z0-9_-]+$", token)) {
    stop("TELEGRAM_BOT_TOKEN is malformed; expected '<digits>:<alphanumeric>'.",
         call. = FALSE)
  }
  if (!grepl("^-?[0-9]+$", chat_id)) {
    stop("TELEGRAM_CHAT_ID must be numeric; got '", chat_id, "'.", call. = FALSE)
  }

  structure(list(token = token, chat_id = chat_id), class = "telegram_config")
}
