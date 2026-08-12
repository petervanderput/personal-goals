#' Configuration loading and validation.
#'
#' Credentials are read from the environment so the same scripts run locally
#' (via a gitignored .Renviron) and in CI (via repository secrets).

#' Packages required at runtime.
#'
#' jsonlite is listed explicitly because httr2 only suggests it, so installing
#' httr2 alone leaves API calls to fail deep inside req_body_json().
REQUIRED_PACKAGES <- c("httr2", "jsonlite", "yaml")

#' Fail immediately if any required package is unavailable.
#'
#' @param packages Character vector of package names.
require_packages <- function(packages = REQUIRED_PACKAGES) {
  absent <- packages[!vapply(packages, requireNamespace, logical(1),
                             quietly = TRUE)]
  if (length(absent) > 0) {
    stop("Missing required package(s): ", paste(absent, collapse = ", "),
         ". Install with install.packages(c(\"",
         paste(absent, collapse = "\", \""), "\")).", call. = FALSE)
  }
  invisible(packages)
}

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
