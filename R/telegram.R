#' Thin wrapper over the Telegram Bot API.
#'
#' Every outbound HTTP call in the project goes through `telegram_call()` so
#' error handling and response validation live in exactly one place.

library(httr2)

TELEGRAM_API_ROOT <- "https://api.telegram.org"

#' Invoke a Telegram Bot API method.
#'
#' @param config A `telegram_config` object.
#' @param method Bot API method name, e.g. "sendMessage".
#' @param body Named list of method parameters.
#' @return The parsed `result` field of the API response.
telegram_call <- function(config, method, body = list()) {
  stopifnot(inherits(config, "telegram_config"), nzchar(method))

  response <- request(TELEGRAM_API_ROOT) |>
    req_url_path_append(paste0("bot", config$token), method) |>
    req_body_json(body) |>
    req_retry(max_tries = 3) |>
    # Handle failures here so the message includes Telegram's own description,
    # which is far more useful than the bare status code httr2 would report.
    req_error(is_error = function(response) FALSE) |>
    req_perform()

  parsed <- resp_body_json(response)
  if (!isTRUE(parsed$ok)) {
    stop(sprintf("Telegram %s failed (HTTP %s): %s", method,
                 resp_status(response),
                 parsed$description %||% "no description returned"),
         call. = FALSE)
  }
  parsed$result
}

#' Send a plain or button-bearing message to the configured chat.
#'
#' @param config A `telegram_config` object.
#' @param text Message body. Markdown is escaped by Telegram unless
#'   `parse_mode` is supplied, so plain text is passed through as-is.
#' @param buttons Optional inline keyboard, as built by `inline_keyboard()`.
#' @return The sent message object.
send_message <- function(config, text, buttons = NULL) {
  stopifnot(nzchar(text))

  body <- list(chat_id = config$chat_id, text = text)
  if (!is.null(buttons)) {
    body$reply_markup <- list(inline_keyboard = buttons)
  }
  telegram_call(config, "sendMessage", body)
}

#' Build a single-row inline keyboard.
#'
#' @param labels Character vector of button captions.
#' @param payloads Character vector of callback payloads, same length as labels.
#' @return A list shaped as Telegram's `inline_keyboard` (array of button rows).
inline_keyboard <- function(labels, payloads) {
  stopifnot(length(labels) == length(payloads), length(labels) > 0)

  row <- Map(function(label, payload) {
    # Telegram rejects callback_data longer than 64 bytes.
    if (nchar(payload, type = "bytes") > 64) {
      stop("callback payload exceeds Telegram's 64-byte limit: ", payload,
           call. = FALSE)
    }
    list(text = label, callback_data = payload)
  }, labels, payloads)

  list(unname(row))
}
