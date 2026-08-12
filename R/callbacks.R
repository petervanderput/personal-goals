#' Callback payloads carried by check-in buttons.
#'
#' Payloads are built when a reminder is sent and parsed when a tap comes back,
#' so both directions live here and can never drift apart.
#'
#' Format: goal_id|period_key|requirement_id|outcome
#'
#' Telegram caps callback data at 64 bytes, which is why identifiers are
#' validated as short and free of the separator.

CALLBACK_SEPARATOR <- "|"
CALLBACK_FIELDS <- 4L
VALID_OUTCOMES <- c("done", "missed", "session")

# Placeholder for goals that have no requirements, keeping the payload a fixed
# number of fields rather than a variable one.
NO_REQUIREMENT <- "-"

#' Build the callback payload for a check-in button.
build_callback_payload <- function(goal_id, period_key,
                                   requirement_id = NO_REQUIREMENT, outcome) {
  if (!outcome %in% VALID_OUTCOMES) {
    stop("Unknown check-in outcome '", outcome, "'.", call. = FALSE)
  }
  payload <- paste(goal_id, period_key, requirement_id, outcome,
                   sep = CALLBACK_SEPARATOR)

  if (nchar(payload, type = "bytes") > 64L) {
    stop("Callback payload exceeds Telegram's 64-byte limit: ", payload,
         call. = FALSE)
  }
  payload
}

#' Parse a callback payload, or return NULL if it is not a check-in.
#'
#' Unrecognised payloads are warned about and skipped rather than aborting the
#' cycle, so one stray button cannot block every later check-in.
parse_callback_payload <- function(payload) {
  if (!is.character(payload) || length(payload) != 1L) return(NULL)

  parts <- strsplit(payload, CALLBACK_SEPARATOR, fixed = TRUE)[[1]]
  if (length(parts) != CALLBACK_FIELDS || !parts[4] %in% VALID_OUTCOMES) {
    warning("Ignoring unrecognised callback payload: '", payload, "'",
            call. = FALSE)
    return(NULL)
  }
  list(goal_id = parts[1], period_key = parts[2], requirement_id = parts[3],
       outcome = parts[4])
}
