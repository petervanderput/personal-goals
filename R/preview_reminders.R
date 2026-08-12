#' Preview the reminders that would be sent at a given moment.
#'
#' Usage:
#'   Rscript R/preview_reminders.R
#'   Rscript R/preview_reminders.R --at "2026-08-17 17:30"
#'   Rscript R/preview_reminders.R --at "2026-08-17 17:30" --send
#'
#' Without --send nothing leaves the machine, which makes this the safe way to
#' check wording and timing rules against a future date. With --send the message
#' is delivered but deliberately not written to the reminder log, so a preview
#' never suppresses the real reminder.

source("R/config.R")
require_packages()
source("R/telegram.R")
source("R/store.R")
source("R/goals.R")
source("R/quota.R")
source("R/callbacks.R")
source("R/reminders.R")

arguments <- commandArgs(trailingOnly = TRUE)

#' Read the value following a named flag, or NULL when the flag is absent.
flag_value <- function(flag, arguments) {
  position <- match(flag, arguments)
  if (is.na(position)) return(NULL)
  if (position == length(arguments)) {
    stop("Flag ", flag, " needs a value.", call. = FALSE)
  }
  arguments[position + 1L]
}

definitions <- load_goals()
should_send <- "--send" %in% arguments
requested_time <- flag_value("--at", arguments)

now <- if (is.null(requested_time)) {
  Sys.time()
} else {
  parsed <- as.POSIXct(requested_time, tz = definitions$timezone)
  if (is.na(parsed)) {
    stop("Could not parse --at '", requested_time,
         "'; expected \"YYYY-MM-DD HH:MM\".", call. = FALSE)
  }
  parsed
}

local_time <- local_time_now(definitions$timezone, now)
checkin_log <- read_log(CHECKIN_LOG_PATH, CHECKIN_LOG_COLUMNS)

cat("Simulated local time:",
    format(local_time, "%Y-%m-%d %H:%M %Z"), "\n")
cat("Check-ins on record:", nrow(checkin_log), "\n\n")

# Sent keys are deliberately empty: a preview answers "what would be sent at
# this moment", not "what is still outstanding".
plans <- plan_reminders(definitions, local_time, character(), checkin_log)

if (length(plans) == 0) {
  cat("No reminders would be sent at that moment.\n")
} else {
  for (plan in plans) {
    cat("--- ", plan$goal_id, " (key ", plan$reminder_key, ") ---\n", sep = "")
    cat(plan$text, "\n\n")
    cat("buttons:",
        paste(vapply(plan$buttons[[1]], `[[`, character(1), "text"),
              collapse = " | "), "\n\n")
  }

  if (should_send) {
    config <- telegram_config()
    for (plan in plans) {
      invisible(send_message(config, plan$text, plan$buttons))
      cat("Sent preview for", plan$goal_id, "(not recorded in the log)\n")
    }
  }
}
