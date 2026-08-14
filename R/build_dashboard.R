#' Build the dashboard from the command line.
#'
#' Usage:
#'   Rscript R/build_dashboard.R
#'   Rscript R/build_dashboard.R --at "2026-09-14 21:00"
#'   Rscript R/build_dashboard.R --log tmp.csv --out _scratch/index.html
#'
#' The cycle rebuilds the page on every run, so this exists for looking at the
#' result by hand. --log and --out render sample data without touching the real
#' log or the published page.

source("R/config.R")
source("R/store.R")
source("R/goals.R")
source("R/quota.R")
source("R/dashboard_model.R")
source("R/dashboard_html.R")
source("R/dashboard.R")

#' Read the value following a named flag, or a default when the flag is absent.
flag_value <- function(flag, arguments, fallback = NULL) {
  position <- match(flag, arguments)
  if (is.na(position)) return(fallback)
  if (position == length(arguments)) {
    stop("Flag ", flag, " needs a value.", call. = FALSE)
  }
  arguments[position + 1L]
}

arguments <- commandArgs(trailingOnly = TRUE)
definitions <- load_goals()

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

log_path <- flag_value("--log", arguments, CHECKIN_LOG_PATH)
result <- build_dashboard(definitions,
                         read_log(log_path, CHECKIN_LOG_COLUMNS), now,
                         flag_value("--out", arguments, DASHBOARD_PATH))

cat(if (result$changed) "Wrote" else "Unchanged:", result$path, "\n")
