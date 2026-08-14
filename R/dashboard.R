#' Writing the dashboard to disk.
#'
#' Kept apart from the model and the markup so that the only thing here is the
#' file handling.

DASHBOARD_PATH <- file.path("docs", "index.html")

#' Write text as UTF-8 regardless of the platform's default encoding.
#'
#' A Windows session would otherwise write the file in its native encoding while
#' the page declares UTF-8, which would then be served as mojibake.
write_utf8 <- function(text, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeLines(enc2utf8(text), connection, useBytes = TRUE)
  invisible(path)
}

#' Write only when the content differs.
#'
#' The page is rebuilt every cycle and committed by CI, so writing an identical
#' file would put an empty change into the history every half hour.
#'
#' @return TRUE when the file was written.
write_if_changed <- function(text, path) {
  if (file.exists(path)) {
    existing <- paste(readLines(path, warn = FALSE), collapse = "\n")
    if (identical(existing, text)) return(FALSE)
  }
  write_utf8(text, path)
  TRUE
}

#' Build the dashboard.
#'
#' @param definitions Output of `load_goals()`.
#' @param checkin_log Check-in log as read from disk.
#' @param now Current time; injected for testability.
#' @param path Destination file.
#' @return A list of the `path` and whether it `changed`.
build_dashboard <- function(definitions, checkin_log, now = Sys.time(),
                            path = DASHBOARD_PATH) {
  if (is.null(definitions$dashboard)) {
    stop("goals.yml has no `dashboard` block, so there is nowhere to publish.",
         call. = FALSE)
  }
  # The dashboard reports session counts against reward windows, which only a
  # quota goal has.
  tracked <- Filter(function(goal) goal$schedule == "quota", definitions$goals)
  if (length(tracked) == 0) {
    stop("No quota goal to build a dashboard from.", call. = FALSE)
  }

  local_time <- local_time_now(definitions$timezone, now)
  model <- dashboard_model(tracked[[1]], checkin_log, local_time,
                          definitions$dashboard)

  list(path = path, changed = write_if_changed(dashboard_html(model), path))
}
