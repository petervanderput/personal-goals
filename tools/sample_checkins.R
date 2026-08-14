#' Generate a sample check-in log for developing the dashboard against.
#'
#' Usage: Rscript tools/sample_checkins.R [path]
#'
#' Real data will not exist in any volume until December, and a page that has only
#' ever been seen empty has not really been seen. The weeks below deliberately
#' cover every day status the dashboard can show.

source("R/store.R")

path <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(path)) path <- file.path("_scratch", "sample.csv")
if (file.exists(path)) file.remove(path)

# Week of 17 Aug: all four sessions, clean.
# Week of 24 Aug: Wednesday made up on the Thursday, Friday never done.
# Week of 31 Aug: Monday never done, the other three fine.
# Week of  7 Sep: in progress.
entries <- list(
  c("2026-W34", "mon", "2026-08-17"),
  c("2026-W34", "wed", "2026-08-19"),
  c("2026-W34", "fri", "2026-08-21"),
  c("2026-W34", "sat", "2026-08-22"),

  c("2026-W35", "mon", "2026-08-24"),
  c("2026-W35", "wed", "2026-08-27"),
  c("2026-W35", "sat", "2026-08-29"),

  c("2026-W36", "wed", "2026-09-02"),
  c("2026-W36", "fri", "2026-09-04"),
  c("2026-W36", "sat", "2026-09-05"),

  c("2026-W37", "mon", "2026-09-07"),
  c("2026-W37", "wed", "2026-09-09")
)

for (entry in entries) {
  append_log(path, CHECKIN_LOG_COLUMNS, list(
    goal_id = "boxing", period_key = entry[1], requirement_id = entry[2],
    outcome = "session", local_date = entry[3],
    recorded_at_utc = paste0(entry[3], "T23:00:00Z")
  ))
}

cat("Wrote", length(entries), "sample check-ins to", path, "\n")
