#' Scheduler entrypoint.
#'
#' One cycle both sends due reminders and collects outstanding button taps.
#' They share a single invocation because CI minutes are the binding constraint:
#' two separate scheduled workflows would double the setup cost per hour for no
#' functional gain.

# Report warnings as they happen rather than batched at exit, so that in a CI
# log they appear next to the operation that raised them.
options(warn = 1)

source("R/config.R")
source("R/telegram.R")
source("R/store.R")
source("R/goals.R")
source("R/collect_checkins.R")
source("R/send_reminders.R")

#' Run one labelled phase, reporting where a failure happened before exiting.
#'
#' Any partial work already written to disk is intentionally left in place: the
#' workflow commits the logs even on failure, because a check-in recorded here
#' will never be delivered by Telegram a second time.
run_phase <- function(label, action) {
  cat("[phase]", label, "\n")
  tryCatch(
    action(),
    error = function(condition) {
      cat("[failed]", label, "\n")
      cat("[error]", conditionMessage(condition), "\n")
      calls <- sys.calls()
      if (length(calls) > 0) {
        cat("[where]", paste(utils::head(deparse(conditionCall(condition)), 3),
                             collapse = " "), "\n")
      }
      quit(status = 1)
    }
  )
}

cat("R", paste(R.version$major, R.version$minor, sep = "."),
    "| httr2", as.character(packageVersion("httr2")),
    "| yaml", as.character(packageVersion("yaml")), "\n")

config <- run_phase("load credentials", function() telegram_config())
definitions <- run_phase("load goal definitions", function() load_goals())
now <- Sys.time()

cat("[time] UTC", utc_timestamp(now), "| local",
    format(local_time_now(definitions$timezone, now), "%Y-%m-%d %H:%M %Z"), "\n")

reminders_sent <- run_phase("send due reminders",
                            function() send_due_reminders(config, definitions, now))
checkins_recorded <- run_phase("collect check-ins",
                               function() collect_checkins(config, now))

cat(sprintf("Cycle complete: %d reminder(s) sent, %d check-in(s) recorded.\n",
            reminders_sent, checkins_recorded))
