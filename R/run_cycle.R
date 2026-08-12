#' Scheduler entrypoint.
#'
#' One cycle both sends due reminders and collects outstanding button taps.
#' They share a single invocation because CI minutes are the binding constraint:
#' two separate scheduled workflows would double the setup cost per hour for no
#' functional gain.

source("R/config.R")
source("R/telegram.R")
source("R/store.R")
source("R/goals.R")
source("R/collect_checkins.R")
source("R/send_reminders.R")

config <- telegram_config()
definitions <- load_goals()
now <- Sys.time()

reminders_sent <- send_due_reminders(config, definitions, now)
checkins_recorded <- collect_checkins(config, now)

cat(sprintf("Cycle complete at %s: %d reminder(s) sent, %d check-in(s) recorded.\n",
            utc_timestamp(now), reminders_sent, checkins_recorded))
