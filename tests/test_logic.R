#' Tests for the pure scheduling, quota, formatting and storage logic.
#'
#' Run with: Rscript tests/test_logic.R
#' Deliberately dependency-free so it runs anywhere R does, and touches no
#' network and no files outside a temporary directory.

source("R/goals.R")
source("R/quota.R")
source("R/callbacks.R")
source("R/store.R")
source("R/telegram.R")
source("R/reminders.R")

failures <- 0L

check <- function(description, condition) {
  if (isTRUE(condition)) {
    cat("  ok   ", description, "\n", sep = "")
  } else {
    cat("  FAIL ", description, "\n", sep = "")
    failures <<- failures + 1L
  }
}

expect_error <- function(description, expression) {
  raised <- inherits(try(force(expression), silent = TRUE), "try-error")
  check(description, raised)
}

#' Build a POSIXlt in Denver for a given local date and time.
denver <- function(text) as.POSIXlt(text, tz = "America/Denver")

monday <- denver("2026-08-17 17:30:00")     # first day of ISO week 2026-W34
thursday <- denver("2026-08-20 17:30:00")
sunday <- denver("2026-08-23 17:30:00")

boxing <- list(
  id = "boxing",
  title = "Get better and more consistent at boxing",
  period = "week",
  schedule = "quota",
  requirements = list(
    list(id = "short", label = "20-30 min session", sessions_per_period = 3L,
         implementation_intention = list(when = "after lunch",
                                         where = "the office gym")),
    list(id = "long", label = "2 hour session", sessions_per_period = 1L,
         by_day = "Wed", coping_plan = "do the full 2 hours at home",
         implementation_intention = list(when = "6-8pm",
                                         where = "the boxing club"))
  ),
  obstacle = "an unexpected task takes the slot",
  coping_plan = "do 20 minutes at home instead",
  nudge = list(at = "17:00", cadence = "risk_only", kickoff_on = "Mon"),
  block = list(
    periods = 8L, starts = "2026-08-17",
    tiers = list(
      list(max_missed = 0, outcome = "Buy the $100 earphones"),
      list(max_missed = 1, outcome = "Buy the $60 earphones"),
      list(max_missed = 3, outcome = "Buy the $30 earphones")
    ),
    otherwise = "Take all Seamus night shifts for one week"
  )
)

#' Build a check-in log from compact "requirement@date" specifications.
checkin_log_of <- function(..., goal_id = "boxing", key = "2026-W34") {
  entries <- c(...)
  if (length(entries) == 0) return(read_log(tempfile(), CHECKIN_LOG_COLUMNS))

  parts <- strsplit(entries, "@", fixed = TRUE)
  data.frame(
    goal_id = goal_id,
    period_key = key,
    requirement_id = vapply(parts, `[`, character(1), 1L),
    outcome = "session",
    local_date = vapply(parts, `[`, character(1), 2L),
    recorded_at_utc = "2026-08-20T00:00:00Z",
    stringsAsFactors = FALSE
  )
}

cat("Time handling\n")
check("local hour", monday$hour == 17)
check("minutes since midnight", minutes_since_midnight(monday) == 1050)
check("parses HH:MM", parse_time_of_day("07:15") == 435)
expect_error("rejects 24:00", parse_time_of_day("24:00"))
expect_error("rejects malformed time", parse_time_of_day("7:5"))

cat("Period keys\n")
check("daily key", period_key("day", monday) == "2026-08-17")
check("weekly key", period_key("week", monday) == "2026-W34")
check("weekly key is stable across the week",
      period_key("week", sunday) == "2026-W34")
check("monthly key", period_key("month", monday) == "2026-08")
check("yearly key", period_key("year", monday) == "2026")

cat("Days left in period\n")
check("monday leaves seven", days_left_in_period("week", monday) == 7)
check("thursday leaves four", days_left_in_period("week", thursday) == 4)
check("sunday leaves one", days_left_in_period("week", sunday) == 1)
check("day period always one", days_left_in_period("day", thursday) == 1)
check("august leaves twelve from the 20th",
      days_left_in_period("month", thursday) == 12)

cat("Quota counting\n")
empty_counts <- session_counts(checkin_log_of(), "boxing", "2026-W34")
check("empty log counts nothing", length(empty_counts) == 0)

logged <- checkin_log_of("short@2026-08-17", "short@2026-08-18",
                         "long@2026-08-19")
counts <- session_counts(logged, "boxing", "2026-W34")
check("counts short sessions", counts[["short"]] == 2)
check("counts long sessions", counts[["long"]] == 1)

repeated <- checkin_log_of("short@2026-08-17", "short@2026-08-17")
check("two taps on one day count once",
      session_counts(repeated, "boxing", "2026-W34")[["short"]] == 1)

check("other periods are excluded",
      length(session_counts(logged, "boxing", "2026-W35")) == 0)
check("other goals are excluded",
      length(session_counts(logged, "running", "2026-W34")) == 0)

cat("Quota progress\n")
progress <- requirement_progress(boxing, counts)
check("short remaining", progress[[1]]$remaining == 1)
check("long satisfied", progress[[2]]$remaining == 0)
check("total remaining", total_remaining(progress) == 1)
check("period not yet satisfied", !is_period_satisfied(boxing, counts))

complete <- session_counts(
  checkin_log_of("short@2026-08-17", "short@2026-08-18", "short@2026-08-19",
                 "long@2026-08-20"),
  "boxing", "2026-W34")
check("period satisfied when all requirements met",
      is_period_satisfied(boxing, complete))

check("overshooting does not go negative",
      requirement_progress(boxing, c(short = 5L))[[1]]$remaining == 0)

cat("Risk detection\n")
check("four owed with four days left is at risk", is_at_risk(4L, 4L))
check("three owed with four days left is not", !is_at_risk(3L, 4L))
check("five owed with four days left is at risk", is_at_risk(5L, 4L))
check("nothing owed is never at risk", !is_at_risk(0L, 1L))

cat("Per-requirement deadlines\n")
long_requirement <- boxing$requirements[[2]]
short_requirement <- boxing$requirements[[1]]
check("monday leaves three days to wednesday",
      days_until_requirement_deadline(long_requirement, monday) == 3)
check("wednesday is the deadline itself",
      days_until_requirement_deadline(long_requirement,
                                      denver("2026-08-19 17:30:00")) == 1)
check("thursday is past the deadline",
      days_until_requirement_deadline(long_requirement, thursday) == 0)
check("no deadline reads as NA",
      is.na(days_until_requirement_deadline(short_requirement, monday)))

deadline_progress <- requirement_progress(boxing, integer(),
                                          denver("2026-08-19 17:30:00"))
check("outstanding requirement at its deadline is urgent",
      is_requirement_urgent(deadline_progress[[2]]))
check("requirement without a deadline is never urgent",
      !is_requirement_urgent(deadline_progress[[1]]))

met <- requirement_progress(boxing, c(long = 1L),
                            denver("2026-08-19 17:30:00"))
check("a satisfied requirement is not urgent", !is_requirement_urgent(met[[2]]))

cat("Intention formatting\n")
check("joins when and where",
      format_intention(list(when = "6-8pm", where = "the club")) ==
        "6-8pm, the club")
check("joins all three parts",
      format_intention(list(when = "a", where = "b", what = "c")) == "a, b, c")
check("no intention yields nothing", is.null(format_intention(NULL)))
check("empty intention yields nothing", is.null(format_intention(list())))

cat("Reward block\n")
keys <- block_period_keys(boxing)
check("block spans eight weeks", length(keys) == 8)
check("block starts at W34", keys[1] == "2026-W34")
check("block ends at W41", keys[8] == "2026-W41")

status <- evaluate_block(boxing, logged, thursday)
check("current week is not judged", status$finished_periods == 0)
check("no misses yet", status$missed_periods == 0)
check("block is active", status$is_active)
check("top tier still available",
      status$best_achievable == "Buy the $100 earphones")

# Two finished weeks, only the first of which was completed.
two_weeks <- rbind(
  checkin_log_of("short@2026-08-17", "short@2026-08-18", "short@2026-08-19",
                 "long@2026-08-20", key = "2026-W34"),
  checkin_log_of("short@2026-08-24", key = "2026-W35")
)
later <- denver("2026-09-01 17:30:00")
progressed <- evaluate_block(boxing, two_weeks, later)
check("two weeks finished", progressed$finished_periods == 2)
check("one week missed", progressed$missed_periods == 1)
check("second tier now the ceiling",
      progressed$best_achievable == "Buy the $60 earphones")

check("four misses fall through to the consequence",
      best_achievable_tier(boxing, 4) ==
        "Take all Seamus night shifts for one week")
check("three misses still earn the lowest tier",
      best_achievable_tier(boxing, 3) == "Buy the $30 earphones")

cat("Reminder planning\n")
definitions <- list(timezone = "America/Denver", goals = list(boxing))

early <- plan_reminders(definitions, denver("2026-08-17 08:00:00"),
                        checkin_log = checkin_log_of())
check("nothing before the nudge time", length(early) == 0)

kickoff <- plan_reminders(definitions, monday, checkin_log = checkin_log_of())
check("kickoff fires on monday", length(kickoff) == 1)
check("kickoff keyed by date", kickoff[[1]]$reminder_key == "2026-08-17")
check("kickoff names the new period", grepl("New week", kickoff[[1]]$text))
check("kickoff offers a button per requirement",
      length(kickoff[[1]]$buttons[[1]]) == 2)

quiet <- plan_reminders(definitions, denver("2026-08-18 17:30:00"),
                        checkin_log = checkin_log_of())
check("risk_only stays silent when the week is still achievable",
      length(quiet) == 0)

risky <- plan_reminders(definitions, thursday, checkin_log = checkin_log_of())
check("nudges once a skip would break the week", length(risky) == 1)
check("risk wording", grepl("is out of time", risky[[1]]$text))

wednesday <- denver("2026-08-19 17:30:00")
deadline_plan <- plan_reminders(definitions, wednesday,
                                checkin_log = checkin_log_of())
check("a requirement deadline nudges even when the week is achievable",
      length(deadline_plan) == 1)
check("deadline wording names the requirement",
      grepl("2 hour session is out of time", deadline_plan[[1]]$text))
check("requirement line shows its deadline",
      grepl("2 hour session: 0 of 1 \\(by Wed\\)", deadline_plan[[1]]$text))
check("requirement line quotes its own cue",
      grepl("6-8pm, the boxing club", deadline_plan[[1]]$text))
check("short session quotes a different cue",
      grepl("after lunch, the office gym", deadline_plan[[1]]$text))
check("coping plan appears when urgent",
      grepl("If an unexpected task takes the slot, then do 20 minutes at home",
            deadline_plan[[1]]$text))
check("urgent requirement gets its own fallback",
      grepl("Fallback for 2 hour session: do the full 2 hours at home",
            deadline_plan[[1]]$text))
check("fallback stays hidden while not urgent",
      !grepl("Fallback for", kickoff[[1]]$text))

overdue <- plan_reminders(definitions, thursday, checkin_log = checkin_log_of())
check("passed deadline is reported as such",
      grepl("\\(was due Wed\\)", overdue[[1]]$text))

done_long <- plan_reminders(definitions, wednesday,
                            checkin_log = checkin_log_of("long@2026-08-17"))
check("no deadline nudge once that session is logged", length(done_long) == 0)

already <- plan_reminders(definitions, monday, sent_keys = "boxing|2026-08-17",
                          checkin_log = checkin_log_of())
check("one message per day at most", length(already) == 0)

satisfied <- plan_reminders(definitions, thursday,
                            checkin_log = checkin_log_of(
                              "short@2026-08-17", "short@2026-08-18",
                              "short@2026-08-19", "long@2026-08-20"))
check("silent once the quota is met", length(satisfied) == 0)

daily_goal <- boxing
daily_goal$nudge$cadence <- "daily"
daily <- plan_reminders(list(timezone = "America/Denver",
                             goals = list(daily_goal)),
                        denver("2026-08-18 17:30:00"),
                        checkin_log = checkin_log_of())
check("daily cadence nudges regardless of risk", length(daily) == 1)
check("daily nudge reports what is left",
      grepl("4 session\\(s\\) left", daily[[1]]$text))
check("daily nudge shows block standing",
      grepl("Still on for: Buy the \\$100 earphones", daily[[1]]$text))

cat("Fixed-schedule goals\n")
fixed_goal <- list(id = "reading", title = "Read before bed", period = "day",
                   schedule = "fixed", remind_at = "21:00",
                   implementation_intention = list(when = "after brushing",
                                                   where = "in bed",
                                                   what = "read ten pages"),
                   stake = "no phone in the bedroom")
fixed_definitions <- list(timezone = "America/Denver", goals = list(fixed_goal))

check("fixed goal waits for its time",
      length(plan_reminders(fixed_definitions, denver("2026-08-17 20:00:00"))) == 0)
fixed_plan <- plan_reminders(fixed_definitions, denver("2026-08-17 21:30:00"))
check("fixed goal fires after its time", length(fixed_plan) == 1)
check("fixed goal keyed by period", fixed_plan[[1]]$reminder_key == "2026-08-17")
check("fixed goal quotes the intention",
      grepl("after brushing, in bed, read ten pages", fixed_plan[[1]]$text))
check("fixed goal offers done and missed",
      length(fixed_plan[[1]]$buttons[[1]]) == 2)

saturday_goal <- list(id = "weekly", title = "Weekly review", period = "week",
                      schedule = "fixed", remind_at = "09:00",
                      remind_on = "Sat")
check("weekly fixed goal ignores other days",
      !is_scheduled_today(saturday_goal, monday))
check("weekly fixed goal fires on its day",
      is_scheduled_today(saturday_goal, denver("2026-08-22 09:30:00")))

cat("Validation\n")
expect_error("rejects uppercase id",
             validate_goal(list(id = "Bad", title = "t", period = "day")))
expect_error("rejects pipe in id",
             validate_goal(list(id = "a|b", title = "t", period = "day")))
expect_error("rejects unknown period",
             validate_goal(list(id = "ok", title = "t", period = "fortnight")))
expect_error("rejects unknown schedule",
             validate_goal(list(id = "ok", title = "t", period = "day",
                                schedule = "whenever")))
expect_error("fixed goal needs remind_at",
             validate_goal(list(id = "ok", title = "t", period = "day")))
expect_error("quota goal needs requirements",
             validate_goal(list(id = "ok", title = "t", period = "week",
                                schedule = "quota",
                                nudge = list(at = "17:00"))))
expect_error("quota goal needs a nudge time",
             validate_goal(list(id = "ok", title = "t", period = "week",
                                schedule = "quota",
                                requirements = list(
                                  list(id = "a", sessions_per_period = 1)))))
expect_error("rejects zero sessions",
             validate_goal(list(id = "ok", title = "t", period = "week",
                                schedule = "quota",
                                nudge = list(at = "17:00"),
                                requirements = list(
                                  list(id = "a", sessions_per_period = 0)))))
expect_error("rejects duplicate requirement ids",
             validate_goal(list(id = "ok", title = "t", period = "week",
                                schedule = "quota",
                                nudge = list(at = "17:00"),
                                requirements = list(
                                  list(id = "a", sessions_per_period = 1),
                                  list(id = "a", sessions_per_period = 2)))))

descending <- boxing
descending$block$tiers <- rev(descending$block$tiers)
expect_error("rejects unsorted tiers", validate_block(descending))

no_fallback <- boxing
no_fallback$block$otherwise <- NULL
expect_error("block needs an otherwise outcome", validate_block(no_fallback))

cat("Callback payloads\n")
payload <- build_callback_payload("boxing", "2026-W34", "short", "session")
parsed <- parse_callback_payload(payload)
check("payload round-trips goal id", parsed$goal_id == "boxing")
check("payload round-trips period", parsed$period_key == "2026-W34")
check("payload round-trips requirement", parsed$requirement_id == "short")
check("payload round-trips outcome", parsed$outcome == "session")
check("payload fits Telegram's limit", nchar(payload, type = "bytes") <= 64)

binary <- parse_callback_payload(
  build_callback_payload("reading", "2026-08-17", outcome = "done"))
check("requirement defaults to placeholder",
      binary$requirement_id == NO_REQUIREMENT)
expect_error("rejects unknown outcome",
             build_callback_payload("g", "k", "r", "maybe"))
check("three-part payload is rejected",
      is.null(suppressWarnings(parse_callback_payload("a|b|done"))))
check("unknown outcome is rejected",
      is.null(suppressWarnings(parse_callback_payload("a|b|c|maybe"))))

cat("Storage\n")
scratch <- file.path(tempdir(), "goal-store-test")
dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
checkin_path <- file.path(scratch, "checkins.csv")

check("missing log reads as empty",
      nrow(read_log(checkin_path, CHECKIN_LOG_COLUMNS)) == 0)

append_log(checkin_path, CHECKIN_LOG_COLUMNS, list(
  goal_id = "boxing", period_key = "2026-W34", requirement_id = "short",
  outcome = "session", local_date = "2026-08-17",
  recorded_at_utc = "2026-08-17T23:30:00Z"
))
append_log(checkin_path, CHECKIN_LOG_COLUMNS, list(
  goal_id = "boxing", period_key = "2026-W34", requirement_id = "long",
  outcome = "session", local_date = "2026-08-19",
  recorded_at_utc = "2026-08-19T23:30:00Z"
))
stored <- read_log(checkin_path, CHECKIN_LOG_COLUMNS)
check("appends accumulate", nrow(stored) == 2)
check("values survive the round trip", stored$requirement_id[2] == "long")
check("column order preserved", identical(names(stored), CHECKIN_LOG_COLUMNS))
check("stored rows are countable",
      session_counts(stored, "boxing", "2026-W34")[["short"]] == 1)

expect_error("rejects incomplete record",
             append_log(checkin_path, CHECKIN_LOG_COLUMNS,
                        list(goal_id = "x")))
expect_error("rejects unknown field",
             append_log(checkin_path, CHECKIN_LOG_COLUMNS,
                        list(goal_id = "x", period_key = "k",
                             requirement_id = "r", outcome = "session",
                             local_date = "d", recorded_at_utc = "t",
                             surprise = 1)))

reminder_path <- file.path(scratch, "reminders.csv")
append_log(reminder_path, REMINDER_LOG_COLUMNS, list(
  goal_id = "boxing", reminder_key = "2026-08-17",
  sent_at_utc = "2026-08-17T23:30:00Z"
))
check("sent keys are composite",
      already_sent_keys(read_log(reminder_path, REMINDER_LOG_COLUMNS)) ==
        "boxing|2026-08-17")

offset_path <- file.path(scratch, "offset.txt")
check("absent offset defaults to zero", read_update_offset(offset_path) == 0)
write_update_offset(4242L, offset_path)
check("offset persists", read_update_offset(offset_path) == 4242)

unlink(scratch, recursive = TRUE)

cat("Definitions file\n")
loaded <- load_goals("goals.yml")
check("goals.yml loads", length(loaded$goals) >= 1)
check("timezone recognised", loaded$timezone %in% OlsonNames())
real_goal <- loaded$goals[[1]]
check("boxing goal is a quota goal", real_goal$schedule == "quota")
check("four sessions a week in total",
      sum(vapply(real_goal$requirements, `[[`, integer(1),
                 "sessions_per_period")) == 4)
check("block runs eight periods", real_goal$block$periods == 8)
check("block keys resolve", length(block_period_keys(real_goal)) == 8)

cat("\n")
if (failures > 0) {
  cat(failures, "check(s) failed.\n")
  quit(status = 1)
}
cat("All checks passed.\n")
