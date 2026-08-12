#' Tests for the pure scheduling, formatting and storage logic.
#'
#' Run with: Rscript tests/test_logic.R
#' Deliberately dependency-free so it runs anywhere R does, and touches no
#' network and no files outside a temporary directory.

source("R/goals.R")
source("R/store.R")
source("R/collect_checkins.R")

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

# A fixed reference instant: Monday 2026-08-10, 09:30 in Denver.
reference_now <- as.POSIXct("2026-08-10 15:30:00", tz = "UTC")
denver_time <- local_time_now("America/Denver", reference_now)

cat("Time handling\n")
check("converts UTC to local hour", denver_time$hour == 9)
check("minutes since midnight", minutes_since_midnight(denver_time) == 570)
check("parses HH:MM", parse_time_of_day("07:15") == 435)
expect_error("rejects 24:00", parse_time_of_day("24:00"))
expect_error("rejects malformed time", parse_time_of_day("7:5"))

cat("Period keys\n")
check("daily key", period_key("day", denver_time) == "2026-08-10")
check("weekly key", period_key("week", denver_time) == "2026-W33")
check("monthly key", period_key("month", denver_time) == "2026-08")
check("yearly key", period_key("year", denver_time) == "2026")

cat("Scheduling\n")
daily_goal <- list(id = "daily", title = "Daily", period = "day",
                   remind_at = "08:00")
weekly_goal <- list(id = "weekly", title = "Weekly", period = "week",
                    remind_at = "08:00", remind_on = "Mon")
saturday_goal <- list(id = "sat", title = "Saturday", period = "week",
                      remind_at = "08:00", remind_on = "Sat")
late_goal <- list(id = "late", title = "Later today", period = "day",
                  remind_at = "22:00")

check("daily goal is scheduled", is_scheduled_today(daily_goal, denver_time))
check("monday goal fires on monday",
      is_scheduled_today(weekly_goal, denver_time))
check("saturday goal does not fire on monday",
      !is_scheduled_today(saturday_goal, denver_time))

due <- due_goals(list(daily_goal, weekly_goal, late_goal), denver_time)
check("two goals due, later one held back", length(due) == 2)
check("period key attached to due goal", due[[1]]$period_key == "2026-08-10")

already <- due_goals(list(daily_goal), denver_time,
                     sent_keys = "daily|2026-08-10")
check("already-sent goal is not resent", length(already) == 0)

cat("Validation\n")
expect_error("rejects uppercase id",
             validate_goal(list(id = "Bad", title = "t", period = "day",
                                remind_at = "08:00")))
expect_error("rejects pipe in id",
             validate_goal(list(id = "a|b", title = "t", period = "day",
                                remind_at = "08:00")))
expect_error("rejects unknown period",
             validate_goal(list(id = "ok", title = "t", period = "fortnight",
                                remind_at = "08:00")))
expect_error("rejects missing remind_at",
             validate_goal(list(id = "ok", title = "t", period = "day")))

cat("Callback payloads\n")
payload <- build_callback_payload("spanish", "2026-08-10", "done")
parsed <- parse_callback_payload(payload)
check("payload round-trips goal id", parsed$goal_id == "spanish")
check("payload round-trips period", parsed$period_key == "2026-08-10")
check("payload round-trips outcome", parsed$outcome == "done")
check("payload fits Telegram's 64-byte limit",
      nchar(payload, type = "bytes") <= 64)
check("two-part payload is rejected",
      is.null(suppressWarnings(parse_callback_payload("test|done"))))
check("unknown outcome is rejected",
      is.null(suppressWarnings(parse_callback_payload("a|b|maybe"))))

cat("Storage\n")
scratch <- file.path(tempdir(), "goal-store-test")
dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
checkin_path <- file.path(scratch, "checkins.csv")

check("missing log reads as empty",
      nrow(read_log(checkin_path, CHECKIN_LOG_COLUMNS)) == 0)

append_log(checkin_path, CHECKIN_LOG_COLUMNS, list(
  goal_id = "spanish", period_key = "2026-08-10", outcome = "done",
  recorded_at_utc = "2026-08-10T15:30:00Z"
))
append_log(checkin_path, CHECKIN_LOG_COLUMNS, list(
  goal_id = "spanish", period_key = "2026-08-11", outcome = "missed",
  recorded_at_utc = "2026-08-11T15:30:00Z"
))
stored <- read_log(checkin_path, CHECKIN_LOG_COLUMNS)
check("appends accumulate", nrow(stored) == 2)
check("values survive the round trip", stored$outcome[2] == "missed")
check("column order preserved",
      identical(names(stored), CHECKIN_LOG_COLUMNS))

expect_error("rejects incomplete record",
             append_log(checkin_path, CHECKIN_LOG_COLUMNS,
                        list(goal_id = "x")))
expect_error("rejects unknown field",
             append_log(checkin_path, CHECKIN_LOG_COLUMNS,
                        list(goal_id = "x", period_key = "k", outcome = "done",
                             recorded_at_utc = "t", surprise = 1)))

offset_path <- file.path(scratch, "offset.txt")
check("absent offset defaults to zero", read_update_offset(offset_path) == 0)
write_update_offset(4242L, offset_path)
check("offset persists", read_update_offset(offset_path) == 4242)

cat("Reminder text\n")
rich_goal <- list(
  id = "spanish", title = "Learn conversational Spanish", period = "day",
  remind_at = "07:15", target = 45, measure = "minutes of practice",
  implementation_intention = list(when = "after coffee", where = "at my desk",
                                  what = "open Anki"),
  obstacle = "I check my phone", coping_plan = "phone in the drawer",
  stake = "$10 to a friend"
)
reminder_text <- format_reminder(rich_goal)
check("includes the title", grepl("Learn conversational Spanish", reminder_text))
check("quotes the implementation intention",
      grepl("after coffee, at my desk, open Anki", reminder_text))
check("states the target", grepl("45 minutes of practice this day", reminder_text))
check("states the coping plan", grepl("phone in the drawer", reminder_text))
check("states the stake", grepl("On the line", reminder_text))

cat("Definitions file\n")
definitions <- load_goals("goals.yml")
check("goals.yml loads", length(definitions$goals) >= 1)
check("timezone present", nzchar(definitions$timezone))

unlink(scratch, recursive = TRUE)

cat("\n")
if (failures > 0) {
  cat(failures, "check(s) failed.\n")
  quit(status = 1)
}
cat("All checks passed.\n")
