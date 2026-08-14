#' Tests for the pure scheduling, session, formatting and storage logic.
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
wednesday <- denver("2026-08-19 17:30:00")
thursday <- denver("2026-08-20 17:30:00")
saturday <- denver("2026-08-22 09:30:00")
sunday <- denver("2026-08-23 17:30:00")

earbuds_commitment <- list(
  id = "earbuds", label = "Earbuds run",
  window = list(kind = "range", from = "2026-08-17", to = "2026-12-06"),
  min_sessions = 3,
  tiers = list(list(max_shortfalls = 0, outcome = "Buy the $100 earbuds")),
  otherwise = "No earbuds"
)

baking_commitment <- list(
  id = "baking", label = "This month",
  window = list(kind = "month"),
  min_sessions = 4,
  tiers = list(
    list(max_shortfalls = 0, outcome = "Wife bakes whatever you want"),
    list(max_shortfalls = 1, outcome = "Something from the bakery")
  ),
  otherwise = "Nothing baked this month"
)

# Four sessions, each anchored to its own weekday: the live boxing shape.
boxing <- list(
  id = "boxing",
  title = "Get better and more consistent at boxing",
  period = "week",
  schedule = "quota",
  starts = "2026-08-17",
  requirements = list(
    list(id = "mon", label = "Monday club session", sessions_per_period = 1L,
         on_day = "Mon", remind_at = "17:00",
         coping_plan = "do the full 2 hours at home",
         implementation_intention = list(when = "6-8pm",
                                         where = "the boxing club")),
    list(id = "wed", label = "Wednesday 30 min", sessions_per_period = 1L,
         on_day = "Wed", remind_at = "16:45",
         implementation_intention = list(when = "immediately after work",
                                         where = "the office gym")),
    list(id = "fri", label = "Friday 30 min", sessions_per_period = 1L,
         on_day = "Fri", remind_at = "16:45"),
    list(id = "sat", label = "Saturday morning session",
         sessions_per_period = 1L, on_day = "Sat", remind_at = "07:30")
  ),
  obstacle = "an unexpected task takes the slot",
  coping_plan = "do 20 minutes at home instead",
  missed_notice_at = "21:30",
  missed_session_consequence = "put your son back to sleep after every wake-up",
  commitments = list(earbuds_commitment, baking_commitment)
)

# Sessions free to land on any day, which take the goal-level nudge instead.
running <- list(
  id = "running",
  title = "Run more",
  period = "week",
  schedule = "quota",
  requirements = list(
    list(id = "short", label = "20-30 min run", sessions_per_period = 3L,
         implementation_intention = list(when = "after lunch",
                                         where = "the office gym")),
    list(id = "long", label = "long run", sessions_per_period = 1L,
         by_day = "Wed", coping_plan = "run the full hour on the treadmill",
         implementation_intention = list(when = "6-8pm", where = "the canal"))
  ),
  obstacle = "an unexpected task takes the slot",
  coping_plan = "do 20 minutes at home instead",
  nudge = list(at = "17:00", cadence = "risk_only", kickoff_on = "Mon"),
  commitments = list(earbuds_commitment)
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

#' Plan reminders for a single goal.
plan_for <- function(goal, local_time, sent_keys = character(),
                     checkin_log = checkin_log_of()) {
  plan_reminders(list(timezone = "America/Denver", goals = list(goal)),
                 local_time, sent_keys, checkin_log)
}

cat("Time handling\n")
check("local hour", monday$hour == 17)
check("minutes since midnight", minutes_since_midnight(monday) == 1050)
check("parses HH:MM", parse_time_of_day("07:15") == 435)
expect_error("rejects 24:00", parse_time_of_day("24:00"))
expect_error("rejects malformed time", parse_time_of_day("7:5"))

cat("Date parsing\n")
check("parses an ISO date", parse_date("2026-08-17", "d") == as.Date("2026-08-17"))
expect_error("rejects a non-date", parse_date("next Tuesday", "d"))
expect_error("rejects an impossible date", parse_date("2026-02-31", "d"))

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

cat("Session counting\n")
empty_counts <- session_counts(checkin_log_of(), "boxing", "2026-W34")
check("empty log counts nothing", length(empty_counts) == 0)

logged <- checkin_log_of("mon@2026-08-17", "wed@2026-08-19")
counts <- session_counts(logged, "boxing", "2026-W34")
check("counts the monday session", counts[["mon"]] == 1)
check("counts the wednesday session", counts[["wed"]] == 1)

repeated <- checkin_log_of("mon@2026-08-17", "mon@2026-08-17")
check("two taps on one day count once",
      session_counts(repeated, "boxing", "2026-W34")[["mon"]] == 1)

check("other periods are excluded",
      length(session_counts(logged, "boxing", "2026-W35")) == 0)
check("other goals are excluded",
      length(session_counts(logged, "running", "2026-W34")) == 0)

cat("Session totals\n")
check("four sessions asked for a week", required_sessions(boxing) == 4)
check("two of four completed", completed_sessions(boxing, counts) == 2)
check("nothing logged is nothing completed",
      completed_sessions(boxing, integer()) == 0)
check("a session logged twice still counts once",
      completed_sessions(boxing, c(mon = 3L)) == 1)

cat("Requirement progress\n")
progress <- requirement_progress(boxing, counts)
check("monday satisfied", progress[[1]]$remaining == 0)
check("friday outstanding", progress[[3]]$remaining == 1)
check("two sessions still owed", total_remaining(progress) == 2)
check("overshooting does not go negative",
      requirement_progress(boxing, c(mon = 5L))[[1]]$remaining == 0)

cat("Anchored days\n")
check("monday has one session", length(sessions_on_day(boxing, monday)) == 1)
check("monday names the right one",
      sessions_on_day(boxing, monday)[[1]]$id == "mon")
check("tuesday has none",
      length(sessions_on_day(boxing, denver("2026-08-18 17:30:00"))) == 0)
check("free sessions are never anchored to a day",
      length(sessions_on_day(running, monday)) == 0)

check("three sessions still to come after monday",
      remaining_scheduled_sessions(boxing, integer(), monday) == 3)
check("today's own session is not counted as still to come",
      remaining_scheduled_sessions(boxing, integer(), wednesday) == 2)
check("nothing left after saturday",
      remaining_scheduled_sessions(boxing, integer(), saturday) == 0)
check("a logged future session is not still to come",
      remaining_scheduled_sessions(boxing, c(sat = 1L), wednesday) == 1)

cat("Risk detection\n")
check("four owed with four days left is at risk", is_at_risk(4L, 4L))
check("three owed with four days left is not", !is_at_risk(3L, 4L))
check("nothing owed is never at risk", !is_at_risk(0L, 1L))

cat("Per-requirement deadlines\n")
long_run <- running$requirements[[2]]
short_run <- running$requirements[[1]]
check("monday leaves three days to wednesday",
      days_until_requirement_deadline(long_run, monday) == 3)
check("wednesday is the deadline itself",
      days_until_requirement_deadline(long_run, wednesday) == 1)
check("thursday is past the deadline",
      days_until_requirement_deadline(long_run, thursday) == 0)
check("no deadline reads as NA",
      is.na(days_until_requirement_deadline(short_run, monday)))
check("an anchored day acts as the deadline",
      days_until_requirement_deadline(boxing$requirements[[2]], monday) == 3)

deadline_progress <- requirement_progress(running, integer(), wednesday)
check("outstanding requirement at its deadline is urgent",
      is_requirement_urgent(deadline_progress[[2]]))
check("requirement without a deadline is never urgent",
      !is_requirement_urgent(deadline_progress[[1]]))
check("a satisfied requirement is not urgent",
      !is_requirement_urgent(requirement_progress(running, c(long = 1L),
                                                  wednesday)[[2]]))

cat("Intention formatting\n")
check("joins when and where",
      format_intention(list(when = "6-8pm", where = "the club")) ==
        "6-8pm, the club")
check("joins all three parts",
      format_intention(list(when = "a", where = "b", what = "c")) == "a, b, c")
check("no intention yields nothing", is.null(format_intention(NULL)))
check("empty intention yields nothing", is.null(format_intention(list())))

cat("Commitment windows\n")
earbuds_keys <- commitment_period_keys(boxing, earbuds_commitment, monday)
check("the earbuds run spans sixteen weeks", length(earbuds_keys) == 16)
check("it starts at W34", earbuds_keys[1] == "2026-W34")
check("it ends at W49", earbuds_keys[16] == "2026-W49")

august_keys <- commitment_period_keys(boxing, baking_commitment, monday)
check("august holds three weeks once the goal has started",
      length(august_keys) == 3)
check("weeks before the start date are dropped", august_keys[1] == "2026-W34")

# The week of 31 August runs into September, and must stay an August week all
# the way through rather than switching month on the 1st.
straddling <- commitment_period_keys(boxing, baking_commitment,
                                     denver("2026-09-01 17:30:00"))
check("a week straddling two months keeps one month",
      identical(straddling, august_keys))

expect_error("rejects an unknown window kind",
             commitment_period_keys(boxing,
                                    modifyList(baking_commitment,
                                               list(window = list(kind = "x"))),
                                    monday))

cat("Commitment evaluation\n")
status <- evaluate_commitment(boxing, earbuds_commitment, logged, thursday)
check("the current week is not judged", status$finished_periods == 0)
check("no shortfalls yet", status$shortfall_periods == 0)
check("the run is active", status$is_active)
check("the top tier is still available",
      status$best_achievable == "Buy the $100 earbuds")

# Two finished weeks: the first complete, the second only two sessions.
two_weeks <- rbind(
  checkin_log_of("mon@2026-08-17", "wed@2026-08-19", "fri@2026-08-21",
                 "sat@2026-08-22", key = "2026-W34"),
  checkin_log_of("mon@2026-08-24", "wed@2026-08-26", key = "2026-W35")
)
in_week_36 <- denver("2026-09-01 17:30:00")

earbuds_status <- evaluate_commitment(boxing, earbuds_commitment, two_weeks,
                                      in_week_36)
check("two weeks have been judged", earbuds_status$finished_periods == 2)
check("a week of two sessions falls short of three",
      earbuds_status$shortfall_periods == 1)
check("one shortfall loses the earbuds",
      earbuds_status$best_achievable == "No earbuds")
check("the earbuds run is reported as lost", earbuds_status$is_lost)
check("a lost commitment says so rather than promising it",
      grepl("Gone, now: No earbuds",
            format_commitment_line(earbuds_status, "week")))

baking_status <- evaluate_commitment(boxing, baking_commitment, two_weeks,
                                     in_week_36)
check("a week of two sessions falls short of four",
      baking_status$shortfall_periods == 1)
check("one shortfall drops to the bakery",
      baking_status$best_achievable == "Something from the bakery")
check("a commitment with tolerance left is not lost", !baking_status$is_lost)

check("both commitments are reported",
      length(evaluate_commitments(boxing, two_weeks, in_week_36)) == 2)
check("a goal without commitments reports none",
      length(evaluate_commitments(running["requirements"], two_weeks,
                                  in_week_36)) == 0)

check("no shortfalls earns the top tier",
      best_commitment_tier(baking_commitment, 0) ==
        "Wife bakes whatever you want")
check("two shortfalls fall through to the consequence",
      best_commitment_tier(baking_commitment, 2) == "Nothing baked this month")

before_the_start <- evaluate_commitment(boxing, earbuds_commitment, logged,
                                        denver("2026-08-10 17:30:00"))
check("a run that has not begun is inactive", !before_the_start$is_active)

cat("Anchored session prompts\n")
check("nothing before the reminder time",
      length(plan_for(boxing, denver("2026-08-17 08:00:00"))) == 0)
check("nothing before the goal starts",
      length(plan_for(boxing, denver("2026-08-10 17:30:00"))) == 0)
check("no session anchored to tuesday",
      length(plan_for(boxing, denver("2026-08-18 17:30:00"))) == 0)

prompt <- plan_for(boxing, monday)
check("monday prompts once", length(prompt) == 1)
check("keyed by date and session",
      prompt[[1]]$reminder_key == "2026-08-17:mon")
check("names today's session", grepl("Today: Monday club session",
                                     prompt[[1]]$text))
check("quotes that session's cue",
      grepl("6-8pm, the boxing club", prompt[[1]]$text))
check("reports the week so far",
      grepl("This week: 0 of 4 sessions", prompt[[1]]$text))
check("shows the coping plan",
      grepl("If an unexpected task takes the slot, then do the full 2 hours",
            prompt[[1]]$text))
check("states what a skip costs",
      grepl("Skip it and: put your son back to sleep", prompt[[1]]$text))
check("shows both commitment standings",
      grepl("Earbuds run: 0 of 16 weeks in, 0 short", prompt[[1]]$text) &&
        grepl("This month: 0 of 3 weeks in, 0 short", prompt[[1]]$text))
check("offers one logging button", length(prompt[[1]]$buttons[[1]]) == 1)

check("no prompt once that session is logged",
      length(plan_for(boxing, monday,
                      checkin_log = checkin_log_of("mon@2026-08-17"))) == 0)
check("no prompt once it has been sent",
      length(plan_for(boxing, monday,
                      sent_keys = "boxing|2026-08-17:mon")) == 0)

wednesday_prompt <- plan_for(boxing, wednesday,
                             checkin_log = checkin_log_of("mon@2026-08-17"))
check("wednesday prompts its own session",
      grepl("Today: Wednesday 30 min", wednesday_prompt[[1]]$text))
check("wednesday quotes its own cue",
      grepl("immediately after work, the office gym",
            wednesday_prompt[[1]]$text))
check("wednesday counts monday already done",
      grepl("This week: 1 of 4 sessions", wednesday_prompt[[1]]$text))
check("a session without its own plan falls back to the goal's",
      grepl("then do 20 minutes at home instead",
            plan_for(boxing, denver("2026-08-21 17:30:00"))[[1]]$text))

cat("Missed-session notices\n")
sent_prompt <- "boxing|2026-08-17:mon"
check("no notice before the cut-off",
      length(plan_for(boxing, denver("2026-08-17 21:00:00"),
                      sent_keys = sent_prompt)) == 0)

notice <- plan_for(boxing, denver("2026-08-17 21:45:00"),
                   sent_keys = sent_prompt)
check("a missed session is reported that night", length(notice) == 1)
check("the notice has its own key",
      notice[[1]]$reminder_key == "2026-08-17:mon:missed")
check("it names the session", grepl("Missed: Monday club session",
                                    notice[[1]]$text))
check("it states tonight's consequence",
      grepl("Tonight: put your son back to sleep", notice[[1]]$text))
check("it counts what is still scheduled",
      grepl("3 session\\(s\\) still scheduled this week", notice[[1]]$text))
check("it offers a way to log a forgotten session",
      length(notice[[1]]$buttons[[1]]) == 1)

check("no notice once the session is logged",
      length(plan_for(boxing, denver("2026-08-17 21:45:00"),
                      sent_keys = sent_prompt,
                      checkin_log = checkin_log_of("mon@2026-08-17"))) == 0)
check("the notice is sent only once",
      length(plan_for(boxing, denver("2026-08-17 21:45:00"),
                      sent_keys = c(sent_prompt,
                                    "boxing|2026-08-17:mon:missed"))) == 0)

quiet_goal <- boxing
quiet_goal$missed_notice_at <- NULL
check("no notice when none is configured",
      length(plan_for(quiet_goal, denver("2026-08-17 21:45:00"),
                      sent_keys = sent_prompt)) == 0)

late_catchup <- plan_for(boxing, denver("2026-08-17 21:45:00"))
check("a day missed entirely yields both prompt and notice",
      length(late_catchup) == 2)

cat("Free-session nudges\n")
check("nothing before the nudge time",
      length(plan_for(running, denver("2026-08-17 08:00:00"))) == 0)

kickoff <- plan_for(running, monday)
check("kickoff fires on monday", length(kickoff) == 1)
check("kickoff keyed by date", kickoff[[1]]$reminder_key == "2026-08-17")
check("kickoff names the new period", grepl("New week", kickoff[[1]]$text))
check("kickoff offers a button per requirement",
      length(kickoff[[1]]$buttons[[1]]) == 2)
check("kickoff shows the commitment standing",
      grepl("Still on for: Buy the \\$100 earbuds", kickoff[[1]]$text))
check("fallback stays hidden while not urgent",
      !grepl("Fallback for", kickoff[[1]]$text))

check("risk_only stays silent when the week is still achievable",
      length(plan_for(running, denver("2026-08-18 17:30:00"))) == 0)

risky <- plan_for(running, thursday)
check("nudges once a skip would break the week", length(risky) == 1)
check("risk wording", grepl("is out of time", risky[[1]]$text))
check("a passed deadline is reported as such",
      grepl("\\(was due Wed\\)", risky[[1]]$text))

deadline_plan <- plan_for(running, wednesday)
check("a requirement deadline nudges even when the week is achievable",
      length(deadline_plan) == 1)
check("deadline wording names the requirement",
      grepl("long run is out of time", deadline_plan[[1]]$text))
check("requirement line shows its deadline",
      grepl("long run: 0 of 1 \\(by Wed\\)", deadline_plan[[1]]$text))
check("coping plan appears when urgent",
      grepl("If an unexpected task takes the slot, then do 20 minutes at home",
            deadline_plan[[1]]$text))
check("urgent requirement gets its own fallback",
      grepl("Fallback for long run: run the full hour on the treadmill",
            deadline_plan[[1]]$text))

check("no deadline nudge once that session is logged",
      length(plan_for(running, wednesday,
                      checkin_log = checkin_log_of("long@2026-08-17",
                                                   goal_id = "running"))) == 0)
check("one nudge per day at most",
      length(plan_for(running, monday, sent_keys = "running|2026-08-17")) == 0)
check("silent once every session is done",
      length(plan_for(running, thursday,
                      checkin_log = checkin_log_of(
                        "short@2026-08-17", "short@2026-08-18",
                        "short@2026-08-19", "long@2026-08-20",
                        goal_id = "running"))) == 0)

# A goal may mix the two styles, in which case the nudge must speak only for the
# sessions that still need a day chosen for them.
mixed <- running
mixed$requirements <- list(boxing$requirements[[1]], running$requirements[[2]])
mixed_plans <- plan_for(mixed, monday)
check("a mixed goal prompts its anchored session and nudges the rest",
      length(mixed_plans) == 2)
check("the nudge covers only the free sessions",
      grepl("long run: 0 of 1", mixed_plans[[2]]$text) &&
        !grepl("Monday club session", mixed_plans[[2]]$text))
check("the nudge counts only the free sessions as left",
      grepl("1 session\\(s\\) left", mixed_plans[[2]]$text))

daily_goal <- running
daily_goal$nudge$cadence <- "daily"
daily <- plan_for(daily_goal, denver("2026-08-18 17:30:00"))
check("daily cadence nudges regardless of risk", length(daily) == 1)
check("daily nudge reports what is left",
      grepl("4 session\\(s\\) left", daily[[1]]$text))

cat("Fixed-schedule goals\n")
fixed_goal <- list(id = "reading", title = "Read before bed", period = "day",
                   schedule = "fixed", remind_at = "21:00",
                   implementation_intention = list(when = "after brushing",
                                                   where = "in bed",
                                                   what = "read ten pages"),
                   stake = "no phone in the bedroom")

check("fixed goal waits for its time",
      length(plan_for(fixed_goal, denver("2026-08-17 20:00:00"))) == 0)
fixed_plan <- plan_for(fixed_goal, denver("2026-08-17 21:30:00"))
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
      is_scheduled_today(saturday_goal, saturday))

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
expect_error("a free session needs a goal-level nudge time",
             validate_goal(list(id = "ok", title = "t", period = "week",
                                schedule = "quota",
                                requirements = list(
                                  list(id = "a", sessions_per_period = 1)))))
check("a fully anchored goal needs no nudge",
      is.list(validate_goal(list(id = "ok", title = "t", period = "week",
                                 schedule = "quota",
                                 requirements = list(
                                   list(id = "a", sessions_per_period = 1,
                                        on_day = "Mon",
                                        remind_at = "17:00"))))))
expect_error("an anchored session needs a reminder time",
             validate_goal(list(id = "ok", title = "t", period = "week",
                                schedule = "quota",
                                requirements = list(
                                  list(id = "a", sessions_per_period = 1,
                                       on_day = "Mon")))))
expect_error("rejects an unknown weekday",
             validate_goal(list(id = "ok", title = "t", period = "week",
                                schedule = "quota",
                                requirements = list(
                                  list(id = "a", sessions_per_period = 1,
                                       on_day = "Moonday",
                                       remind_at = "17:00")))))
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
expect_error("rejects an unparseable start date",
             validate_goal(modifyList(boxing, list(starts = "soon"))))
expect_error("a missed-session notice needs a consequence",
             validate_goal(modifyList(
               boxing, list(missed_session_consequence = NULL))))

check("the live boxing shape validates", is.list(validate_goal(boxing)))

descending <- baking_commitment
descending$tiers <- rev(descending$tiers)
expect_error("rejects unsorted tiers", validate_commitment(boxing, descending))
expect_error("a commitment needs an otherwise outcome",
             validate_commitment(boxing, modifyList(
               baking_commitment, list(otherwise = NULL))))
expect_error("a commitment cannot ask for more than the schedule offers",
             validate_commitment(boxing, modifyList(
               baking_commitment, list(min_sessions = 5))))
expect_error("rejects an unknown window kind",
             validate_commitment(boxing, modifyList(
               baking_commitment, list(window = list(kind = "fortnight")))))
expect_error("a range window must not end before it starts",
             validate_commitment(boxing, modifyList(
               earbuds_commitment,
               list(window = list(kind = "range", from = "2026-08-17",
                                  to = "2026-08-10")))))
expect_error("commitments need a quota schedule",
             validate_goal(list(id = "ok", title = "t", period = "day",
                                schedule = "fixed", remind_at = "21:00",
                                commitments = list(earbuds_commitment))))
repeated_commitments <- boxing
repeated_commitments$commitments <- list(earbuds_commitment, earbuds_commitment)
expect_error("rejects duplicate commitment ids",
             validate_goal(repeated_commitments))

cat("Callback payloads\n")
payload <- build_callback_payload("boxing", "2026-W34", "mon", "session")
parsed <- parse_callback_payload(payload)
check("payload round-trips goal id", parsed$goal_id == "boxing")
check("payload round-trips period", parsed$period_key == "2026-W34")
check("payload round-trips requirement", parsed$requirement_id == "mon")
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
  goal_id = "boxing", period_key = "2026-W34", requirement_id = "mon",
  outcome = "session", local_date = "2026-08-17",
  recorded_at_utc = "2026-08-17T23:30:00Z"
))
append_log(checkin_path, CHECKIN_LOG_COLUMNS, list(
  goal_id = "boxing", period_key = "2026-W34", requirement_id = "wed",
  outcome = "session", local_date = "2026-08-19",
  recorded_at_utc = "2026-08-19T23:30:00Z"
))
stored <- read_log(checkin_path, CHECKIN_LOG_COLUMNS)
check("appends accumulate", nrow(stored) == 2)
check("values survive the round trip", stored$requirement_id[2] == "wed")
check("column order preserved", identical(names(stored), CHECKIN_LOG_COLUMNS))
check("stored rows are countable",
      session_counts(stored, "boxing", "2026-W34")[["mon"]] == 1)

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
  goal_id = "boxing", reminder_key = "2026-08-17:mon",
  sent_at_utc = "2026-08-17T23:30:00Z"
))
check("sent keys are composite",
      already_sent_keys(read_log(reminder_path, REMINDER_LOG_COLUMNS)) ==
        "boxing|2026-08-17:mon")

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
check("four sessions a week in total", required_sessions(real_goal) == 4)
check("every session is anchored to a weekday",
      all(vapply(real_goal$requirements,
                 function(r) !is.null(r$on_day), logical(1))))
check("the anchored days are mon, wed, fri and sat",
      identical(vapply(real_goal$requirements, `[[`, character(1), "on_day"),
                c("Mon", "Wed", "Fri", "Sat")))
check("two commitments are configured", length(real_goal$commitments) == 2)
check("the earbuds run resolves to sixteen weeks",
      length(commitment_period_keys(real_goal, real_goal$commitments[[1]],
                                    monday)) == 16)
check("a missed session has a same-night consequence",
      !is.null(real_goal$missed_session_consequence))

cat("\n")
if (failures > 0) {
  cat(failures, "check(s) failed.\n")
  quit(status = 1)
}
cat("All checks passed.\n")
