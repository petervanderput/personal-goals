# Tasks

## Phase 1 — prove the notification loop

- [x] Confirm toolchain: R 4.5.1, Quarto (bundled with RStudio), Git
- [x] Store credentials in gitignored `.Renviron`
- [x] Telegram API wrapper with single-point error handling
- [x] Move the project out of OneDrive to avoid `.git` sync conflicts
- [x] Message the bot so Telegram permits delivery
- [x] Create the GitHub repository and push `main`
- [x] Add `TELEGRAM_BOT_TOKEN` and `TELEGRAM_CHAT_ID` as Actions secrets
- [x] Confirm a button tap is recorded into `data/checkins.csv`
- [x] Confirm the workflow completes in CI

## Phase 2 — the boxing goal

- [x] Quota schedule: count sessions per period on any day
- [x] Risk-aware nudging, so a message only lands when it carries information
- [x] Reward blocks with ascending tiers and a fallback consequence
- [x] Count sessions by distinct local date, making repeat taps idempotent
- [x] Preview tool to check wording and timing without sending
- [x] Rewrite the test suite for the quota model (120 checks)
- [x] Per-requirement cues, since the club and home sessions differ
- [x] Per-requirement deadlines, so the club session cannot silently expire
- [x] Coping plans: shrink the session rather than cancel it
- [ ] Confirm the first live Monday prompt on 17 August
- [ ] Confirm a session tap increments the weekly count in CI

## Phase 2b — fixed days and layered stakes

- [x] Anchor each session to its own weekday, time and cue (`on_day`/`remind_at`)
- [x] One prompt per session per day, keyed by date and session
- [x] Commitments replace the single reward block: independent windows, each with
      its own `min_sessions` and tiers
- [x] Calendar-month windows, with a week belonging to the month of its Monday
- [x] `starts` date, so no window judges a week before the goal existed
- [x] Same-night notice when an anchored session goes unlogged, carrying a button
      to claim training that was forgotten
- [x] Say when a reward is gone rather than continuing to promise it
- [x] Test suite for the anchored model (186 checks)
- [ ] Confirm the Wednesday and Friday prompt times against the actual end of the
      workday

## Phase 3 — review and feedback

- [ ] Weekly review dashboard in `PersonalGoals.qmd` (`format: dashboard`)
- [ ] Publish the dashboard to GitHub Pages and send the link on Sundays
- [ ] End-of-window message announcing which tier was earned
- [ ] Reminders at temporal landmarks, where motivation to restart peaks

## Phase 4 — optional upgrades

- [ ] Cloudflare Worker webhook for instant check-ins instead of polling
- [ ] Editing goals without hand-editing YAML
- [ ] Weekly summary to an accountability partner

## Known limitations

- Session length is self-reported by tapping a button; nothing verifies that a
  30 minute session really ran 30 minutes.
- A tap that cannot be acknowledged leaves its buttons in place. Counting
  distinct dates and capping each requirement makes that harmless for session
  goals, but a fixed goal can still record both done and missed for one period,
  and the review logic should take the most recent.
- Check-ins are recorded within one polling interval rather than instantly, which
  is why acknowledgement often expires.
- Training on a day nothing is scheduled cannot be logged, because the buttons
  only exist on that day's prompt. Makeup sessions therefore do not count.
- The missed-session notice fires at a fixed time, so a session that runs late
  and is logged afterwards still triggers the notice.

## Open questions

- Whether the night-shift consequence is tracked only, or enforced by telling
  someone. Pre-committed stakes work far better when a third party holds them, so
  a tracked-only consequence is the weakest part of the setup.
- Whether the monthly baking window should be the calendar month (as built) or a
  rolling four weeks.
- Whether the week beginning Monday 7 December counts toward the earbuds run, or
  whether the run ends with the week before it (as built).
