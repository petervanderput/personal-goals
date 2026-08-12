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
- [ ] Confirm the first live kickoff nudge on Monday 17 August
- [ ] Confirm a session tap increments the weekly count in CI

## Phase 3 — review and feedback

- [ ] Weekly review dashboard in `PersonalGoals.qmd` (`format: dashboard`)
- [ ] Publish the dashboard to GitHub Pages and send the link on Sundays
- [ ] End-of-block message announcing which tier was earned
- [ ] Reminders at temporal landmarks, where motivation to restart peaks

## Phase 4 — optional upgrades

- [ ] Cloudflare Worker webhook for instant check-ins instead of polling
- [ ] Editing goals without hand-editing YAML
- [ ] Weekly summary to an accountability partner

## Known limitations

- Session length is self-reported by which button you tap; nothing verifies that
  a short session really ran 20 minutes.
- A tap that cannot be acknowledged leaves its buttons in place. Counting
  distinct dates makes that harmless for quota goals, but a fixed goal can still
  record both done and missed for one period, and the review logic should take
  the most recent.
- Check-ins are recorded within one polling interval rather than instantly, which
  is why acknowledgement often expires.

## Open questions

- Whether the Seamus night-shift consequence is tracked only, or enforced by
  telling someone. Pre-committed stakes work far better when a third party holds
  them, so a tracked-only consequence is the weakest part of the setup.
