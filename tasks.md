# Tasks

## Phase 1 — prove the notification loop

- [x] Confirm toolchain: R 4.5.1, Quarto (bundled with RStudio), Git
- [x] Store credentials in gitignored `.Renviron`
- [x] Telegram API wrapper with single-point error handling
- [x] Validate bot token (`getMe` returns @PVDPBot)
- [x] Goal definitions schema in `goals.yml`
- [x] Scheduling rules for day / week / month / year periods
- [x] Append-only CSV storage with idempotent reminder log
- [x] Check-in collector for Done / Missed button taps
- [x] Test suite for the pure logic (40 checks passing)
- [x] GitHub Actions cycle workflow
- [x] Move the project out of OneDrive to avoid `.git` sync conflicts
- [x] Message the bot so Telegram permits delivery
- [x] Confirm a test message with buttons is delivered
- [x] Create the GitHub repository and push `main`
- [x] Add `TELEGRAM_BOT_TOKEN` and `TELEGRAM_CHAT_ID` as Actions secrets
- [x] Confirm a button tap is recorded into `data/checkins.csv`
- [ ] Confirm the workflow completes in CI and commits its logs
- [ ] Replace the example goal with real goals

## Known limitations

- A tap that cannot be acknowledged leaves its buttons in place, so the same
  goal and period can be checked in twice. The log is append-only by design, so
  the review logic in phase 2 must treat repeated entries for one goal and
  period as a single outcome, taking the most recent.
- Check-ins are recorded within one polling interval rather than instantly,
  which is why acknowledgement often expires. A webhook receiver would fix both.

## Phase 2 — review and feedback

- [ ] Streak and grace-window evaluation against the check-in log
- [ ] Weekly review dashboard in `PersonalGoals.qmd` (`format: dashboard`)
- [ ] Publish the dashboard to GitHub Pages and send the link on Sundays
- [ ] Nudge when a grace window is about to be breached
- [ ] Reminders at temporal landmarks, where motivation to restart peaks

## Phase 3 — optional upgrades

- [ ] Cloudflare Worker webhook for instant check-ins instead of polling
- [ ] Editing goals without hand-editing YAML
- [ ] Weekly summary to an accountability partner

## Open questions

- Confirm `America/Denver` is the right timezone for reminders
- Decide whether stakes are tracked only, or enforced through a third party
