# Personal Goals

A goal tracker with no server and no app: goals are defined in a YAML file, a
scheduled job sends reminders to Telegram, and check-ins are recorded by tapping
a button on the notification itself.

## Why this shape

Push notification delivery is the only genuinely hard requirement in a personal
goal tracker; everything else is small-scale CRUD. Using a Telegram bot instead
of native or web push removes the entire mobile toolchain, costs nothing, and
supports inline buttons, which means the daily loop never requires opening an
app. Low-friction logging matters because recording progress is itself one of
the better-evidenced drivers of goal attainment.

## Requirements

R with `httr2`, `jsonlite` and `yaml`. `jsonlite` is required even though httr2
only suggests it, because every API call encodes and decodes JSON.

```r
install.packages(c("httr2", "jsonlite", "yaml"))
```

## Architecture

```
goals.yml                  Goal definitions (hand-edited)
data/reminders_sent.csv    Which reminders have been sent, for idempotency
data/checkins.csv          Append-only log of check-ins
data/telegram_offset.txt   Last consumed Telegram update id

docs/index.html            The published dashboard, rebuilt by CI

R/config.R                 Credential and dependency validation
R/telegram.R               Bot API wrapper; all HTTP lives here
R/callbacks.R              Button payload encoding and decoding
R/goals.R                  Definition loading, validation, calendar helpers
R/quota.R                  Session counting, risk, commitment evaluation
R/reminders.R              Decides what to say and when (pure)
R/send_reminders.R         Performs the sends
R/collect_checkins.R       Polls for button taps and records them
R/dashboard_model.R        Log to view model: day statuses, rollups, tallies
R/dashboard_html.R         View model to a self-contained page
R/dashboard.R              Writes the page, only when it changed
R/run_cycle.R              Scheduler entrypoint: sends, collects, rebuilds

R/preview_reminders.R      Show what would be sent at any given moment
R/build_dashboard.R        Build the page by hand, optionally from sample data
R/discover_chat_id.R       Print chat ids that have messaged the bot
R/inspect_updates.R        Dump the pending Telegram update queue
tools/sample_checkins.R    Generate a sample log to develop the page against
tests/test_logic.R         Dependency-free tests of the pure logic
```

Planning is separated from sending, so every timing rule is testable without
touching the network. The current time is always passed in rather than read from
the clock, which is what lets the tests assert behaviour on specific dates.

Storage is CSV rather than a database because the data lives in git and is
committed back by CI. Line-oriented text produces readable diffs and stays
mergeable, whereas a SQLite file would be a binary blob rewritten in full on
every check-in. Volume is a few rows per day.

Reminders and check-ins share one scheduled invocation because CI minutes are
the binding constraint. GitHub Actions is not a running server, so taps are
polled with `getUpdates`; Telegram queues updates for 24 hours, and the stored
offset marks which have been consumed. A tap is therefore recorded within one
polling interval rather than instantly. If that ever matters, the upgrade is a
Cloudflare Worker acting as a real webhook receiver.

Both the reminder log and the update offset exist to make the cycle idempotent,
since scheduled runs can fire late or more than once. The workflow commits its
logs even when the cycle fails, because a check-in is recorded before it is
acknowledged and Telegram will not deliver the same tap twice.

## Goal schedules

A goal is either a single anchored commitment or a set of counted sessions.

`schedule: fixed` fires on a known day at `remind_at` and is checked in once per
period as done or missed. Weekly, monthly and yearly goals take an anchor day in
`remind_on`. Fixed cues are the better-evidenced choice for habit formation,
since an implementation intention needs something to hang on.

`schedule: quota` asks for a number of sessions per period, listed as
`requirements`. Progress is a count rather than a pass/fail flag, which is what
lets a reward tolerate an occasional miss while the schedule still names a day
for every session.

Each requirement carries its own `implementation_intention`, because sessions of
different kinds happen at different times and places, and the cue is the part
that drives follow-through.

### Anchored and free sessions

A requirement with `on_day` and `remind_at` is prompted on that weekday at that
time, with its own cue and its own logging button. `remind_at` is when to
prompt, which is normally earlier than the session itself; the session time
belongs in the cue.

A requirement without `on_day` can happen on any day and is covered instead by
the goal-level `nudge`. Under the default `risk_only` cadence that message goes
out only on `kickoff_on`, or once the sessions still owed equal the days
remaining and skipping would make the period impossible. That keeps reminders
informative rather than habituating; `cadence: daily` nudges every day. A free
requirement may also name a `by_day` weekday it must happen by, which triggers a
nudge even when the period as a whole is still comfortably achievable.

A goal can mix both styles. Anchored sessions get their own prompts and the
nudge speaks only for the rest.

### Missed sessions and catch-ups

When a goal sets `missed_notice_at` and `missed_session_consequence`, any
anchored session still unlogged at that time gets a notice naming the
consequence for that night. The notice carries a logging button too, so training
that went unrecorded can still be claimed.

`makeup.at` turns a day with no session of its own into a chance to recover one.
On such a day, if an earlier session is still outstanding, a catch-up prompt
offers a button per missed session, so training on an unscheduled day counts
toward the period. Without it a missed day would be unrecoverable even when the
time was made up. The prompt is sent only while something is outstanding, and it
says how many of the missed sessions the remaining days can still absorb.

Sessions are counted as distinct local dates rather than rows, so a repeated tap
counts once, and counts are capped per requirement. That encodes the rule that
one session of a given type counts once per day, and stops a double tap standing
in for a session that never happened.

## Data model

Each goal carries fields drawn from the goal-setting literature, because the
structure is what makes a reminder effective:

| Field | Purpose |
| --- | --- |
| `outcome`, `requirements` | Specific measurable goals outperform "do your best" (Locke & Latham) |
| `implementation_intention` | An if-then cue: when, where, what (Gollwitzer & Sheeran 2006) |
| `obstacle`, `coping_plan` | Mental contrasting, the WOOP protocol (Oettingen) |
| `difficulty` | Harder goals outperform easy ones when commitment holds |
| `intrinsic` | When true, no tangible reward, which would erode intrinsic motivation (Deci et al. 1999) |
| `commitments` | Pre-committed outcomes over a window of periods |
| `missed_session_consequence` | An immediate cost, the loss framing that outperforms an equivalent bonus |

Reminder text quotes the implementation intention verbatim, since the if-then
cue is what carries the behavioural effect rather than the goal title.

### Commitments

A goal can carry several commitments, each judging the same sessions over its
own window against its own minimum. A period counts as short when it finishes
below `min_sessions`, so a commitment that allows one missed session a week and
one that demands all four can run side by side.

| Field | Meaning |
| --- | --- |
| `window.kind: range` | A fixed run of periods between `from` and `to` |
| `window.kind: month` | The calendar month of the current period, judged afresh each month |
| `min_sessions` | Sessions a period needs to avoid counting as short |
| `tiers` | Ascending `max_shortfalls` with the `outcome` each still earns |
| `otherwise` | The outcome once every tier is out of reach |

Only periods that have finished are judged, so a period in progress is never
counted against you, and periods before the goal's `starts` date are ignored
entirely. The first tier whose tolerance still holds is what remains achievable;
once none do, reminders say the reward is gone rather than continuing to dangle
it. A week belongs to the month containing its Monday, so a week straddling two
months is judged in exactly one of them.

## Dashboard

Live at **https://petervanderput.github.io/personal-goals/**, served by GitHub
Pages from `main` at `/docs`. Pages does not serve private repositories on the
free plan, so this repository is public; the credentials live in Actions secrets
and have never been committed, but the check-in log is readable by anyone.

`docs/index.html` is a single self-contained page: no stylesheet, script or font
is fetched, so it renders immediately on a phone over a slow connection. It is
rebuilt at the end of every cycle from the check-in log and written only when the
result differs, which keeps one commit per real change instead of one per poll.
For that reason the page is stamped to the day rather than the minute.

The layout is one narrow column, capped at 460px so a desktop looks deliberate
without altering the phone layout, with 16px body text and a 16px form control,
the size below which mobile browsers zoom on focus.

Three horizons are shown, each as markers on an axis rather than a connected
line, since sessions are separate events and a line between them would imply a
trend that does not exist:

| Horizon | One marker per | Bar |
| --- | --- | --- |
| The chosen week | day | the session scheduled that day |
| The current month | week | the `month` commitment's `min_sessions` |
| The whole run | week | the `range` commitment's `min_sessions` |

The month and run charts take their bars from the commitments rather than
repeating a number, so changing a reward changes the charts with it.

| Marker | Meaning |
| --- | --- |
| Green tick | The session scheduled that day was done |
| Green tick, amber ring | A session made up on a day it was not scheduled for |
| Amber arrow | Scheduled here, but done on another day |
| Red cross | Scheduled, the day has passed, never done |
| Blue dot | Today |
| Grey circle | Still to come |
| Faint dot | No session scheduled, so nothing to miss |

Tuesday, Thursday and Sunday carry no session, so they can never show a cross.
The tally card counts days trained, days skipped and makeups, all derived from
these same day statuses rather than recounted, so the card cannot disagree with
the charts above it.

A `dashboard.digest` block sends a link at the end of each period, once, through
the same reminder log that makes everything else idempotent.

## Setup

1. Create a bot with `@BotFather` in Telegram and copy the token.
2. Send the bot any message. Telegram refuses delivery to a chat that has never
   contacted the bot.
3. Copy `.Renviron.example` to `.Renviron` and fill in the token and your
   numeric chat id. `.Renviron` is gitignored.
4. Add the same two values as GitHub Actions repository secrets named
   `TELEGRAM_BOT_TOKEN` and `TELEGRAM_CHAT_ID`.

## Usage

```sh
Rscript tests/test_logic.R                              # run the test suite
Rscript R/preview_reminders.R --at "2026-08-17 17:30"   # preview, sends nothing
Rscript R/preview_reminders.R --at "..." --send         # deliver a preview
Rscript R/run_cycle.R                                   # send due, collect, build
Rscript R/build_dashboard.R                             # rebuild docs/index.html
Rscript R/discover_chat_id.R                            # find your chat id
Rscript R/inspect_updates.R                             # inspect pending updates
```

To see the page with data in it before any exists, render a sample elsewhere:

```sh
Rscript tools/sample_checkins.R _scratch/sample.csv
Rscript R/build_dashboard.R --log _scratch/sample.csv --out _scratch/index.html \
  --at "2026-09-10 20:00"
```

Edit `goals.yml` to add goals. Times are local to the `timezone` set at the top
of that file, and an unrecognised timezone is a hard error rather than a silent
fallback to UTC.
