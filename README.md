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

## Architecture

```
goals.yml                  Goal definitions (hand-edited)
data/reminders_sent.csv    Which reminders have been sent, for idempotency
data/checkins.csv          Append-only Done/Missed log
data/telegram_offset.txt   Last consumed Telegram update id
R/config.R                 Credential loading and validation
R/telegram.R               Bot API wrapper; all HTTP lives here
R/goals.R                  Definitions, scheduling rules, reminder text (pure)
R/store.R                  Append-only CSV helpers
R/send_reminders.R         Decides and sends what is due
R/collect_checkins.R       Polls for button taps and records them
R/run_cycle.R              Scheduler entrypoint: sends, then collects
tests/test_logic.R         Dependency-free tests of the pure logic
```

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
since scheduled runs can fire late or more than once.

## Data model

Each goal in `goals.yml` carries fields drawn from the goal-setting literature,
because the structure is what makes a reminder effective:

| Field | Purpose |
| --- | --- |
| `outcome`, `measure`, `target` | Specific measurable goals outperform "do your best" (Locke & Latham) |
| `implementation_intention` | An if-then cue: when, where, what (Gollwitzer & Sheeran 2006) |
| `obstacle`, `coping_plan` | Mental contrasting, the WOOP protocol (Oettingen) |
| `difficulty` | Harder goals outperform easy ones when commitment holds |
| `grace` | A rate-based allowance; one missed day does not harm habit formation (Lally et al. 2010) |
| `intrinsic` | When true, no tangible reward, which would erode intrinsic motivation (Deci et al. 1999) |
| `stake` | Pre-committed loss, which outperforms an equivalent bonus |

Reminder text quotes the implementation intention verbatim, since the if-then
cue is what carries the behavioural effect rather than the goal title.

## Requirements

R with `httr2`, `jsonlite` and `yaml`. `jsonlite` is required even though httr2
only suggests it, because every API call encodes and decodes JSON.

```r
install.packages(c("httr2", "jsonlite", "yaml"))
```

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
Rscript tests/test_logic.R        # run the test suite
Rscript R/discover_chat_id.R      # print chat ids that have messaged the bot
Rscript R/test_connection.R       # send a test message with buttons
Rscript R/run_cycle.R             # send due reminders, collect taps
```

Edit `goals.yml` to add goals. Reminder times are local to the `timezone` set at
the top of that file.
