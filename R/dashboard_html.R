#' Rendering the view model as a single self-contained HTML page.
#'
#' No external stylesheet, script or font, because the page is opened on a phone
#' from a link and must render before anything else has a chance to load. All
#' glyphs are HTML entities rather than literal characters, so the source file
#' stays pure ASCII and cannot be mangled by an encoding difference between a
#' Windows laptop and a CI container.

#' Marker, colours and wording for each day or week status.
#'
#' `tone` colours the glyph and `ring` the circle around it. They differ for a
#' makeup, which is a green tick because the session happened, inside an amber
#' ring because it happened on another day.
STATUS_GLYPHS <- list(
  done     = list(glyph = "&#10003;", tone = "good", ring = "good",
                  words = "done"),
  makeup   = list(glyph = "&#10003;", tone = "good", ring = "makeup",
                  words = "made up"),
  moved    = list(glyph = "&#8594;", tone = "makeup", ring = "makeup",
                  words = "moved"),
  missed   = list(glyph = "&#10007;", tone = "bad", ring = "bad",
                  words = "skipped"),
  today    = list(glyph = "&#9679;", tone = "now", ring = "now",
                  words = "today"),
  upcoming = list(glyph = "&#9675;", tone = "waiting", ring = "muted",
                  words = "to come"),
  rest     = list(glyph = "&#183;", tone = "rest", ring = "muted",
                  words = "rest day")
)

#' A single status marker: a glyph inside a coloured ring.
render_marker <- function(status) {
  tone <- STATUS_GLYPHS[[status]]

  sprintf("<span class=\"marker %s ring-%s\">%s</span>", tone$tone, tone$ring,
          tone$glyph)
}

#' Escape the characters that would otherwise break out of HTML text.
escape_html <- function(text) {
  if (is.null(text)) return("")
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  gsub("\"", "&quot;", text, fixed = TRUE)
}

#' Render the complete dashboard document.
dashboard_html <- function(model) {
  paste(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    # width=device-width is what makes the page lay out at phone width instead
    # of being rendered wide and zoomed out.
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<meta name=\"color-scheme\" content=\"dark\">",
    sprintf("<title>%s</title>", escape_html(model$title)),
    dashboard_style(),
    "</head>",
    "<body>",
    "<main>",
    render_header(model),
    render_tally(model$tally),
    render_week_section(model),
    render_horizon_section(model$month$label, model$month, columns = 5),
    render_horizon_section(paste("Run:", model$run$label), model$run,
                           columns = 8),
    render_commitments(model$commitments, model$period),
    render_legend(),
    "</main>",
    dashboard_script(),
    "</body>",
    "</html>",
    sep = "\n"
  )
}

#' Stylesheet: one column, sized for a phone held in one hand.
dashboard_style <- function() {
  paste(
    "<style>",
    ":root {",
    "  --bg: #0f1115; --card: #191d24; --line: #2b313b;",
    "  --text: #e8eaed; --muted: #9aa0a6;",
    "  --good: #34d399; --bad: #f87171; --makeup: #fbbf24; --now: #60a5fa;",
    "}",
    "* { box-sizing: border-box; }",
    "body {",
    "  margin: 0; background: var(--bg); color: var(--text);",
    "  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,",
    "    Helvetica, Arial, sans-serif;",
    "  font-size: 16px; line-height: 1.5;",
    "  -webkit-text-size-adjust: 100%;",
    "}",
    # A max width keeps the page comfortable on a desktop without changing the
    # phone layout, which is the one that matters.
    "main { max-width: 460px; margin: 0 auto; padding: 16px 16px 40px; }",
    "h1 { font-size: 20px; line-height: 1.3; margin: 0 0 4px; }",
    "h2 { font-size: 15px; text-transform: uppercase; letter-spacing: 0.06em;",
    "     color: var(--muted); margin: 0 0 10px; font-weight: 600; }",
    ".stamp { color: var(--muted); font-size: 13px; margin: 0 0 20px; }",
    ".card { background: var(--card); border: 1px solid var(--line);",
    "        border-radius: 14px; padding: 16px; margin-bottom: 16px; }",
    ".summary { display: flex; gap: 8px; text-align: center; }",
    ".summary > div { flex: 1; }",
    ".figure { font-size: 30px; font-weight: 700; line-height: 1.1; }",
    ".figure-label { font-size: 12px; color: var(--muted);",
    "                text-transform: uppercase; letter-spacing: 0.04em; }",
    ".good { color: var(--good); } .bad { color: var(--bad); }",
    ".makeup { color: var(--makeup); } .now { color: var(--now); }",
    ".waiting, .rest { color: var(--muted); }",
    ".progress { font-size: 15px; margin: 0 0 14px; }",
    ".progress strong { font-size: 17px; }",
    ".left { color: var(--muted); }",
    # The track: a faint line behind the markers, drawn per cell so that a row
    # of markers reads as points on an axis rather than a connected line.
    ".track { display: grid; gap: 2px 0; }",
    ".node { position: relative; text-align: center; padding: 14px 0 6px; }",
    ".node::before { content: \"\"; position: absolute; left: 0; right: 0;",
    "  top: 28px; height: 2px; background: var(--line); }",
    ".marker { position: relative; display: inline-flex; align-items: center;",
    "  justify-content: center; width: 26px; height: 26px; border-radius: 50%;",
    "  background: var(--card); border: 2px solid var(--line);",
    "  font-size: 15px; line-height: 1; }",
    ".ring-good { border-color: var(--good); }",
    ".ring-bad { border-color: var(--bad); }",
    ".ring-makeup { border-color: var(--makeup); }",
    ".ring-now { border-color: var(--now); }",
    ".tick { font-size: 12px; color: var(--muted); margin-top: 5px; }",
    ".tick-strong { color: var(--text); font-weight: 600; }",
    "select { width: 100%; margin-bottom: 14px; padding: 10px 12px;",
    "  font-size: 16px; font-family: inherit; color: var(--text);",
    "  background: #22272f; border: 1px solid var(--line); border-radius: 10px;",
    "  appearance: none; }",
    ".detail { list-style: none; margin: 14px 0 0; padding: 0;",
    "  border-top: 1px solid var(--line); }",
    ".detail li { display: flex; gap: 10px; align-items: baseline;",
    "  padding: 9px 0; border-bottom: 1px solid var(--line); font-size: 14px; }",
    ".detail .day { width: 34px; color: var(--muted); flex: none; }",
    ".detail .what { flex: 1; }",
    ".detail .state { flex: none; font-size: 12px;",
    "  text-transform: uppercase; letter-spacing: 0.04em; }",
    ".hidden { display: none; }",
    # Stacked rather than two columns: at phone width an outcome and a progress
    # line side by side both wrap, which is harder to read than two short lines.
    ".stake { padding: 12px 0; border-bottom: 1px solid var(--line);",
    "  font-size: 14px; }",
    ".stake:first-of-type { padding-top: 0; }",
    ".stake:last-child { border-bottom: 0; padding-bottom: 0; }",
    ".stake-outcome { font-size: 15px; }",
    ".stake-progress { color: var(--muted); margin-top: 2px; }",
    ".legend { display: flex; flex-wrap: wrap; gap: 10px 16px;",
    "  font-size: 13px; color: var(--muted); }",
    ".legend .key { display: inline-flex; align-items: center; gap: 6px;",
    "  white-space: nowrap; }",
    ".legend .marker { width: 22px; height: 22px; font-size: 13px;",
    "  background: var(--bg); }",
    "</style>",
    sep = "\n"
  )
}

#' Title and the day the page was last rebuilt.
render_header <- function(model) {
  paste(
    sprintf("<h1>%s</h1>", escape_html(model$title)),
    sprintf("<p class=\"stamp\">Updated %s</p>",
            escape_html(model$generated_at)),
    sep = "\n"
  )
}

#' The three lifetime figures.
render_tally <- function(tally) {
  figure <- function(value, label, tone) {
    sprintf(paste0("<div><div class=\"figure %s\">%d</div>",
                   "<div class=\"figure-label\">%s</div></div>"),
            tone, value, label)
  }

  paste(
    "<section class=\"card summary\">",
    figure(tally$boxed, "Boxed", "good"),
    figure(tally$skipped, "Skipped", "bad"),
    figure(tally$makeups, "Made up", "makeup"),
    "</section>",
    sep = "\n"
  )
}

#' The week section: a picker, then one panel per week.
render_week_section <- function(model) {
  panels <- vapply(model$weeks, function(week) {
    render_week_panel(week, week$key == model$selected_week)
  }, character(1))

  paste(
    "<section class=\"card\">",
    "<h2>By week</h2>",
    render_week_picker(model),
    paste(panels, collapse = "\n"),
    "</section>",
    sep = "\n"
  )
}

#' Dropdown listing every week of the run.
render_week_picker <- function(model) {
  options <- vapply(model$weeks, function(week) {
    marker <- if (week$is_current) " (this week)" else ""
    sprintf("<option value=\"%s\"%s>%s%s &mdash; %d of %d</option>",
            escape_html(week$key),
            if (week$key == model$selected_week) " selected" else "",
            escape_html(week$label), marker, week$completed, week$required)
  }, character(1))

  paste0("<select id=\"week-picker\" aria-label=\"Choose a week\">\n",
         paste(options, collapse = "\n"), "\n</select>")
}

#' One week: the seven-day track, its progress line and a per-session list.
render_week_panel <- function(week, is_selected) {
  nodes <- lapply(week$days, function(day) {
    list(status = day$status,
         tick = day$weekday,
         emphasise = day$status %in% c("done", "makeup", "missed"))
  })

  paste(
    sprintf("<div class=\"week-panel%s\" data-week=\"%s\">",
            if (is_selected) "" else " hidden", escape_html(week$key)),
    sprintf("<p class=\"progress\"><strong>%s</strong> &mdash; %s</p>",
            escape_html(week_progress_words(week)),
            escape_html(week_remaining_words(week))),
    render_track(nodes, columns = 7),
    render_week_detail(week),
    "</div>",
    sep = "\n"
  )
}

#' "3 of 4 sessions" for a week.
week_progress_words <- function(week) {
  sprintf("%d of %d sessions", week$completed, week$required)
}

#' What is still owed this week, or what it ended up missing.
#'
#' A finished week is short rather than having sessions left, since there is no
#' longer any time in it to do them.
week_remaining_words <- function(week) {
  if (week$remaining == 0L) return("all done")
  if (week$is_future) return(sprintf("%d to come", week$remaining))
  if (week$is_current) return(sprintf("%d left", week$remaining))
  sprintf("%d short", week$remaining)
}

#' Named list of what happened on each day that was not a rest day.
render_week_detail <- function(week) {
  active <- Filter(function(day) day$status != "rest", week$days)
  if (length(active) == 0) return("")

  rows <- vapply(active, function(day) {
    tone <- STATUS_GLYPHS[[day$status]]
    sprintf(paste0("<li><span class=\"day\">%s</span>",
                   "<span class=\"what\">%s</span>",
                   "<span class=\"state %s\">%s</span></li>"),
            escape_html(day$weekday), escape_html(day$label %||% ""),
            tone$tone, tone$words)
  }, character(1))

  paste0("<ul class=\"detail\">\n", paste(rows, collapse = "\n"), "\n</ul>")
}

#' A month or run section: progress line then a track of one node per week.
render_horizon_section <- function(heading, horizon, columns) {
  nodes <- lapply(horizon$weeks, function(week) {
    list(status = week$status, tick = week$tick,
         emphasise = week$status %in% c("done", "missed", "today"))
  })

  paste(
    "<section class=\"card\">",
    sprintf("<h2>%s</h2>", escape_html(heading)),
    sprintf("<p class=\"progress\"><strong>%d of %d weeks</strong> &mdash; %s</p>",
            horizon$met, horizon$total,
            escape_html(horizon_remaining_words(horizon))),
    render_track(nodes, columns),
    "</section>",
    sep = "\n"
  )
}

#' "at 3+ sessions, 1 short, 11 to go".
horizon_remaining_words <- function(horizon) {
  parts <- sprintf("at %d+ sessions", horizon$min_sessions)
  if (horizon$short > 0) parts <- c(parts, sprintf("%d short", horizon$short))
  if (horizon$left > 0) parts <- c(parts, sprintf("%d to go", horizon$left))
  paste(parts, collapse = ", ")
}

#' Lay markers out on a wrapping grid so a long run stays readable on a phone.
render_track <- function(nodes, columns) {
  cells <- vapply(nodes, function(node) {
    sprintf("<div class=\"node\">%s<div class=\"tick%s\">%s</div></div>",
            render_marker(node$status),
            if (isTRUE(node$emphasise)) " tick-strong" else "",
            escape_html(node$tick))
  }, character(1))

  paste0(sprintf("<div class=\"track\" style=\"grid-template-columns:repeat(%d,1fr)\">\n",
                 columns),
         paste(cells, collapse = "\n"), "\n</div>")
}

#' Where each reward currently stands.
render_commitments <- function(commitments, period) {
  running <- Filter(function(status) status$is_active, commitments)
  if (length(running) == 0) return("")

  rows <- vapply(running, function(status) {
    sprintf(paste0("<div class=\"stake\">",
                   "<div class=\"stake-outcome %s\">%s</div>",
                   "<div class=\"stake-progress\">%s &mdash; %d of %d %ss in, ",
                   "%d short</div></div>"),
            if (status$is_lost) "bad" else "good",
            escape_html(status$best_achievable), escape_html(status$label),
            status$finished_periods, status$total_periods, escape_html(period),
            status$shortfall_periods)
  }, character(1))

  paste(
    "<section class=\"card\">",
    "<h2>On the line</h2>",
    paste(rows, collapse = "\n"),
    "</section>",
    sep = "\n"
  )
}

#' Key to the markers, without which the amber ones are guesswork.
#'
#' Shows the real markers rather than bare glyphs, since done and made up share a
#' glyph and are told apart only by their ring.
render_legend <- function() {
  entries <- c("done", "makeup", "moved", "missed", "today", "upcoming")

  items <- vapply(entries, function(status) {
    sprintf("<span class=\"key\">%s %s</span>", render_marker(status),
            STATUS_GLYPHS[[status]]$words)
  }, character(1))

  paste0("<section class=\"legend\">", paste(items, collapse = ""),
         "</section>")
}

#' Switch which week panel is visible. Everything else is already in the page.
dashboard_script <- function() {
  paste(
    "<script>",
    "(function () {",
    "  var picker = document.getElementById('week-picker');",
    "  var panels = document.querySelectorAll('.week-panel');",
    "  if (!picker) { return; }",
    "  picker.addEventListener('change', function () {",
    "    panels.forEach(function (panel) {",
    "      panel.classList.toggle('hidden',",
    "        panel.getAttribute('data-week') !== picker.value);",
    "    });",
    "  });",
    "})();",
    "</script>",
    sep = "\n"
  )
}
