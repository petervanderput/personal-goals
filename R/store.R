#' Append-only CSV storage.
#'
#' CSV is deliberate rather than a database: the data lives in the git repo and
#' is written back by CI, so a line-oriented text format produces readable
#' diffs and stays mergeable. A SQLite file would be an opaque binary blob
#' rewritten in full on every check-in. Volume here is a few rows per day.

REMINDER_LOG_COLUMNS <- c("goal_id", "period_key", "sent_at_utc")
CHECKIN_LOG_COLUMNS <- c("goal_id", "period_key", "outcome", "recorded_at_utc")

REMINDER_LOG_PATH <- file.path("data", "reminders_sent.csv")
CHECKIN_LOG_PATH <- file.path("data", "checkins.csv")
OFFSET_PATH <- file.path("data", "telegram_offset.txt")

#' Read an append-only log, returning an empty frame if it does not exist yet.
#'
#' @param path CSV path.
#' @param columns Expected column names.
read_log <- function(path, columns) {
  if (!file.exists(path)) {
    empty <- as.data.frame(
      setNames(replicate(length(columns), character()), columns),
      stringsAsFactors = FALSE
    )
    return(empty)
  }
  log_data <- read.csv(path, colClasses = "character", stringsAsFactors = FALSE)

  missing_columns <- setdiff(columns, names(log_data))
  if (length(missing_columns) > 0) {
    stop("Log '", path, "' is missing column(s): ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  log_data
}

#' Append one record to an append-only log, writing the header if new.
#'
#' @param path CSV path.
#' @param columns Expected column names, defining field order.
#' @param record Named list matching `columns`.
append_log <- function(path, columns, record) {
  unexpected <- setdiff(names(record), columns)
  if (length(unexpected) > 0) {
    stop("Cannot append unknown field(s) to '", path, "': ",
         paste(unexpected, collapse = ", "), call. = FALSE)
  }
  missing_fields <- setdiff(columns, names(record))
  if (length(missing_fields) > 0) {
    stop("Cannot append incomplete record to '", path, "'; missing: ",
         paste(missing_fields, collapse = ", "), call. = FALSE)
  }

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  is_new_file <- !file.exists(path)
  row <- as.data.frame(record[columns], stringsAsFactors = FALSE)

  write.table(row, path, sep = ",", row.names = FALSE, col.names = is_new_file,
              append = !is_new_file, qmethod = "double")
  invisible(row)
}

#' Composite keys of reminders already sent, as "goal_id|period_key".
already_sent_keys <- function(reminder_log) {
  if (nrow(reminder_log) == 0) return(character())
  paste(reminder_log$goal_id, reminder_log$period_key, sep = "|")
}

#' Current UTC timestamp in ISO 8601, used for every recorded time.
utc_timestamp <- function(now = Sys.time()) {
  format(as.POSIXct(now), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

#' Read the Telegram getUpdates offset, or 0 when no updates consumed yet.
read_update_offset <- function(path = OFFSET_PATH) {
  if (!file.exists(path)) return(0L)

  raw_value <- trimws(readLines(path, warn = FALSE)[1])
  offset <- suppressWarnings(as.integer(raw_value))
  if (is.na(offset)) {
    stop("Offset file '", path, "' does not contain an integer: '", raw_value,
         "'.", call. = FALSE)
  }
  offset
}

#' Persist the Telegram getUpdates offset.
write_update_offset <- function(offset, path = OFFSET_PATH) {
  stopifnot(is.numeric(offset), length(offset) == 1L, !is.na(offset))

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(as.character(as.integer(offset)), path)
  invisible(offset)
}
