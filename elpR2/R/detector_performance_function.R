#' Evaluate detector performance against truth selection tables
#'
#' Reads truth and detector selection tables, restricts both to the site/date/
#' time periods in a schedule, and calculates precision and recall at each
#' detector score threshold.
#'
#' Selection tables must contain \code{Begin File}, \code{Begin Time (s)},
#' \code{End Time (s)}, and \code{File Offset (s)}. The source filename must
#' contain a \code{YYYYMMDD_HHMMSS} recording timestamp and, when no \code{Site}
#' column is present, a site identifier in the \code{cc##c} format. Schedule
#' files are tab-delimited and must contain \code{Begin Date}, \code{Begin Time},
#' \code{End Date}, \code{End Time}, and \code{Site}.
#'
#' An event is included when its time interval overlaps a scheduled interval at
#' the same site. Truth and detector events are considered a match when their
#' intervals overlap at the same site; if \code{class_column} is supplied, the
#' class values must also match. Event start time is the recording timestamp
#' in \code{Begin File} plus \code{File Offset (s)}; event duration is
#' \code{End Time (s)} minus \code{Begin Time (s)}.
#'
#' @param truth_tables A list of selection-table data frames, or a directory
#'   containing tab-delimited selection tables.
#' @param test_tables A list of detector selection-table data frames, or a
#'   directory containing tab-delimited selection tables.
#' @param schedule_path A tab-delimited schedule filename or a data frame with
#'   \code{Begin Date}, \code{Begin Time}, \code{End Date}, \code{End Time}, and
#'   \code{Site} columns.
#' @param score_column Name of the numeric detector score column.
#' @param score_thresholds Numeric vector of inclusive score thresholds.
#' @param min_overlap_percent Minimum overlap percentage for a match. A match
#'   qualifies when the overlap is at least this percentage of either event.
#' @param class_column Optional column name identifying event classes. If
#'   supplied, it must exist in both truth and test tables.
#'
#' @return A list containing \code{metrics}, the schedule-filtered
#'   \code{truth_events} and \code{test_events}, the normalized \code{schedule},
#'   and total scheduled recording hours (summed across sites).
#' @export
detector_performance_function <- function(
    truth_tables,
    test_tables,
    schedule_path,
    score_column = "Score",
    score_thresholds = seq(0, 1, by = 0.01),
    min_overlap_percent = 0,
    class_column = NULL
) {
  required_schedule <- c("Begin Date", "Begin Time", "End Date", "End Time", "Site")
  required_table <- c("Begin File", "Begin Time (s)", "End Time (s)", "File Offset (s)")

  if (!is.numeric(score_thresholds) || length(score_thresholds) == 0L ||
      any(!is.finite(score_thresholds))) {
    stop("'score_thresholds' must be a non-empty vector of finite numbers.")
  }
  if (length(min_overlap_percent) != 1L || !is.numeric(min_overlap_percent) ||
      !is.finite(min_overlap_percent) || min_overlap_percent < 0 ||
      min_overlap_percent > 100) {
    stop("'min_overlap_percent' must be a number between 0 and 100.")
  }

  read_tables <- function(x, input_name) {
    if (is.character(x) && length(x) == 1L) {
      if (!dir.exists(x)) {
        stop("'", input_name, "' directory does not exist: ", x)
      }
      paths <- list.files(x, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
      if (length(paths) == 0L) {
        stop("No .txt selection tables found in '", input_name, "': ", x)
      }
      tables <- lapply(paths, function(path) {
        tryCatch(
          utils::read.delim(
            path, check.names = FALSE, quote = "\"", comment.char = "",
            stringsAsFactors = FALSE
          ),
          error = function(e) {
            stop("Could not read selection table '", path, "': ", e$message)
          }
        )
      })
      names(tables) <- basename(paths)
    } else if (is.data.frame(x)) {
      tables <- list(x)
    } else if (is.list(x) && length(x) > 0L &&
               all(vapply(x, is.data.frame, logical(1)))) {
      tables <- x
    } else {
      stop("'", input_name, "' must be a directory, data frame, or non-empty list of data frames.")
    }
    tables
  }

  parse_date <- function(x, column_name) {
    value <- as.character(x)
    parsed <- as.Date(value, format = "%m/%d/%Y")
    missing <- is.na(parsed)
    parsed[missing] <- as.Date(value[missing], format = "%Y-%m-%d")
    missing <- is.na(parsed)
    parsed[missing] <- as.Date(value[missing], format = "%m/%d/%y")
    if (anyNA(parsed)) {
      stop("Could not parse one or more dates in schedule column '", column_name, "'.")
    }
    parsed
  }

  parse_time_seconds <- function(x, column_name) {
    value <- trimws(as.character(x))
    formats <- c("%H:%M:%S", "%H:%M")
    parsed <- rep(NA_real_, length(value))
    for (format in formats) {
      missing <- is.na(parsed)
      parsed[missing] <- as.numeric(strptime(value[missing], format = format, tz = "UTC")) %% 86400
    }
    if (anyNA(parsed)) {
      stop("Could not parse one or more times in schedule column '", column_name, "'.")
    }
    parsed
  }

  if (is.character(schedule_path) && length(schedule_path) == 1L) {
    if (!file.exists(schedule_path)) {
      stop("Schedule file does not exist: ", schedule_path)
    }
    schedule <- tryCatch(
      utils::read.delim(
        schedule_path, check.names = FALSE, quote = "\"", comment.char = "",
        stringsAsFactors = FALSE
      ),
      error = function(e) {
        stop("Could not read schedule file '", schedule_path, "': ", e$message)
      }
    )
  } else if (is.data.frame(schedule_path)) {
    schedule <- schedule_path
  } else {
    stop("'schedule_path' must be a schedule filename or data frame.")
  }
  missing_schedule <- setdiff(required_schedule, names(schedule))
  if (length(missing_schedule) > 0L) {
    stop("Schedule is missing required columns: ", paste(missing_schedule, collapse = ", "))
  }
  if (nrow(schedule) == 0L) {
    stop("Schedule contains no periods.")
  }

  schedule$Site <- toupper(trimws(as.character(schedule$Site)))
  if (anyNA(schedule$Site) || any(schedule$Site == "")) {
    stop("Schedule contains a missing or empty site.")
  }
  start_date <- parse_date(schedule[["Begin Date"]], "Begin Date")
  end_date <- parse_date(schedule[["End Date"]], "End Date")
  start_seconds <- parse_time_seconds(schedule[["Begin Time"]], "Begin Time")
  end_seconds <- parse_time_seconds(schedule[["End Time"]], "End Time")
  schedule$Start <- as.POSIXct(start_date, tz = "UTC") + start_seconds
  schedule$End <- as.POSIXct(end_date, tz = "UTC") + end_seconds
  if (any(schedule$End <= schedule$Start)) {
    stop("Every schedule period must end after it begins.")
  }

  merged_periods <- lapply(unique(schedule$Site), function(site) {
    periods <- schedule[schedule$Site == site, c("Start", "End"), drop = FALSE]
    periods <- periods[order(periods$Start), , drop = FALSE]
    starts <- periods$Start[1]
    ends <- periods$End[1]
    if (nrow(periods) > 1L) {
      for (i in seq.int(2L, nrow(periods))) {
        if (periods$Start[i] <= ends[length(ends)]) {
          ends[length(ends)] <- max(ends[length(ends)], periods$End[i])
        } else {
          starts <- c(starts, periods$Start[i])
          ends <- c(ends, periods$End[i])
        }
      }
    }
    data.frame(Site = site, Start = starts, End = ends, stringsAsFactors = FALSE)
  })
  merged_schedule <- do.call(rbind, merged_periods)
  rownames(merged_schedule) <- NULL
  schedule_hours <- sum(as.numeric(difftime(
    merged_schedule$End, merged_schedule$Start, units = "hours"
  )))
  scheduled_overlap_seconds <- function(site, start, end) {
    start_seconds <- as.numeric(start)
    end_seconds <- as.numeric(end)
    period_starts <- as.numeric(merged_schedule$Start)
    period_ends <- as.numeric(merged_schedule$End)
    periods <- merged_schedule[
      merged_schedule$Site == site &
        period_starts < end_seconds &
        period_ends > start_seconds,
      ,
      drop = FALSE
    ]
    if (nrow(periods) == 0L) {
      return(0)
    }
    period_starts <- as.numeric(periods$Start)
    period_ends <- as.numeric(periods$End)
    sum(pmax(
      0,
      pmin(end_seconds, period_ends) - pmax(start_seconds, period_starts)
    ))
  }

  truth_tables <- read_tables(truth_tables, "truth_tables")
  test_tables <- read_tables(test_tables, "test_tables")

  standardize_tables <- function(tables, input_name, is_test = FALSE) {
    rows <- lapply(seq_along(tables), function(i) {
      table <- tables[[i]]
      table_name <- names(tables)[i]
      if (is.null(table_name) || is.na(table_name) || table_name == "") {
        table_name <- paste0(input_name, "_", i)
      }
      missing <- setdiff(required_table, names(table))
      if (length(missing) > 0L) {
        stop("Table '", table_name, "' is missing required columns: ",
             paste(missing, collapse = ", "))
      }
      if (is_test && !score_column %in% names(table)) {
        stop("Detector table '", table_name, "' is missing score column '", score_column, "'.")
      }
      if (!is.null(class_column) && !class_column %in% names(table)) {
        stop("Table '", table_name, "' is missing class column '", class_column, "'.")
      }
      if (nrow(table) == 0L) {
        return(data.frame(
          File_Name = character(), Site = character(),
          Start = as.POSIXct(character(), tz = "UTC"),
          End = as.POSIXct(character(), tz = "UTC"),
          Score = numeric(), Class = character(),
          Source_Table = character(), Source_Row = integer(),
          stringsAsFactors = FALSE
        ))
      }

      file_name <- basename(as.character(table[["Begin File"]]))
      if (anyNA(file_name) || any(file_name == "")) {
        stop("Table '", table_name, "' contains a missing or empty 'Begin File' value.")
      }
      recording_stamp <- regmatches(
        file_name,
        regexpr("[0-9]{8}[_T.-]?[0-9]{6}", file_name, perl = TRUE)
      )
      if (any(recording_stamp == "")) {
        bad <- which(recording_stamp == "")[1]
        stop("Could not find a YYYYMMDD_HHMMSS timestamp in '",
             file_name[bad], "' (table '", table_name, "').")
      }
      recording_stamp <- gsub("[_T.-]", "", recording_stamp)
      file_start <- as.POSIXct(
        strptime(recording_stamp, format = "%Y%m%d%H%M%S", tz = "UTC")
      )
      if (anyNA(file_start)) {
        stop("Could not parse a recording timestamp in table '", table_name, "'.")
      }

      site_match <- regmatches(
        file_name,
        regexpr("[[:alpha:]]{2}[[:digit:]]{2}[[:alpha:]]", file_name, perl = TRUE)
      )
      if ("Site" %in% names(table)) {
        site <- toupper(trimws(as.character(table$Site)))
        derive_site <- is.na(site) | site == ""
        site[derive_site] <- toupper(site_match[derive_site])
      } else {
        site <- toupper(site_match)
      }
      if (anyNA(site) || any(site == "")) {
        stop("Could not determine a site for one or more rows in table '", table_name,
             "'. Add a 'Site' column or include a cc##c site code in 'Begin File'.")
      }

      begin_time <- suppressWarnings(as.numeric(as.character(table[["Begin Time (s)"]])))
      end_time <- suppressWarnings(as.numeric(as.character(table[["End Time (s)"]])))
      offset <- suppressWarnings(as.numeric(as.character(table[["File Offset (s)"]])))
      if (any(!is.finite(begin_time) | !is.finite(end_time) | !is.finite(offset))) {
        stop("Table '", table_name, "' contains non-numeric or missing selection times.")
      }
      if (any(end_time < begin_time)) {
        stop("Table '", table_name, "' contains an end time before a begin time.")
      }
      score <- if (is_test) {
        suppressWarnings(as.numeric(as.character(table[[score_column]])))
      } else {
        rep(NA_real_, nrow(table))
      }
      if (is_test && any(!is.finite(score))) {
        stop("Detector table '", table_name, "' contains missing or non-numeric values in '",
             score_column, "'.")
      }

      event_start <- file_start + offset
      event_end <- event_start + (end_time - begin_time)
      eligible <- vapply(seq_len(nrow(table)), function(row) {
        scheduled_overlap_seconds(site[row], event_start[row], event_end[row]) > 0
      }, logical(1))

      data.frame(
        File_Name = file_name,
        Site = site,
        Start = event_start,
        End = event_end,
        Score = score,
        Class = if (is.null(class_column)) {
          rep("All", nrow(table))
        } else {
          as.character(table[[class_column]])
        },
        Source_Table = rep(table_name, nrow(table)),
        Source_Row = seq_len(nrow(table)),
        stringsAsFactors = FALSE
      )[eligible, , drop = FALSE]
    })
    result <- do.call(rbind, rows)
    rownames(result) <- NULL
    result
  }

  truth_events <- standardize_tables(truth_tables, "truth_tables")
  test_events <- standardize_tables(test_tables, "test_tables", is_test = TRUE)
  if (!is.null(class_column) &&
      (anyNA(truth_events$Class) || anyNA(test_events$Class))) {
    stop("The class column contains missing values.")
  }

  metrics <- lapply(score_thresholds, function(threshold) {
    detections <- which(test_events$Score >= threshold)
    matched_test <- logical(length(detections))
    matched_truth <- logical(nrow(truth_events))
    truth_start_seconds <- as.numeric(truth_events$Start)
    truth_end_seconds <- as.numeric(truth_events$End)

    for (detection_index in seq_along(detections)) {
      detection <- test_events[detections[detection_index], , drop = FALSE]
      detection_start_seconds <- as.numeric(detection$Start)
      detection_end_seconds <- as.numeric(detection$End)
      candidates <- which(
        truth_events$Site == detection$Site &
          truth_start_seconds < detection_end_seconds &
          truth_end_seconds > detection_start_seconds
      )
      if (!is.null(class_column)) {
        candidates <- candidates[truth_events$Class[candidates] == detection$Class]
      }
      if (length(candidates) > 0L) {
        overlap <- vapply(candidates, function(candidate) {
          scheduled_overlap_seconds(
            detection$Site,
            max(detection_start_seconds, truth_start_seconds[candidate]),
            min(detection_end_seconds, truth_end_seconds[candidate])
          )
        }, numeric(1))
        detection_duration <- detection_end_seconds - detection_start_seconds
        truth_duration <- truth_end_seconds[candidates] - truth_start_seconds[candidates]
        detection_percent <- ifelse(detection_duration > 0, 100 * overlap / detection_duration, 0)
        truth_percent <- ifelse(truth_duration > 0, 100 * overlap / truth_duration, 0)
        matched <- overlap > 0 & (
          detection_percent >= min_overlap_percent |
            truth_percent >= min_overlap_percent
        )
        if (any(matched)) {
          matched_test[detection_index] <- TRUE
          matched_truth[candidates[matched]] <- TRUE
        }
      }
    }

    tp_test <- sum(matched_test)
    fp_test <- length(detections) - tp_test
    tp_truth <- sum(matched_truth)
    precision <- if (length(detections) == 0L) NA_real_ else tp_test / length(detections)
    recall <- if (nrow(truth_events) == 0L) NA_real_ else tp_truth / nrow(truth_events)
    f1 <- if (is.na(precision) || is.na(recall) || precision + recall == 0) {
      0
    } else {
      2 * precision * recall / (precision + recall)
    }

    data.frame(
      Threshold = threshold,
      Total_Truth = nrow(truth_events),
      Total_Detections = length(detections),
      TP_Truth = tp_truth,
      FN_Truth = nrow(truth_events) - tp_truth,
      TP_Detections = tp_test,
      FP_Detections = fp_test,
      Precision = precision,
      Recall = recall,
      F1_Score = f1,
      FP_per_hour = if (schedule_hours > 0) fp_test / schedule_hours else NA_real_,
      Total_Hours = schedule_hours
    )
  })

  list(
    metrics = do.call(rbind, metrics),
    truth_events = truth_events,
    test_events = test_events,
    schedule = schedule,
    total_hours = schedule_hours
  )
}
