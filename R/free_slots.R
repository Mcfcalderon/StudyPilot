# ============ SMART SCHEDULER: FREE TIME (lubridate intervals) ============
# Uses lubridate::interval() for precise anti-overlap calculation
obtener_espacio_libre <- function(gcal_events = NULL, schedule_data = NULL,
                                  ai_blocks = NULL,
                                  start_date = Sys.Date(), end_date = Sys.Date() + 6,
                                  sleep_start = "23:00", sleep_end = "07:00") {
  library(lubridate)
  tz_local <- "America/Lima"

  all_free <- list()
  dates <- seq.Date(start_date, end_date, by = "day")
  day_names <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

  # Pre-build unified busy list from GCal/fused events + existing AI blocks
  all_busy_events <- NULL
  if (!is.null(gcal_events) && is.data.frame(gcal_events) && nrow(gcal_events) > 0 &&
      !"error" %in% names(gcal_events)) {
    all_busy_events <- gcal_events
  }
  if (!is.null(ai_blocks) && is.data.frame(ai_blocks) && nrow(ai_blocks) > 0) {
    if (is.null(all_busy_events)) {
      all_busy_events <- ai_blocks
    } else {
      # rbind only common columns
      common <- intersect(names(all_busy_events), names(ai_blocks))
      if (length(common) > 0) {
        all_busy_events <- rbind(all_busy_events[, common, drop = FALSE],
                                  ai_blocks[, common, drop = FALSE])
      }
    }
  }

  for (dd in dates) {
    dd <- as.Date(dd, origin = "1970-01-01")
    day_name <- day_names[wday(dd)]

    day_start <- as.POSIXct(paste(dd, "00:00:00"), tz = tz_local)
    day_end   <- as.POSIXct(paste(dd, "23:59:59"), tz = tz_local)

    busy_intervals <- list()

    # Sleep block
    sl_s <- as.POSIXct(paste(dd, paste0(sleep_start, ":00")), tz = tz_local)
    sl_e <- as.POSIXct(paste(dd, paste0(sleep_end, ":00")), tz = tz_local)
    if (sl_s > sl_e) {
      busy_intervals <- c(busy_intervals,
        list(interval(sl_s, day_end, tzone = tz_local)),
        list(interval(day_start, sl_e, tzone = tz_local))
      )
    } else {
      busy_intervals <- c(busy_intervals, list(interval(sl_s, sl_e, tzone = tz_local)))
    }

    # All busy events (GCal + fused PDF + existing AI) for this day
    if (!is.null(all_busy_events) && nrow(all_busy_events) > 0 && "start" %in% names(all_busy_events)) {
      day_evts <- all_busy_events[!is.na(all_busy_events$start) &
                                  nchar(all_busy_events$start) > 10 &
                                  as.Date(substr(all_busy_events$start, 1, 10)) == dd, ]
      for (k in seq_len(nrow(day_evts))) {
        ev_s <- tryCatch(as.POSIXct(paste0(substr(day_evts$start[k], 1, 16), ":00"), tz = tz_local), error = function(e) NULL)
        ev_e <- tryCatch(as.POSIXct(paste0(substr(day_evts$end[k], 1, 16), ":00"), tz = tz_local), error = function(e) NULL)
        if (!is.null(ev_s) && !is.null(ev_e) && ev_e > ev_s) {
          busy_intervals <- c(busy_intervals, list(interval(ev_s, ev_e, tzone = tz_local)))
        }
      }
    }

    # MongoDB schedule (recurring weekly classes)
    if (!is.null(schedule_data) && is.data.frame(schedule_data) && nrow(schedule_data) > 0) {
      day_sched <- schedule_data[schedule_data$dia == day_name, ]
      for (k in seq_len(nrow(day_sched))) {
        sc_s <- tryCatch(as.POSIXct(paste(dd, paste0(day_sched$hora_inicio[k], ":00")), tz = tz_local), error = function(e) NULL)
        sc_e <- tryCatch(as.POSIXct(paste(dd, paste0(day_sched$hora_fin[k], ":00")), tz = tz_local), error = function(e) NULL)
        if (!is.null(sc_s) && !is.null(sc_e) && sc_e > sc_s) {
          busy_intervals <- c(busy_intervals, list(interval(sc_s, sc_e, tzone = tz_local)))
        }
      }
    }

    # Sort busy by start, merge overlapping
    if (length(busy_intervals) > 0) {
      starts <- sapply(busy_intervals, int_start)
      ends   <- sapply(busy_intervals, int_end)
      ord <- order(starts)
      starts <- starts[ord]; ends <- ends[ord]

      merged_s <- starts[1]; merged_e <- ends[1]
      merged <- list()
      for (j in seq_along(starts)) {
        if (starts[j] <= merged_e) {
          merged_e <- max(merged_e, ends[j])
        } else {
          merged <- c(merged, list(c(merged_s, merged_e)))
          merged_s <- starts[j]; merged_e <- ends[j]
        }
      }
      merged <- c(merged, list(c(merged_s, merged_e)))

      # Extract free gaps
      cursor <- as.numeric(day_start)
      for (m in merged) {
        if (m[1] > cursor) {
          gap_min <- round((m[1] - cursor) / 60)
          if (gap_min >= 30) {
            free_s <- as.POSIXct(cursor, origin = "1970-01-01", tz = tz_local)
            free_e <- as.POSIXct(m[1], origin = "1970-01-01", tz = tz_local)
            all_free <- c(all_free, list(data.frame(
              date = as.character(dd), day = day_name,
              start_time = format(free_s, "%H:%M"),
              end_time = format(free_e, "%H:%M"),
              start_iso = format(free_s, "%Y-%m-%dT%H:%M:%S"),
              end_iso = format(free_e, "%Y-%m-%dT%H:%M:%S"),
              duration_min = gap_min,
              stringsAsFactors = FALSE
            )))
          }
        }
        cursor <- m[2]
      }
      # Last gap
      if (cursor < as.numeric(day_end)) {
        gap_min <- round((as.numeric(day_end) - cursor) / 60)
        if (gap_min >= 30) {
          free_s <- as.POSIXct(cursor, origin = "1970-01-01", tz = tz_local)
          all_free <- c(all_free, list(data.frame(
            date = as.character(dd), day = day_name,
            start_time = format(free_s, "%H:%M"),
            end_time = "23:59",
            start_iso = format(free_s, "%Y-%m-%dT%H:%M:%S"),
            end_iso = format(day_end, "%Y-%m-%dT%H:%M:%S"),
            duration_min = gap_min,
            stringsAsFactors = FALSE
          )))
        }
      }
    }
  }

  if (length(all_free) == 0) return(data.frame(date = character(), day = character(),
    start_time = character(), end_time = character(), start_iso = character(),
    end_iso = character(), duration_min = integer()))
  do.call(rbind, all_free)
}
# Keep old name as alias for compatibility
get_free_time_slots <- obtener_espacio_libre
