# ============ GOOGLE CALENDAR VIA PUBLIC ICS FEED ============
# No API key needed — uses the public ICS URL

gcal_get_events <- function(calendar_email) {
  ics_url <- paste0("https://calendar.google.com/calendar/ical/",
                    URLencode(calendar_email, reserved = TRUE),
                    "/public/basic.ics")

  tryCatch({
    tmp <- tempfile(fileext = ".ics")
    download.file(ics_url, tmp, quiet = TRUE, method = "libcurl")
    lines <- readLines(tmp, warn = FALSE, encoding = "UTF-8")
    unlink(tmp)

    if (length(lines) < 5 || !any(grepl("BEGIN:VCALENDAR", lines))) {
      return(data.frame(error = "No se pudo leer el calendario. Verifica que sea público."))
    }

    parse_ics_events(lines)
  }, error = function(e) {
    data.frame(error = paste0("Error de conexión: ", e$message))
  })
}

parse_ics_date <- function(dt_string) {
  dt_string <- trimws(dt_string)
  if (nchar(dt_string) == 8) {
    paste0(substr(dt_string,1,4), "-", substr(dt_string,5,6), "-", substr(dt_string,7,8))
  } else if (nchar(dt_string) >= 15) {
    is_utc <- grepl("Z$", dt_string)
    raw <- paste0(substr(dt_string,1,4), "-", substr(dt_string,5,6), "-", substr(dt_string,7,8),
           "T", substr(dt_string,10,11), ":", substr(dt_string,12,13))
    # Convert UTC to Peru time (UTC-5) if Z suffix present
    if (is_utc) {
      utc_time <- as.POSIXct(paste0(raw, ":00"), tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
      local_time <- format(utc_time, "%Y-%m-%dT%H:%M", tz = "America/Lima")
      return(local_time)
    }
    raw
  } else {
    dt_string
  }
}

parse_ics_events <- function(lines) {
  events <- list()
  in_event <- FALSE
  current <- list()

  for (line in lines) {
    if (grepl("^BEGIN:VEVENT", line)) {
      in_event <- TRUE
      current <- list(summary = "", start = "", end = "", description = "",
                      location = "", rrule = "", start_raw = "", end_raw = "",
                      exdates = character(0), recurrence_id = "", uid = "")
    } else if (grepl("^END:VEVENT", line) && in_event) {
      in_event <- FALSE
      events <- c(events, list(current))
    } else if (in_event) {
      if (grepl("^SUMMARY", line)) current$summary <- sub("^SUMMARY[^:]*:", "", line)
      else if (grepl("^DTSTART", line)) {
        raw <- sub("^DTSTART[^:]*:", "", line)
        current$start_raw <- trimws(raw)
        current$start <- parse_ics_date(raw)
      }
      else if (grepl("^DTEND", line)) {
        raw <- sub("^DTEND[^:]*:", "", line)
        current$end_raw <- trimws(raw)
        current$end <- parse_ics_date(raw)
      }
      else if (grepl("^DESCRIPTION", line)) current$description <- sub("^DESCRIPTION[^:]*:", "", line)
      else if (grepl("^LOCATION", line)) current$location <- sub("^LOCATION[^:]*:", "", line)
      else if (grepl("^RRULE", line)) current$rrule <- sub("^RRULE:", "", line)
      else if (grepl("^EXDATE", line)) {
        raw <- sub("^EXDATE[^:]*:", "", line)
        current$exdates <- c(current$exdates, parse_ics_date(trimws(raw)))
      }
      else if (grepl("^RECURRENCE-ID", line)) {
        current$recurrence_id <- parse_ics_date(sub("^RECURRENCE-ID[^:]*:", "", line))
      }
      else if (grepl("^UID", line)) {
        current$uid <- sub("^UID[^:]*:", "", line)
      }
    }
  }

  if (length(events) == 0) return(data.frame(error = "No se encontraron eventos."))

  # Date range for expansion (6 months each way to cover full semesters)
  cutoff_past <- Sys.Date() - 180
  cutoff_future <- Sys.Date() + 180

  # Collect override dates by UID (events with RECURRENCE-ID replace a recurring instance)
  # Key: UID|date → the override replaces the recurring instance on that date
  override_map <- list()  # uid -> vector of dates
  for (ev in events) {
    if (nchar(ev$recurrence_id) > 0 && nchar(ev$uid) > 0) {
      d <- substr(ev$recurrence_id, 1, 10)
      override_map[[ev$uid]] <- c(override_map[[ev$uid]], d)
    }
  }

  # Expand recurring events (skip EXDATE and overridden dates)
  expanded <- list()
  for (ev in events) {
    if (nchar(ev$rrule) > 0) {
      # Get override dates for this UID
      uid_overrides <- if (nchar(ev$uid) > 0 && ev$uid %in% names(override_map)) {
        as.Date(override_map[[ev$uid]])
      } else {
        as.Date(character(0))
      }
      expanded <- c(expanded, expand_rrule(ev, cutoff_past, cutoff_future, ev$exdates, uid_overrides))
    } else {
      expanded <- c(expanded, list(ev))
    }
  }

  if (length(expanded) == 0) return(data.frame(error = "No se encontraron eventos."))

  df <- data.frame(
    summary = sapply(expanded, function(e) e$summary),
    start = sapply(expanded, function(e) e$start),
    end = sapply(expanded, function(e) e$end),
    description = sapply(expanded, function(e) e$description),
    location = sapply(expanded, function(e) e$location),
    stringsAsFactors = FALSE
  )

  df$date <- as.Date(substr(df$start, 1, 10))
  df <- df[!is.na(df$date) & df$date >= cutoff_past & df$date <= cutoff_future, ]
  # Deduplicate: same summary + same start = duplicate
  df$dedup_key <- paste0(df$summary, "|", df$start)
  df <- df[!duplicated(df$dedup_key), ]
  df$dedup_key <- NULL
  df <- df[order(df$date), ]
  df
}

# Expand RRULE recurring events into individual instances
expand_rrule <- function(ev, cutoff_past, cutoff_future, exdates = character(0), uid_override_dates = as.Date(character(0))) {
  # Dates to skip: EXDATE + dates with override events (same UID, different name)
  skip_dates <- as.Date(substr(exdates, 1, 10))
  skip_dates <- c(skip_dates[!is.na(skip_dates)], uid_override_dates[!is.na(uid_override_dates)])
  rrule <- ev$rrule
  start_date <- as.Date(substr(ev$start, 1, 10))
  has_time <- nchar(ev$start) > 10
  time_suffix <- if (has_time) substr(ev$start, 11, nchar(ev$start)) else ""
  end_time_suffix <- if (nchar(ev$end) > 10) substr(ev$end, 11, nchar(ev$end)) else ""

  # Duration between start and end (in days for date-only, preserved for datetime)
  end_date <- as.Date(substr(ev$end, 1, 10))
  dur_days <- as.integer(end_date - start_date)

  # Parse RRULE fields
  get_rrparam <- function(param) {
    m <- regmatches(rrule, regexpr(paste0(param, "=([^;]+)"), rrule))
    if (length(m) > 0) sub(paste0(param, "="), "", m) else ""
  }

  freq <- toupper(get_rrparam("FREQ"))
  count <- suppressWarnings(as.integer(get_rrparam("COUNT")))
  until_raw <- get_rrparam("UNTIL")
  until_date <- if (nchar(until_raw) >= 8) {
    ud <- as.Date(paste0(substr(until_raw, 1, 4), "-", substr(until_raw, 5, 6), "-", substr(until_raw, 7, 8)))
    # Adjust for UTC timezone offset: if UNTIL ends with Z and time < 12:00 UTC,
    # the actual local date (Lima, UTC-5) is the previous day
    if (grepl("Z$", until_raw) && nchar(until_raw) >= 15) {
      hour_utc <- suppressWarnings(as.integer(substr(until_raw, 10, 11)))
      if (!is.na(hour_utc) && hour_utc < 12) ud <- ud - 1
    }
    ud
  } else {
    cutoff_future
  }
  byday <- get_rrparam("BYDAY")
  interval <- suppressWarnings(as.integer(get_rrparam("INTERVAL")))
  if (is.na(interval)) interval <- 1
  # For events without UNTIL or COUNT, expand up to cutoff_future (controlled by date range)
  # No artificial count limit — let UNTIL and date range be the boundaries
  if (is.na(count)) count <- 500L

  # Day mapping for BYDAY
  day_map <- c(SU=0, MO=1, TU=2, WE=3, TH=4, FR=5, SA=6)

  # Generate dates
  dates <- c()

  if (freq == "WEEKLY") {
    if (nchar(byday) > 0) {
      days <- trimws(strsplit(byday, ",")[[1]])
      target_wdays <- unname(day_map[days])
      target_wdays <- target_wdays[!is.na(target_wdays)]
    } else {
      target_wdays <- as.integer(format(start_date, "%w"))
    }

    # Generate weekly occurrences
    week_start <- start_date
    max_date <- min(until_date, cutoff_future)
    n <- 0
    max_count <- if (!is.na(count)) count else 200

    while (week_start <= max_date && n < max_count) {
      for (wd in target_wdays) {
        # Find the day in this week matching wd
        current_wd <- as.integer(format(week_start, "%w"))
        d <- week_start + (wd - current_wd)
        if (d >= start_date && d <= max_date && n < max_count) {
          dates <- c(dates, d)
          n <- n + 1
        }
      }
      week_start <- week_start + 7 * interval
    }

  } else if (freq == "DAILY") {
    max_date <- min(until_date, cutoff_future)
    n <- 0
    max_count <- if (!is.na(count)) count else 200
    d <- start_date
    while (d <= max_date && n < max_count) {
      dates <- c(dates, d)
      d <- d + interval
      n <- n + 1
    }

  } else {
    # Unsupported frequency — return single event
    return(list(ev))
  }

  # Convert numeric dates back to Date
  dates <- as.Date(dates, origin = "1970-01-01")
  # Filter to date range and skip excluded dates
  dates <- dates[dates >= cutoff_past & dates <= cutoff_future]
  dates <- dates[!dates %in% skip_dates]

  # Generate event instances
  lapply(dates, function(d) {
    new_start <- paste0(as.character(d), time_suffix)
    new_end_date <- d + dur_days
    new_end <- paste0(as.character(new_end_date), end_time_suffix)
    list(summary = ev$summary, start = new_start, end = new_end,
         description = ev$description, location = ev$location)
  })
}

gcal_parse_to_activities <- function(events) {
  if (nrow(events) == 0 || "error" %in% names(events)) return(data.frame())
  if (!"date" %in% names(events)) events$date <- as.Date(substr(events$start, 1, 10))
  events$is_exam <- grepl("examen|quiz|pc[0-9]|parcial|final|entrega|proyecto|evaluaci|tarea|lab",
                          events$summary, ignore.case = TRUE)
  events
}

# ============ GENERATE ICS FILE FROM ACTIVITIES ============
generate_ics <- function(activities, courses_df) {
  lines <- c(
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "PRODID:-//StudyPilot//ES",
    "CALSCALE:GREGORIAN",
    "METHOD:PUBLISH",
    "X-WR-CALNAME:StudyPilot - Actividades"
  )

  for (i in seq_len(nrow(activities))) {
    act <- activities[i, ]
    # Get course name
    cname <- ""
    if ("course_id" %in% names(act) && act$course_id %in% courses_df$id) {
      cname <- courses_df$short[courses_df$id == act$course_id]
    }

    # Parse date
    act_date <- tryCatch(as.Date(act$date), error = function(e) Sys.Date())
    dt_start <- format(act_date, "%Y%m%d")
    dt_end <- format(act_date + 1, "%Y%m%d")

    # Summary
    summary <- paste0(cname, if (nchar(cname) > 0) " - " else "", act$name)
    if ("weight" %in% names(act) && !is.na(act$weight) && act$weight > 0) {
      summary <- paste0(summary, " (", act$weight, "%)")
    }

    # Description
    desc <- ""
    if ("type" %in% names(act)) desc <- paste0("Tipo: ", act$type)
    if ("weight" %in% names(act) && !is.na(act$weight)) desc <- paste0(desc, "\\nPeso: ", act$weight, "%")
    if ("notes" %in% names(act) && nchar(act$notes) > 0) desc <- paste0(desc, "\\n", act$notes)

    # Priority based on weight
    priority <- if ("weight" %in% names(act) && !is.na(act$weight) && act$weight >= 20) "1" else "5"

    # UID
    uid <- paste0("studypilot-", act$act_id, "-", dt_start, "@studypilot.app")

    lines <- c(lines,
      "BEGIN:VEVENT",
      paste0("UID:", uid),
      paste0("DTSTART;VALUE=DATE:", dt_start),
      paste0("DTEND;VALUE=DATE:", dt_end),
      paste0("SUMMARY:", summary),
      paste0("DESCRIPTION:", desc),
      paste0("PRIORITY:", priority),
      # Alarm: 1 day before
      "BEGIN:VALARM",
      "TRIGGER:-P1D",
      "ACTION:DISPLAY",
      paste0("DESCRIPTION:Recordatorio: ", summary),
      "END:VALARM",
      "END:VEVENT"
    )
  }

  lines <- c(lines, "END:VCALENDAR")
  paste(lines, collapse = "\r\n")
}

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

# ============ ESTANDARIZACIÓN DE TIMESTAMPS ============
# Limpia start/end: quita Z, +offset, asegura formato YYYY-MM-DDTHH:MM:SS puro
estandarizar_timestamps <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  for (col in c("start", "end")) {
    if (!col %in% names(df)) next
    vals <- df[[col]]
    if (inherits(vals, "POSIXct")) {
      vals <- format(vals, "%Y-%m-%dT%H:%M:%S")
    } else if (is.character(vals)) {
      vals <- sub("\\+\\d{2}:\\d{2}$", "", vals)
      vals <- sub("Z$", "", vals)
      # Asegurar :SS si falta (16 chars = sin segundos)
      needs_ss <- !is.na(vals) & nchar(vals) == 16
      vals[needs_ss] <- paste0(vals[needs_ss], ":00")
    }
    df[[col]] <- vals
  }
  df
}

# ============ SCHEMA ESTÁNDAR PARA TODOS LOS DATAFRAMES DE CALENDARIO ============
CALENDAR_SCHEMA <- c("summary", "start", "end", "location", "color", "is_ai", "source")

estandarizar_evento <- function(df, source_label = "unknown") {

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(data.frame(
      summary = character(), start = character(), end = character(),
      location = character(), color = character(), is_ai = logical(),
      source = character(), stringsAsFactors = FALSE
    ))
  }
  out <- data.frame(
    summary   = if ("summary" %in% names(df)) as.character(df$summary) else rep("", nrow(df)),
    start     = if ("start" %in% names(df)) as.character(df$start) else rep("", nrow(df)),
    end       = if ("end" %in% names(df)) as.character(df$end) else rep("", nrow(df)),
    location  = if ("location" %in% names(df)) as.character(df$location) else rep("", nrow(df)),
    color     = if ("color" %in% names(df)) as.character(df$color) else rep("", nrow(df)),
    is_ai     = if ("is_ai" %in% names(df)) as.logical(df$is_ai) else rep(FALSE, nrow(df)),
    source    = rep(source_label, nrow(df)),
    stringsAsFactors = FALSE
  )
  # Sanitize NAs — color queda "" para que la detección por nombre funcione
  out$summary[is.na(out$summary)] <- ""
  out$start[is.na(out$start)] <- ""
  out$end[is.na(out$end)] <- ""
  out$location[is.na(out$location)] <- ""
  out$color[is.na(out$color)] <- ""
  out$is_ai[is.na(out$is_ai)] <- FALSE
  out
}

# ============ ASIGNACIÓN DINÁMICA DE COLORES POR CURSO ============
# Mapea el summary de cada evento al color del curso configurado por el usuario.
# Si no hay match, usa detección por nombre. Si tampoco, asigna de la paleta cíclica.
# NUNCA sobreescribe colores ya asignados (ej: AI blocks con color explícito).
COURSE_COLOR_PALETTE <- c(
  "#2563eb", "#16a34a", "#7c3aed", "#ea580c", "#db2777", "#0891b2",
  "#dc2626", "#eab308", "#6366f1", "#14b8a6", "#84cc16", "#f43f5e"
)

aplicar_colores_cursos <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  # Collect GCal colors first (these are untouchable)
  gcal_colors_used <- unique(df$color[!is.na(df$color) & nchar(df$color) > 0 &
                                       !is.na(df$source) & df$source == "gcal"])

  # Get courses from global env
  cursos <- tryCatch(get0("courses", envir = globalenv()), error = function(e) NULL)

  # Build name→color map from user's courses
  curso_color_map <- list()
  if (!is.null(cursos) && is.data.frame(cursos) && nrow(cursos) > 0) {
    for (i in seq_len(nrow(cursos))) {
      # Map by id, name, and short name (case-insensitive match)
      if (nchar(cursos$color[i]) > 0) {
        curso_color_map[[tolower(cursos$name[i])]] <- cursos$color[i]
        curso_color_map[[tolower(cursos$short[i])]] <- cursos$color[i]
        curso_color_map[[tolower(cursos$id[i])]] <- cursos$color[i]
      }
    }
  }

  # Track auto-assigned colors for courses not in the table
  auto_idx <- 1
  auto_assigned <- list()

  for (i in seq_len(nrow(df))) {
    # Skip if color already set (AI blocks, user overrides)
    if (!is.na(df$color[i]) && nchar(df$color[i]) > 0) next

    name_lower <- tolower(df$summary[i])

    # 1. Try exact match from user's courses table
    matched <- FALSE
    for (key in names(curso_color_map)) {
      if (grepl(key, name_lower, fixed = TRUE) || grepl(name_lower, key, fixed = TRUE)) {
        df$color[i] <- curso_color_map[[key]]
        matched <- TRUE
        break
      }
    }
    if (matched) next

    # 2. Name-based heuristic detection
    clr <- if (grepl("pco|planifica", name_lower)) "#0891b2"
    else if (grepl("dise[ñn]o|principios", name_lower)) "#16a34a"
    else if (grepl("data|analy", name_lower)) "#2563eb"
    else if (grepl("gesti[oó]n", name_lower)) "#ea580c"
    else if (grepl("estrat[eé]g", name_lower)) "#db2777"
    else if (grepl("[eé]tica", name_lower)) "#eab308"
    else if (grepl("comer|almuerz|comida|lunch|cena|desayun", name_lower)) "#94a3b8"
    else if (grepl("examen|quiz|pc[0-9]|parcial|final|evaluaci", name_lower)) "#dc2626"
    else NULL

    if (!is.null(clr)) {
      df$color[i] <- clr
      next
    }

    # 3. Auto-assign from palette (consistent per unique course name)
    # Extract base course name (first 3 words)
    base_name <- paste(head(strsplit(name_lower, "\\s+")[[1]], 3), collapse = " ")
    if (base_name %in% names(auto_assigned)) {
      df$color[i] <- auto_assigned[[base_name]]
    } else {
      df$color[i] <- COURSE_COLOR_PALETTE[((auto_idx - 1) %% length(COURSE_COLOR_PALETTE)) + 1]
      auto_assigned[[base_name]] <- df$color[i]
      auto_idx <- auto_idx + 1
    }
  }

  # Anti-collision: if a PDF event ended up with a color identical to a GCal event, swap it
  if (length(gcal_colors_used) > 0) {
    alt_palette <- c("#6366f1","#14b8a6","#84cc16","#f43f5e","#a855f7","#0d9488","#c026d3","#65a30d")
    alt_idx <- 1
    for (i in seq_len(nrow(df))) {
      if (is.na(df$source[i]) || df$source[i] == "gcal") next  # Never touch GCal colors
      if (isTRUE(df$is_ai[i])) next  # Never touch AI colors
      if (df$color[i] %in% gcal_colors_used) {
        # Find an alternative color not used by GCal
        for (j in seq_along(alt_palette)) {
          candidate <- alt_palette[((alt_idx - 1 + j - 1) %% length(alt_palette)) + 1]
          if (!candidate %in% gcal_colors_used) {
            df$color[i] <- candidate
            alt_idx <- alt_idx + 1
            break
          }
        }
      }
    }
  }

  df
}

# Convert PDF schedule (recurring weekly blocks) to calendar events for a specific week
pdf_schedule_to_events <- function(schedule_data, week_start, colors_map = NULL) {
  if (is.null(schedule_data) || nrow(schedule_data) == 0) return(estandarizar_evento(NULL))

  day_map <- c("Domingo" = 0, "Lunes" = 1, "Martes" = 2, "Mi\u00e9rcoles" = 3, "Miercoles" = 3,
               "Jueves" = 4, "Viernes" = 5, "S\u00e1bado" = 6, "Sabado" = 6)

  events <- do.call(rbind, lapply(seq_len(nrow(schedule_data)), function(i) {
    s <- schedule_data[i, ]
    dia_name <- s$dia
    dia_name <- iconv(dia_name, to = "ASCII//TRANSLIT")
    dia_name <- gsub("[^A-Za-z]", "", trimws(dia_name))
    # Match against normalized day names
    norm_map <- c("Domingo" = 0, "Lunes" = 1, "Martes" = 2, "Miercoles" = 3,
                  "Jueves" = 4, "Viernes" = 5, "Sabado" = 6)
    day_offset <- norm_map[dia_name]
    if (is.na(day_offset)) return(NULL)

    # week_start is the Sunday of the week
    event_date <- week_start + day_offset - 1
    # Color queda "" para que aplicar_colores_cursos lo asigne dinámicamente
    clr <- if (!is.null(colors_map) && s$curso %in% names(colors_map)) {
      colors_map[[s$curso]]
    } else ""

    data.frame(
      summary  = s$curso,
      start    = paste0(event_date, "T", s$hora_inicio),
      end      = paste0(event_date, "T", s$hora_fin),
      location = if ("aula" %in% names(s)) as.character(s$aula) else "",
      color    = clr,
      is_ai    = FALSE,
      source   = "pdf",
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(events) || nrow(events) == 0) return(estandarizar_evento(NULL))
  events
}

# ============ MOTOR DE FUSIÓN Y ANTI-DUPLICADOS ============
# Cruza eventos del PDF con eventos de Google Calendar.
# Si GCal ya tiene un evento que se solapa con un bloque del PDF
# (mismo día, solapamiento >= 50% de la duración del PDF), descarta el PDF.
fusionar_horarios <- function(df_pdf, df_gcal) {
  library(lubridate)
  tz_local <- "America/Lima"

  df_pdf  <- estandarizar_evento(df_pdf, "pdf")
  df_gcal <- estandarizar_evento(df_gcal, "gcal")

  if (nrow(df_pdf) == 0 && nrow(df_gcal) == 0) return(estandarizar_evento(NULL))
  if (nrow(df_pdf) == 0) return(df_gcal)
  if (nrow(df_gcal) == 0) return(df_pdf)

  # Only compare timed events (not all-day)
  pdf_timed  <- df_pdf[nchar(df_pdf$start) > 10, ]
  gcal_timed <- df_gcal[nchar(df_gcal$start) > 10, ]

  if (nrow(pdf_timed) == 0) return(df_gcal)

  # Parse start/end as POSIXct for interval comparison
  parse_dt <- function(s) {
    tryCatch(as.POSIXct(paste0(substr(s, 1, 16), ":00"), tz = tz_local, format = "%Y-%m-%dT%H:%M:%S"),
             error = function(e) NA)
  }

  pdf_starts <- sapply(pdf_timed$start, parse_dt)
  pdf_ends   <- sapply(pdf_timed$end, parse_dt)
  gcal_starts <- sapply(gcal_timed$start, parse_dt)
  gcal_ends   <- sapply(gcal_timed$end, parse_dt)

  # Mark PDF events that overlap with GCal events (same day, overlap >= 30 min)
  keep_pdf <- rep(TRUE, nrow(pdf_timed))

  for (i in seq_len(nrow(pdf_timed))) {
    if (is.na(pdf_starts[i]) || is.na(pdf_ends[i])) next
    pdf_date <- as.Date(as.POSIXct(pdf_starts[i], origin = "1970-01-01", tz = tz_local))

    for (j in seq_len(nrow(gcal_timed))) {
      if (is.na(gcal_starts[j]) || is.na(gcal_ends[j])) next
      gcal_date <- as.Date(as.POSIXct(gcal_starts[j], origin = "1970-01-01", tz = tz_local))
      if (pdf_date != gcal_date) next

      # Calculate overlap
      overlap_start <- max(pdf_starts[i], gcal_starts[j])
      overlap_end   <- min(pdf_ends[i], gcal_ends[j])
      overlap_min   <- as.numeric(difftime(
        as.POSIXct(overlap_end, origin = "1970-01-01", tz = tz_local),
        as.POSIXct(overlap_start, origin = "1970-01-01", tz = tz_local),
        units = "mins"
      ))

      if (overlap_min >= 30) {
        keep_pdf[i] <- FALSE
        break
      }
    }
  }

  # Build final: GCal events + non-overlapping PDF events
  pdf_unique <- pdf_timed[keep_pdf, ]
  # Also keep any all-day PDF events (rare but possible)
  pdf_allday <- df_pdf[nchar(df_pdf$start) <= 10, ]

  result <- rbind(df_gcal, pdf_unique, pdf_allday)
  result
}
