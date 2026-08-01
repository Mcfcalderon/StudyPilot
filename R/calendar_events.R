# ============ CALENDAR EVENTS — timestamps, schema, colores, fusion ============
# Movido desde google_cal.R (Fase D). Funciones puras sourced por global.R.

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
