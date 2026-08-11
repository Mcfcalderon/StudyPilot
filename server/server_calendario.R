# ============ server_calendario.R — Calendario: Render + CRUD + GCal Sync ============
# Sourced con local=TRUE desde app.R (comparte input/output/session/rv/rv_gcal/uid).
#
# LOGICAS CRITICAS conservadas:
#   1. Timezone Fix — estandarizar_timestamps() quita Z/offset, strings puros sin UTC.
#   2. CRUD — Click-to-edit, drag-move; listeners envian a db_mongo en tiempo real.
#   3. Jerarquia de Colores — GCal intocable; PDF se reasigna si choca; AI conserva su CSS.
#   4. Offline-First — renderCalendar se alimenta del cache MongoDB cargado en login.
#   5. (Smart Scheduler con Sys.time() vive en server_smart_scheduler.R)

# ====================================================================
# HORARIO MAESTRO — Reactive que fusiona PDF + GCal + AI
# ====================================================================
horario_maestro <- reactive({
  rv$refresh
  tryCatch({
    sched_data <- tryCatch(mg_schedule_get(uid()), error = function(e) data.frame())
    cw <- rv$cal_week
    week_start <- get_semester_start() + (cw - 1) * 7
    df_pdf <- pdf_schedule_to_events(sched_data, week_start)
    message("[HM] Step1 PDF: ", nrow(df_pdf), " events")

    df_gcal <- estandarizar_evento(rv_gcal$events, "gcal")
    message("[HM] Step2 GCal: ", nrow(df_gcal), " events")

    df_fused <- fusionar_horarios(df_pdf, df_gcal)
    message("[HM] Step3 Fused: ", nrow(df_fused), " events")

    df_ai <- estandarizar_evento(rv_gcal$ai_blocks, "ai")
    if (nrow(df_ai) > 0) {
      df_ai$is_ai <- TRUE
      for (col in c("summary", "start", "end", "location", "color", "source")) {
        df_ai[[col]] <- as.character(df_ai[[col]])
        if (nrow(df_fused) > 0) df_fused[[col]] <- as.character(df_fused[[col]])
      }
      df_fused <- if (nrow(df_fused) > 0) rbind(df_fused, df_ai) else df_ai
    }
    message("[HM] Step4 +AI: ", nrow(df_fused), " events total")

    overrides <- rv_gcal$overrides
    if (!is.null(overrides) && is.data.frame(overrides) && nrow(overrides) > 0 && nrow(df_fused) > 0) {
      for (ov in seq_len(nrow(overrides))) {
        o <- overrides[ov, ]
        match_idx <- which(df_fused$summary == o$orig_title &
                           substr(df_fused$start, 1, 16) == substr(o$orig_start, 1, 16))
        if (length(match_idx) > 0) {
          idx <- match_idx[1]
          date_part <- substr(df_fused$start[idx], 1, 10)
          df_fused$summary[idx] <- o$new_title
          df_fused$start[idx] <- paste0(date_part, "T", o$new_start_time, ":00")
          df_fused$end[idx]   <- paste0(date_part, "T", o$new_end_time, ":00")
          df_fused$color[idx] <- o$new_color
        }
      }
    }

    hidden <- rv_gcal$hidden_events
    if (!is.null(hidden) && length(hidden) > 0 && nrow(df_fused) > 0) {
      ev_keys <- paste0(df_fused$summary, "|", substr(df_fused$start, 1, 16))
      df_fused <- df_fused[!ev_keys %in% hidden, ]
    }

    # Jerarquia de Colores (#3)
    df_fused <- aplicar_colores_cursos(df_fused)
    # Timezone Fix (#1)
    df_fused <- estandarizar_timestamps(df_fused)

    message("[HM] Final: ", nrow(df_fused), " events ready for calendar")
    df_fused
  }, error = function(e) {
    message("[StudyPilot] horario_maestro CRASH: ", e$message)
    estandarizar_evento(NULL)
  })
})


# ====================================================================
# Offline-First (#5): Cachear al localStorage para vista offline
# ====================================================================
observe({
  df <- horario_maestro()
  if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
    events_list <- lapply(seq_len(nrow(df)), function(i) {
      list(summary = df$summary[i], start = df$start[i], end = df$end[i],
           color = df$color[i], is_ai = df$is_ai[i], source = df$source[i])
    })
    session$sendCustomMessage("cache_calendar", list(events = events_list))
  }
})

# ====================================================================
# VISUAL CALENDAR — Renderizado HTML server-side
# ====================================================================
output$visual_calendar <- renderUI({
  tryCatch({
    rv$refresh
    gcal_events <- horario_maestro()

    if (is.null(gcal_events) || !is.data.frame(gcal_events) || nrow(gcal_events) == 0) {
      return(tags$div(class = "text-center text-muted py-4",
        tags$p("No hay eventos en el calendario."),
        tags$p(class = "small", "Sube tu horario con IA, sincroniza Google Calendar, o usa Autocompletar.")
      ))
    }

    cw <- rv$cal_week
    week_start <- get_semester_start() + (cw - 1) * 7
    today <- Sys.Date()
    day_names <- c("DOM", "LUN", "MAR", "MIE", "JUE", "VIE", "SAB")
    first_hour <- CAL_FIRST_HOUR; last_hour <- CAL_LAST_HOUR
    total_hours <- last_hour - first_hour

    headers <- list(tags$div(class = "cal-header cal-header-time", ""))
    for (d in 0:6) {
      dd <- week_start + d - 1
      headers <- c(headers, list(tags$div(
        class = paste("cal-header", if (dd == today) "today" else ""),
        tags$div(class = "cal-day-name", day_names[d + 1]),
        tags$div(class = "cal-day-num", format(dd, "%d"))
      )))
    }

    time_col <- tags$div(class = "cal-times",
      lapply(first_hour:last_hour, function(h) {
        tags$div(class = "cal-hour-label", paste0(sprintf("%02d", h), ":00"))
      })
    )

    day_cols <- lapply(0:6, function(d) {
      dd <- week_start + d - 1
      hour_lines <- lapply(first_hour:last_hour, function(h) tags$div(class = "cal-hour-line"))
      event_divs <- list()

      day_evts <- tryCatch({
        idx <- which(
          !is.na(gcal_events$start) &
          nchar(gcal_events$start) > 10 &
          as.Date(substr(gcal_events$start, 1, 10)) == dd
        )
        if (length(idx) > 0) gcal_events[idx, ] else NULL
      }, error = function(e) NULL)

      if (!is.null(day_evts) && nrow(day_evts) > 0) {
        ev_times <- lapply(seq_len(nrow(day_evts)), function(k) {
          start_str <- substr(day_evts$start[k], 1, 16)
          end_str   <- substr(day_evts$end[k], 1, 16)
          sh <- suppressWarnings(as.numeric(substr(start_str, 12, 13)) +
                                 as.numeric(substr(start_str, 15, 16)) / 60)
          eh <- suppressWarnings(as.numeric(substr(end_str, 12, 13)) +
                                 as.numeric(substr(end_str, 15, 16)) / 60)
          if (is.na(sh) || is.na(eh)) return(list(sh = NA, eh = NA))
          if (eh <= sh) eh <- sh + 1
          list(sh = sh, eh = eh)
        })

        for (k in seq_len(nrow(day_evts))) {
          ev <- day_evts[k, ]
          sh <- ev_times[[k]]$sh
          eh <- ev_times[[k]]$eh
          if (is.na(sh) || is.na(eh)) next

          top_px    <- (sh - first_hour) * HOUR_H
          height_px <- max((eh - sh) * HOUR_H - 3, 12)
          start_str <- substr(ev$start, 1, 16)
          end_str   <- substr(ev$end, 1, 16)
          time_label <- paste0(substr(start_str, 12, 16), " - ", substr(end_str, 12, 16))

          valid_times <- Filter(function(o) !is.na(o$sh), ev_times)
          n_overlaps <- sum(sapply(valid_times, function(o) o$sh < eh && o$eh > sh))
          overlap_idx <- sum(sapply(seq_len(k), function(j) {
            !is.na(ev_times[[j]]$sh) && ev_times[[j]]$sh < eh && ev_times[[j]]$eh > sh
          }))
          evt_width <- if (n_overlaps > 1) paste0(floor(90 / n_overlaps), "%") else "92%"
          evt_left  <- if (n_overlaps > 1) paste0(floor((overlap_idx - 1) * 90 / n_overlaps) + 2, "%") else "4%"

          clr <- if (!is.null(ev$color) && !is.na(ev$color) && nchar(ev$color) > 0) ev$color else "#3b82f6"
          clr_hex_map <- c(blue = "#3b82f6", green = "#22c55e", cyan = "#06b6d4", orange = "#f97316",
                           pink = "#ec4899", red = "#ef4444", yellow = "#eab308", gray = "#94a3b8",
                           purple = "#8b5cf6", teal = "#14b8a6", indigo = "#6366f1")
          if (tolower(clr) %in% names(clr_hex_map)) clr <- clr_hex_map[tolower(clr)]

          is_ai   <- isTRUE(ev$is_ai)
          ai_class <- if (is_ai) " cal-ev-ai" else ""
          ev_location <- if (!is.null(ev$location) && !is.na(ev$location)) ev$location else ""
          is_gcal <- isTRUE(!is.null(ev$source) && ev$source == "gcal")

          # CRUD (#2): TODOS los eventos son clickeables
          ev_onclick <- paste0(
            "Shiny.setInputValue('cal_event_click',{title:'",
            gsub("'", "\\'", ev$summary), "',start:'", start_str, "',end:'", end_str,
            "',color:'", clr, "',is_ai:", if (is_ai) "true" else "false",
            ",is_gcal:", if (is_gcal) "true" else "false",
            ",source:'", if (!is.null(ev$source)) ev$source else "unknown",
            "',idx:", k, "},{priority:'event'})"
          )

          bg_style <- paste0(
            "top:", top_px, "px;height:", height_px, "px;width:", evt_width, ";left:", evt_left, ";",
            "background-color:", clr, ";border-left:3px solid ", clr, ";",
            "cursor:pointer;", if (is_ai) "opacity:0.92;" else ""
          )

          event_divs <- c(event_divs, list(
            tags$div(class = paste0("cal-event", ai_class),
              style = bg_style,
              onclick = ev_onclick,
              tags$div(class = "cal-ev-name", ev$summary),
              tags$div(class = "cal-ev-time", time_label),
              if (nchar(ev_location) > 0) tags$div(class = "cal-ev-room", ev_location)
            )
          ))
        }
      }

      # Current time indicator (only for today)
      now_indicator <- NULL
      if (dd == today) {
        now_h <- as.numeric(format(Sys.time(), "%H")) + as.numeric(format(Sys.time(), "%M")) / 60
        now_top <- (now_h - first_hour) * HOUR_H
        if (now_top > 0 && now_top < total_hours * HOUR_H) {
          now_indicator <- tags$div(class = "cal-now-line",
            style = paste0("top:", round(now_top), "px;"))
        }
      }

      # Current time indicator (only for today)
      now_indicator <- NULL
      if (dd == today) {
        now_h <- as.numeric(format(Sys.time(), "%H")) + as.numeric(format(Sys.time(), "%M")) / 60
        now_top <- (now_h - first_hour) * HOUR_H
        if (now_top > 0 && now_top < total_hours * HOUR_H) {
          now_indicator <- tags$div(class = "cal-now-line",
            style = paste0("top:", round(now_top), "px;"))
        }
      }

      tags$div(class = "cal-day-col",
        style = paste0("height:", total_hours * HOUR_H, "px;"),
        hour_lines, event_divs, now_indicator
      )
    })

    # All-day events
    has_allday <- FALSE
    allday_spans <- list()
    week_sun <- week_start - 1
    week_sat <- week_start + 5

    allday_idx <- which(!is.na(gcal_events$start) & nchar(gcal_events$start) <= 10)
    if (length(allday_idx) > 0) {
      allday_evts <- gcal_events[allday_idx, ]
      allday_evts <- allday_evts[!duplicated(paste0(allday_evts$summary, "|", allday_evts$start)), ]
      for (j in seq_len(nrow(allday_evts))) {
        ev_start <- tryCatch(as.Date(substr(allday_evts$start[j], 1, 10)), error = function(e) NA)
        ev_end   <- tryCatch(as.Date(substr(allday_evts$end[j], 1, 10)), error = function(e) NA)
        if (is.na(ev_start) || is.na(ev_end)) next
        vis_start <- max(ev_start, week_sun)
        vis_end   <- min(ev_end - 1, week_sat)
        if (vis_start > week_sat || vis_end < week_sun) next
        col_start <- as.integer(vis_start - week_sun) + 2
        col_end   <- as.integer(vis_end - week_sun) + 3
        has_allday <- TRUE
        allday_spans <- c(allday_spans, list(
          tags$div(style = paste0(
            "grid-column:", col_start, "/", col_end, ";",
            "background:#fff3cd;color:#856404;border-left:3px solid #ffc107;",
            "padding:3px 8px;border-radius:4px;font-size:0.75rem;font-weight:700;",
            "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;"),
            allday_evts$summary[j])
        ))
      }
    }

    # Assemble final grid
    tags$div(
      tags$div(class = "cal-wrapper cal-header-row", headers),
      if (has_allday) tags$div(class = "cal-allday-grid",
        style = "display:grid;grid-template-columns:55px repeat(7,1fr);border:1px solid #e2e8f0;border-top:none;border-bottom:none;background:#fffbeb;padding:3px 0;gap:2px 0;",
        tags$div(),
        allday_spans),
      tags$div(class = "cal-scroll-container", id = "cal-scroll-box",
        style = "max-height:550px; overflow-y:auto; position:relative;",
        tags$div(class = "cal-wrapper cal-body-row",
          time_col,
          day_cols
        )
      ),
      tags$script(HTML(paste0(
        "setTimeout(function(){var c=document.getElementById('cal-scroll-box');",
        "if(c) c.scrollTop=", CAL_SCROLL_TO * HOUR_H, ";}, 300);"
      )))
    )
  }, error = function(e) {
    message("[StudyPilot] visual_calendar render error: ", e$message)
    tags$div(class = "alert alert-danger m-3",
      tags$b("Error al renderizar el calendario"),
      tags$p(class = "small mb-0", "Intenta recargar la pagina. Detalle: ", e$message)
    )
  })
})

# ====================================================================
# CALENDAR WEEK LABEL
# ====================================================================
output$cal_week_label <- renderUI({
  w <- rv$cal_week
  ws <- get_semester_start() + (w - 1) * 7
  we <- ws + 6
  tags$h5(class = "mb-0 fw-bold",
    paste0(format(ws, "%d %b"), " - ", format(we, "%d %b %Y"))
  )
})

# ====================================================================
# CALENDAR NAVIGATION
# ====================================================================
observeEvent(input$cal_prev, { rv$cal_week <- rv$cal_week - 1 })
observeEvent(input$cal_next, { rv$cal_week <- rv$cal_week + 1 })
observeEvent(input$cal_today, {
  days_diff <- as.integer(Sys.Date() - get_semester_start())
  rv$cal_week <- floor((days_diff + 1) / 7) + 1
})

# ====================================================================
# SCHEDULE GRID (table view of extracted PDF schedule)
# ====================================================================
output$schedule_grid <- renderUI({
  rv$refresh
  sched <- tryCatch(mg_schedule_get(uid()), error = function(e) data.frame())
  if (is.null(sched) || !is.data.frame(sched) || nrow(sched) == 0) return(NULL)
  # Normalize day names (strip accents for consistent matching)
  sched$dia <- iconv(sched$dia, to = "ASCII//TRANSLIT")
  sched$dia <- gsub("[^A-Za-z]", "", sched$dia)

  days <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado")
  colors <- c("#3b82f6", "#22c55e", "#8b5cf6", "#f97316", "#ec4899", "#06b6d4", "#eab308", "#10b981")
  cursos_uniq <- unique(sched$curso)
  color_map <- setNames(colors[seq_along(cursos_uniq) %% length(colors) + 1], cursos_uniq)

  all_hours <- c(
    as.integer(substr(sched$hora_inicio, 1, 2)),
    as.integer(substr(sched$hora_fin, 1, 2))
  )
  min_h <- max(7, min(all_hours, na.rm = TRUE))
  max_h <- min(23, max(all_hours, na.rm = TRUE) + 1)
  hours <- min_h:max_h

  grid_rows <- lapply(hours, function(h) {
    cells <- lapply(days, function(d) {
      matches <- which(sched$dia == d & as.integer(substr(sched$hora_inicio, 1, 2)) == h)
      if (length(matches) > 0) {
        s <- sched[matches[1], ]
        h_start <- as.integer(substr(s$hora_inicio, 1, 2))
        h_end   <- as.integer(substr(s$hora_fin, 1, 2))
        span <- max(1, h_end - h_start)
        col  <- color_map[s$curso]
        aula <- if (!is.null(s$aula) && nchar(s$aula) > 0 && s$aula != "TBD") s$aula else ""
        short_name <- if (nchar(s$curso) > 20) paste0(substr(s$curso, 1, 18), "...") else s$curso
        tags$td(
          rowspan = span,
          style = paste0("background:", col, ";color:white;font-size:11px;padding:4px 6px;",
                        "border-radius:6px;vertical-align:top;min-width:120px;"),
          tags$div(class = "fw-bold", short_name),
          tags$div(style = "opacity:0.9;font-size:10px", paste0(s$hora_inicio, "-", s$hora_fin)),
          if (nchar(aula) > 0) tags$div(style = "opacity:0.8;font-size:10px", paste0("@ ", aula))
        )
      } else {
        covered <- FALSE
        for (prev_h in min_h:(h - 1)) {
          prev_matches <- which(sched$dia == d & as.integer(substr(sched$hora_inicio, 1, 2)) == prev_h)
          if (length(prev_matches) > 0) {
            ps <- sched[prev_matches[1], ]
            if (h < as.integer(substr(ps$hora_fin, 1, 2))) { covered <- TRUE; break }
          }
        }
        if (!covered) tags$td(style = "min-width:120px;height:40px;") else NULL
      }
    })
    cells <- Filter(Negate(is.null), cells)
    tags$tr(
      tags$td(style = "font-size:11px;font-weight:bold;color:#64748b;white-space:nowrap;padding:4px 8px;vertical-align:top;",
              paste0(sprintf("%02d", h), ":00")),
      cells
    )
  })

  div(class = "card mt-2",
    div(class = "card-header py-2 d-flex align-items-center gap-2",
      tags$b("Mi Horario de Clases"),
      tags$span(class = "badge bg-primary", paste0(nrow(sched), " bloques"))
    ),
    div(class = "card-body p-0", style = "overflow-x:auto;",
      tags$table(class = "table table-bordered mb-0", style = "border-collapse:collapse;",
        tags$thead(style = "background:#f1f5f9;",
          tags$tr(
            tags$th(style = "width:60px;font-size:12px;", "Hora"),
            lapply(days, function(d) {
              tags$th(style = "text-align:center;font-size:12px;min-width:120px;", d)
            })
          )
        ),
        tags$tbody(grid_rows)
      )
    )
  )
})

# ====================================================================
# DOWNLOAD ICS
# ====================================================================
output$download_ics <- downloadHandler(
  filename = function() {
    paste0("StudyPilot_actividades_", format(Sys.Date(), "%Y%m%d"), ".ics")
  },
  content = function(file) {
    a <- acts()
    all_courses <- courses
    custom <- tryCatch(mg_custom_courses_get(uid()), error = function(e) data.frame())
    if (nrow(custom) > 0) {
      custom_df <- data.frame(
        id = custom$id, name = custom$name, short = custom$short,
        credits = custom$credits,
        professor = if ("professor" %in% names(custom)) custom$professor else "",
        formula = if ("formula" %in% names(custom)) custom$formula else "",
        eval_day = if ("eval_day" %in% names(custom)) custom$eval_day else 5L,
        color = if ("color" %in% names(custom)) custom$color else "#666",
        stringsAsFactors = FALSE
      )
      all_courses <- rbind(all_courses, custom_df)
    }
    pending <- a[a$done == 0, ]
    if (nrow(pending) == 0) pending <- a
    ics_content <- generate_ics(pending, all_courses)
    writeLines(ics_content, file)
  }
)
