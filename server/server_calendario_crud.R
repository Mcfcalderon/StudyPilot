# ============ server_calendario_crud.R — CRUD y sync del calendario ============
# Sourced con local=TRUE; comparte scope con server_calendario.R
# (horario_maestro, input/output/session, rv, rv_gcal, uid).

# ====================================================================
# SCHEDULE FROM PDF (AI extraction)
# ====================================================================
observeEvent(input$schedule_extract_btn, {
  f <- input$schedule_file
  if (is.null(f)) {
    shinyjs::html("schedule_status_div",
      '<div class="alert alert-warning py-1 small">Sube un PDF de horario primero.</div>')
    return()
  }
  shinyjs::disable("schedule_extract_btn")
  shinyjs::html("schedule_status_div",
    '<div class="alert alert-info py-1 small"><span class="spinner-border spinner-border-sm me-2"></span>Extrayendo horario con IA...</div>')

  current_uid <- uid()

  session$onFlushed(function() {
    tryCatch({
      ext <- tolower(tools::file_ext(f$name))
      text <- if (ext == "pdf") paste(pdftools::pdf_text(f$datapath), collapse = "\n")
              else readLines(f$datapath, warn = FALSE) |> paste(collapse = "\n")

      message("[StudyPilot] Schedule PDF text length: ", nchar(text))
      if (nchar(text) < 50) {
        shinyjs::html("schedule_status_div",
          '<div class="alert alert-danger py-1 small">El PDF no tiene texto extraible.</div>')
        shinyjs::enable("schedule_extract_btn")
        return()
      }
      sched <- ai_extract_schedule(text)
      if (nrow(sched) == 0) {
        shinyjs::html("schedule_status_div",
          '<div class="alert alert-warning py-1 small">No se encontraron bloques de horario.</div>')
      } else {
        mg_schedule_set(current_uid, sched)
        rv$refresh <- isolate(rv$refresh) + 1
        shinyjs::html("schedule_status_div",
          paste0('<div class="alert alert-success py-1 small">Horario extraido: ',
                 nrow(sched), ' bloques de clase.</div>'))
      }
    }, error = function(e) {
      err_msg <- e$message
      if (grepl("429", err_msg)) err_msg <- "Demasiadas solicitudes al API de IA. Espera 1-2 minutos."
      shinyjs::html("schedule_status_div",
        paste0('<div class="alert alert-danger py-1 small">', err_msg, '</div>'))
    })
    shinyjs::enable("schedule_extract_btn")
  }, once = TRUE)
})

observeEvent(input$schedule_clear_btn, {
  mg_schedule_set(uid(), data.frame())
  rv$refresh <- rv$refresh + 1
  shinyjs::html("schedule_status_div",
    '<div class="alert alert-info py-1 small">Horario limpiado.</div>')
})

# ====================================================================
# CRUD (#2): Click-to-edit modal for ALL events (AI, PDF, GCal)
# ====================================================================
observeEvent(input$cal_event_click, {
  ev <- input$cal_event_click
  if (is.null(ev)) return()
  is_ai   <- isTRUE(ev$is_ai)
  is_gcal <- isTRUE(ev$is_gcal)
  src_label <- if (is_ai) "Bloque IA"
               else if (is_gcal) "Google Calendar"
               else "Horario PDF"
  cur_color <- if (!is.null(ev$color) && nchar(ev$color) > 0) ev$color else "#3b82f6"

  color_choices <- c("Azul" = "#2563eb", "Verde" = "#16a34a", "Morado" = "#7c3aed",
                     "Naranja" = "#ea580c", "Rosa" = "#db2777", "Cyan" = "#0891b2",
                     "Rojo" = "#dc2626", "Amarillo" = "#eab308",
                     "Teal" = "#14b8a6", "Gris" = "#94a3b8")

  showModal(modalDialog(
    title = paste0("Editar: ", ev$title),
    tags$span(class = "badge bg-secondary mb-2", src_label),
    textInput("cal_edit_title", "Titulo:", value = ev$title),
    textInput("cal_edit_start", "Hora inicio (HH:MM):", value = substr(ev$start, 12, 16)),
    textInput("cal_edit_end", "Hora fin (HH:MM):", value = substr(ev$end, 12, 16)),
    selectInput("cal_edit_color", "Color:",
      choices = color_choices, selected = cur_color, width = "200px"),
    if (is_gcal) tags$p(class = "text-warning small",
      "Los cambios en eventos de Google son locales."),
    footer = tagList(
      actionButton("cal_edit_delete", "Eliminar", class = "btn-outline-danger"),
      modalButton("Cancelar"),
      actionButton("cal_edit_save", "Guardar", class = "btn-primary")
    ),
    easyClose = TRUE
  ))
})

# ---- CRUD: Save edits (AI -> in-memory + MongoDB, PDF/GCal -> override + MongoDB) ----
observeEvent(input$cal_edit_save, {
  ev <- input$cal_event_click
  if (is.null(ev)) return()
  is_ai <- isTRUE(ev$is_ai)

  if (is_ai) {
    ai_blocks <- rv_gcal$ai_blocks
    if (!is.null(ai_blocks) && nrow(ai_blocks) > 0) {
      match_idx <- which(ai_blocks$summary == ev$title &
                         substr(ai_blocks$start, 1, 16) == substr(ev$start, 1, 16))
      if (length(match_idx) > 0) {
        date_part <- substr(ai_blocks$start[match_idx[1]], 1, 10)
        ai_blocks$summary[match_idx[1]] <- input$cal_edit_title
        ai_blocks$start[match_idx[1]]   <- paste0(date_part, "T", input$cal_edit_start, ":00")
        ai_blocks$end[match_idx[1]]     <- paste0(date_part, "T", input$cal_edit_end, ":00")
        ai_blocks$color[match_idx[1]]   <- input$cal_edit_color
        rv_gcal$ai_blocks <- ai_blocks
        save_ai_blocks_mongo()
      }
    }
  } else {
    override <- data.frame(
      orig_title = ev$title, orig_start = ev$start,
      new_title = input$cal_edit_title,
      new_start_time = input$cal_edit_start,
      new_end_time   = input$cal_edit_end,
      new_color      = input$cal_edit_color,
      stringsAsFactors = FALSE
    )
    existing <- rv_gcal$overrides
    if (is.null(existing) || !is.data.frame(existing) || nrow(existing) == 0) {
      existing <- override
    } else {
      dup <- which(existing$orig_title == override$orig_title &
                   substr(existing$orig_start, 1, 16) == substr(override$orig_start, 1, 16))
      if (length(dup) > 0) existing <- existing[-dup, ]
      existing <- rbind(existing, override)
    }
    rv_gcal$overrides <- existing
    tryCatch(mg_cal_overrides_set(uid(), existing),
             error = function(e) message("[StudyPilot] Override save error: ", e$message))
  }
  rv$refresh <- rv$refresh + 1
  removeModal()
  showNotification("Evento actualizado", type = "message")
})

# ---- CRUD: Delete (AI -> remove from memory, PDF/GCal -> hide) ----
observeEvent(input$cal_edit_delete, {
  ev <- input$cal_event_click
  if (is.null(ev)) return()
  is_ai <- isTRUE(ev$is_ai)

  if (is_ai) {
    ai_blocks <- rv_gcal$ai_blocks
    if (!is.null(ai_blocks) && nrow(ai_blocks) > 0) {
      match_idx <- which(ai_blocks$summary == ev$title &
                         substr(ai_blocks$start, 1, 16) == substr(ev$start, 1, 16))
      if (length(match_idx) > 0) {
        rv_gcal$ai_blocks <- ai_blocks[-match_idx[1], ]
        save_ai_blocks_mongo()
      }
    }
  } else {
    hidden <- rv_gcal$hidden_events
    if (is.null(hidden)) hidden <- character()
    hidden <- c(hidden, paste0(ev$title, "|", substr(ev$start, 1, 16)))
    rv_gcal$hidden_events <- hidden
    tryCatch(mg_cal_hidden_set(uid(), hidden),
             error = function(e) message("[StudyPilot] Hidden save error: ", e$message))
  }
  rv$refresh <- rv$refresh + 1
  removeModal()
  showNotification("Evento eliminado", type = "warning")
})

# ---- CRUD: Drag-to-move handler (from JS) ----
observeEvent(input$cal_drag_move, {
  d <- input$cal_drag_move
  if (is.null(d) || is.null(d$title)) return()

  ai_blocks <- rv_gcal$ai_blocks
  if (!is.null(ai_blocks) && nrow(ai_blocks) > 0) {
    match_idx <- which(ai_blocks$summary == d$title)
    if (length(match_idx) > 0) {
      idx <- match_idx[1]
      date_part <- substr(ai_blocks$start[idx], 1, 10)
      ai_blocks$start[idx] <- paste0(date_part, "T", d$new_start, ":00")
      ai_blocks$end[idx]   <- paste0(date_part, "T", d$new_end, ":00")
      rv_gcal$ai_blocks <- ai_blocks
      save_ai_blocks_mongo()
      rv$refresh <- rv$refresh + 1
      return()
    }
  }

  override <- data.frame(
    orig_title = d$title, orig_start = paste0("any_T", d$old_time),
    new_title = d$title, new_start_time = d$new_start,
    new_end_time = d$new_end, new_color = "",
    stringsAsFactors = FALSE
  )
  existing <- rv_gcal$overrides
  if (is.null(existing) || !is.data.frame(existing) || nrow(existing) == 0) {
    existing <- override
  } else {
    existing <- rbind(existing, override)
  }
  rv_gcal$overrides <- existing
  tryCatch(mg_cal_overrides_set(uid(), existing),
           error = function(e) message("[StudyPilot] Drag override error: ", e$message))
  rv$refresh <- rv$refresh + 1
  showNotification("Evento movido", type = "message", duration = 2)
})

# ====================================================================
# GOOGLE CALENDAR SYNC (only on explicit user click — Offline-First #5)
# ====================================================================
observeEvent(input$gcal_sync, {
  email <- trimws(input$gcal_email)
  if (nchar(email) < 5) {
    shinyjs::html("gcal_status_div",
      '<div class="alert alert-warning mt-2 small">Ingresa un email valido</div>')
    return()
  }

  rv_gcal$events <- NULL
  output$gcal_events_table <- renderUI(NULL)
  shinyjs::disable("gcal_sync")
  shinyjs::html("gcal_status_div",
    '<div class="alert alert-info mt-2 small py-2"><span class="spinner-border spinner-border-sm me-2" role="status"></span><b>Sincronizando calendario...</b></div>')

  events <- gcal_get_events(email)

  shinyjs::enable("gcal_sync")
  if ("error" %in% names(events)) {
    shinyjs::html("gcal_status_div", paste0(
      '<div class="alert alert-danger mt-2 small">',
      '<b>No se pudo conectar.</b> Tu calendario debe ser publico.<br>',
      '<span class="text-muted">Ve a Google Calendar &gt; Configuracion &gt; Hacer disponible para el publico</span><br>',
      '<span class="text-muted small">Error: ', events$error, '</span></div>'))
  } else if (nrow(events) == 0) {
    shinyjs::html("gcal_status_div",
      '<div class="alert alert-warning mt-2 small">No se encontraron eventos.</div>')
  } else {
    rv_gcal$events <- estandarizar_evento(events, "gcal")
    parsed <- gcal_parse_to_activities(events)
    n_exams <- sum(parsed$is_exam)
    shinyjs::html("gcal_status_div", paste0(
      '<div class="alert alert-success mt-2 small">', nrow(events),
      ' eventos encontrados (', n_exams, ' evaluaciones detectadas)</div>'))
  }
})

observeEvent(input$gcal_clear, {
  rv_gcal$events <- NULL
  shinyjs::html("gcal_status_div",
    '<div class="alert alert-secondary mt-2 small">Calendario limpiado.</div>')
  output$gcal_events_table <- renderUI(NULL)
})

# Auto-clear events when email changes
observeEvent(input$gcal_email, {
  rv_gcal$events <- NULL
  output$gcal_events_table <- renderUI(NULL)
}, ignoreInit = TRUE)

# ---- GCal events summary table ----
output$gcal_events_table <- renderUI({
  events <- rv_gcal$events
  if (is.null(events) || nrow(events) == 0) return(NULL)

  parsed <- gcal_parse_to_activities(events)

  rows <- lapply(seq_len(nrow(parsed)), function(i) {
    e <- parsed[i, ]
    is_exam <- e$is_exam
    bg   <- if (is_exam) "bg-danger bg-opacity-10" else ""
    icon <- if (is_exam) "!" else "*"
    tags$tr(class = bg,
      tags$td(class = "small", icon),
      tags$td(class = "small fw-bold", e$summary),
      tags$td(class = "small", format(e$date, "%a %d %b")),
      tags$td(class = "small text-muted", substr(e$start, 12, 16)),
      tags$td(class = "small text-muted", e$location)
    )
  })

  tags$div(class = "mt-3",
    tags$b(class = "small", "Eventos sincronizados:"),
    tags$div(style = "max-height:200px;overflow-y:auto;",
      tags$table(class = "table table-sm table-hover mb-0",
        tags$thead(tags$tr(
          tags$th(""), tags$th("Evento"), tags$th("Fecha"),
          tags$th("Hora"), tags$th("Lugar")
        )),
        tags$tbody(rows)
      )
    )
  )
})


# ====================================================================
# INICIO DEL CICLO (Semana 1) — configurable y persistente
# ====================================================================
observeEvent(input$cycle_start_save, {
  d <- tryCatch(as.Date(input$cycle_start), error = function(e) NA)
  if (is.na(d)) { showNotification("Fecha invalida", type = "error"); return() }
  session$userData$semester_start(d)
  tryCatch(mg_settings_set(uid(), "semester_start", as.character(d)),
           error = function(e) message("[StudyPilot] semester_start save error: ", e$message))
  rv$cal_week <- current_week(); rv$view_week <- current_week()
  rv$refresh <- isolate(rv$refresh) + 1
  showNotification(paste0("Ciclo iniciado el ", format(d, "%d %b %Y"),
                          " -> hoy es Semana ", current_week(), "/", TOTAL_WEEKS), type = "message", duration = 6)
})

