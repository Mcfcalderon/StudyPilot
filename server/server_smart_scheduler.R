# ============ server_smart_scheduler.R — Smart Scheduler Implacable ============
# Sourced con local=TRUE desde app.R (comparte input/output/session/rv/rv_gcal/uid).
# Depende de: horario_maestro (definido en server_calendario.R, mismo scope).
#
# DATA PIPELINE:
#   Step 1 — Cargar cursos, actividades y notas desde MongoDB.
#   Step 2 — Calcular prioridades (4 factores ponderados: creditos, deficit, peso, urgencia).
#   Step 3 — White Space Real: fusionar PDF+GCal, inyectar bloque "PASADO" con Sys.time()
#            exacto para recortar el dia de hoy. Calcular horizonte semestral.
#   Step 4 — Enviar a LLM con contexto de semestre y temas vinculados.
#   Step 5 — Convertir respuesta a eventos calendario con className="evento-ia",
#            isReadOnly=FALSE, persistir con mg_ai_blocks_set(), trigger refresh.
#
# LOGICA CRITICA #4 — Sys.time() Implacable:
#   El calculo del White Space DEBE recortar el dia usando Sys.time() exacto
#   para no agendar horas de estudio en el pasado.

# ====================================================================
# SMART SCHEDULER: Observer principal (btn_gen_schedule)
# ====================================================================
observeEvent(input$btn_gen_schedule, {
  shinyjs::disable("btn_gen_schedule")
  shinyjs::html("smart_sched_status",
    paste0('<div class="alert alert-info py-2 small">',
           '<span class="spinner-border spinner-border-sm me-2"></span>',
           '<b>Analizando evaluaciones y calendario...</b> (30-60 seg)</div>'))

  # Capturar valores reactivos ANTES del onFlushed (no hay contexto reactivo dentro)
  current_uid <- uid()
  sleep_s <- input$sleep_start
  sleep_e <- input$sleep_end

  session$onFlushed(function() {
    tryCatch({

      # ==============================================================
      # STEP 1: Cargar cursos, actividades y notas
      # ==============================================================
      message("[StudyPilot] Smart Scheduler Step 1: Loading data...")
      all_c <- get0("courses", envir = globalenv())
      all_a <- mg_activities_all(current_uid)
      all_g <- mg_grades_all(current_uid)

      if (nrow(all_c) == 0 || nrow(all_a) == 0) {
        shinyjs::html("smart_sched_status",
          '<div class="alert alert-warning py-2 small">No hay cursos o actividades. Sube tus silabos primero.</div>')
        shinyjs::enable("btn_gen_schedule")
        return()
      }


      # ==============================================================
      # STEP 2: Calcular prioridades (orden estricto por priority_score)
      # ==============================================================
      message("[StudyPilot] Smart Scheduler Step 2: Calculating priorities...")
      prioridades <- calcular_prioridades_estudio(all_c, all_a, all_g)
      if (nrow(prioridades) == 0) {
        shinyjs::html("smart_sched_status",
          '<div class="alert alert-warning py-2 small">No hay evaluaciones pendientes para planificar.</div>')
        shinyjs::enable("btn_gen_schedule")
        return()
      }
      # ESTRICTAMENTE ordenado por score descendente
      prioridades <- prioridades[order(-prioridades$priority_score), ]
      message("[StudyPilot] Smart Scheduler: ", nrow(prioridades), " activities prioritized")

      # ==============================================================
      # STEP 3: White Space Real con Sys.time() Implacable (#4)
      # ==============================================================
      message("[StudyPilot] Smart Scheduler Step 3: Finding free time (semester vision)...")
      gcal_events  <- isolate(rv_gcal$events)
      existing_ai  <- isolate(rv_gcal$ai_blocks)
      sched_data   <- tryCatch(mg_schedule_get(current_uid), error = function(e) data.frame())

      # --- Horizonte semestral: hoy -> ultima evaluacion pendiente ---
      last_eval_date <- tryCatch({
        pending <- all_a[all_a$done == 0 & !is.na(all_a$date), ]
        if (nrow(pending) > 0) max(as.Date(pending$date), na.rm = TRUE) else Sys.Date() + 14
      }, error = function(e) Sys.Date() + 14)

      semester_end <- SEMESTER_START + 16 * 7
      end_horizon  <- min(max(last_eval_date + 1, Sys.Date() + 7), semester_end)
      weeks_remaining <- ceiling(as.numeric(end_horizon - Sys.Date()) / 7)
      message("[StudyPilot] Horizon: ", Sys.Date(), " -> ", end_horizon,
              " (", weeks_remaining, " weeks, last eval: ", last_eval_date, ")")

      # --- Fusionar PDF + GCal como template de "busy" ---
      fused_for_free <- tryCatch({
        week_sun <- Sys.Date() - as.integer(format(Sys.Date(), "%w"))
        df_p <- pdf_schedule_to_events(sched_data, week_sun)
        df_g <- estandarizar_evento(gcal_events, "gcal")
        fusionar_horarios(df_p, df_g)
      }, error = function(e) {
        message("[StudyPilot] Fusion for free slots failed: ", e$message)
        estandarizar_evento(gcal_events, "gcal")
      })

      # --- LOGICA #4 IMPLACABLE: Bloquear todo antes de Sys.time() exacto ---
      # Si son las 15:00, ningun hueco libre antes de 15:01 debe existir
      now_time <- Sys.time()
      now_block <- estandarizar_evento(data.frame(
        summary = "PASADO",
        start   = paste0(Sys.Date(), "T00:00:00"),
        end     = format(now_time, "%Y-%m-%dT%H:%M:%S"),
        location = "", color = "", is_ai = FALSE,
        stringsAsFactors = FALSE
      ), "system")
      fused_for_free <- rbind(fused_for_free, now_block)

      # --- Obtener espacio libre (lubridate intervals anti-overlap) ---
      free_slots <- obtener_espacio_libre(
        gcal_events   = fused_for_free,
        schedule_data = sched_data,
        ai_blocks     = existing_ai,
        start_date    = Sys.Date(),
        end_date      = min(Sys.Date() + 6, end_horizon),
        sleep_start   = sleep_s,
        sleep_end     = sleep_e
      )
      message("[StudyPilot] Smart Scheduler: ", nrow(free_slots), " free slots, ",
              if (nrow(free_slots) > 0) paste0(sum(free_slots$duration_min), " min total") else "0 min",
              " | horizon=", weeks_remaining, " weeks")

      if (nrow(free_slots) == 0) {
        shinyjs::html("smart_sched_status",
          '<div class="alert alert-warning py-2 small">No se encontraron huecos libres en tu calendario esta semana.</div>')
        shinyjs::enable("btn_gen_schedule")
        return()
      }
      message("[StudyPilot] Smart Scheduler: ", nrow(free_slots), " free slots found")

      # ==============================================================
      # STEP 4: Enviar a LLM con contexto de semestre y temas vinculados
      # ==============================================================
      message("[StudyPilot] Smart Scheduler Step 4: Generating with AI...")
      study_blocks <- generate_smart_schedule_llm(
        prioridades, free_slots,
        weeks_remaining = weeks_remaining,
        semester_end    = as.character(end_horizon)
      )

      if (nrow(study_blocks) == 0) {
        shinyjs::html("smart_sched_status",
          '<div class="alert alert-warning py-2 small">La IA no pudo generar bloques. Intenta de nuevo.</div>')
        shinyjs::enable("btn_gen_schedule")
        return()
      }
      message("[StudyPilot] Smart Scheduler: ", nrow(study_blocks), " study blocks generated!")

      # ==============================================================
      # STEP 5: Convertir a eventos + identidad visual + persistencia
      # ==============================================================
      # Colores por prioridad: alta=purple, media=orange, baja=cyan, descanso=gray
      block_colors <- ifelse(
        study_blocks$tipo_bloque == "descanso", "gray",
        ifelse(study_blocks$prioridad == "alta", "purple",
          ifelse(study_blocks$prioridad == "media", "orange", "cyan")))

      ai_events <- estandarizar_evento(data.frame(
        summary  = study_blocks$titulo,
        start    = paste0(study_blocks$fecha, "T", study_blocks$hora_inicio),
        end      = paste0(study_blocks$fecha, "T", study_blocks$hora_fin),
        location = "",
        color    = block_colors,
        is_ai    = TRUE,
        stringsAsFactors = FALSE
      ), "ai")

      # Identidad visual: CSS de rayas translucidas + arrastrable
      ai_events$className  <- "evento-ia"
      ai_events$isReadOnly <- FALSE

      # Persistir a MongoDB
      mg_ai_blocks_set(current_uid, ai_events)
      message("[StudyPilot] Smart Scheduler: AI blocks persisted to MongoDB")

      # Actualizar reactivo global -> redibuja calendario automaticamente
      rv_gcal$ai_blocks <- ai_events
      rv$refresh <- isolate(rv$refresh) + 1

      shinyjs::html("smart_sched_status", paste0(
        '<div class="alert alert-success py-2 small">',
        nrow(study_blocks), ' bloques inyectados en tu calendario. ',
        'Mira abajo para verlos en la grilla.</div>'))

    }, error = function(e) {
      err_msg <- e$message
      if (grepl("429|503", err_msg)) {
        err_msg <- "API de IA temporalmente no disponible. Espera 1 min e intenta de nuevo."
      }
      shinyjs::html("smart_sched_status",
        paste0('<div class="alert alert-danger py-2 small">', err_msg, '</div>'))
    })
    shinyjs::enable("btn_gen_schedule")
  }, once = TRUE)
})

# ====================================================================
# CLEAR AI BLOCKS: Limpiar bloques generados por la IA
# ====================================================================
observeEvent(input$btn_clear_ai_blocks, {
  rv_gcal$ai_blocks <- NULL
  tryCatch(mg_ai_blocks_set(uid(), data.frame()),
           error = function(e) message("[StudyPilot] Clear AI blocks error: ", e$message))
  rv$refresh <- rv$refresh + 1
  shinyjs::html("smart_sched_status",
    '<div class="alert alert-info py-2 small">Bloques de estudio IA limpiados.</div>')
  showNotification("Bloques IA eliminados", type = "warning")
})

# ====================================================================
# DIAGNOSTICO: Resumen de prioridades (tabla reactiva, solo debug)
# ====================================================================
output$smart_sched_debug <- renderUI({
  # Solo renderiza si hay bloques AI activos (feedback visual)
  ai <- rv_gcal$ai_blocks
  if (is.null(ai) || !is.data.frame(ai) || nrow(ai) == 0) return(NULL)

  n_estudio  <- sum(!grepl("Descanso", ai$summary, ignore.case = TRUE))
  n_descanso <- nrow(ai) - n_estudio
  cursos_cubiertos <- unique(gsub("^.*\\[|\\]:.*$", "", ai$summary))

  tags$div(class = "small text-muted mt-1",
    paste0("Bloques activos: ", n_estudio, " estudio + ", n_descanso, " descanso",
           " | Cursos: ", paste(head(cursos_cubiertos, 4), collapse = ", "),
           if (length(cursos_cubiertos) > 4) paste0(" +", length(cursos_cubiertos) - 4) else "")
  )
})
