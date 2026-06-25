# ============ server_analytics.R — Analytics: Deuda Academica ============
# Sourced con local=TRUE.
#
# Logica: Para cada examen pendiente con temas vinculados:
#   1. Contar temas totales del examen (desde course_topics).
#   2. Contar bloques IA de estudio completados para ese curso (rv_gcal$ai_blocks).
#   3. Calcular ratio de preparacion.
#   4. Si ratio < 50%, marcar Alerta de Deuda Academica.

# ====================================================================
# HELPER: Calcular metricas de preparacion por examen
# ====================================================================
calc_prep_metrics <- function(all_acts, ai_blocks, course_topics_list) {
  today <- Sys.Date()
  # Examenes pendientes con peso >= 15%
  exams <- all_acts[all_acts$done == 0 &
                    all_acts$type %in% c("examen", "quiz") &
                    all_acts$weight >= 15 &
                    as.Date(all_acts$date) >= today, ]
  if (nrow(exams) == 0) return(data.frame())

  exams <- exams[order(as.Date(exams$date)), ]

  metrics <- do.call(rbind, lapply(seq_len(nrow(exams)), function(i) {
    ex <- exams[i, ]
    cid <- ex$course_id

    # Temas vinculados al curso
    topics <- course_topics_list[[cid]]
    n_topics <- if (!is.null(topics)) length(topics) else 0

    # Temas vinculados a esta actividad especifica
    act_topics <- if ("temas_vinculados" %in% names(ex) && !is.null(ex$temas_vinculados[[1]])) {
      ex$temas_vinculados[[1]]
    } else character(0)
    n_act_topics <- length(act_topics)

    # Usar temas de la actividad si existen, sino del curso
    n_target <- max(n_act_topics, n_topics, 1)

    # Bloques de estudio IA para este curso
    n_blocks <- 0
    if (!is.null(ai_blocks) && is.data.frame(ai_blocks) && nrow(ai_blocks) > 0) {
      cname_full <- if (cid %in% courses$id) courses$name[courses$id == cid] else ""
      cshort <- if (cid %in% courses$id) courses$short[courses$id == cid] else ""
      course_blocks <- ai_blocks[
        grepl(cid, ai_blocks$summary, fixed = TRUE) |
        (nchar(cname_full) > 3 & grepl(cname_full, ai_blocks$summary, fixed = TRUE)) |
        (nchar(cshort) > 3 & grepl(cshort, ai_blocks$summary, fixed = TRUE)), ]


      # Excluir bloques de descanso
      course_blocks <- course_blocks[!grepl("Descanso|descanso", course_blocks$summary), ]
      n_blocks <- nrow(course_blocks)
    }

    # Ratio: bloques completados vs temas objetivo
    # Cada tema necesita al menos 1 bloque de estudio
    prep_ratio <- min(n_blocks / n_target, 1.0)
    days_left <- as.integer(as.Date(ex$date) - today)

    # Nivel de alerta
    alert <- if (prep_ratio < 0.3 && days_left <= 7) "critica"
             else if (prep_ratio < 0.5) "alta"
             else if (prep_ratio < 0.75) "media"
             else "ok"

    cname <- if (cid %in% courses$id) courses$short[courses$id == cid] else cid

    data.frame(
      course_id = cid, course_name = cname, exam_name = ex$name,
      weight = ex$weight, date = ex$date, days_left = days_left,
      n_topics = n_target, n_blocks = n_blocks,
      prep_ratio = round(prep_ratio, 2), alert = alert,
      stringsAsFactors = FALSE
    )
  }))

  metrics
}

# ====================================================================
# PREPARATION GAUGES — One gauge per upcoming exam
# ====================================================================
output$analytics_prep_gauges <- renderUI({
  rv$refresh
  all_acts <- acts()
  ai_blocks <- rv_gcal$ai_blocks

  metrics <- calc_prep_metrics(all_acts, ai_blocks, course_topics)

  if (nrow(metrics) == 0) {
    return(tags$div(class = "text-center text-muted py-4",
      tags$p("No hay examenes pendientes para analizar."),
      tags$p(class = "small", "Agrega actividades tipo examen o quiz con peso >= 15%.")
    ))
  }

  cards <- lapply(seq_len(nrow(metrics)), function(i) {
    m <- metrics[i, ]
    pct <- round(m$prep_ratio * 100)

    # Color by alert level
    col <- switch(m$alert,
      critica = "danger", alta = "warning",
      media   = "info",   ok  = "success", "secondary")

    alert_badge <- switch(m$alert,
      critica = tags$span(class = "badge bg-danger", "DEUDA CRITICA"),
      alta    = tags$span(class = "badge bg-warning text-dark", "Deuda Alta"),
      media   = tags$span(class = "badge bg-info", "En Progreso"),
      ok      = tags$span(class = "badge bg-success", "Preparado"),
      NULL)

    card(class = "mb-3",
      style = paste0("border-left:4px solid var(--bs-", col, ");"),
      card_body(class = "py-3",
        div(class = "d-flex justify-content-between align-items-center mb-2",
          div(
            tags$h6(class = "fw-bold mb-0", m$course_name),
            tags$small(class = "text-muted", m$exam_name)
          ),
          div(class = "text-end",
            alert_badge,
            tags$div(class = "small text-muted mt-1",
              paste0(m$days_left, " dias | ", m$weight, "%"))
          )
        ),
        # Progress bar (gauge)
        div(class = "progress", style = "height:20px;",
          div(class = paste0("progress-bar bg-", col),
            style = paste0("width:", pct, "%;"),
            role = "progressbar",
            paste0(pct, "%")
          )
        ),
        tags$div(class = "d-flex justify-content-between mt-1",
          tags$small(class = "text-muted",
            paste0(m$n_blocks, " bloques de estudio")),
          tags$small(class = "text-muted",
            paste0(m$n_topics, " temas objetivo"))
        )
      )
    )
  })

  layout_columns(col_widths = breakpoints(sm = c(12), lg = c(6, 6)), !!!cards)
})

# ====================================================================
# DEBT SUMMARY — Overview table
# ====================================================================
output$analytics_debt_summary <- renderUI({
  rv$refresh
  all_acts <- acts()
  ai_blocks <- rv_gcal$ai_blocks
  metrics <- calc_prep_metrics(all_acts, ai_blocks, course_topics)

  if (nrow(metrics) == 0) return(NULL)

  n_critica <- sum(metrics$alert == "critica")
  n_alta <- sum(metrics$alert == "alta")
  n_ok <- sum(metrics$alert == "ok")
  total <- nrow(metrics)
  avg_prep <- round(mean(metrics$prep_ratio) * 100)

  col <- if (n_critica > 0) "danger" else if (n_alta > 0) "warning" else "success"

  div(class = paste0("alert alert-", col),
    div(class = "d-flex justify-content-around text-center",
      div(
        tags$span(class = "fs-3 fw-bold", paste0(avg_prep, "%")),
        tags$br(), tags$small("Preparacion promedio")
      ),
      div(
        tags$span(class = "fs-3 fw-bold text-danger", n_critica + n_alta),
        tags$br(), tags$small("Alertas activas")
      ),
      div(
        tags$span(class = "fs-3 fw-bold text-success", n_ok),
        tags$br(), tags$small("Examenes preparados")
      ),
      div(
        tags$span(class = "fs-3 fw-bold", total),
        tags$br(), tags$small("Examenes pendientes")
      )
    )
  )
})

# ====================================================================
# AI RECOMMENDATION — Actionable advice based on debt
# ====================================================================
output$analytics_recommendation <- renderUI({
  rv$refresh
  all_acts <- acts()
  ai_blocks <- rv_gcal$ai_blocks
  metrics <- calc_prep_metrics(all_acts, ai_blocks, course_topics)

  if (nrow(metrics) == 0) {
    return(tags$p(class = "text-muted small", "Sin examenes para analizar."))
  }

  # Find the most urgent exam with debt
  debt <- metrics[metrics$alert %in% c("critica", "alta"), ]

  if (nrow(debt) == 0) {
    return(div(class = "alert alert-success",
      tags$b("Sin deuda academica detectada."),
      tags$p(class = "small mb-0",
        "Todos tus examenes tienen cobertura de estudio adecuada. ",
        "Sigue usando el Smart Scheduler para mantener el ritmo.")
    ))
  }

  # Sort by urgency (least days_left first, then lowest prep_ratio)
  debt <- debt[order(debt$days_left, debt$prep_ratio), ]
  worst <- debt[1, ]
  blocks_needed <- max(worst$n_topics - worst$n_blocks, 1)

  div(class = "alert alert-warning",
    tags$b(paste0("Prioridad: ", worst$course_name, " - ", worst$exam_name)),
    tags$p(class = "small mt-1 mb-1",
      paste0("Tienes ", worst$n_blocks, " de ", worst$n_topics,
             " bloques necesarios (", round(worst$prep_ratio * 100),
             "% cubierto). Faltan ", worst$days_left, " dias.")),
    tags$p(class = "small mb-0 fw-bold",
      paste0("Accion: Genera ", blocks_needed,
             " bloques adicionales con el Smart Scheduler ",
             "o estudia manualmente los temas faltantes.")),
    tags$hr(class = "my-2"),
    if (nrow(debt) > 1) {
      tags$div(class = "small text-muted",
        paste0("Tambien en deuda: ",
               paste(debt$course_name[-1], collapse = ", "),
               " (", nrow(debt) - 1, " mas)"))
    }
  )
})

