# ============ server_analytics.R — Analytics: Deuda Academica ============
# Sourced con local=TRUE.
#
# Logica: Para cada examen pendiente con temas vinculados:
#   1. Contar temas totales del examen (desde course_topics).
#   2. Contar bloques IA de estudio completados para ese curso (rv_gcal$ai_blocks).
#   3. Calcular ratio de preparacion.
#   4. Si ratio < 50%, marcar Alerta de Deuda Academica.

# calc_prep_metrics() vive en R/analytics_calc.R (la usan Analytics y el Dashboard)

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


# ====================================================================
# 4.1: Deuda Académica badge in navbar
# ====================================================================
output$debt_alert_badge <- renderUI({
  rv$refresh
  all_acts <- tryCatch(acts(), error = function(e) data.frame())
  ai_blocks <- rv_gcal$ai_blocks
  metrics <- tryCatch(
    calc_prep_metrics(all_acts, ai_blocks, course_topics),
    error = function(e) data.frame())
  if (nrow(metrics) == 0) return(NULL)
  n_debt <- sum(metrics$alert %in% c("critica", "alta"))
  if (n_debt == 0) return(NULL)
  tags$a(
    href = "#", onclick = "Shiny.setInputValue('go_analytics', Math.random())",
    style = "text-decoration:none;",
    tags$span(class = "badge bg-danger",
      style = "font-size:0.75rem;animation:pulse 2s infinite;",
      paste0(n_debt, " Deuda", if (n_debt > 1) "s" else ""))
  )
})

# Navigate to Analytics when badge is clicked
observeEvent(input$go_analytics, {
  bslib::nav_select("main_nav", selected = "Analytics")
})

# ============ Metricas de uso (Fase D) ============
output$usage_summary <- renderUI({
  rv$refresh
  u <- tryCatch(mg_usage_all(uid()), error = function(e) data.frame())
  if (!is.data.frame(u) || nrow(u) == 0) return(NULL)
  tab <- sort(table(u$event), decreasing = TRUE)
  labs <- c(login = "Inicios de sesion", smart_schedule = "Horarios IA generados",
            grades_save = "Guardados de notas", chat = "Consultas al chat",
            syllabus_extract = "Silabos extraidos")
  items <- lapply(names(tab), function(k) tags$li(paste0(if (!is.na(labs[k])) labs[k] else k, ": ", as.integer(tab[[k]]))))
  tags$div(class = "card mt-3", tags$div(class = "card-body py-2",
    tags$b("Tu actividad en StudyPilot"), tags$ul(class = "small mb-0 mt-1", items)))
})

