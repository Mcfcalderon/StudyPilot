# ============ server_dashboard.R — KPIs, Timeline, Countdown ============
# Depends on: acts(), all_grades_cache(), calc_avg_fast() from app.R

# ============ WEEK TIMELINE ============
output$week_timeline <- renderUI({
  cw <- current_week()
  blocks <- lapply(1:TOTAL_WEEKS, function(w) {
    cls <- if (w < cw) "wk-past" else if (w == cw) "wk-current" else "wk-future"
    d <- SEMESTER_START + (w - 1) * 7
    tags$div(class = paste("wk-block", cls),
      tags$div(class = "wk-num", paste0("S", w)),
      tags$div(class = "wk-date", format(d, "%d/%m"))
    )
  })
  div(class = "wk-timeline", blocks)
})

# ============ KPI STATS ============
output$stat_pct <- renderText({
  a <- acts(); done <- sum(a$done); total <- nrow(a)
  paste0(if (total > 0) round(done / total * 100) else 0, "%")
})

output$stat_pending <- renderText(sum(!acts()$done))

output$stat_overdue <- renderText({
  nrow(acts() |> filter(done == 0, date < as.character(Sys.Date())))
})

output$stat_high <- renderText({
  nrow(acts() |> filter(done == 0, weight >= 20))
})

output$stat_weeks <- renderText(TOTAL_WEEKS - current_week() + 1)

output$stat_avg <- renderText({
  cached <- all_grades_cache()
  sw <- 0; sn <- 0
  for (cid in courses$id) {
    avg <- calc_avg_fast(cid, cached)
    cr <- courses$credits[courses$id == cid]
    if (avg$partial > 0) { sw <- sw + cr; sn <- sn + avg$partial * cr }
  }
  if (sw > 0) round(sn / sw, 1) else "-"
})

# ============ UPCOMING ACTIVITIES TABLE ============
output$upcoming_table <- renderDT({
  a <- acts() |>
    filter(done == 0) |>
    mutate(
      days_left = as.integer(as.Date(date) - Sys.Date()),
      curso = ifelse(course_id == "_personal", "\U0001F4CC Personal",
        ifelse(course_id %in% courses$id, courses$short[match(course_id, courses$id)], course_id)),
      prioridad = priority_class(weight)
    ) |>
    arrange(date) |>
    head(10) |>
    select(Curso = curso, Actividad = name, Tipo = type, `Peso%` = weight,
           Fecha = date, `Días` = days_left, Prioridad = prioridad)

  datatable(a, options = list(pageLength = 10, dom = 't'), rownames = FALSE,
            selection = "none") |>
    formatStyle("Días", color = styleInterval(c(0, 3, 7), c("#dc2626", "#dc2626", "#ca8a04", "#16a34a"))) |>
    formatStyle("Peso%", fontWeight = "bold")
})

# ============ COUNTDOWN CARDS (used by Exam mode) ============
output$countdown_cards <- renderUI({
  a <- acts() |> filter(done == 0, weight >= 10) |>
    mutate(days_left = as.integer(as.Date(date) - Sys.Date())) |>
    arrange(date)
  if (nrow(a) == 0) return(tags$p(class = "text-muted", "No hay evaluaciones pendientes."))

  cards <- lapply(seq_len(nrow(a)), function(i) {
    r <- a[i, ]
    c_info <- courses |> filter(id == r$course_id)
    col <- if (r$days_left < 0) "#dc2626"
           else if (r$days_left <= 3) "#dc2626"
           else if (r$days_left <= 7) "#ca8a04"
           else "#16a34a"
    pri <- if (r$weight >= 20) "border-start-danger" else "border-start-warning"

    card(class = paste("border-start border-4", pri),
      card_body(
        div(class = "d-flex justify-content-between",
          tags$span(class = "badge", style = paste0("background:", c_info$color), c_info$short),
          div(class = "text-end",
            tags$span(style = paste0("font-size:1.5rem;font-weight:800;color:", col),
              if (r$days_left < 0) paste0("\u26A0", abs(r$days_left)) else r$days_left),
            tags$div(class = "small text-muted", if (r$days_left < 0) "atrasado" else "días")
          )
        ),
        tags$h6(class = "mt-2 mb-1", r$name),
        tags$div(class = "small text-muted",
          paste0("\U0001F4C5 ", format(as.Date(r$date), "%A %d %b"), " · S", r$week, " · Peso: ", r$weight, "%")
        )
      )
    )
  })
  layout_columns(col_widths = rep(4, length(cards)), !!!cards)
})

# ====================================================================
# 4.3: Card "Esta Semana" — resumen compacto del día/semana
# ====================================================================
output$esta_semana_card <- renderUI({
  rv$refresh
  a <- tryCatch(acts(), error = function(e) data.frame())
  today <- Sys.Date()

  # Próximas 3 actividades pendientes
  prox <- if (nrow(a) > 0) {
    p <- a[a$done == 0 & !is.na(a$date) & as.Date(a$date) >= today, ]
    if (nrow(p) > 0) p[order(as.Date(p$date)), ][seq_len(min(3, nrow(p))), ] else data.frame()
  } else data.frame()

  # Alerta de deuda académica
  metrics <- tryCatch(calc_prep_metrics(a, rv_gcal$ai_blocks, course_topics),
                      error = function(e) data.frame())
  n_debt <- if (nrow(metrics) > 0) sum(metrics$alert %in% c("critica", "alta")) else 0

  # Próxima clase del horario (hoy o siguiente)
  sched <- tryCatch(mg_schedule_get(uid()), error = function(e) data.frame())

  prox_items <- if (nrow(prox) > 0) {
    lapply(seq_len(nrow(prox)), function(i) {
      r <- prox[i, ]
      dl <- as.integer(as.Date(r$date) - today)
      cname <- if (r$course_id %in% courses$id) courses$short[courses$id == r$course_id] else r$course_id
      tags$div(class = "d-flex justify-content-between align-items-center py-1 border-bottom",
        tags$span(class = "small", tags$b(cname), " — ", r$name),
        tags$span(class = paste0("badge ", if (dl <= 2) "bg-danger" else if (dl <= 5) "bg-warning text-dark" else "bg-secondary"),
          if (dl == 0) "Hoy" else paste0(dl, "d")))
    })
  } else list(tags$div(class = "small text-muted", "Sin actividades próximas. 🎉"))

  div(class = "card mb-3", style = "border-left:4px solid #4f46e5;",
    div(class = "card-body py-2",
      div(class = "d-flex justify-content-between align-items-center mb-2",
        tags$h6(class = "fw-bold mb-0", "📆 Esta Semana"),
        if (n_debt > 0) tags$a(href = "#", onclick = "Shiny.setInputValue('go_analytics', Math.random())",
          tags$span(class = "badge bg-danger", paste0("⚠ ", n_debt, " deuda", if (n_debt > 1) "s" else "")))
        else tags$span(class = "badge bg-success", "Al día")
      ),
      tags$div(class = "small fw-bold text-muted mb-1", "Próximas entregas:"),
      prox_items
    )
  )
})
