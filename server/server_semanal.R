# ============ server_semanal.R — Vista Semanal ============
# Sourced con local=TRUE.
# Se alimenta de acts() (cache offline cargado en login).
# Formato 24h, colores predefinidos por curso.

# ====================================================================
# WEEK NAVIGATION
# ====================================================================
observeEvent(input$week_prev, { rv$view_week <- max(1, rv$view_week - 1) })
observeEvent(input$week_next, { rv$view_week <- min(TOTAL_WEEKS, rv$view_week + 1) })

output$week_title <- renderUI({
  w <- rv$view_week
  d1 <- SEMESTER_START + (w - 1) * 7
  d2 <- d1 + 5
  tags$h5(class = "mb-0",
    paste0("Semana ", w, " (", format(d1, "%d %b"), " - ", format(d2, "%d %b"), ")"))
})

# ====================================================================
# WEEK VIEW — 6-day grid from Monday to Saturday
# ====================================================================
output$week_view <- renderUI({
  w <- rv$view_week
  day_names <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado")
  a <- acts()

  days <- lapply(1:6, function(d) {
    dd <- SEMESTER_START + (w - 1) * 7 + (d - 1)
    is_today <- dd == Sys.Date()
    dd_str <- as.character(dd)

    day_acts <- a |> filter(date == dd_str)

    act_items <- lapply(seq_len(nrow(day_acts)), function(i) {
      r <- day_acts[i, ]
      cname <- courses$short[match(r$course_id, courses$id)]
      if (is.na(cname)) cname <- r$course_id
      div(class = paste("act-item", if (r$done) "done" else ""),
        paste0(if (r$done) "[OK] " else "[ ] ",
               r$name, " (", cname, ", ", r$weight, "%)")
      )
    })

    card(
      card_header(class = if (is_today) "bg-primary text-white" else "",
        paste0(day_names[d], " ", format(dd, "%d %b"),
               if (is_today) " (HOY)" else "")
      ),
      card_body(class = "py-2",
        if (length(act_items) > 0) act_items else NULL,
        if (length(act_items) == 0) tags$p(class = "text-muted small fst-italic", "Sin actividades")
      )
    )
  })

  layout_columns(
    col_widths = breakpoints(sm = c(12, 12, 12, 12, 12, 12),
                             md = c(6, 6, 6, 6, 6, 6),
                             lg = c(4, 4, 4, 4, 4, 4)),
    !!!days)
})

