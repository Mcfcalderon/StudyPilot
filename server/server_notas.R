# ============ server_notas.R — Notas / Calificaciones ============
# Sourced con local=TRUE desde app.R.
#
# REGLA CRITICA: Excluir actividades formativas (peso 0 o is_calificada == FALSE)
# del calculo de promedios y del panel de notas. Solo las actividades
# calificadas (weight > 0 && is_calificada != FALSE) aparecen aqui.

# ====================================================================
# GRADES PANELS — One card per course with grade inputs
# ====================================================================
output$grades_panels <- renderUI({
  cached <- all_grades_cache()
  all_acts <- acts()
  if (nrow(courses) == 0) {
    return(tags$div(class = "text-muted",
      "No hay cursos. Sube tus silabos en la pestana Cursos."))
  }

  panels <- lapply(seq_len(nrow(courses)), function(i) {
    c_info <- courses[i, ]

    # FILTRO CRITICO: solo actividades calificadas (excluir formativas)
    evals <- all_acts |> filter(course_id == c_info$id) |> arrange(date)
    if (nrow(evals) > 0) {
      if ("is_calificada" %in% names(evals)) {
        evals <- evals[is.na(evals$is_calificada) | evals$is_calificada == TRUE, ]
      }
      evals <- evals[!is.na(evals$weight) & evals$weight > 0, ]
    }
    if (nrow(evals) == 0) {
      evals <- data.frame(act_id = integer(), name = character(),
        code = character(), type = character(), weight = numeric())
    }

    # Generate code if empty
    for (j in seq_len(nrow(evals))) {
      if (is.null(evals$code[j]) || nchar(evals$code[j]) == 0) evals$code[j] <- paste0("E", j)
    }

    # Get existing grades from cache
    g <- cached[cached$course_id == c_info$id, ]
    if (nrow(g) > 0 && "code" %in% names(g) && "grade" %in% names(g)) {
      g <- g[, c("code", "grade")]
    } else {
      g <- data.frame(code = character(), grade = numeric())
    }

    avg <- calc_avg_fast(c_info$id, cached)
    col <- if (avg$pct_graded == 0) "secondary" else if (avg$partial >= 13) "success" else if (avg$partial >= 10.5) "warning" else "danger"

    # Build grade input rows
    grade_inputs <- lapply(seq_len(nrow(evals)), function(j) {
      ev <- evals[j, ]
      current <- g$grade[g$code == ev$code]
      current <- if (length(current) == 0 || all(is.na(current))) NA_real_ else current[1]
      type_badge <- switch(ev$type,
        examen = "danger", proyecto = "warning",
        ec = "info", quiz = "primary", "secondary")
      tags$tr(
        tags$td(ev$name),
        tags$td(tags$span(class = paste0("badge bg-", type_badge), ev$type)),
        tags$td(class = "fw-bold text-primary", paste0(ev$weight, "%")),
        tags$td(numericInput(
          paste0("grade_", c_info$id, "_", ev$code), NULL,
          value = if (!is.na(current)) current else NA,
          min = 0, max = 20, step = 0.5, width = "80px"))
      )
    })

    card(class = "mb-3", style = paste0("border-left: 4px solid ", c_info$color),
      card_header(class = "d-flex justify-content-between",
        tags$span(c_info$short, tags$small(class = "text-muted ms-2", c_info$id)),
        tags$span(class = paste0("badge bg-", col),
          if (avg$pct_graded == 0) "Sin evaluar" else paste0(avg$partial, " / ", avg$pct_graded, "% evaluado"))
      ),
      card_body(
        tags$div(class = "small text-muted mb-2", paste0("Formula: ", c_info$formula)),
        if (nrow(evals) == 0) {
          tags$div(class = "text-muted small", "Sin evaluaciones registradas")
        } else {
          tags$table(class = "table table-sm table-striped",
            tags$thead(tags$tr(
              tags$th("Evaluacion"), tags$th("Tipo"),
              tags$th("Peso"), tags$th("Nota"))),
            tags$tbody(grade_inputs))
        },
        div(class = "alert alert-light py-2 mt-2",
          div(class = "d-flex justify-content-between align-items-center",
            div(
              tags$small("Promedio parcial:"),
              tags$span(class = paste0("fs-4 fw-bold text-", col), avg$partial)
            ),
            div(class = "text-end small",
              if (avg$remaining > 0 && !is.na(avg$needed))
                tags$span(
                  if (avg$needed <= 20) {
                    paste0("Necesitas ", avg$needed, " en el ",
                           avg$remaining, "% restante para aprobar")
                  } else {
                    tags$span(class = "text-danger fw-bold",
                      paste0("Necesitas ", avg$needed, " - dificil"))
                  }
                )
              else if (avg$pct_graded == 0)
                tags$span(class = "text-muted", "Sin notas registradas aun")
            )
          )
        ),
        actionButton(
          paste0("save_grades_", c_info$id), "Guardar notas",
          class = "btn-sm btn-outline-primary",
          onclick = paste0(
            "Shiny.setInputValue(\'save_grade_course\', \'",
            c_info$id, "\', {priority: \'event\'})")),
        tags$span(id = paste0("grade_spinner_", c_info$id))
      )
    )
  })
  layout_columns(col_widths = breakpoints(sm = c(12), lg = c(6, 6)), !!!panels)
})


# ====================================================================
# SAVE GRADES — Single handler via JavaScript onclick
# Connects to mg_grade_set() / mg_grade_delete() in db_mongo.R
# ====================================================================
observeEvent(input$save_grade_course, {
  cid <- input$save_grade_course
  if (is.null(cid) || nchar(cid) == 0) return()

  # Show spinner
  shinyjs::html(paste0("grade_spinner_", cid),
    '<span class="spinner-border spinner-border-sm text-success ms-2"></span> <small class="text-success">Calculando promedio...</small>')
  shinyjs::disable(paste0("save_grades_", cid))

  tryCatch({
    all_acts <- acts()
    evals <- all_acts[all_acts$course_id == cid, ]
    # Apply same formativa filter as grades_panels UI
    if ("is_calificada" %in% names(evals)) {
      evals <- evals[is.na(evals$is_calificada) | evals$is_calificada == TRUE, ]
    }
    evals <- evals[!is.na(evals$weight) & evals$weight > 0, ]
    evals <- evals[order(evals$date), ]
    # Generate codes to match UI
    for (jj in seq_len(nrow(evals))) {
      if (is.null(evals$code[jj]) || !is.character(evals$code[jj]) || nchar(evals$code[jj]) == 0) evals$code[jj] <- paste0("E", jj)
    }
    saved <- 0
    current_uid <- uid()

    for (j in seq_len(nrow(evals))) {
      ev <- evals[j, ]
      ev_code <- if (is.null(ev$code) || nchar(ev$code) == 0) paste0("E", j) else ev$code
      val <- input[[paste0("grade_", cid, "_", ev_code)]]

      if (!is.null(val) && !is.na(val) && is.numeric(val)) {
        mg_grade_set(current_uid, cid, ev_code, val)
        saved <- saved + 1
      } else {
        # Delete grade if input is empty (user cleared it)
        mg_grade_delete(current_uid, cid, ev_code)
      }
    }

    rv$grades_refresh <- isolate(rv$grades_refresh) + 1
    cname <- if (cid %in% courses$id) courses$short[courses$id == cid] else cid
    showNotification(paste0(saved, " notas de ", cname, " guardadas"), type = "message")
  }, error = function(e) {
    showNotification(paste0("Error: ", e$message), type = "error")
  })

  shinyjs::html(paste0("grade_spinner_", cid), "")
  shinyjs::enable(paste0("save_grades_", cid))
})

# ====================================================================
# OVERALL AVERAGE — Promedio ponderado por creditos
# Excluye formativas via calc_avg_fast (que ya filtra weight > 0)
# ====================================================================
output$overall_avg <- renderText({
  cached <- all_grades_cache()
  sw <- 0; sn <- 0
  for (cid in courses$id) {
    avg <- calc_avg_fast(cid, cached)
    cr <- courses$credits[courses$id == cid]
    if (avg$partial > 0) { sw <- sw + cr; sn <- sn + avg$partial * cr }
  }
  if (sw > 0) round(sn / sw, 1) else "-"
})

# Credits label (reactive — updates when courses change)
output$overall_credits_label <- renderUI({
  rv$refresh
  total_cr <- sum(courses$credits, na.rm = TRUE)
  tags$span(class = "small text-muted",
    if (total_cr > 0) paste0("Basado en ", total_cr, " creditos")
    else "Agrega cursos para ver tu promedio"
  )
})
