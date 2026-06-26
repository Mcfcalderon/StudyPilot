# ============ server_cursos.R — Cursos: CRUD + Silabos + Extraccion IA ============
# Sourced con local=TRUE desde app.R.
# Reglas:
#   - fileInput con multiple=TRUE para batch upload de silabos.
#   - Batch upload usa withProgress + tryCatch por archivo (a prueba de fallos).
#   - guardar_curso_extraido() es el helper compartido single/batch.

# ====================================================================
# REACTIVE: Todos los cursos (hardcoded + custom de MongoDB)
# ====================================================================
all_courses_reactive <- reactive({
  rv$refresh
  custom <- tryCatch(mg_custom_courses_get(uid()), error = function(e) data.frame())
  if (nrow(custom) > 0) {
    data.frame(
      id = custom$id, name = custom$name, short = custom$short,
      credits = if ("credits" %in% names(custom)) custom$credits else 3L,
      professor = if ("professor" %in% names(custom)) custom$professor else "",
      formula = if ("formula" %in% names(custom)) custom$formula else "",
      eval_day = if ("eval_day" %in% names(custom)) custom$eval_day else 5L,
      color = if ("color" %in% names(custom)) custom$color else "#2563eb",
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(id = character(), name = character(), short = character(),
      credits = integer(), professor = character(), formula = character(),
      eval_day = integer(), color = character(), stringsAsFactors = FALSE)
  }
})

# ====================================================================
# Update all course dropdowns when courses change
# ====================================================================
observe({
  ac <- all_courses_reactive()
  choices <- if (nrow(ac) > 0) setNames(ac$id, ac$short) else c("Sin cursos" = "")
  updateSelectInput(session, "del_course_id", choices = choices)
  updateSelectInput(session, "pomo_course", choices = choices)
  updateSelectInput(session, "ai_upload_course", choices = choices)
  updateSelectInput(session, "syllabus_course", choices = choices)
  updateSelectInput(session, "quiz_course", choices = choices)
  updateSelectInput(session, "act_filter_course", choices = c("Todos" = "all", choices))
})

# ====================================================================
# COURSE CARDS
# ====================================================================
output$course_cards <- renderUI({
  rv$refresh
  cached <- all_grades_cache()
  if (nrow(courses) == 0) return(tags$div(class = "text-muted py-3", "No hay cursos. Sube tus silabos abajo."))

  cards <- lapply(seq_len(nrow(courses)), function(i) {
    c_info <- courses[i, ]
    avg <- calc_avg_fast(c_info$id, cached)
    topics <- course_topics[[c_info$id]]
    graded_codes <- cached$code[cached$course_id == c_info$id]
    pending <- evaluations |> filter(course_id == c_info$id, !code %in% graded_codes)
    col <- if (avg$partial >= 10.5) "success" else "danger"

    card(class = "mb-3", style = paste0("border-left:4px solid ", c_info$color),
      card_header(c_info$name),
      card_body(
        tags$div(class = "small text-muted",
          paste0(c_info$id, " | ", c_info$credits, " cr | ", c_info$professor)),
        tags$div(class = "mt-2", paste0("Promedio: "),
          tags$b(class = paste0("text-", col), avg$partial),
          paste0(" (", avg$pct_graded, "% evaluado)")
        ),
        div(class = "progress mt-1 mb-2", style = "height:6px",
          div(class = paste0("progress-bar bg-", col),
            style = paste0("width:", avg$pct_graded, "%"))
        ),
        if (nrow(pending) > 0) tagList(
          tags$div(class = "small fw-bold", "Pendientes:"),
          lapply(seq_len(min(nrow(pending), 4)), function(j) {
            tags$div(class = "small text-muted",
              paste0("-> ", pending$label[j], " (", pending$weight[j], "%) S", pending$week[j]))
          })
        ),
        tags$details(class = "mt-2",
          tags$summary(class = "small fw-bold", "Temas del curso"),
          tags$ol(class = "small text-muted", style = "line-height:1.6",
            lapply(topics, function(t) tags$li(t))
          )
        )
      )
    )
  })
  layout_columns(col_widths = breakpoints(sm = c(12), lg = c(6, 6)), !!!cards)
})

# ====================================================================
# ADD / DELETE COURSE
# ====================================================================
observeEvent(input$add_course_btn, {
  id <- trimws(input$new_course_id)
  name <- trimws(input$new_course_name)
  short <- trimws(input$new_course_short)
  if (nchar(id) < 2 || nchar(name) < 3) {
    showNotification("Completa codigo y nombre del curso", type = "error")
    return()
  }
  # 3.3: Check for duplicate course code
  existing <- tryCatch(mg_custom_courses_get(uid()), error = function(e) data.frame())
  if (nrow(existing) > 0 && id %in% existing$id) {
    showNotification(paste0("El curso \"", id, "\" ya existe. Usa un codigo diferente."), type = "error")
    return()
  }
  tryCatch({
    mg_custom_course_add(uid(), id, name, short, input$new_course_credits,
      input$new_course_prof, "", as.integer(input$new_course_day), input$new_course_color)
    showNotification(paste0("Curso \"", short, "\" agregado"), type = "message")
    rv$refresh <- rv$refresh + 1
  }, error = function(e) showNotification(paste0("Error: ", e$message), type = "error"))
})

observeEvent(input$del_course_btn, {
  cid <- input$del_course_id
  if (is.null(cid) || nchar(cid) == 0) return()
  tryCatch({
    mg_custom_course_delete(uid(), cid)
    db_courses <- tryCatch(mg_custom_courses_get(uid()), error = function(e) data.frame())
    if (nrow(db_courses) > 0) {
      assign("courses", tibble::as_tibble(db_courses), envir = globalenv())
    } else {
      assign("courses", tibble::tibble(id = character(), name = character(), short = character(),
        credits = integer(), professor = character(), formula = character(),
        eval_day = integer(), color = character()), envir = globalenv())
    }
    showNotification(paste0("Curso \"", cid, "\" eliminado"), type = "warning")
    rv$refresh <- rv$refresh + 1
  }, error = function(e) showNotification(paste0("Error: ", e$message), type = "error"))
})


# ====================================================================
# SYLLABUS: Upload only (no AI extraction)
# ====================================================================
observeEvent(input$syllabus_upload_btn, {
  req(input$syllabus_file)
  f <- input$syllabus_file
  ext <- tolower(tools::file_ext(f$name))
  cid <- input$syllabus_course
  text <- ""
  tryCatch({
    if (ext == "txt") text <- paste(readLines(f$datapath, warn = FALSE), collapse = "\n")
    else if (ext == "pdf" && requireNamespace("pdftools", quietly = TRUE))
      text <- paste(pdftools::pdf_text(f$datapath), collapse = "\n")
    else if (ext == "docx" && requireNamespace("readtext", quietly = TRUE))
      text <- readtext::readtext(f$datapath)$text
    if (nchar(text) > 0) {
      mg_syllabus_add(uid(), cid, f$name, text)
      rv$refresh <- rv$refresh + 1
      output$syllabus_status <- renderPrint(cat(paste0("Silabo \"", f$name, "\" subido (", nchar(text), " car.)")))
    }
  }, error = function(e) output$syllabus_status <- renderPrint(cat(paste0("Error: ", e$message))))
})

# ====================================================================
# HELPER: Read file content (shared by single + batch)
# ====================================================================
read_syllabus_file <- function(filepath, ext) {
  if (ext == "txt") paste(readLines(filepath, warn = FALSE), collapse = "\n")
  else if (ext == "pdf" && requireNamespace("pdftools", quietly = TRUE))
    paste(pdftools::pdf_text(filepath), collapse = "\n")
  else if (ext == "docx" && requireNamespace("readtext", quietly = TRUE))
    readtext::readtext(filepath)$text
  else ""
}

# ====================================================================
# HELPER: Save one extracted course to MongoDB (shared single + batch)
# ====================================================================
guardar_curso_extraido <- function(user_id, d, syllabus_text = NULL) {
  ensure_init()
  cid <- if (!is.null(d$codigo) && nchar(d$codigo) > 0) d$codigo else paste0("C", format(Sys.time(), "%H%M%S"))
  existing <- mg_custom_courses_get(user_id)
  if (nrow(existing) > 0 && cid %in% existing$id) {
    mg_custom_course_delete(user_id, cid)
  }
  cname <- if (!is.null(d$nombre_curso)) d$nombre_curso else "Curso sin nombre"
  short <- substr(cname, 1, 20)
  credits <- if (!is.null(d$creditos)) as.integer(d$creditos) else 3L
  prof <- if (!is.null(d$profesor)) d$profesor else ""
  formula <- if (!is.null(d$formula)) d$formula else ""
  colors <- c("#2563eb", "#16a34a", "#7c3aed", "#ea580c", "#db2777", "#0891b2", "#d97706", "#059669")
  clr <- sample(colors, 1)

  mg_custom_course_add(user_id, cid, cname, short, credits, prof, formula, 5L, clr)

  evals <- d$evaluaciones
  n_evals <- 0
  if (!is.null(evals) && is.data.frame(evals) && nrow(evals) > 0) {
    for (i in seq_len(nrow(evals))) {
      ev_weight <- suppressWarnings(as.numeric(evals$peso[i]))
      ev_week <- suppressWarnings(as.integer(evals$semana[i]))
      ev_type <- if (!is.null(evals$tipo[i])) evals$tipo[i] else "ec"
      ev_date <- if (!is.na(ev_week) && ev_week > 0) as.character(week_to_date(ev_week, 5L)) else as.character(Sys.Date())
      if (is.na(ev_weight)) ev_weight <- 0
      if (is.na(ev_week) || ev_week == 0) ev_week <- date_to_week(ev_date)
      mg_activity_add(user_id, cid, ev_type, evals$nombre[i], ev_date, ev_weight, paste0("Semana ", ev_week))
    }
    n_evals <- nrow(evals)
  }

  if (!is.null(syllabus_text) && nchar(syllabus_text) > 0) {
    mg_syllabus_add(user_id, cid, paste0("Silabo_", short, ".pdf"), syllabus_text)
  }

  if (!is.null(d$temas) && length(d$temas) > 0) {
    topics_text <- paste0("Temas del curso ", cname, ":\n",
      paste(seq_along(d$temas), d$temas, sep = ". ", collapse = "\n"))
    mg_note_add(user_id, cid, topics_text, "Temas extraidos del silabo")
    ct <- get0("course_topics", envir = globalenv())
    if (is.null(ct)) ct <- list()
    ct[[cid]] <- d$temas
    assign("course_topics", ct, envir = globalenv())
  }

  list(cid = cid, cname = cname, n_evals = n_evals)
}

# ====================================================================
# EXTRACT COURSE FROM SYLLABUS WITH AI (single + batch)
# fileInput con multiple=TRUE. Batch usa withProgress + tryCatch.
# ====================================================================
observeEvent(input$syllabus_extract_btn, {
  req(input$syllabus_file)
  files <- input$syllabus_file  # dataframe: name, size, type, datapath
  n_files <- nrow(files)

  shinyjs::disable("syllabus_extract_btn")
  current_uid <- uid()

  # ---- SINGLE FILE: preview + confirm workflow ----
  if (n_files == 1) {
    f <- files[1, ]
    ext <- tolower(tools::file_ext(f$name))
    shinyjs::html("syllabus_status_div",
      '<div class="alert alert-info py-2 small"><span class="spinner-border spinner-border-sm me-2"></span><b>Extrayendo curso con IA...</b> 15-30 seg.</div>')

    text <- tryCatch(read_syllabus_file(f$datapath, ext), error = function(e) "")
    if (nchar(text) < 50) {
      shinyjs::html("syllabus_status_div",
        '<div class="alert alert-danger py-2 small">No se pudo leer el archivo o esta vacio.</div>')
      shinyjs::enable("syllabus_extract_btn")
      return()
    }
    rv_extract$text <- text

    session$onFlushed(function() {
      tryCatch({
        result <- ai_extract_syllabus(text)
        if (!is.null(result$error)) {
          shinyjs::html("syllabus_status_div",
            paste0('<div class="alert alert-danger py-2 small">', result$error, '</div>'))
          shinyjs::enable("syllabus_extract_btn")
          return()
        }
        rv_extract$data <- result
        shinyjs::html("syllabus_status_div",
          '<div class="alert alert-success py-2 small">Curso extraido. Revisa el preview y confirma.</div>')

        # Build preview HTML
        evals <- result$evaluaciones
        eval_rows <- ""
        if (!is.null(evals) && is.data.frame(evals)) {
          for (i in seq_len(nrow(evals))) {
            eval_rows <- paste0(eval_rows,
              '<tr><td>', evals$nombre[i], '</td><td>', evals$codigo[i],
              '</td><td>', evals$peso[i], '%</td><td>S', evals$semana[i],
              '</td><td>', evals$tipo[i], '</td></tr>')
          }
        }
        temas_html <- ""
        if (!is.null(result$temas)) {
          temas_html <- paste0('<ul class="small mb-0">',
            paste0('<li>', result$temas, '</li>', collapse = ""), '</ul>')
        }
        preview_html <- paste0(
          '<div class="card shadow-sm"><div class="card-body py-3">',
          '<h6 class="fw-bold mb-1">',
            ifelse(is.null(result$nombre_curso), "Curso", result$nombre_curso), '</h6>',
          '<div class="small text-muted mb-2">',
            'Codigo: <b>', ifelse(is.null(result$codigo), "---", result$codigo), '</b> | ',
            'Creditos: <b>', ifelse(is.null(result$creditos), "---", result$creditos), '</b> | ',
            'Profesor: <b>', ifelse(is.null(result$profesor), "---", result$profesor), '</b>',
          '</div>',
          if (nchar(eval_rows) > 0) paste0(
            '<b class="small">Evaluaciones:</b>',
            '<table class="table table-sm table-bordered small mt-1 mb-2"><thead><tr>',
            '<th>Evaluacion</th><th>Codigo</th><th>Peso</th><th>Semana</th><th>Tipo</th>',
            '</tr></thead><tbody>', eval_rows, '</tbody></table>') else "",
          if (nchar(temas_html) > 0) paste0('<b class="small">Temas:</b>', temas_html) else "",
          '</div></div>')

        output$syllabus_preview <- renderUI(HTML(preview_html))
        shinyjs::show("syllabus_confirm_div")
        shinyjs::enable("syllabus_confirm")
        shinyjs::enable("syllabus_cancel")
      }, error = function(e) {
        shinyjs::html("syllabus_status_div",
          paste0('<div class="alert alert-danger py-2 small">Error: ', e$message, '</div>'))
      })
      shinyjs::enable("syllabus_extract_btn")
    }, once = TRUE)
    return()
  }

  # ---- BATCH MODE: multiple files -> withProgress + tryCatch per file ----
  shinyjs::html("syllabus_status_div",
    paste0('<div class="alert alert-info py-2 small">',
           '<span class="spinner-border spinner-border-sm me-2"></span>',
           '<b>Procesando ', n_files, ' silabos en batch...</b> ',
           'Esto puede tomar ', n_files * 20, '-', n_files * 40, ' seg.</div>'))

  session$onFlushed(function() {
    ok <- 0; fail <- 0; errors <- character()

    withProgress(message = "Analizando Silabos", value = 0, {
      for (fi in seq_len(n_files)) {
        f <- files[fi, ]
        fname <- f$name
        ext <- tolower(tools::file_ext(fname))

        incProgress(1 / n_files, detail = paste0("(", fi, "/", n_files, ") ", fname))

        tryCatch({
          text <- read_syllabus_file(f$datapath, ext)
          if (nchar(text) < 50) {
            fail <- fail + 1
            errors <- c(errors, paste0(fname, ": archivo vacio o ilegible"))
            next
          }

          result <- ai_extract_syllabus(text)
          if (!is.null(result$error)) {
            fail <- fail + 1
            errors <- c(errors, paste0(fname, ": ", result$error))
            next
          }

          saved <- guardar_curso_extraido(current_uid, result, text)
          ok <- ok + 1
          message("[StudyPilot] Batch: saved \"", saved$cname, "\" (",
                  saved$n_evals, " evals) from ", fname)

        }, error = function(e) {
          fail <<- fail + 1
          errors <<- c(errors, paste0(fname, ": ", e$message))
          message("[StudyPilot] Batch error on ", fname, ": ", e$message)
          showNotification(paste0("Error en ", fname, ": ", e$message),
                          type = "error", duration = 8)
        })
      }
    })

    # Refresh courses after batch
    tryCatch({
      db_courses <- mg_custom_courses_get(current_uid)
      if (nrow(db_courses) > 0) assign("courses", tibble::as_tibble(db_courses), envir = globalenv())
    }, error = function(e) NULL)
    rv$refresh <- isolate(rv$refresh) + 1

    error_html <- if (fail > 0) paste0('<br><small class="text-muted">Errores:<br>',
      paste0("- ", errors, collapse = "<br>"), '</small>') else ""

    shinyjs::html("syllabus_status_div", paste0(
      '<div class="alert ',
      if (fail == 0) 'alert-success' else if (ok > 0) 'alert-warning' else 'alert-danger',
      ' py-2 small">',
      if (ok > 0) paste0('<b>', ok, ' curso(s)</b> creados exitosamente. ') else "",
      if (fail > 0) paste0('<b>', fail, '</b> archivo(s) fallaron. ') else "",
      error_html, '</div>'))

    shinyjs::enable("syllabus_extract_btn")
    gc()
  }, once = TRUE)
})

# ====================================================================
# CONFIRM: Save extracted course (single file workflow)
# ====================================================================
observeEvent(input$syllabus_confirm, {
  req(rv_extract$data)
  d <- rv_extract$data
  message("[StudyPilot] Confirm: saving \"", d$nombre_curso, "\"")

  shinyjs::disable("syllabus_confirm")
  shinyjs::disable("syllabus_cancel")
  shinyjs::html("syllabus_status_div",
    '<div class="alert alert-info py-2 small"><span class="spinner-border spinner-border-sm text-success me-2"></span><b>Guardando...</b></div>')

  tryCatch({
    saved <- guardar_curso_extraido(uid(), d, rv_extract$text)

    db_courses <- tryCatch(mg_custom_courses_get(uid()), error = function(e) data.frame())
    if (nrow(db_courses) > 0) assign("courses", tibble::as_tibble(db_courses), envir = globalenv())
    rv$refresh <- rv$refresh + 1
    rv_extract$data <- NULL
    rv_extract$text <- NULL
    output$syllabus_preview <- renderUI(NULL)
    shinyjs::hide("syllabus_confirm_div")
    shinyjs::html("syllabus_status_div",
      paste0('<div class="alert alert-success py-2 small">Curso <b>', saved$cname,
             '</b> creado con ', saved$n_evals, ' evaluaciones.</div>'))
  }, error = function(e) {
    shinyjs::html("syllabus_status_div",
      paste0('<div class="alert alert-danger py-2 small">Error: ', e$message, '</div>'))
    shinyjs::enable("syllabus_confirm")
    shinyjs::enable("syllabus_cancel")
  })
})

# ====================================================================
# CANCEL extraction
# ====================================================================
observeEvent(input$syllabus_cancel, {
  rv_extract$data <- NULL
  rv_extract$text <- NULL
  output$syllabus_preview <- renderUI(NULL)
  shinyjs::hide("syllabus_confirm_div")
  shinyjs::html("syllabus_status_div", "")
})

# ====================================================================
# SYLLABI LIST (uploaded files)
# ====================================================================
output$syllabi_list <- renderUI({
  rv$refresh
  syllabi <- mg_syllabi_get(uid())
  if (nrow(syllabi) == 0) return(tags$p(class = "text-muted small", "Sin silabos subidos."))
  lapply(seq_len(nrow(syllabi)), function(i) {
    s <- syllabi[i, ]
    cname <- courses$short[match(s$course_id, courses$id)]
    div(class = "d-flex align-items-center gap-2 py-1 border-bottom small",
      tags$span(class = "badge bg-primary", cname),
      tags$span(s$filename),
      tags$span(class = "text-muted", paste0(nchar(s$content), " car."))
    )
  })
})
