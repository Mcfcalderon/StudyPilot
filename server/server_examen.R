# ============ server_examen.R — Quiz IA + Exam Mode + Material IA ============
# Sourced con local=TRUE. Lee temas desde MongoDB via course_topics.

# ====================================================================
# EXAM MODE: Update exam selector from pending activities
# ====================================================================
observe({
  a <- acts() |> filter(done == 0, weight >= 15, type %in% c("examen", "quiz"))
  if (nrow(a) == 0) {
    updateSelectInput(session, "exam_select", choices = c("Sin examenes pendientes" = "0"))
    return()
  }
  a <- a |> mutate(days_left = as.integer(as.Date(date) - Sys.Date())) |> arrange(date)
  choices <- setNames(seq_len(nrow(a)), paste0(
    courses$short[match(a$course_id, courses$id)], " -- ", a$name,
    " (", a$days_left, "d, ", a$weight, "%)"
  ))
  updateSelectInput(session, "exam_select", choices = choices)
})

selected_exam <- reactive({
  val <- input$exam_select
  if (is.null(val) || length(val) == 0 || val == "" || val == "0") return(NULL)
  idx <- suppressWarnings(as.integer(val))
  if (length(idx) == 0 || is.na(idx) || idx < 1) return(NULL)
  a <- acts() |> filter(done == 0, weight >= 15, type %in% c("examen", "quiz"))
  if (nrow(a) == 0) return(NULL)
  a <- a |> mutate(days_left = as.integer(as.Date(date) - Sys.Date())) |> arrange(date)
  if (idx > nrow(a)) return(NULL)
  a[idx, ]
})

output$exam_info <- renderUI({
  ex <- selected_exam()
  if (is.null(ex)) return(NULL)
  c_info <- courses |> filter(id == ex$course_id)
  col <- if (ex$days_left <= 3) "danger" else if (ex$days_left <= 7) "warning" else "success"
  tagList(
    tags$div(class = "text-center mt-2",
      tags$h1(class = paste0("text-", col), style = "font-weight:800",
              if (ex$days_left < 0) paste0("!", abs(ex$days_left)) else ex$days_left),
      tags$p(class = "text-muted", if (ex$days_left < 0) "dias atrasado" else "dias restantes")
    ),
    tags$div(class = "small",
      tags$b("Curso: "), c_info$name, tags$br(),
      tags$b("Peso: "), paste0(ex$weight, "%"), tags$br(),
      tags$b("Fecha: "), format(as.Date(ex$date), "%A %d de %B"), tags$br(),
      tags$b("Semana: "), ex$week
    )
  )
})

# Exam topics checklist — reads from MongoDB course_topics
output$exam_topics_checklist <- renderUI({
  ex <- selected_exam()
  if (is.null(ex)) return(tags$p(class = "text-muted", "Selecciona un examen"))

  topics <- course_topics[[ex$course_id]]
  if (is.null(topics) || length(topics) == 0)
    return(tags$p(class = "text-muted small", "Sin temas definidos. Sube el silabo."))

  is_final <- grepl("final|EF|EXM2", ex$code, ignore.case = TRUE)
  show_topics <- if (is_final) topics else topics[1:ceiling(length(topics) / 2)]
  label <- if (is_final) "Todo el curso (Final)" else "Primera mitad (Parcial)"

  checks <- mg_exam_checks_get(uid(), ex$course_id)

  topic_items <- lapply(seq_along(show_topics), function(i) {
    key <- paste0(ex$code, "_", i)
    is_checked <- any(checks$topic_key == key & checks$checked == 1)
    div(class = paste("topic-item", if (is_checked) "checked" else ""),
      checkboxInput(paste0("etopic_", key), label = paste0(i, ". ", show_topics[i]),
                   value = is_checked, width = "100%")
    )
  })

  tagList(
    tags$div(class = "small text-muted mb-2", paste0("Temas -- ", label)),
    topic_items
  )
})

# Save exam topic checks
observe({
  ex <- selected_exam()
  if (is.null(ex)) return()
  topics <- course_topics[[ex$course_id]]
  if (is.null(topics)) return()
  show_topics <- if (grepl("final|EF|EXM2", ex$code, ignore.case = TRUE)) topics
                 else topics[1:ceiling(length(topics) / 2)]
  for (i in seq_along(show_topics)) {
    key <- paste0(ex$code, "_", i)
    val <- input[[paste0("etopic_", key)]]
    if (!is.null(val)) {
      mg_exam_check_set(uid(), ex$course_id, key, as.integer(val))
    }
  }
})

output$exam_progress_bar <- renderUI({
  ex <- selected_exam()
  if (is.null(ex)) return(NULL)
  topics <- course_topics[[ex$course_id]]
  if (is.null(topics) || length(topics) == 0) return(NULL)
  show_topics <- if (grepl("final|EF", ex$code, ignore.case = TRUE)) topics
                 else topics[1:ceiling(length(topics) / 2)]
  checks <- mg_exam_checks_get(uid(), ex$course_id)
  checked <- sum(sapply(seq_along(show_topics), function(i) {
    key <- paste0(ex$code, "_", i)
    any(checks$topic_key == key & checks$checked == 1)
  }))
  pct <- round(checked / length(show_topics) * 100)
  col <- if (pct >= 80) "success" else if (pct >= 50) "warning" else "danger"
  div(class = "progress", style = "height:10px;",
    div(class = paste0("progress-bar bg-", col), style = paste0("width:", pct, "%")),
    tags$small(class = "text-muted ms-2", paste0(checked, "/", length(show_topics), " temas"))
  )
})

# Pomodoro link to exam
observeEvent(input$exam_pomo, {
  ex <- selected_exam()
  if (!is.null(ex) && ex$course_id %in% courses$id) {
    updateSelectInput(session, "pomo_course", selected = ex$course_id)
    updateNavbarPage(session, "main_nav", selected = "Pomodoro")
  }
})


# ====================================================================
# QUIZ IA: Generate + Submit + Render
# ====================================================================
observeEvent(input$quiz_generate, {
  shinyjs::disable("quiz_generate")
  course_id <- input$quiz_course
  n_q <- input$quiz_n
  q_type <- input$quiz_type

  rv_quiz$loading <- TRUE
  rv_quiz$error_msg <- NULL
  rv_quiz$exam <- NULL
  rv_quiz$submitted <- FALSE
  rv_quiz$results <- NULL

  session$onFlushed(function() {
    tryCatch({
      exam <- generate_practice_exam(course_id, n_q, q_type)
      if (!is.null(exam) && length(exam) > 0) {
        rv_quiz$exam <- exam
        updateActionButton(session, "quiz_submit", disabled = FALSE)
      } else {
        rv_quiz$error_msg <- "No se pudieron generar preguntas. Verifica que el curso tenga temas."
      }
    }, error = function(e) {
      rv_quiz$error_msg <- paste0("Error: ", e$message)
    })
    rv_quiz$loading <- FALSE
    shinyjs::enable("quiz_generate")
  }, once = TRUE)
})

output$quiz_content <- renderUI({
  if (isTRUE(rv_quiz$loading)) {
    return(tags$div(class = "text-center py-5",
      tags$div(class = "spinner-border text-primary"),
      tags$p(class = "mt-2 text-muted", "Generando preguntas con IA... (15-30 seg)")
    ))
  }
  if (!is.null(rv_quiz$error_msg)) {
    return(tags$div(class = "alert alert-warning", rv_quiz$error_msg))
  }
  exam <- rv_quiz$exam
  if (is.null(exam)) return(tags$div(class = "text-center text-muted py-5",
    tags$h5("Haz clic en Generar para comenzar"),
    tags$p("Se generaran preguntas basadas en los temas del curso.")
  ))

  cname <- courses$short[courses$id == input$quiz_course]
  questions_ui <- lapply(seq_along(exam), function(i) {
    q <- exam[[i]]
    div(class = "mb-4 p-3 border rounded",
      tags$h6(class = "fw-bold", paste0("Pregunta ", i, " -- ", q$topic)),
      tags$p(q$q),
      if (q$type == "mc") {
        radioButtons(paste0("quiz_ans_", i), NULL,
                    choices = setNames(seq_along(q$opts), q$opts),
                    selected = character(0), width = "100%")
      } else {
        textAreaInput(paste0("quiz_ans_", i), "Tu respuesta:",
                     rows = 4, width = "100%", placeholder = "Escribe tu respuesta aqui...")
      }
    )
  })
  tagList(
    tags$div(class = "alert alert-info py-2",
      paste0(cname, " -- ", length(exam), " preguntas")),
    questions_ui
  )
})

observeEvent(input$quiz_submit, {
  exam <- rv_quiz$exam
  if (is.null(exam)) return()
  answers <- lapply(seq_along(exam), function(i) input[[paste0("quiz_ans_", i)]])
  rv_quiz$results <- grade_exam(answers, exam)
  rv_quiz$submitted <- TRUE
  updateActionButton(session, "quiz_submit", disabled = TRUE)
})

output$quiz_score_summary <- renderUI({
  res <- rv_quiz$results
  if (is.null(res)) return(NULL)
  col <- if (!is.na(res$pct) && res$pct >= 70) "success"
         else if (!is.na(res$pct) && res$pct >= 50) "warning" else "danger"
  n_correct <- sum(sapply(res$results, function(r) isTRUE(r$correct)))
  n_wrong <- sum(sapply(res$results, function(r) isFALSE(r$correct)))
  n_open <- sum(sapply(res$results, function(r) is.na(r$correct)))
  div(class = paste0("alert alert-", col, " mt-3"),
    div(class = "text-center",
      tags$h2(class = "mb-1 fw-bold", paste0(res$score, "/", res$total)),
      if (!is.na(res$pct)) tags$h5(paste0(res$pct, "% de acierto"))
    ),
    tags$hr(class = "my-2"),
    div(class = "d-flex justify-content-around text-center",
      div(tags$span(class = "fs-4 text-success fw-bold", n_correct), tags$br(), tags$small("Correctas")),
      div(tags$span(class = "fs-4 text-danger fw-bold", n_wrong), tags$br(), tags$small("Incorrectas")),
      if (n_open > 0) div(tags$span(class = "fs-4 text-primary fw-bold", n_open), tags$br(), tags$small("Abiertas"))
    )
  )
})

output$quiz_results <- renderUI({
  res <- rv_quiz$results
  if (is.null(res)) return(tags$p(class = "text-muted", "Genera y califica un examen para ver resultados."))
  items <- lapply(seq_along(res$results), function(i) {
    r <- res$results[[i]]
    is_correct <- isTRUE(r$correct)
    is_open <- is.na(r$correct)
    icon <- if (is_open) "?" else if (is_correct) "OK" else "X"
    bg <- if (is_open) "light" else if (is_correct) "success" else "danger"
    div(class = paste0("alert alert-", bg, " py-2 mb-2"),
      tags$div(class = "fw-bold", paste0(icon, " Pregunta ", i, ": ", r$question)),
      if (!is_open) tagList(
        tags$div(class = "small", paste0("Tu respuesta: ", r$user_answer)),
        if (!is_correct) tags$div(class = "small fw-bold", paste0("Respuesta correcta: ", r$right_answer))
      ) else tagList(
        tags$div(class = "small", paste0("Tu respuesta: ", substr(r$user_answer, 1, 200))),
        tags$div(class = "small fw-bold text-primary", paste0("Guia: ", r$right_answer))
      ),
      tags$div(class = "small text-muted fst-italic mt-1", r$explanation)
    )
  })
  tagList(items)
})

# ====================================================================
# AI STUDY MATERIAL: Upload + Summary + Questions
# ====================================================================
rv_ai <- reactiveValues(result = NULL, text = NULL)

observeEvent(input$ai_study_file, {
  req(input$ai_study_file)
  f <- input$ai_study_file
  ext <- tolower(tools::file_ext(f$name))
  text <- ""
  tryCatch({
    if (ext %in% c("txt", "csv")) text <- paste(readLines(f$datapath, warn = FALSE), collapse = "\n")
    else if (ext == "pdf" && requireNamespace("pdftools", quietly = TRUE))
      text <- paste(pdftools::pdf_text(f$datapath), collapse = "\n")
    else if (ext == "docx" && requireNamespace("readtext", quietly = TRUE))
      text <- readtext::readtext(f$datapath)$text
    else if (ext %in% c("xlsx", "xls") && requireNamespace("readxl", quietly = TRUE)) {
      sheets <- readxl::excel_sheets(f$datapath)
      text <- paste(sapply(sheets, function(s)
        paste(capture.output(print(readxl::read_excel(f$datapath, sheet = s))), collapse = "\n")),
        collapse = "\n\n")
    }
    rv_ai$text <- text
    output$ai_upload_status <- renderPrint(
      cat(paste0("\"", f$name, "\" cargado (", nchar(text), " caracteres).")))
  }, error = function(e) output$ai_upload_status <- renderPrint(cat(paste0("Error: ", e$message))))
})

observeEvent(input$ai_gen_summary, {
  req(rv_ai$text)
  if (nchar(rv_ai$text) < 50) {
    output$ai_result_content <- renderUI(tags$div(class = "alert alert-warning", "Sube un archivo primero."))
    return()
  }
  text_val <- rv_ai$text
  cid <- input$ai_upload_course
  cname <- courses$name[courses$id == cid]
  shinyjs::disable("ai_gen_summary")
  current_uid <- uid()
  output$ai_result_content <- renderUI(tags$div(class = "alert alert-info py-3 text-center",
    HTML('<span class="spinner-border spinner-border-sm me-2"></span><b>Generando resumen con IA...</b>')))

  session$onFlushed(function() {
    tryCatch({
      response <- ai_generate_summary(text_val, cname)
      if (grepl("^Error al generar", response)) {
        output$ai_result_content <- renderUI(tags$div(class = "alert alert-danger", response))
      } else {
        mg_note_add(current_uid, cid, response, "Resumen IA")
        isolate({ rv$refresh <- rv$refresh + 1 })
        html_response <- markdown::markdownToHTML(text = response, fragment.only = TRUE)
        html_response <- gsub(
          '<pre><code class="mermaid">(.+?)</code></pre>',
          '<div class="mermaid">\\1</div>', html_response, perl = TRUE)
        output$ai_result_content <- renderUI(tags$div(
          tags$div(class = "alert alert-success py-2", "Resumen generado y guardado"),
          tags$div(class = "ai-formatted", style = "max-height:600px;overflow-y:auto",
            HTML(html_response)),
          tags$script(HTML("setTimeout(function(){try{mermaid.run()}catch(e){}},300);"))
        ))
      }
    }, error = function(e) {
      output$ai_result_content <- renderUI(tags$div(class = "alert alert-danger", e$message))
    })
    shinyjs::enable("ai_gen_summary")
  }, once = TRUE)
})

observeEvent(input$ai_gen_questions2, {
  req(rv_ai$text)
  if (nchar(rv_ai$text) < 50) {
    output$ai_result_content <- renderUI(tags$div(class = "alert alert-warning", "Sube un archivo primero."))
    return()
  }
  text_val <- rv_ai$text
  cid <- input$ai_upload_course
  cname <- courses$name[courses$id == cid]
  shinyjs::disable("ai_gen_questions2")
  output$ai_result_content <- renderUI(tags$div(class = "alert alert-info py-3 text-center",
    HTML('<span class="spinner-border spinner-border-sm me-2"></span><b>Generando preguntas con IA...</b>')))

  session$onFlushed(function() {
    tryCatch({
      ai_qs <- ai_generate_questions(text_val, cname, n_questions = 8)
      if (!is.null(ai_qs$error)) {
        output$ai_result_content <- renderUI(tags$div(class = "alert alert-danger", ai_qs$error))
        shinyjs::enable("ai_gen_questions2")
        return()
      }
      if (is.null(exam_questions[[cid]])) exam_questions[[cid]] <<- list()
      exam_questions[[cid]] <<- c(exam_questions[[cid]], ai_qs)
      output$ai_result_content <- renderUI(tags$div(
        tags$div(class = "alert alert-success py-2",
          paste0(length(ai_qs), " preguntas generadas. Ve a Examenes para usarlas.")),
        lapply(seq_along(ai_qs), function(i) {
          q <- ai_qs[[i]]
          tipo_badge <- if (q$type == "mc") tags$span(class = "badge bg-primary ms-2", "Opcion Multiple")
                        else tags$span(class = "badge bg-info ms-2", "Abierta")
          tema_badge <- if (!is.null(q$topic) && nchar(q$topic) > 0)
            tags$span(class = "badge bg-secondary ms-2", q$topic)
          collapse_id <- paste0("q_collapse_", i)
          tags$div(class = "card mb-3 shadow-sm", style = "border-left:4px solid #2563eb;",
            tags$div(class = "card-body py-2 px-3",
              tags$div(class = "d-flex align-items-center mb-1",
                tags$span(class = "fw-bold", style = "color:#2563eb;", paste0("Pregunta ", i)),
                tipo_badge, tema_badge),
              tags$p(class = "mb-1", style = "font-size:0.9rem;", q$q),
              tags$div(class = "mt-2",
                tags$a(class = "btn btn-sm btn-outline-secondary py-0",
                  `data-bs-toggle` = "collapse", href = paste0("#", collapse_id), "Ver explicacion"),
                tags$div(id = collapse_id, class = "collapse mt-2",
                  tags$div(class = "alert alert-light py-2 small mb-0",
                    if (!is.null(q$expl) && nchar(q$expl) > 0)
                      tags$div(tags$b("Explicacion: "), q$expl)
                  )
                )
              )
            )
          )
        })
      ))
    }, error = function(e) {
      output$ai_result_content <- renderUI(tags$div(class = "alert alert-danger", e$message))
    })
    shinyjs::enable("ai_gen_questions2")
  }, once = TRUE)
})

# Processed notes list
output$ai_processed_notes <- renderUI({
  rv$refresh
  notes <- mg_notes_all(uid())
  ai_notes <- notes[grepl("IA|RESUMEN|organizado", notes$source, ignore.case = TRUE), ]
  if (nrow(ai_notes) == 0) return(tags$p(class = "text-muted small", "Sin material procesado con IA."))
  items <- lapply(seq_len(nrow(ai_notes)), function(i) {
    n <- ai_notes[i, ]
    cname <- courses$short[match(n$course_id, courses$id)]
    nid <- if ("note_id" %in% names(n)) n$note_id else i
    preview <- substr(gsub("\\s+", " ", n$text_content), 1, 80)
    tags$details(class = "mb-1 border rounded",
      tags$summary(class = "ai-note-compact",
        tags$span(class = "badge bg-primary", cname),
        tags$span(class = "fw-bold", n$source),
        tags$span(class = "note-preview", preview),
        actionButton(paste0("del_ainote_", nid), "X", class = "btn-sm btn-outline-danger py-0 px-1")
      ),
      tags$div(class = "ai-formatted p-3", style = "max-height:400px;overflow-y:auto",
        HTML(tryCatch(
          markdown::markdownToHTML(text = substr(n$text_content, 1, 5000), fragment.only = TRUE),
          error = function(e) paste0("<pre>", substr(n$text_content, 1, 5000), "</pre>")))
      )
    )
  })
  tagList(items)
})

# Delete all notes
observeEvent(input$del_all_notes, {
  notes <- mg_notes_all(uid())
  ai_notes <- notes[grepl("IA|RESUMEN|organizado", notes$source, ignore.case = TRUE), ]
  if (nrow(ai_notes) > 0) {
    for (i in seq_len(nrow(ai_notes))) {
      nid <- if ("note_id" %in% names(ai_notes)) ai_notes$note_id[i] else i
      tryCatch(mg_note_delete(uid(), nid), error = function(e) NULL)
    }
  }
  rv$refresh <- rv$refresh + 1
  showNotification("Material IA eliminado", type = "warning")
})

# Study guide content (from study_guides.R)
output$study_guide_content <- renderUI({
  ex <- selected_exam()
  if (is.null(ex)) return(NULL)
  guide <- get_study_guide(ex$course_id, ex$code)
  if (is.null(guide)) return(tags$p(class = "text-muted small", "Sin guia disponible para este examen."))
  tags$div(class = "ai-formatted", style = "max-height:600px;overflow-y:auto;",
    HTML(markdown::markdownToHTML(text = guide, fragment.only = TRUE)))
})
