# ============ ui_examen.R — Examenes Practica IA + Material IA ============

ui_examen <- function() {
  nav_panel(
    title = "Examenes",

    # ---- Quiz IA (Practice Exam) ----
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(3, 9)),
      div(
        tags$b(class = "small", "Configuracion"),
        selectInput("quiz_course", "Curso:",
          choices = setNames(courses$id, courses$short),
          selected = if (nrow(courses) > 0) courses$id[1] else NULL, width = "100%"),
        sliderInput("quiz_n", "Preguntas:", min = 3, max = 20, value = 8, step = 1),
        selectInput("quiz_type", "Tipo:",
          choices = c("Todas" = "all", "Opcion multiple" = "mc", "Abiertas" = "open")),
        actionButton("quiz_generate", "Generar", class = "btn-sm btn-primary w-100 mt-1"),
        actionButton("quiz_submit", "Calificar", class = "btn-sm btn-success w-100 mt-1", disabled = TRUE),
        uiOutput("quiz_score_summary")
      ),
      div(
        tags$b(class = "small", "Examen de Practica"),
        uiOutput("quiz_content")
      )
    ),
    tags$h5(class = "fw-bold mb-2 mt-3", "Resultados Detallados"),
    uiOutput("quiz_results"),

    # ---- Material IA ----
    tags$hr(),
    tags$h5(class = "fw-bold mb-2", "Material de Estudio con IA"),
    tags$p(class = "text-muted small",
      "Sube un archivo y la IA genera resumenes, conceptos clave, mapas y preguntas."),
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(5, 7)),
      div(
        div(class = "d-flex flex-wrap gap-2 align-items-end mt-1",
          div(style = "min-width:120px",
            selectInput("ai_upload_course", NULL,
              choices = setNames(courses$id, courses$short), width = "100%")),
          div(style = "flex:1;min-width:200px",
            fileInput("ai_study_file", NULL,
              accept = c(".pdf", ".docx", ".xlsx", ".txt", ".csv"), width = "100%"))
        ),
        verbatimTextOutput("ai_upload_status"),
        div(class = "d-flex gap-2 mt-1",
          actionButton("ai_gen_summary", "Resumen", class = "btn-sm btn-primary"),
          actionButton("ai_gen_questions2", "Preguntas", class = "btn-sm btn-outline-primary")
        )
      ),
      div(uiOutput("ai_result_content"))
    ),
    div(class = "d-flex justify-content-between align-items-center mt-3 mb-1",
      tags$h5(class = "fw-bold mb-0", "Material Procesado"),
      actionButton("del_all_notes", "Borrar todos", class = "btn-sm btn-outline-danger")
    ),
    uiOutput("ai_processed_notes")
  )
}

