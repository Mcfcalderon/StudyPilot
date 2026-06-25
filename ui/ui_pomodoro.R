# ============ ui_pomodoro.R — Timer, Música, Evaluaciones, Material IA ============

ui_pomodoro <- function() {
  nav_panel(
    title = "\U0001F4D6 Estudio",

    # ---- Timer + Controls ----
    div(class = "text-center py-2",
      div(class = "d-flex justify-content-center align-items-center flex-wrap gap-2 mb-1",
        selectInput("pomo_course", "Curso:",
          choices = setNames(courses$id, courses$short), width = "140px"),
        numericInput("pomo_duration", "Min:", value = 25, min = 1, max = 120, step = 1, width = "70px"),
        div(class = "d-flex gap-1 align-items-end",
          actionButton("music_white", "\U0001F30A", class = "btn-sm btn-outline-secondary", title = "Ruido Blanco"),
          actionButton("music_brown", "\U0001F327", class = "btn-sm btn-outline-secondary", title = "Ruido Marrón"),
          actionButton("music_pink", "\U0001F338", class = "btn-sm btn-outline-secondary", title = "Ruido Rosa"),
          actionButton("music_stop", "\u23F9", class = "btn-sm btn-outline-danger", title = "Parar música")
        ),
        tags$input(type = "range", id = "music-volume", min = 0, max = 100, value = 30,
                   style = "width:80px", title = "Volumen")
      ),
      div(id = "pomo-mode-label", class = "text-muted small", "\U0001F3AF Tiempo de Estudio"),
      div(id = "pomo-timer", class = "pomo-timer-display", "25:00"),
      div(class = "d-flex justify-content-center gap-2",
        actionButton("pomo_toggle", "\u25B6 Iniciar", class = "btn-primary"),
        actionButton("pomo_reset", "\U0001F504", class = "btn-outline-secondary", title = "Reset"),
        actionButton("pomo_skip", "\u23ED", class = "btn-outline-secondary", title = "Saltar")
      ),
      div(id = "pomo-dots", class = "mt-2"),
      div(class = "d-flex justify-content-center gap-3 mt-1 text-muted small",
        tags$span("Sesiones: ", tags$b(id = "pomo-sessions", "0")),
        tags$span("Total: ", tags$b(id = "pomo-total", "0 min")),
        actionButton("pomo_undo", "\u21A9", class = "btn-sm btn-outline-danger py-0", title = "Deshacer sesión")
      ),
      div(class = "mt-1 small text-muted",
        tags$a("\u2615 Lofi", href = "https://www.youtube.com/watch?v=jfKfPfyJRdk", target = "_blank"), " \u00b7 ",
        tags$a("\U0001F3B9 Synth", href = "https://www.youtube.com/watch?v=4xDzrJKXOOY", target = "_blank"), " \u00b7 ",
        tags$a("\U0001F327 Jazz", href = "https://www.youtube.com/watch?v=jTnGXTIBLKk", target = "_blank"), " \u00b7 ",
        tags$a("\U0001F4D6 Focus", href = "https://www.youtube.com/watch?v=TURbeWK2wwg", target = "_blank")
      )
    ),

    # ---- Countdown de evaluaciones importantes ----
    tags$h5(class = "fw-bold mb-2 mt-3", "\u23F3 Evaluaciones Importantes Pendientes"),
    uiOutput("countdown_cards"),

    # ---- Preparación de Examen (integrado) ----
    tags$hr(),
    tags$h5(class = "fw-bold mb-2", "\U0001F3AF Preparación de Examen"),
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
      card(
        card_header("\U0001F3AF Seleccionar Examen"),
        card_body(
          selectInput("exam_select", "Examen próximo:", choices = NULL, width = "100%"),
          uiOutput("exam_info"),
          actionButton("exam_pomo", "\U0001F345 Estudiar con Pomodoro", class = "btn-primary mt-2 w-100")
        )
      ),
      card(
        card_header("\U0001F4D6 Checklist de Temas"),
        card_body(
          uiOutput("exam_topics_checklist"),
          div(class = "mt-2", uiOutput("exam_progress_bar"))
        )
      )
    ),

    # ---- Guía de Estudio ----
    tags$h5(class = "fw-bold mb-2 mt-3", "\U0001F4DA Guía de Estudio"),
    uiOutput("study_guide_content"),

    # ---- Material IA ----
    tags$hr(),
    tags$h5(class = "fw-bold mb-2", "\U0001F916 Material de Estudio con IA"),
    tags$p(class = "text-muted small", "Sube un archivo y la IA genera resúmenes, conceptos clave, mapas y preguntas."),
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(5, 7)),
      div(
        div(class = "d-flex flex-wrap gap-2 align-items-end mt-1",
          div(style = "min-width:120px",
            selectInput("ai_upload_course", NULL, choices = setNames(courses$id, courses$short), width = "100%")),
          div(style = "flex:1;min-width:200px",
            fileInput("ai_study_file", NULL, accept = c(".pdf",".docx",".xlsx",".txt",".csv"), width = "100%"))
        ),
        verbatimTextOutput("ai_upload_status"),
        div(class = "d-flex gap-2 mt-1",
          actionButton("ai_gen_summary", "\U0001F916 Resumen", class = "btn-sm btn-primary"),
          actionButton("ai_gen_questions2", "\u2753 Preguntas", class = "btn-sm btn-outline-primary")
        )
      ),
      div(uiOutput("ai_result_content"))
    ),
    div(class = "d-flex justify-content-between align-items-center mt-3 mb-1",
      tags$h5(class = "fw-bold mb-0", "\U0001F4DA Material Procesado"),
      actionButton("del_all_notes", "\U0001F5D1 Borrar todos", class = "btn-sm btn-outline-danger")
    ),
    uiOutput("ai_processed_notes")
  )
}
