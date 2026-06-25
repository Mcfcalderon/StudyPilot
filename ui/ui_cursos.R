# ============ ui_cursos.R — Panel de Cursos + Silabos ============

ui_cursos <- function() {
  nav_panel(
    title = "Cursos",

    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(8, 4)),

      # ---- Course cards (rendered server-side) ----
      div(uiOutput("course_cards")),

      # ---- Add course form ----
      div(
        tags$b("Agregar Curso"),
        textInput("new_course_id", "Codigo:", placeholder = "IN5001", width = "100%"),
        textInput("new_course_name", "Nombre:", placeholder = "Simulacion de Procesos", width = "100%"),
        textInput("new_course_short", "Corto:", placeholder = "Simulacion", width = "100%"),
        div(class = "d-flex gap-2",
          div(style = "flex:1", numericInput("new_course_credits", "Cred:", value = 4, min = 1, max = 8, width = "100%")),
          div(style = "flex:1", selectInput("new_course_day", "Dia eval:",
            choices = c("Lun" = 1, "Mar" = 2, "Mie" = 3, "Jue" = 4, "Vie" = 5, "Sab" = 6), width = "100%"))
        ),
        textInput("new_course_prof", "Profesor:", placeholder = "Apellido, Nombre", width = "100%"),
        selectInput("new_course_color", "Color:", width = "100%",
          choices = c("Azul" = "#2563eb", "Verde" = "#16a34a", "Morado" = "#7c3aed",
                      "Naranja" = "#ea580c", "Rosa" = "#db2777", "Cyan" = "#0891b2")),
        actionButton("add_course_btn", "Agregar", class = "btn-sm btn-primary w-100 mt-1")
      )
    ),

    # ---- Delete course ----
    div(class = "d-flex flex-wrap gap-2 align-items-end mt-2",
      div(style = "min-width:200px",
        selectInput("del_course_id", "Eliminar curso:", choices = NULL, width = "100%")
      ),
      div(actionButton("del_course_btn", "Eliminar", class = "btn-sm btn-outline-danger"))
    ),

    # ---- Crear Curso desde Silabo (IA) ----
    tags$h5(class = "fw-bold mt-3", "Crear Curso desde Silabo (IA)"),
    tags$p(class = "text-muted small",
      "Sube un silabo PDF y la IA extraera automaticamente el curso, evaluaciones, pesos y temas."),
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(5, 7)),
      div(
        fileInput("syllabus_file", "Subir silabos (PDF, Word, TXT — multiples):",
                  accept = c(".pdf", ".docx", ".txt"), multiple = TRUE, width = "100%"),
        div(class = "d-flex gap-2 flex-wrap",
          actionButton("syllabus_extract_btn", "Extraer Curso con IA", class = "btn-sm btn-primary"),
          actionButton("syllabus_upload_btn", "Solo Subir", class = "btn-sm btn-outline-secondary"),
          selectInput("syllabus_course", NULL, choices = setNames(courses$id, courses$short), width = "140px")
        ),
        div(id = "syllabus_status_div"),
        verbatimTextOutput("syllabus_status")
      ),
      div(
        uiOutput("syllabus_preview"),
        shinyjs::hidden(div(id = "syllabus_confirm_div", class = "d-flex gap-2 mt-2 mb-3",
          actionButton("syllabus_confirm", "Confirmar y Guardar Curso", class = "btn btn-success"),
          actionButton("syllabus_cancel", "Cancelar", class = "btn btn-outline-danger")
        )),
        tags$hr(),
        tags$b(class = "small", "Silabos subidos:"),
        uiOutput("syllabi_list")
      )
    )
  )
}

