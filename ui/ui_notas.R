# ============ ui_notas.R — Panel de Notas / Calificaciones ============

ui_notas <- function() {
  nav_panel(
    title = "Notas",

    div(class = "text-center py-3 mb-3",
      tags$span(class = "text-muted", "Promedio Ponderado Estimado"),
      textOutput("overall_avg") |> tags$h1(class = "text-success fw-bold mb-0"),
      uiOutput("overall_credits_label"),
      div(class = "mt-2 d-flex justify-content-center align-items-center gap-2 flex-wrap",
        downloadButton("download_grades_pdf", "Exportar reporte (PDF)", class = "btn-sm btn-outline-primary"),
        tags$small(class = "text-muted ms-2", "Umbral aprobacion:"),
        selectInput("pass_grade_sel", NULL, width = "120px",
          choices = c("13 (UTEC)" = 13, "11" = 11, "10.5" = 10.5), selected = 13)
      )
    ),

    uiOutput("grades_panels")
  )
}

