# ============ ui_notas.R — Panel de Notas / Calificaciones ============

ui_notas <- function() {
  nav_panel(
    title = "Notas",

    div(class = "text-center py-3 mb-3",
      tags$span(class = "text-muted", "Promedio Ponderado Estimado"),
      textOutput("overall_avg") |> tags$h1(class = "text-success fw-bold mb-0"),
      uiOutput("overall_credits_label"),
      div(class = "mt-2",
        downloadButton("download_grades_pdf", "Exportar reporte (PDF)", class = "btn-sm btn-outline-primary")
      )
    ),

    uiOutput("grades_panels")
  )
}

