# ============ ui_analytics.R — Analytics: Deuda Academica ============

ui_analytics <- function() {
  nav_panel(
    title = "Analytics",

    tags$h4(class = "fw-bold mb-3", "Estado de Preparacion por Examen"),
    tags$p(class = "text-muted small",
      "Compara los temas vinculados a cada examen con los bloques de estudio ",
      "completados en el calendario. Si hay deficit, se activa una Alerta de Deuda Academica."),

    # ---- Preparation gauges (one per upcoming exam) ----
    uiOutput("analytics_prep_gauges"),

    tags$hr(),

    # ---- Deuda overview ----
    tags$h5(class = "fw-bold mb-2", "Resumen de Deuda Academica"),
    uiOutput("analytics_debt_summary"),

    # ---- Recommendation ----
    tags$h5(class = "fw-bold mb-2 mt-3", "Recomendacion IA"),
    uiOutput("analytics_recommendation")
  )
}

