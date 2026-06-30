# ============ ui_dashboard.R — Dashboard: KPIs + Actividades ============

ui_dashboard <- function() {
  nav_panel(
    title = "\U0001F4CA Inicio",
    # 4.3: Resumen compacto "Esta Semana"
    uiOutput("esta_semana_card"),
    tags$h5(class = "fw-bold mb-2 mt-2", "\U0001F4CD Progreso del Ciclo"),
    uiOutput("week_timeline"),
    # KPI cards (responsive flex grid)
    div(class = "d-flex flex-wrap gap-3 mb-3", style = "justify-content:center;",
      tags$div(class = "kpi-card", style = "background:linear-gradient(135deg,#059669,#10b981);",
        tags$div(class = "kpi-label", "\u2705 Progreso"),
        tags$div(class = "kpi-value", textOutput("stat_pct", inline = TRUE))),
      tags$div(class = "kpi-card", style = "background:linear-gradient(135deg,#2563eb,#3b82f6);",
        tags$div(class = "kpi-label", "\u23F3 Pendientes"),
        tags$div(class = "kpi-value", textOutput("stat_pending", inline = TRUE))),
      tags$div(class = "kpi-card", style = "background:linear-gradient(135deg,#dc2626,#ef4444);",
        tags$div(class = "kpi-label", "\u26A0\uFE0F Atrasadas"),
        tags$div(class = "kpi-value", textOutput("stat_overdue", inline = TRUE))),
      tags$div(class = "kpi-card", style = "background:linear-gradient(135deg,#d97706,#f59e0b);",
        tags$div(class = "kpi-label", "\u2B50 Alta Prior."),
        tags$div(class = "kpi-value", textOutput("stat_high", inline = TRUE))),
      tags$div(class = "kpi-card", style = "background:linear-gradient(135deg,#0891b2,#06b6d4);",
        tags$div(class = "kpi-label", "\U0001F4C5 Semanas"),
        tags$div(class = "kpi-value", textOutput("stat_weeks", inline = TRUE))),
      tags$div(class = "kpi-card", style = "background:linear-gradient(135deg,#6366f1,#8b5cf6);",
        tags$div(class = "kpi-label", "\U0001F393 Promedio"),
        tags$div(class = "kpi-value", textOutput("stat_avg", inline = TRUE)))
    ),
    tags$h5(class = "fw-bold mb-2 mt-3", "\U0001F4CB Próximas Actividades"),
    div(style = "overflow-x:auto; -webkit-overflow-scrolling:touch;",
      DTOutput("upcoming_table")
    )
  )
}
