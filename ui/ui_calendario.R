# ============ ui_calendario.R — Panel de Calendario y Horario ============
# Incluye: subida de PDF, Google Calendar sync, Smart Scheduler trigger,
#          navegación semanal y visual_calendar.

ui_calendario <- function() {
  nav_panel(
    title = "\U0001F4C5 Calendario",

    # ---- Panel unificado de configuración (Cascada Inteligente) ----
    div(class = "card mb-3",
      div(class = "card-body py-2",

        # Fila 1: Subir PDF de horario
        div(class = "d-flex flex-wrap gap-2 align-items-center mb-2",
          tags$small(class = "fw-bold", "\U0001F4CB Horario Base:"),
          div(style = "flex:1;max-width:300px",
            fileInput("schedule_file", NULL,
                      accept = c(".pdf", ".txt", ".docx"),
                      placeholder = "Subir consolidado PDF", width = "100%")
          ),
          actionButton("schedule_extract_btn", "\U0001F916 Extraer con IA",
                       class = "btn-sm btn-success"),
          actionButton("schedule_clear_btn", "\U0001F5D1",
                       class = "btn-sm btn-outline-danger btn-sm",
                       title = "Limpiar horario base")
        ),
        div(id = "schedule_status_div"),
        tags$hr(class = "my-2"),

        # Fila 2: Google Calendar + Smart Scheduler
        div(class = "d-flex flex-wrap gap-2 align-items-center",
          tags$small(class = "fw-bold text-muted", "\U0001F4C5 Google:"),
          div(style = "flex:1;max-width:250px",
            textInput("gcal_email", NULL, value = "",
                      placeholder = "tu.email@gmail.com", width = "100%")
          ),
          actionButton("gcal_sync", "\U0001F504 Sync",
                       class = "btn-sm btn-outline-primary",
                       title = "Sincronizar Google Calendar"),
          tags$span(class = "text-muted mx-1", "|"),
          div(style = "width:80px",
            textInput("sleep_start", NULL, value = "23:00",
                      placeholder = "Dormir", width = "100%")
          ),
          div(style = "width:80px",
            textInput("sleep_end", NULL, value = "07:00",
                      placeholder = "Despertar", width = "100%")
          ),
          actionButton("btn_gen_schedule", "\u2728 Generar Horario Inteligente",
                       class = "btn-sm btn-primary",
                       style = "background:linear-gradient(135deg,#6366f1,#8b5cf6);border:none;font-weight:600;"),
          downloadButton("download_ics", "\U0001F4C5 .ics",
                         class = "btn-sm btn-outline-success")
        ),
        div(id = "gcal_status_div"),
        div(id = "smart_sched_status"),
        tags$p(class = "text-muted small mb-0 mt-1",
          "Flujo: Sube PDF \u2192 Sincroniza Google \u2192 \u2728 La IA fusiona todo y llena huecos con bloques de estudio.")
      )
    ),

    # ---- Grilla de horario de clases (PDF extraído) ----
    uiOutput("schedule_grid"),

    # ---- Navegación semanal ----
    div(class = "d-flex justify-content-between align-items-center mb-2",
      actionButton("cal_prev", "\u25C0", class = "btn-sm btn-outline-primary"),
      div(class = "d-flex align-items-center gap-2",
        actionButton("cal_today", "Hoy", class = "btn-sm btn-primary",
                     style = "font-size:0.78rem;padding:3px 12px;"),
        uiOutput("cal_week_label")
      ),
      actionButton("cal_next", "\u25B6", class = "btn-sm btn-outline-primary")
    ),

    # ---- Calendario visual (renderizado server-side) ----
    div(class = "cal-container", uiOutput("visual_calendar"))
  )
}
