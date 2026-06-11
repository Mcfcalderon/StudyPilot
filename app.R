source("global.R")
source("db_mongo.R")
source("ai_functions.R")
source("exam_bank.R")
source("study_guides.R")
source("google_cal.R")
library(shinyjs)
library(sodium)
# Lazy init — just test connection, don't seed
assign(".app_initialized", FALSE, envir = globalenv())
# Reset courses to empty at app startup (prevents stale cache between sessions)
assign("courses", tibble::tibble(id=character(), name=character(), short=character(),
  credits=integer(), professor=character(), formula=character(),
  eval_day=integer(), color=character()), envir = globalenv())

ensure_init <- function() {
  if (!isTRUE(get0(".app_initialized", envir = globalenv()))) {
    tryCatch({
      message("[StudyPilot] Testing MongoDB connection...")
      test <- mongolite::mongo(collection = "connection_test", url = MONGO_URI)
      cnt <- test$count()
      test$disconnect()
      assign(".app_initialized", TRUE, envir = globalenv())
      message("[StudyPilot] MongoDB connected OK (test count=", cnt, ")")
      # auto_complete_old disabled — users mark activities manually
    }, error = function(e) {
      assign(".app_initialized", FALSE, envir = globalenv())
      message("[StudyPilot] MongoDB connection FAILED: ", e$message)
    })
  }
}

# ============================================================
# UI
# ============================================================
ui <- page_navbar(
  title = tags$span(
    tags$span("🚀", style = "margin-right:6px"),
    "StudyPilot",
    tags$span("", style = "color:#93c5fd;margin-left:6px;font-weight:400")
  ),
  theme = bs_theme(
    version = 5,
    primary = "#2563eb", success = "#16a34a",
    danger = "#dc2626", warning = "#eab308",
    info = "#0891b2", secondary = "#64748b",
    "font-size-base" = "0.88rem",
    "body-bg" = "#f1f5f9",
    "card-border-width" = "0",
    "border-radius" = "0.75rem",
    "nav-link-font-weight" = "500"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(src = "pomodoro.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.min.js"),
    tags$script(HTML("mermaid.initialize({startOnLoad:false, theme:'default', securityLevel:'loose'});")),
  ),
  id = "main_nav",
  bg = "#2563eb",

  # ---- Dashboard ----
  nav_panel(
    title = "📊 Dashboard",
    tags$h5(class = "fw-bold mb-2 mt-2", "📍 Progreso del Ciclo"),
    uiOutput("week_timeline"),
    layout_columns(
      col_widths = breakpoints(sm = c(6, 6, 6, 6, 6, 6), md = c(4, 4, 4, 4, 4, 4), lg = c(2, 2, 2, 2, 2, 2)),
      value_box("✅ Progreso", textOutput("stat_pct"), theme = "success"),
      value_box("⏳ Pendientes", textOutput("stat_pending"), theme = "primary"),
      value_box("⚠️ Atrasadas", textOutput("stat_overdue"), theme = "danger"),
      value_box("⭐ Alta Prior.", textOutput("stat_high"), theme = "warning"),
      value_box("📅 Semanas", textOutput("stat_weeks"), theme = "info"),
      value_box("🎓 Promedio", textOutput("stat_avg"), theme = "secondary")
    ),
    tags$h5(class = "fw-bold mb-2 mt-3", "📋 Próximas Actividades"),
    div(style = "overflow-x:auto; -webkit-overflow-scrolling:touch;",
      DTOutput("upcoming_table")
    )
  ),

  # ---- Pomodoro ----
  nav_panel(
    title = "🍅 Pomodoro",
    # Timer + controls in one compact row
    div(class = "text-center py-2",
      div(class = "d-flex justify-content-center align-items-center flex-wrap gap-2 mb-1",
        selectInput("pomo_course", "Curso:", choices = setNames(courses$id, courses$short), width = "140px", selected = "HH5101"),
        numericInput("pomo_duration", "Min:", value = 25, min = 1, max = 120, step = 1, width = "70px"),
        div(class = "d-flex gap-1 align-items-end",
          actionButton("music_white", "🌊", class = "btn-sm btn-outline-secondary", title = "Ruido Blanco"),
          actionButton("music_brown", "🌧", class = "btn-sm btn-outline-secondary", title = "Ruido Marrón"),
          actionButton("music_pink", "🌸", class = "btn-sm btn-outline-secondary", title = "Ruido Rosa"),
          actionButton("music_stop", "⏹", class = "btn-sm btn-outline-danger", title = "Parar música")
        ),
        tags$input(type = "range", id = "music-volume", min = 0, max = 100, value = 30, style = "width:80px", title = "Volumen")
      ),
      div(id = "pomo-mode-label", class = "text-muted small", "🎯 Tiempo de Estudio"),
      div(id = "pomo-timer", class = "pomo-timer-display", "25:00"),
      div(class = "d-flex justify-content-center gap-2",
        actionButton("pomo_toggle", "▶ Iniciar", class = "btn-primary"),
        actionButton("pomo_reset", "🔄", class = "btn-outline-secondary", title = "Reset"),
        actionButton("pomo_skip", "⏭", class = "btn-outline-secondary", title = "Saltar")
      ),
      div(id = "pomo-dots", class = "mt-2"),
      div(class = "d-flex justify-content-center gap-3 mt-1 text-muted small",
        tags$span("Sesiones: ", tags$b(id = "pomo-sessions", "0")),
        tags$span("Total: ", tags$b(id = "pomo-total", "0 min")),
        actionButton("pomo_undo", "↩", class = "btn-sm btn-outline-danger py-0", title = "Deshacer sesión")
      ),
      div(class = "mt-1 small text-muted",
        tags$a("☕ Lofi", href = "https://www.youtube.com/watch?v=jfKfPfyJRdk", target = "_blank"), " · ",
        tags$a("🎹 Synth", href = "https://www.youtube.com/watch?v=4xDzrJKXOOY", target = "_blank"), " · ",
        tags$a("🌧 Jazz", href = "https://www.youtube.com/watch?v=jTnGXTIBLKk", target = "_blank"), " · ",
        tags$a("📖 Focus", href = "https://www.youtube.com/watch?v=TURbeWK2wwg", target = "_blank")
      )
    ),
    tags$h5(class = "fw-bold mb-2 mt-3", "⏳ Evaluaciones Importantes Pendientes"),
    uiOutput("countdown_cards")
  ),

  # ---- Modo Examen ----
  nav_panel(
    title = "🎯 Modo Examen",
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
      card(
        card_header("🎯 Seleccionar Examen"),
        card_body(
          selectInput("exam_select", "Examen próximo:", choices = NULL, width = "100%"),
          uiOutput("exam_info"),
          actionButton("exam_pomo", "🍅 Estudiar con Pomodoro", class = "btn-primary mt-2 w-100")
        )
      ),
      card(
        card_header("📖 Checklist de Temas"),
        card_body(
          uiOutput("exam_topics_checklist"),
          div(class = "mt-2", uiOutput("exam_progress_bar"))
        )
      )
    ),
    tags$h5(class = "fw-bold mb-2 mt-3", "📚 Guía de Estudio"),
    uiOutput("study_guide_content"),
    tags$div(class = "alert alert-info py-2 small mt-2",
      "📎 Para subir material y generar resúmenes con IA, usa la pestaña ", tags$b("🤖 IA"), "."
    )
  ),

  # ---- IA ----
  nav_panel(
    title = "🤖 IA",
    tags$h4(class = "fw-bold mt-2", "🤖 Asistente de Estudio con IA"),
    tags$p(class = "text-muted small mb-3", "Sube un archivo y la IA genera resúmenes, conceptos, mapas y preguntas automáticamente."),
    div(class = "d-flex flex-wrap gap-3 mb-3 justify-content-center",
      div(class = "text-center", style = "min-width:80px", tags$h4(class = "mb-0", "📝"), tags$small(class = "fw-bold", "Resumen")),
      div(class = "text-center", style = "min-width:80px", tags$h4(class = "mb-0", "🔑"), tags$small(class = "fw-bold", "Conceptos")),
      div(class = "text-center", style = "min-width:80px", tags$h4(class = "mb-0", "🗺"), tags$small(class = "fw-bold", "Mapa")),
      div(class = "text-center", style = "min-width:80px", tags$h4(class = "mb-0", "❓"), tags$small(class = "fw-bold", "Preguntas"))
    ),
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(5, 7)),
      div(
        tags$b(class = "small", "📎 Subir Material"),
        div(class = "d-flex flex-wrap gap-2 align-items-end mt-1",
          div(style = "min-width:120px", selectInput("ai_upload_course", NULL, choices = setNames(courses$id, courses$short), width = "100%")),
          div(style = "flex:1;min-width:200px", fileInput("ai_study_file", NULL, accept = c(".pdf",".docx",".xlsx",".txt",".csv"), width = "100%"))
        ),
        verbatimTextOutput("ai_upload_status"),
        div(class = "d-flex gap-2 mt-1",
          actionButton("ai_gen_summary", "🤖 Resumen", class = "btn-sm btn-primary"),
          actionButton("ai_gen_questions2", "❓ Preguntas", class = "btn-sm btn-outline-primary")
        )
      ),
      div(
        tags$b(class = "small", "📄 Resultado"),
        uiOutput("ai_result_content"),
        tags$p(class = "text-muted small mt-1", "Sube un archivo y genera resumen para ver resultados.")
      )
    ),
    div(class = "d-flex justify-content-between align-items-center mt-3 mb-1",
      tags$h5(class = "fw-bold mb-0", "📚 Material Procesado con IA"),
      actionButton("del_all_notes", "🗑 Borrar todos", class = "btn-sm btn-outline-danger")
    ),
    uiOutput("ai_processed_notes"),
  ),

  # ---- Examen de Práctica ----
  nav_panel(
    title = "📝 Examen Práctica",
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(3, 9)),
      div(
        tags$b(class = "small", "⚙ Configuración"),
        selectInput("quiz_course", "Curso:", choices = setNames(courses$id, courses$short),
                   selected = "HH5101", width = "100%"),
        sliderInput("quiz_n", "Preguntas:", min = 3, max = 20, value = 8, step = 1),
        selectInput("quiz_type", "Tipo:", choices = c("Todas" = "all", "Opción múltiple" = "mc", "Abiertas" = "open")),
        actionButton("quiz_generate", "🎲 Generar", class = "btn-sm btn-primary w-100 mt-1"),
        actionButton("quiz_submit", "✅ Calificar", class = "btn-sm btn-success w-100 mt-1", disabled = TRUE),
        uiOutput("quiz_score_summary")
      ),
      div(
        tags$b(class = "small", "📝 Examen de Práctica"),
        uiOutput("quiz_content")
      )
    ),
    tags$h5(class = "fw-bold mb-2 mt-3", "📊 Resultados Detallados"),
    uiOutput("quiz_results")
  ),

  # ---- Actividades ----
  nav_panel(
    title = "📋 Actividades",
    layout_columns(
      col_widths = breakpoints(sm = c(6, 6, 6, 6), lg = c(3, 3, 3, 3)),
      selectInput("act_filter_course", "Curso:", choices = c("Todos" = "all", setNames(courses$id, courses$short))),
      selectInput("act_filter_type", "Tipo:", choices = c("Todos" = "all", "Examen" = "examen", "Proyecto" = "proyecto", "EC" = "ec", "Quiz/PC" = "quiz")),
      selectInput("act_filter_status", "Estado:", choices = c("Pendientes" = "pending", "Todas" = "all", "Completadas" = "done")),
      selectInput("act_filter_pri", "Prioridad:", choices = c("Todas" = "all", "🔴 Alta ≥20%" = "high", "🟡 Media" = "medium", "🟢 Baja" = "low"))
    ),
    div(class = "d-flex flex-wrap gap-2 mb-3",
      actionButton("act_new", "+ Nueva Actividad", class = "btn-primary"),
      actionButton("act_mark_done", "✅ Marcar hechas", class = "btn-outline-success btn-sm"),
      actionButton("act_edit", "✏️ Editar", class = "btn-outline-secondary btn-sm"),
      actionButton("act_delete", "🗑 Eliminar", class = "btn-outline-danger btn-sm")
    ),
    DTOutput("activities_table")
  ),

  # ---- Notas ----
  nav_panel(
    title = "🎓 Notas",
    div(class = "text-center py-3 mb-3",
      tags$span(class = "text-muted", "Promedio Ponderado Estimado"),
      textOutput("overall_avg") |> tags$h1(class = "text-success fw-bold mb-0"),
      tags$span(class = "small text-muted", paste0("Basado en ", sum(courses$credits), " créditos"))
    ),
    uiOutput("grades_panels")
  ),

  # ---- Semanal ----
  nav_panel(
    title = "📅 Semanal",
    div(class = "d-flex align-items-center gap-3 mb-3",
      actionButton("week_prev", "◀", class = "btn-sm btn-outline-primary"),
      uiOutput("week_title"),
      actionButton("week_next", "▶", class = "btn-sm btn-outline-primary")
    ),
    uiOutput("week_view")
  ),

  # ---- Horario ----
  nav_panel(
    title = "🕐 Horario",
    # Schedule from PDF with AI
    div(class = "card mb-3",
      div(class = "card-body py-2",
        div(class = "d-flex flex-wrap gap-2 align-items-center",
          tags$small(class = "fw-bold", "📋 Generar horario:"),
          div(style = "flex:1;max-width:300px",
            fileInput("schedule_file", NULL, accept = c(".pdf", ".txt", ".docx"),
                      placeholder = "Subir PDF de horario", width = "100%")
          ),
          actionButton("schedule_extract_btn", "🤖 Extraer con IA", class = "btn-sm btn-success"),
          actionButton("schedule_clear_btn", "🗑 Limpiar", class = "btn-sm btn-outline-danger")
        ),
        div(id = "schedule_status_div")
      )
    ),
    # Google Calendar sync
    div(class = "d-flex flex-wrap gap-2 align-items-center mb-2 mt-1 px-1",
      tags$small(class = "fw-bold text-muted", "📅 Calendar:"),
      div(style = "flex:1;max-width:350px",
        textInput("gcal_email", NULL, value = "", placeholder = "tu.email@gmail.com", width = "100%")
      ),
      actionButton("gcal_sync", "🔄", class = "btn-sm btn-primary", title = "Sincronizar"),
      actionButton("gcal_clear", "🗑", class = "btn-sm btn-outline-secondary", title = "Limpiar"),
      downloadButton("download_ics", "📅 Descargar .ics", class = "btn-sm btn-outline-success")
    ),
    div(id = "gcal_status_div"),
    # Schedule grid (from AI or manual)
    uiOutput("schedule_grid"),
    div(class = "d-flex justify-content-between align-items-center mb-2",
      actionButton("cal_prev", "◀", class = "btn-sm btn-outline-primary"),
      div(class = "d-flex align-items-center gap-2",
        actionButton("cal_today", "Hoy", class = "btn-sm btn-primary", style = "font-size:0.78rem;padding:3px 12px;"),
        uiOutput("cal_week_label")
      ),
      actionButton("cal_next", "▶", class = "btn-sm btn-outline-primary")
    ),
    div(class = "cal-container", uiOutput("visual_calendar"))
  ),

  # ---- Cursos ----
  nav_panel(
    title = "📚 Cursos",
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(8, 4)),
      div(uiOutput("course_cards")),
      div(
        tags$b("➕ Agregar Curso"),
        textInput("new_course_id", "Código:", placeholder = "IN5001", width = "100%"),
        textInput("new_course_name", "Nombre:", placeholder = "Simulación de Procesos", width = "100%"),
        textInput("new_course_short", "Corto:", placeholder = "Simulación", width = "100%"),
        div(class = "d-flex gap-2",
          div(style = "flex:1", numericInput("new_course_credits", "Créd:", value = 4, min = 1, max = 8, width = "100%")),
          div(style = "flex:1", selectInput("new_course_day", "Día eval:", choices = c("Lun"=1,"Mar"=2,"Mié"=3,"Jue"=4,"Vie"=5,"Sáb"=6), width = "100%"))
        ),
        textInput("new_course_prof", "Profesor:", placeholder = "Apellido, Nombre", width = "100%"),
        selectInput("new_course_color", "Color:", width = "100%",
          choices = c("Azul"="#2563eb","Verde"="#16a34a","Morado"="#7c3aed","Naranja"="#ea580c","Rosa"="#db2777","Cyan"="#0891b2")),
        actionButton("add_course_btn", "➕ Agregar", class = "btn-sm btn-primary w-100 mt-1")
      )
    ),
    div(class = "d-flex flex-wrap gap-2 align-items-end mt-2",
      div(style = "min-width:200px",
        selectInput("del_course_id", "Eliminar curso:", choices = NULL, width = "100%")
      ),
      div(actionButton("del_course_btn", "🗑 Eliminar", class = "btn-sm btn-outline-danger"))
    ),
    # Crear Curso desde Sílabo
    tags$h5(class = "fw-bold mt-3", "📄 Crear Curso desde Sílabo (IA)"),
    tags$p(class = "text-muted small", "Sube un sílabo PDF y la IA extraerá automáticamente el curso, evaluaciones, pesos y temas."),
    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(5, 7)),
      div(
        fileInput("syllabus_file", "Subir sílabo (PDF, Word, TXT):",
                  accept = c(".pdf", ".docx", ".txt"), width = "100%"),
        div(class = "d-flex gap-2 flex-wrap",
          actionButton("syllabus_extract_btn", "🤖 Extraer Curso con IA", class = "btn-sm btn-primary"),
          actionButton("syllabus_upload_btn", "📄 Solo Subir", class = "btn-sm btn-outline-secondary"),
          selectInput("syllabus_course", NULL, choices = setNames(courses$id, courses$short), width = "140px")
        ),
        div(id = "syllabus_status_div"),
        verbatimTextOutput("syllabus_status")
      ),
      div(
        # Preview of extracted course
        uiOutput("syllabus_preview"),
        # Confirm/Cancel buttons (hidden until preview is ready)
        shinyjs::hidden(div(id = "syllabus_confirm_div", class = "d-flex gap-2 mt-2 mb-3",
          actionButton("syllabus_confirm", "✅ Confirmar y Guardar Curso", class = "btn btn-success"),
          actionButton("syllabus_cancel", "❌ Cancelar", class = "btn btn-outline-danger")
        )),
        tags$hr(),
        tags$b(class = "small", "Sílabos subidos:"),
        uiOutput("syllabi_list")
      )
    ),
  ),

  nav_spacer(),

  # Logout button in navbar
  nav_item(
    actionLink("logout_btn", label = tags$span(
      tags$i(class = "bi bi-box-arrow-right", style = "margin-right:4px"),
      "Cerrar sesión"
    ), style = "color:white;opacity:0.9;font-size:0.85rem;text-decoration:none;cursor:pointer;")
  ),
  # Floating chat widget
  nav_item(
    tags$div(id = "chat-float-btn", class = "chat-float-btn", onclick = "toggleChat()",
      "💬"
    ),
    tags$div(id = "chat-float-panel", class = "chat-float-panel", style = "display:none",
      tags$div(class = "chat-float-header",
        tags$span(class = "fw-bold", "💬 Asistente IA"),
        tags$span(class = "chat-float-close", onclick = "toggleChat()", "✕")
      ),
      uiOutput("chat_display"),
      tags$div(class = "chat-float-input",
        tags$div(class = "d-flex gap-1",
          tags$div(style = "flex:1", textInput("chat_input", NULL, placeholder = "Pregunta algo...", width = "100%")),
          actionButton("chat_send", "📤", class = "btn-sm btn-primary")
        )
      )
    ),
    tags$script(HTML("function toggleChat(){var p=document.getElementById('chat-float-panel');p.style.display=p.style.display==='none'?'flex':'none';}"))
  ),
  nav_item(
    tags$span(class = "navbar-text text-white",
      tags$span(class = "d-none d-md-inline", style = "font-size:0.82rem;opacity:0.9",
        paste0("📅 ", format(Sys.Date(), "%A %d %b %Y"), "  ·  📍 Semana ", current_week(), "/", TOTAL_WEEKS)
      )
    )
  )
)

# Custom auth wrapper (replaces shinymanager)
app_ui <- ui
ui <- fluidPage(
  useShinyjs(),
  # PWA meta tags for installability
  tags$head(
    tags$link(rel = "manifest", href = "pwa-manifest.json"),
    tags$meta(name = "theme-color", content = "#1e293b"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
    tags$meta(name = "apple-mobile-web-app-title", content = "StudyPilot"),
    tags$link(rel = "apple-touch-icon", href = "icon-512.svg"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "icon-512.svg"),
    # Keepalive heartbeat + auto-reconnect (all devices)
    tags$script(HTML("
      // Ping every 50 seconds to prevent idle timeout
      setInterval(function() {
        if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket) {
          Shiny.setInputValue('keepalive', Date.now());
        }
      }, 50000);
      // Auto-reload on disconnect (instead of showing gray screen)
      $(document).on('shiny:disconnected', function() {
        setTimeout(function() { location.reload(); }, 3000);
      });
    "))
  ),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"),
    tags$style(HTML("
      #auth_overlay {
        position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        z-index: 9999; display: flex; align-items: center; justify-content: center;
      }
      .auth-card {
        background: white; border-radius: 16px; padding: 40px 36px; width: 400px;
        max-width: 92vw; box-shadow: 0 20px 60px rgba(0,0,0,0.3);
      }
      .auth-card h2 { font-weight: 800; margin-bottom: 4px; }
      .auth-card .subtitle { color: #6b7280; margin-bottom: 24px; }
      .auth-card .form-label { font-weight: 600; font-size: 0.9rem; }
      .auth-card .form-control { border-radius: 8px; padding: 10px 14px; }
      .auth-card .btn-primary {
        width: 100%; padding: 12px; border-radius: 10px; font-weight: 600;
        font-size: 1rem; background: #2563eb; border: none;
      }
      .auth-card .btn-primary:hover { background: #1d4ed8; }
      .auth-toggle { color: #2563eb; cursor: pointer; font-weight: 500; }
      .auth-toggle:hover { text-decoration: underline; }
      .auth-msg { font-size: 0.85rem; margin-top: 8px; padding: 8px 12px; border-radius: 8px; }
      .auth-msg.error { background: #fef2f2; color: #dc2626; border: 1px solid #fecaca; }
      .auth-msg.success { background: #f0fdf4; color: #16a34a; border: 1px solid #bbf7d0; }
    "))
  ),

  # ---- Auth overlay ----
  div(id = "auth_overlay",
    div(class = "auth-card",
      # Login form
      div(id = "login_panel",
        tags$div(style = "text-align:center",
          tags$h2("🚀 StudyPilot"),
          tags$p(class = "subtitle", "Plataforma de estudio inteligente")
        ),
        tags$h5(class = "fw-bold mb-3", "Iniciar sesión"),
        div(class = "mb-3",
          tags$label(class = "form-label", "Usuario:"),
          textInput("login_user", NULL, placeholder = "Tu usuario", width = "100%")
        ),
        div(class = "mb-3",
          tags$label(class = "form-label", "Contraseña:"),
          passwordInput("login_pass", NULL, width = "100%")
        ),
        actionButton("login_btn", "Iniciar sesión", class = "btn btn-primary mt-1"),
        div(id = "login_msg_div"),
        tags$div(class = "text-center mt-3",
          tags$p(class = "small text-muted mb-0", "¿No tienes cuenta?"),
          tags$span(class = "auth-toggle", onclick = "toggleAuthPanel('register')", "Crear cuenta")
        )
      ),

      # Register form (hidden initially via CSS, not shinyjs)
      div(id = "register_panel", style = "display:none;",
        tags$div(style = "text-align:center",
          tags$h2("🚀 StudyPilot"),
          tags$p(class = "subtitle", "Crear nueva cuenta")
        ),
        div(class = "mb-2",
          tags$label(class = "form-label", "Nombre completo:"),
          textInput("reg_name", NULL, placeholder = "Ej: María López", width = "100%")
        ),
        div(class = "mb-2",
          tags$label(class = "form-label", "Usuario:"),
          textInput("reg_user", NULL, placeholder = "Ej: maria.lopez", width = "100%")
        ),
        div(class = "mb-2",
          tags$label(class = "form-label", "Contraseña:"),
          passwordInput("reg_pass", NULL, width = "100%")
        ),
        div(class = "mb-3",
          tags$label(class = "form-label", "Confirmar contraseña:"),
          passwordInput("reg_pass2", NULL, width = "100%")
        ),
        actionButton("register_btn", "Crear cuenta", class = "btn btn-primary mt-1"),
        div(id = "register_msg_div"),
        tags$div(class = "text-center mt-3",
          tags$span(class = "auth-toggle", onclick = "toggleAuthPanel('login')", "← Ya tengo cuenta")
        )
      )
    )
  ),

  # ---- Main app (hidden until login) ----
  shinyjs::hidden(div(id = "main_app", app_ui)),

  # JS to toggle login/register panels + Enter key handlers
  tags$script(HTML("
    function toggleAuthPanel(panel) {
      if (panel === 'register') {
        document.getElementById('login_panel').style.display = 'none';
        document.getElementById('register_panel').style.display = 'block';
      } else {
        document.getElementById('register_panel').style.display = 'none';
        document.getElementById('login_panel').style.display = 'block';
      }
    }

    // Enter key to submit forms
    document.addEventListener('keydown', function(e) {
      if (e.key !== 'Enter') return;
      var el = e.target;
      var id = el.id || '';

      // Login form
      if (id === 'login_user' || id === 'login_pass') {
        e.preventDefault();
        document.getElementById('login_btn').click();
      }
      // Register form
      else if (id === 'reg_name' || id === 'reg_user' || id === 'reg_pass' || id === 'reg_pass2') {
        e.preventDefault();
        document.getElementById('register_btn').click();
      }
      // Chat input
      else if (id === 'chat_input') {
        e.preventDefault();
        document.getElementById('chat_send').click();
      }
    });
  "))
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- Custom Authentication ----
  auth_user <- reactiveVal(NULL)

  # Hardcoded users (always available)
  hardcoded_users <- data.frame(
    user = c("marvin", "admin"),
    password = c("utec2026", "admin123"),
    name = c("Marvin", "Admin"),
    stringsAsFactors = FALSE
  )

  # Helper: update auth message divs immediately via shinyjs
  auth_msg <- function(div_id, text, type = "error") {
    cls <- switch(type,
      error = "auth-msg error",
      success = "auth-msg success",
      loading = "auth-msg"
    )
    style <- if (type == "loading") ' style="background:#eff6ff;color:#2563eb;border:1px solid #bfdbfe;"' else ""
    icon <- if (type == "loading") '<span class="spinner-border spinner-border-sm me-2" role="status"></span>' else ""
    shinyjs::html(div_id, html = paste0('<div class="', cls, '"', style, '>', icon, text, '</div>'))
  }

  # Login handler
  observeEvent(input$login_btn, {
    user <- trimws(input$login_user)
    pass <- input$login_pass

    if (nchar(user) == 0 || nchar(pass) == 0) {
      auth_msg("login_msg_div", "Ingresa usuario y contraseña.")
      return()
    }

    shinyjs::disable("login_btn")
    auth_msg("login_msg_div", "Verificando credenciales...", "loading")

    # Check hardcoded users first
    match <- hardcoded_users[hardcoded_users$user == user & hardcoded_users$password == pass, ]
    if (nrow(match) > 0) {
      auth_msg("login_msg_div", paste0("¡Bienvenido, ", match$name[1], "!"), "success")
      auth_user(list(user = user, name = match$name[1]))
      shinyjs::enable("login_btn")
      return()
    }

    # Check MongoDB users with password hashing
    login_ok <- tryCatch({
      ensure_init()
      u <- mongolite::mongo(collection = "users", url = MONGO_URI)
      db_users <- u$find(paste0('{"user":"', user, '"}'))
      message("[StudyPilot] Login: found ", nrow(db_users), " matching users in MongoDB")
      result <- FALSE
      if (nrow(db_users) > 0) {
        stored_pass <- db_users$password[1]
        verified <- FALSE
        if (grepl("^\\$7\\$", stored_pass)) {
          verified <- tryCatch(sodium::password_verify(stored_pass, pass), error = function(e) FALSE)
        }
        if (!verified && stored_pass == pass) {
          verified <- TRUE
          hashed <- sodium::password_store(pass)
          u$update(paste0('{"user":"', user, '"}'), paste0('{"$set":{"password":"', hashed, '"}}'))
          message("[StudyPilot] Migrated password to hash for user: ", user)
        }
        if (verified) {
          auth_msg("login_msg_div", paste0("¡Bienvenido, ", db_users$name[1], "!"), "success")
          auth_user(list(user = user, name = db_users$name[1]))
          result <- TRUE
        }
      }
      u$disconnect()
      result
    }, error = function(e) {
      message("[StudyPilot] Login MongoDB ERROR: ", e$message)
      FALSE
    })

    shinyjs::enable("login_btn")
    if (!login_ok) {
      auth_msg("login_msg_div", "Usuario o contraseña incorrectos.")
    }
  })

  # Register handler
  observeEvent(input$register_btn, {
    name <- trimws(input$reg_name)
    user <- trimws(input$reg_user)
    pass <- input$reg_pass
    pass2 <- input$reg_pass2

    if (nchar(name) < 2 || nchar(user) < 3 || nchar(pass) < 4) {
      auth_msg("register_msg_div", "Completa todos los campos (usuario mín. 3 caracteres, contraseña mín. 4).")
      return()
    }
    if (!grepl("^[a-zA-Z0-9._]+$", user)) {
      auth_msg("register_msg_div", "El usuario solo puede contener letras, números, puntos y guiones bajos.")
      return()
    }
    if (pass != pass2) {
      auth_msg("register_msg_div", "Las contraseñas no coinciden.")
      return()
    }
    if (user %in% hardcoded_users$user) {
      auth_msg("register_msg_div", "Ese nombre de usuario no está disponible.")
      return()
    }

    shinyjs::disable("register_btn")
    auth_msg("register_msg_div", "Creando cuenta...", "loading")

    tryCatch({
      ensure_init()
      if (!.app_initialized) stop("No se pudo conectar a la base de datos.")
      message("[StudyPilot] Register: checking if user '", user, "' exists")
      # Use direct connection to ensure it works
      u <- mongolite::mongo(collection = "users", url = MONGO_URI)
      existing <- u$count(paste0('{"user":"', user, '"}'))
      if (existing > 0) {
        u$disconnect()
        auth_msg("register_msg_div", "Ese usuario ya existe. Intenta con otro.")
        shinyjs::enable("register_btn")
        return()
      }
      hashed_pass <- sodium::password_store(pass)
      u$insert(data.frame(user = user, password = hashed_pass, name = name, admin = FALSE, stringsAsFactors = FALSE))
      # Verify it was saved
      saved <- u$count(paste0('{"user":"', user, '"}'))
      u$disconnect()
      if (saved > 0) {
        message("[StudyPilot] Register: user '", user, "' VERIFIED in MongoDB")
        auth_msg("register_msg_div", paste0("✅ Cuenta creada exitosamente. Ahora inicia sesión con <b>", user, "</b>."), "success")
        updateTextInput(session, "reg_name", value = "")
        updateTextInput(session, "reg_user", value = "")
        updateTextInput(session, "reg_pass", value = "")
        updateTextInput(session, "reg_pass2", value = "")
      } else {
        auth_msg("register_msg_div", "Error: la cuenta no se pudo guardar.")
      }
    }, error = function(e) {
      message("[StudyPilot] Register ERROR: ", e$message)
      auth_msg("register_msg_div", paste0("Error: ", e$message))
    })
    shinyjs::enable("register_btn")
  })

  # Show main app after login (runs ONCE)
  observeEvent(auth_user(), {
    req(auth_user())
    shinyjs::hide("auth_overlay", anim = TRUE, animType = "fade")
    shinyjs::show("main_app", anim = TRUE, animType = "fade")
    ensure_init()
    # Populate global courses from MongoDB so all UI elements have data
    db_courses <- tryCatch(mg_custom_courses_get(uid()), error = function(e) {
      message("[StudyPilot] Error loading courses: ", e$message)
      data.frame()
    })
    message("[StudyPilot] Post-login: found ", nrow(db_courses), " courses in MongoDB")
    if (nrow(db_courses) > 0) {
      assign("courses", tibble::as_tibble(db_courses), envir = globalenv())
      message("[StudyPilot] Updated global courses: ", paste(db_courses$id, collapse=", "))
    }
    # Load course topics from study_notes
    tryCatch({
      all_notes <- mg_notes_all(uid())
      if (nrow(all_notes) > 0) {
        topic_notes <- all_notes[grepl("Temas", all_notes$source, ignore.case = TRUE), ]
        topics_list <- list()
        for (j in seq_len(nrow(topic_notes))) {
          cid_t <- topic_notes$course_id[j]
          raw <- topic_notes$text_content[j]
          # Parse "1. Topic\n2. Topic\n..." format
          topic_lines <- strsplit(raw, "\n")[[1]]
          topic_lines <- topic_lines[grepl("^\\d+\\.", topic_lines)]
          topic_lines <- sub("^\\d+\\.\\s*", "", topic_lines)
          if (length(topic_lines) > 0) topics_list[[cid_t]] <- topic_lines
        }
        assign("course_topics", topics_list, envir = globalenv())
        message("[StudyPilot] Loaded topics for ", length(topics_list), " courses")
      }
    }, error = function(e) message("[StudyPilot] Topics load error: ", e$message))
    rv$refresh <- isolate(rv$refresh) + 1
  })

  # Logout handler
  observeEvent(input$logout_btn, {
    auth_user(NULL)
    shinyjs::show("auth_overlay")
    shinyjs::hide("main_app")
    updateTextInput(session, "login_user", value = "")
    updateTextInput(session, "login_pass", value = "")
    shinyjs::html("login_msg_div", "")
  })

  # ---- Reactive: all activities ----
  rv <- reactiveValues(
    refresh = 0,
    grades_refresh = 0,
    view_week = current_week(),
    cal_week = current_week()
  )

  # Helper: current user id (safe in reactive contexts)
  uid <- function() {
    u <- auth_user()
    if (is.null(u)) return("")
    u$user
  }

  acts <- reactive({
    rv$refresh
    mg_activities_all(uid())
  })

  # Cache ALL grades in one call — reused by all panels
  all_grades_cache <- reactive({
    rv$grades_refresh
    rv$refresh
    mg_grades_all(uid())
  })

  # Fast course avg using cached data (no extra MongoDB calls)
  calc_avg_fast <- function(cid, cached_grades) {
    g <- cached_grades[cached_grades$course_id == cid, ]
    if (nrow(g) > 0 && "code" %in% names(g) && "grade" %in% names(g)) g <- g[, c("code", "grade")]
    else g <- data.frame(code = character(), grade = numeric())
    # Use activities from MongoDB for evaluation weights
    all_a <- acts()
    evals <- all_a[all_a$course_id == cid, ]
    if (nrow(evals) > 0) {
      for (j in seq_len(nrow(evals))) {
        if (is.null(evals$code[j]) || nchar(evals$code[j]) == 0) evals$code[j] <- paste0("E", j)
      }
      evals <- evals[, c("code", "weight")]
    } else {
      evals <- data.frame(code = character(), weight = numeric())
    }
    if (nrow(g) == 0 || nrow(evals) == 0) return(list(partial = 0, pct_graded = 0, earned = 0, needed = NA, remaining = 100))
    merged <- merge(evals, g, by = "code", all.x = TRUE)
    names(merged)[names(merged) == "grade"] <- "actual_grade"
    graded <- merged[!is.na(merged$actual_grade), ]
    earned <- sum(graded$actual_grade * graded$weight / 100, na.rm = TRUE)
    pct_graded <- sum(graded$weight, na.rm = TRUE)
    partial <- if (pct_graded > 0) earned / (pct_graded / 100) else 0
    remaining <- 100 - pct_graded
    needed <- if (remaining > 0) (10.5 - earned) / (remaining / 100) else 0
    list(partial = round(partial, 2), pct_graded = pct_graded, earned = round(earned, 2),
         needed = round(max(0, needed), 2), remaining = remaining)
  }

  # ---- DASHBOARD ----
  output$week_timeline <- renderUI({
    cw <- current_week()
    blocks <- lapply(1:TOTAL_WEEKS, function(w) {
      cls <- if (w < cw) "wk-past" else if (w == cw) "wk-current" else "wk-future"
      d <- SEMESTER_START + (w - 1) * 7
      tags$div(class = paste("wk-block", cls),
        tags$div(class = "wk-num", paste0("S", w)),
        tags$div(class = "wk-date", format(d, "%d/%m"))
      )
    })
    div(class = "wk-timeline", blocks)
  })

  output$stat_pct <- renderText({
    a <- acts(); done <- sum(a$done); total <- nrow(a)
    paste0(if(total > 0) round(done/total*100) else 0, "%")
  })
  output$stat_pending <- renderText(sum(!acts()$done))
  output$stat_overdue <- renderText({
    a <- acts() |> filter(done == 0, date < as.character(Sys.Date()))
    nrow(a)
  })
  output$stat_high <- renderText({
    a <- acts() |> filter(done == 0, weight >= 20)
    nrow(a)
  })
  output$stat_weeks <- renderText(TOTAL_WEEKS - current_week() + 1)
  output$stat_avg <- renderText({
    cached <- all_grades_cache()
    sw <- 0; sn <- 0
    for (cid in courses$id) {
      avg <- calc_avg_fast(cid, cached)
      cr <- courses$credits[courses$id == cid]
      if (avg$partial > 0) { sw <- sw + cr; sn <- sn + avg$partial * cr }
    }
    if (sw > 0) round(sn/sw, 1) else "-"
  })

  output$upcoming_table <- renderDT({
    a <- acts() |>
      filter(done == 0) |>
      mutate(
        days_left = as.integer(as.Date(date) - Sys.Date()),
        curso = ifelse(course_id == "_personal", "📌 Personal",
                       ifelse(course_id %in% courses$id, courses$short[match(course_id, courses$id)], course_id)),
        prioridad = priority_class(weight)
      ) |>
      arrange(date) |>
      head(10) |>
      select(Curso = curso, Actividad = name, Tipo = type, `Peso%` = weight,
             Fecha = date, `Días` = days_left, Prioridad = prioridad)
    datatable(a, options = list(pageLength = 10, dom = 't'), rownames = FALSE,
              selection = "none") |>
      formatStyle("Días", color = styleInterval(c(0, 3, 7), c("#dc2626", "#dc2626", "#ca8a04", "#16a34a"))) |>
      formatStyle("Peso%", fontWeight = "bold")
  })

  # ---- COUNTDOWN ----
  output$countdown_cards <- renderUI({
    a <- acts() |> filter(done == 0, weight >= 10) |>
      mutate(days_left = as.integer(as.Date(date) - Sys.Date())) |>
      arrange(date)
    if (nrow(a) == 0) return(tags$p(class = "text-muted", "No hay evaluaciones pendientes."))

    cards <- lapply(seq_len(nrow(a)), function(i) {
      r <- a[i, ]
      c_info <- courses |> filter(id == r$course_id)
      col <- if(r$days_left < 0) "#dc2626" else if(r$days_left <= 3) "#dc2626" else if(r$days_left <= 7) "#ca8a04" else "#16a34a"
      pri <- if(r$weight >= 20) "border-start-danger" else "border-start-warning"

      card(class = paste("border-start border-4", pri),
        card_body(
          div(class = "d-flex justify-content-between",
            tags$span(class = "badge", style = paste0("background:", c_info$color), c_info$short),
            div(class = "text-end",
              tags$span(style = paste0("font-size:1.5rem;font-weight:800;color:", col),
                       if(r$days_left < 0) paste0("⚠", abs(r$days_left)) else r$days_left),
              tags$div(class = "small text-muted", if(r$days_left < 0) "atrasado" else "días")
            )
          ),
          tags$h6(class = "mt-2 mb-1", r$name),
          tags$div(class = "small text-muted",
            paste0("📅 ", format(as.Date(r$date), "%A %d %b"), " · S", r$week, " · Peso: ", r$weight, "%")
          )
        )
      )
    })
    layout_columns(col_widths = rep(4, length(cards)), !!!cards)
  })

  # ---- EXAM MODE ----
  observe({
    a <- acts() |> filter(done == 0, weight >= 15, type %in% c("examen", "quiz"))
    if (nrow(a) == 0) {
      updateSelectInput(session, "exam_select", choices = c("Sin exámenes pendientes" = "0"))
      return()
    }
    a <- a |> mutate(days_left = as.integer(as.Date(date) - Sys.Date())) |> arrange(date)
    choices <- setNames(seq_len(nrow(a)), paste0(
      courses$short[match(a$course_id, courses$id)], " — ", a$name, " (", a$days_left, "d, ", a$weight, "%)"
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
    col <- if(ex$days_left <= 3) "danger" else if(ex$days_left <= 7) "warning" else "success"
    tagList(
      tags$div(class = "text-center mt-2",
        tags$h1(class = paste0("text-", col), style = "font-weight:800",
                if(ex$days_left < 0) paste0("⚠ ", abs(ex$days_left)) else ex$days_left),
        tags$p(class = "text-muted", if(ex$days_left < 0) "días atrasado" else "días restantes")
      ),
      tags$div(class = "small",
        tags$b("Curso: "), c_info$name, tags$br(),
        tags$b("Peso: "), paste0(ex$weight, "%"), tags$br(),
        tags$b("Fecha: "), format(as.Date(ex$date), "%A %d de %B"), tags$br(),
        tags$b("Semana: "), ex$week
      )
    )
  })

  output$exam_topics_checklist <- renderUI({
    ex <- selected_exam()
    if (is.null(ex)) return(tags$p(class = "text-muted", "Selecciona un examen"))

    # Determine which topics to show
    if (ex$course_id == "HH5101") {
      # Ética: PC1 = semanas 1-4, PC2 = semanas 5-11
      is_pc2 <- grepl("PC2|EF1", ex$code, ignore.case = TRUE) || ex$week >= 10
      if (is_pc2) {
        show_topics <- etica_pc2_topics
        label <- "Temas PC2 (Semanas 5-11)"
        relevant_readings <- etica_readings |> filter(week >= 5, week <= 11)
      } else {
        show_topics <- etica_pc1_topics
        label <- "Temas PC1 (Semanas 1-4)"
        relevant_readings <- etica_readings |> filter(week <= 4)
      }
    } else {
      topics <- course_topics[[ex$course_id]]
      if (is.null(topics)) return(tags$p("Sin temas definidos"))
      is_final <- grepl("final|EF|EXM2", ex$code, ignore.case = TRUE)
      show_topics <- if (is_final) topics else topics[1:ceiling(length(topics)/2)]
      label <- if (is_final) "Todo el curso (Final)" else "Primera mitad (Parcial)"
      relevant_readings <- NULL
    }

    checks <- mg_exam_checks_get(uid(), ex$course_id)

    topic_items <- lapply(seq_along(show_topics), function(i) {
      key <- paste0(ex$code, "_", i)
      is_checked <- any(checks$topic_key == key & checks$checked == 1)
      div(class = paste("topic-item", if(is_checked) "checked" else ""),
        checkboxInput(paste0("etopic_", key), label = paste0(i, ". ", show_topics[i]),
                     value = is_checked, width = "100%")
      )
    })

    tagList(
      tags$div(class = "small text-muted mb-2", paste0("📖 Temas — ", label)),
      topic_items,
      if (!is.null(relevant_readings) && nrow(relevant_readings) > 0) {
        tagList(
          tags$hr(),
          tags$div(class = "small", tags$b("📚 Lecturas para este examen:")),
          lapply(seq_len(nrow(relevant_readings)), function(i) {
            r <- relevant_readings[i, ]
            tags$div(class = "small text-muted", paste0("S", r$week, ": ", r$reading,
              if(nchar(r$tema) > 0) paste0(" — ", tags$b(r$tema)) else ""))
          })
        )
      }
    )
  })

  # Save exam topic checks
  observe({
    ex <- selected_exam()
    if (is.null(ex)) return()

    if (ex$course_id == "HH5101") {
      is_pc2 <- grepl("PC2|EF1", ex$code, ignore.case = TRUE) || ex$week >= 10
      n <- if (is_pc2) length(etica_pc2_topics) else length(etica_pc1_topics)
    } else {
      topics <- course_topics[[ex$course_id]]
      if (is.null(topics)) return()
      is_final <- grepl("final|EF|EXM2", ex$code, ignore.case = TRUE)
      n <- if (is_final) length(topics) else ceiling(length(topics)/2)
    }

    lapply(seq_len(n), function(i) {
      key <- paste0(ex$code, "_", i)
      input_id <- paste0("etopic_", key)
      val <- input[[input_id]]
      if (!is.null(val)) {
        mg_exam_check_set(uid(), ex$course_id, key, as.integer(val))
      }
    })
  })

  output$exam_progress_bar <- renderUI({
    ex <- selected_exam()
    if (is.null(ex)) return(NULL)
    # Match same logic as checklist
    if (ex$course_id == "HH5101") {
      is_pc2 <- grepl("PC2|EF1", ex$code, ignore.case = TRUE) || ex$week >= 10
      n <- if (is_pc2) length(etica_pc2_topics) else length(etica_pc1_topics)
    } else {
      topics <- course_topics[[ex$course_id]]
      is_final <- grepl("final|EF|EXM2", ex$code, ignore.case = TRUE)
      n <- if (is_final) length(topics) else ceiling(length(topics)/2)
    }
    checks <- mg_exam_checks_get_checked(uid(), ex$course_id)
    relevant <- checks |> filter(grepl(paste0("^", ex$code), topic_key))
    pct <- if(n > 0) round(nrow(relevant)/n*100) else 0
    div(
      tags$div(class = "small", paste0("Progreso: ", nrow(relevant), "/", n, " (", pct, "%)")),
      div(class = "progress mt-1",
        div(class = "progress-bar bg-success", role = "progressbar",
            style = paste0("width:", pct, "%"), paste0(pct, "%"))
      )
    )
  })

  observeEvent(input$exam_pomo, {
    ex <- selected_exam()
    if (!is.null(ex)) {
      updateSelectInput(session, "pomo_course", selected = ex$course_id)
      nav_select("main_nav", selected = "🍅 Pomodoro")
    }
  })

  # ---- STUDY GUIDES ----
  output$study_guide_content <- renderUI({
    ex <- selected_exam()
    if (is.null(ex)) return(tags$p(class = "text-muted", "Selecciona un examen para ver las guías de estudio."))

    # Only Ética PC2 has guides for now
    if (ex$course_id == "HH5101") {
      guides <- etica_pc2_guides
    } else {
      # Generic: show topics with key questions
      topics <- course_topics[[ex$course_id]]
      if (is.null(topics)) return(tags$p(class = "text-muted", "No hay guías de estudio para este curso aún."))
      return(tagList(
        tags$div(class = "alert alert-info py-2 small", "Guías detalladas disponibles para Ética. Para otros cursos se muestran los temas del sílabo."),
        tags$ol(class = "small", lapply(topics, function(t) tags$li(t)))
      ))
    }

    # Render each guide as an accordion
    guide_items <- lapply(seq_along(guides), function(i) {
      g <- guides[[i]]
      accordion_panel(
        title = paste0("📖 ", g$title, " (S", g$week, ")"),
        value = paste0("guide_", i),

        # Summary
        tags$div(class = "mb-3",
          tags$h6(class = "text-primary", "📝 Resumen"),
          tags$p(class = "small", g$summary),
          tags$div(class = "small text-muted", tags$b("📚 Lectura: "), g$reading)
        ),

        # Key concepts
        tags$div(class = "mb-3",
          tags$h6(class = "text-primary", "🔑 Conceptos Clave"),
          tags$div(
            lapply(g$concepts, function(c) {
              tags$div(class = "mb-2 p-2 bg-light rounded small",
                tags$b(class = "text-dark", paste0(c$term, ": ")),
                tags$span(c$def)
              )
            })
          )
        ),

        # Diagram as styled HTML
        tags$div(class = "mb-3",
          tags$h6(class = "text-primary", "🗺 Mapa Conceptual"),
          render_diagram(g$diagram)
        ),

        # Key questions
        tags$div(
          tags$h6(class = "text-primary", "❓ Preguntas Clave para Repasar"),
          tags$ol(class = "small",
            lapply(g$key_questions, function(q) tags$li(class = "mb-1", q))
          )
        )
      )
    })

    tagList(
      tags$div(class = "alert alert-success py-2 small mb-3",
        paste0("📚 ", length(guides), " temas con resúmenes, diagramas y preguntas clave. Expande cada tema para estudiar.")
      ),
      accordion(!!!guide_items, open = FALSE, id = "guide_accordion")
    )
  })

  # ---- SELECTABLE STUDY NOTES ----
  output$study_notes_selectable <- renderUI({
    rv$refresh
    notes <- mg_notes_all(uid())
    if (nrow(notes) == 0) return(tags$p(class = "text-muted small", "Sin material subido. Usa el panel de la izquierda para agregar archivos."))

    note_checks <- lapply(seq_len(nrow(notes)), function(i) {
      n <- notes[i, ]
      cname <- courses$short[match(n$course_id, courses$id)]
      div(class = "d-flex align-items-center gap-2 mb-1 p-1 border rounded small",
        checkboxInput(paste0("sel_note_", n$id), label = NULL, value = FALSE, width = "auto"),
        tags$span(class = "badge bg-info", cname),
        tags$span(n$source, style = "flex:1"),
        tags$span(class = "text-muted", paste0(nchar(n$text_content), " car.")),
        actionButton(paste0("del_note_", n$id), "✕", class = "btn-sm btn-outline-danger py-0 px-1")
      )
    })
    tagList(note_checks)
  })

  output$selected_notes_content <- renderUI({
    NULL
  })

  observeEvent(input$show_selected_notes, {
    notes <- mg_notes_all(uid())
    if (nrow(notes) == 0) return()

    selected_ids <- c()
    for (i in seq_len(nrow(notes))) {
      if (isTRUE(input[[paste0("sel_note_", notes$id[i])]])) {
        selected_ids <- c(selected_ids, notes$id[i])
      }
    }

    if (length(selected_ids) == 0) {
      output$selected_notes_content <- renderUI({
        tags$div(class = "alert alert-warning py-2 small mt-2", "Selecciona al menos un archivo marcando las casillas de arriba.")
      })
      return()
    }

    sel_notes <- notes[notes$id %in% selected_ids, ]
    output$selected_notes_content <- renderUI({
      items <- lapply(seq_len(nrow(sel_notes)), function(i) {
        n <- sel_notes[i, ]
        cname <- courses$short[match(n$course_id, courses$id)]
        # Limit display length
        display_text <- if (nchar(n$text_content) > 3000) {
          paste0(substr(n$text_content, 1, 3000), "\n\n... [truncado, ", nchar(n$text_content), " caracteres totales]")
        } else n$text_content

        card(class = "mt-2",
          card_header(class = "py-1 small", paste0("📄 ", n$source, " (", cname, ")")),
          card_body(class = "py-2",
            tags$pre(class = "small bg-light p-2 rounded", style = "white-space:pre-wrap;max-height:400px;overflow-y:auto;font-size:0.75rem", display_text)
          )
        )
      })
      tagList(items)
    })
  })

  output$study_notes_list <- renderUI({
    rv$refresh
    notes <- mg_notes_all(uid())
    if (nrow(notes) == 0) return(tags$p(class = "text-muted small", "Sin notas guardadas."))

    lapply(seq_len(nrow(notes)), function(i) {
      n <- notes[i, ]
      cname <- courses$short[match(n$course_id, courses$id)]
      nid <- if ("note_id" %in% names(n)) n$note_id else i
      div(class = "study-note mb-2",
        div(class = "d-flex justify-content-between",
          tags$b(class = "small", paste0(cname, " — ", n$source)),
          actionButton(paste0("del_note_", nid), "✕", class = "btn-sm btn-outline-danger py-0 px-1")
        ),
        tags$div(class = "small text-muted mt-1",
                substr(n$text_content, 1, 250), if(nchar(n$text_content) > 250) "..." else "")
      )
    })
  })

  # Delete study notes
  observe({
    notes <- mg_notes_all(uid())
    if (nrow(notes) == 0) return()
    note_ids <- if ("note_id" %in% names(notes)) notes$note_id else seq_len(nrow(notes))
    lapply(note_ids, function(nid) {
      observeEvent(input[[paste0("del_note_", nid)]], {
        mg_note_delete(uid(), nid)
        rv$refresh <- rv$refresh + 1
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ---- ACTIVITIES ----
  output$activities_table <- renderDT({
    a <- acts()
    # Filters
    if (input$act_filter_course != "all") a <- a |> filter(course_id == input$act_filter_course)
    if (input$act_filter_type != "all") a <- a |> filter(type == input$act_filter_type)
    if (input$act_filter_status == "pending") a <- a |> filter(done == 0)
    if (input$act_filter_status == "done") a <- a |> filter(done == 1)
    if (input$act_filter_pri == "high") a <- a |> filter(weight >= 20)
    if (input$act_filter_pri == "medium") a <- a |> filter(weight >= 10, weight < 20)
    if (input$act_filter_pri == "low") a <- a |> filter(weight < 10)

    a <- a |>
      mutate(
        Curso = ifelse(course_id == "_personal", "📌 Personal",
                       ifelse(course_id %in% courses$id, courses$short[match(course_id, courses$id)], course_id)),
        Días = as.integer(as.Date(date) - Sys.Date()),
        Estado = ifelse(done == 1, "✅", ifelse(Días < 0, "⚠️", "⬜")),
        Prioridad = priority_class(weight)
      ) |>
      select(ID = act_id, Estado, Curso, Actividad = name, Tipo = type, `Peso%` = weight,
             Semana = week, Fecha = date, Días, Prioridad)

    datatable(a, options = list(pageLength = 20, order = list(list(8, 'asc'))),
              rownames = FALSE, selection = "multiple") |>
      formatStyle("Días", color = styleInterval(c(0, 3, 7), c("#dc2626", "#dc2626", "#ca8a04", "#16a34a"))) |>
      formatStyle("Peso%", fontWeight = "bold") |>
      formatStyle("Prioridad",
        backgroundColor = styleEqual(c("high", "medium", "low"), c("#fef2f2", "#fffbeb", "#f0fdf4")))
  })

  # Helper: get filtered activities (same filters as table)
  filtered_acts <- function() {
    a <- acts()
    if (input$act_filter_course != "all") a <- a[a$course_id == input$act_filter_course, ]
    if (input$act_filter_type != "all") a <- a[a$type == input$act_filter_type, ]
    if (input$act_filter_status == "pending") a <- a[a$done == 0, ]
    if (input$act_filter_status == "done") a <- a[a$done == 1, ]
    if (input$act_filter_pri == "high") a <- a[a$weight >= 20, ]
    if (input$act_filter_pri == "medium") a <- a[a$weight >= 10 & a$weight < 20, ]
    if (input$act_filter_pri == "low") a <- a[a$weight < 10, ]
    a[order(a$date), ]
  }

  # ---- NUEVA ACTIVIDAD ----
  observeEvent(input$act_new, {
    course_choices <- c("Personal (sin curso)" = "_personal")
    if (nrow(courses) > 0) course_choices <- c(course_choices, setNames(courses$id, courses$short))
    showModal(modalDialog(
      title = "Nueva Actividad",
      textInput("new_act_name", "Nombre:", placeholder = "Ej: Entregar informe, Estudiar para parcial, Ir al gym..."),
      selectInput("new_act_course", "Curso (opcional):", choices = course_choices),
      selectInput("new_act_type", "Tipo:", choices = c("tarea" = "tarea", "ec", "examen", "proyecto", "quiz", "personal" = "personal")),
      numericInput("new_act_weight", "Peso (%):", value = 0, min = 0, max = 100),
      dateInput("new_act_date", "Fecha límite:", value = Sys.Date() + 7),
      textInput("new_act_notes", "Notas:", placeholder = "Opcional"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("new_act_save", "✅ Crear", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$new_act_save, {
    if (is.null(input$new_act_name) || nchar(input$new_act_name) == 0) {
      showNotification("Escribe un nombre para la actividad", type = "warning"); return()
    }
    act_course <- if (input$new_act_course == "_personal") "_personal" else input$new_act_course
    mg_activity_add(uid(), act_course, input$new_act_type,
      input$new_act_name, as.character(input$new_act_date),
      input$new_act_weight, input$new_act_notes)
    rv$refresh <- rv$refresh + 1
    removeModal()
    showNotification(paste0("✅ Actividad '", input$new_act_name, "' creada"), type = "message")
  })

  # ---- MARCAR HECHAS (múltiple) ----
  observeEvent(input$act_mark_done, {
    sel <- input$activities_table_rows_selected
    if (length(sel) == 0) { showNotification("Selecciona actividades primero", type = "warning"); return() }
    af <- filtered_acts()
    count <- 0
    for (s in sel) {
      if (s <= nrow(af)) {
        row <- af[s, ]
        aid <- if ("act_id" %in% names(row)) row$act_id else s
        new_done <- 1L - row$done
        mg_activity_toggle(uid(), aid, new_done)
        count <- count + 1
      }
    }
    rv$refresh <- rv$refresh + 1
    showNotification(paste0("✅ ", count, " actividad(es) actualizada(s)"), type = "message", duration = 3)
  })

  # ---- EDITAR ACTIVIDAD ----
  observeEvent(input$act_edit, {
    sel <- input$activities_table_rows_selected
    if (length(sel) == 0) { showNotification("Selecciona una actividad primero", type = "warning"); return() }
    if (length(sel) > 1) { showNotification("Selecciona solo una actividad para editar", type = "warning"); return() }
    af <- filtered_acts()
    if (sel[1] > nrow(af)) return()
    row <- af[sel[1], ]
    showModal(modalDialog(
      title = paste0("Editar: ", row$name),
      textInput("edit_act_name", "Nombre:", value = row$name),
      selectInput("edit_act_type", "Tipo:", choices = c("ec", "examen", "proyecto", "quiz"), selected = row$type),
      numericInput("edit_act_weight", "Peso (%):", value = row$weight, min = 0, max = 100),
      dateInput("edit_act_date", "Fecha:", value = as.Date(row$date)),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("edit_act_save", "💾 Guardar", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
    rv$editing_act_id <- row$act_id
  })

  observeEvent(input$edit_act_save, {
    aid <- rv$editing_act_id
    mg_activity_update(uid(), aid, input$edit_act_name, input$edit_act_weight, as.character(input$edit_act_date))
    rv$refresh <- rv$refresh + 1
    removeModal()
    showNotification("✅ Actividad actualizada", type = "message")
  })

  # ---- ELIMINAR ACTIVIDADES (múltiple) ----
  observeEvent(input$act_delete, {
    sel <- input$activities_table_rows_selected
    if (length(sel) == 0) { showNotification("Selecciona actividades primero", type = "warning"); return() }
    af <- filtered_acts()
    names_list <- sapply(sel, function(s) if (s <= nrow(af)) af$name[s] else "")
    names_list <- names_list[nchar(names_list) > 0]
    showModal(modalDialog(
      title = paste0("Eliminar ", length(names_list), " actividad(es)"),
      tags$ul(lapply(names_list, function(n) tags$li(n))),
      tags$p(class = "text-danger", "Esta acción no se puede deshacer."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("delete_act_confirm", "🗑 Eliminar", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
    rv$deleting_act_ids <- sapply(sel, function(s) if (s <= nrow(af)) af$act_id[s] else NA)
    rv$deleting_act_ids <- rv$deleting_act_ids[!is.na(rv$deleting_act_ids)]
  })

  observeEvent(input$delete_act_confirm, {
    for (aid in rv$deleting_act_ids) {
      mg_activity_delete(uid(), aid)
    }
    rv$refresh <- rv$refresh + 1
    removeModal()
    showNotification(paste0("🗑 ", length(rv$deleting_act_ids), " actividad(es) eliminada(s)"), type = "warning")
  })

  # ---- GRADES ----
  output$grades_panels <- renderUI({
    cached <- all_grades_cache()
    all_acts <- acts()
    if (nrow(courses) == 0) return(tags$div(class = "text-muted", "No hay cursos. Sube tus sílabos en la pestaña Cursos."))

    panels <- lapply(seq_len(nrow(courses)), function(i) {
      c_info <- courses[i, ]
      # Use activities from MongoDB instead of hardcoded evaluations
      evals <- all_acts |> filter(course_id == c_info$id) |> arrange(date)
      if (nrow(evals) == 0) evals <- data.frame(act_id=integer(), name=character(), code=character(), type=character(), weight=numeric())
      # Generate code if empty
      for (j in seq_len(nrow(evals))) {
        if (is.null(evals$code[j]) || nchar(evals$code[j]) == 0) evals$code[j] <- paste0("E", j)
      }
      g <- cached[cached$course_id == c_info$id, ]
      if (nrow(g) > 0 && "code" %in% names(g) && "grade" %in% names(g)) g <- g[, c("code", "grade")]
      else g <- data.frame(code = character(), grade = numeric())
      avg <- calc_avg_fast(c_info$id, cached)
      col <- if (avg$partial >= 13) "success" else if (avg$partial >= 10.5) "warning" else "danger"

      grade_inputs <- lapply(seq_len(nrow(evals)), function(j) {
        ev <- evals[j, ]
        current <- g$grade[g$code == ev$code]
        current <- if (length(current) == 0 || all(is.na(current))) NA_real_ else current[1]
        tags$tr(
          tags$td(ev$name),
          tags$td(tags$span(class = paste0("badge bg-", switch(ev$type, examen="danger", proyecto="warning", ec="info", quiz="primary", "secondary")), ev$type)),
          tags$td(class = "fw-bold text-primary", paste0(ev$weight, "%")),
          tags$td(numericInput(paste0("grade_", c_info$id, "_", ev$code), NULL,
                              value = if(!is.na(current)) current else NA,
                              min = 0, max = 20, step = 0.5, width = "80px"))
        )
      })

      card(class = "mb-3", style = paste0("border-left: 4px solid ", c_info$color),
        card_header(class = "d-flex justify-content-between",
          tags$span(c_info$short, tags$small(class = "text-muted ms-2", c_info$id)),
          tags$span(class = paste0("badge bg-", col), paste0(avg$partial, " / ", avg$pct_graded, "% evaluado"))
        ),
        card_body(
          tags$div(class = "small text-muted mb-2", paste0("Fórmula: ", c_info$formula)),
          if (nrow(evals) == 0) tags$div(class = "text-muted small", "Sin evaluaciones registradas")
          else tags$table(class = "table table-sm table-striped",
            tags$thead(tags$tr(tags$th("Evaluación"), tags$th("Tipo"), tags$th("Peso"), tags$th("Nota"))),
            tags$tbody(grade_inputs)
          ),
          div(class = "alert alert-light py-2 mt-2",
            div(class = "d-flex justify-content-between align-items-center",
              div(tags$small("Promedio parcial:"),
                  tags$span(class = paste0("fs-4 fw-bold text-", col), avg$partial)),
              div(class = "text-end small",
                if (avg$remaining > 0 && !is.na(avg$needed))
                  tags$span(
                    if (avg$needed <= 20)
                      paste0("Necesitas ", avg$needed, " en el ", avg$remaining, "% restante para aprobar")
                    else
                      tags$span(class = "text-danger fw-bold", paste0("⚠ Necesitas ", avg$needed, " — difícil"))
                  )
                else if (avg$pct_graded == 0)
                  tags$span(class = "text-muted", "Sin notas registradas aún")
              )
            )
          ),
          actionButton(paste0("save_grades_", c_info$id), "💾 Guardar notas",
                      class = "btn-sm btn-outline-primary",
                      onclick = paste0("Shiny.setInputValue('save_grade_course', '", c_info$id, "', {priority: 'event'})")),
          tags$span(id = paste0("grade_spinner_", c_info$id))
        )
      )
    })
    layout_columns(col_widths = breakpoints(sm = c(12), lg = c(6, 6)), !!!panels)
  })

  # Save grades — single handler via JavaScript onclick
  observeEvent(input$save_grade_course, {
    cid <- input$save_grade_course
    if (is.null(cid) || nchar(cid) == 0) return()

    # Show spinner
    shinyjs::html(paste0("grade_spinner_", cid),
      '<span class="spinner-border spinner-border-sm text-success ms-2"></span> <small class="text-success">Calculando promedio...</small>')
    shinyjs::disable(paste0("save_grades_", cid))

    tryCatch({
      all_acts <- acts()
      evals <- all_acts[all_acts$course_id == cid, ]
      saved <- 0
      current_uid <- uid()
      for (j in seq_len(nrow(evals))) {
        ev <- evals[j, ]
        ev_code <- if (is.null(ev$code) || nchar(ev$code) == 0) paste0("E", j) else ev$code
        val <- input[[paste0("grade_", cid, "_", ev_code)]]
        if (!is.null(val) && !is.na(val) && is.numeric(val)) {
          mg_grade_set(current_uid, cid, ev_code, val)
          saved <- saved + 1
        } else {
          # Delete grade if input is empty (user cleared it)
          mg_grade_delete(current_uid, cid, ev_code)
        }
      }
      rv$grades_refresh <- isolate(rv$grades_refresh) + 1
      cname <- if (cid %in% courses$id) courses$short[courses$id == cid] else cid
      showNotification(paste0("✅ ", saved, " notas de ", cname, " guardadas"), type = "message")
    }, error = function(e) {
      showNotification(paste0("❌ Error: ", e$message), type = "error")
    })

    shinyjs::html(paste0("grade_spinner_", cid), "")
    shinyjs::enable(paste0("save_grades_", cid))
  })

  output$overall_avg <- renderText({
    cached <- all_grades_cache()
    sw <- 0; sn <- 0
    for (cid in courses$id) {
      avg <- calc_avg_fast(cid, cached)
      cr <- courses$credits[courses$id == cid]
      if (avg$partial > 0) { sw <- sw + cr; sn <- sn + avg$partial * cr }
    }
    if (sw > 0) round(sn/sw, 1) else "-"
  })

  # ---- WEEK VIEW ----
  observeEvent(input$week_prev, { rv$view_week <- max(1, rv$view_week - 1) })
  observeEvent(input$week_next, { rv$view_week <- min(TOTAL_WEEKS, rv$view_week + 1) })

  output$week_title <- renderUI({
    w <- rv$view_week
    d1 <- SEMESTER_START + (w - 1) * 7
    d2 <- d1 + 5
    tags$h5(class = "mb-0", paste0("Semana ", w, " (", format(d1, "%d %b"), " – ", format(d2, "%d %b"), ")"))
  })

  output$week_view <- renderUI({
    w <- rv$view_week
    day_names <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    a <- acts()

    days <- lapply(1:6, function(d) {
      dd <- SEMESTER_START + (w - 1) * 7 + (d - 1)
      is_today <- dd == Sys.Date()
      dd_str <- as.character(dd)

      day_acts <- a |> filter(date == dd_str)

      act_items <- lapply(seq_len(nrow(day_acts)), function(i) {
        r <- day_acts[i, ]
        cname <- courses$short[match(r$course_id, courses$id)]
        div(class = paste("act-item", if(r$done) "done" else ""),
          paste0(if(r$done) "✅ " else "⬜ ", r$name, " (", cname, ", ", r$weight, "%)")
        )
      })

      card(
        card_header(class = if(is_today) "bg-primary text-white" else "",
          paste0(day_names[d], " ", format(dd, "%d %b"), if(is_today) " (HOY)" else "")
        ),
        card_body(class = "py-2",
          if (length(act_items) > 0) act_items else NULL,
          if (length(act_items) == 0) tags$p(class = "text-muted small fst-italic", "Sin actividades")
        )
      )
    })

    layout_columns(col_widths = breakpoints(sm = c(12, 12, 12, 12, 12, 12), md = c(6, 6, 6, 6, 6, 6), lg = c(4, 4, 4, 4, 4, 4)), !!!days)
  })

  # ---- SCHEDULE ----
  # Visual Calendar with multi-hour event blocks
  HOUR_H <- 40  # pixels per hour (compact)
  CAL_FIRST_HOUR <- 0
  CAL_LAST_HOUR <- 23

  output$visual_calendar <- renderUI({
    rv$refresh
    gcal_events <- rv_gcal$events
    cw <- rv$cal_week
    week_start <- SEMESTER_START + (cw - 1) * 7
    today <- Sys.Date()
    day_names <- c("DOM","LUN","MAR","MIÉ","JUE","VIE","SÁB")
    first_hour <- CAL_FIRST_HOUR; last_hour <- CAL_LAST_HOUR
    total_hours <- last_hour - first_hour

    # Headers
    headers <- list(tags$div(class = "cal-header cal-header-time", ""))
    for (d in 0:6) {
      dd <- week_start + d - 1
      headers <- c(headers, list(tags$div(
        class = paste("cal-header", if(dd == today) "today" else ""),
        tags$div(class = "cal-day-name", day_names[d + 1]),
        tags$div(class = "cal-day-num", format(dd, "%d"))
      )))
    }

    # Time labels column
    time_col <- tags$div(class = "cal-times",
      lapply(first_hour:last_hour, function(h) {
        tags$div(class = "cal-hour-label", paste0(sprintf("%02d",h), ":00"))
      })
    )

    # Day columns with absolute-positioned events
    day_cols <- lapply(0:6, function(d) {
      dd <- week_start + d - 1
      # Hour grid lines
      hour_lines <- lapply(first_hour:last_hour, function(h) tags$div(class = "cal-hour-line"))

      event_divs <- list()

      # Google Calendar events (timed only — all-day rendered separately above)
      if (!is.null(gcal_events) && nrow(gcal_events) > 0 && !"error" %in% names(gcal_events)) {
        day_evts <- gcal_events[as.Date(substr(gcal_events$start,1,10)) == dd & nchar(gcal_events$start) > 10, ]
        if (nrow(day_evts) > 0) {
          # Parse all event times first (for overlap detection)
          ev_times <- lapply(seq_len(nrow(day_evts)), function(k) {
            ev <- day_evts[k, ]
            sh <- suppressWarnings(as.numeric(substr(ev$start,12,13)) + as.numeric(substr(ev$start,15,16))/60)
            eh <- suppressWarnings(as.numeric(substr(ev$end,12,13)) + as.numeric(substr(ev$end,15,16))/60)
            # All-day events: skip here, rendered separately as banners
            if (is.na(sh) || is.na(eh)) { next }
            if (eh <= sh) eh <- sh + 1
            list(sh = sh, eh = eh)
          })

          for (k in seq_len(nrow(day_evts))) {
            ev <- day_evts[k, ]
            sh <- ev_times[[k]]$sh
            eh <- ev_times[[k]]$eh
            top_px <- (sh - first_hour) * HOUR_H
            height_px <- max((eh - sh) * HOUR_H - 2, 20)
            time_label <- paste0(substr(ev$start,12,16), " – ", substr(ev$end,12,16))

            # Detect overlaps: count events that overlap with this one
            n_overlaps <- sum(sapply(ev_times, function(o) o$sh < eh && o$eh > sh))
            # Find this event's index among overlapping events
            overlap_idx <- sum(sapply(seq_len(k), function(j) ev_times[[j]]$sh < eh && ev_times[[j]]$eh > sh))
            evt_width <- if (n_overlaps > 1) paste0(floor(90 / n_overlaps), "%") else "92%"
            evt_left <- if (n_overlaps > 1) paste0(floor((overlap_idx - 1) * 90 / n_overlaps) + 2, "%") else "4%"

            # Color by course name (matching Google Calendar colors)
            name_lower <- tolower(ev$summary)
            clr <- if (grepl("pco", name_lower)) "cyan"
              else if (grepl("dise", name_lower)) "green"
              else if (grepl("data|analy", name_lower)) "blue"
              else if (grepl("gesti", name_lower)) "orange"
              else if (grepl("estrat", name_lower)) "pink"
              else if (grepl("etica|\u00e9tica", name_lower)) "yellow"
              else if (grepl("comer|almuerz|comida|lunch|cena|desayun", name_lower)) "gray"
              else if (grepl("examen|quiz|pc[0-9]|parcial|final|evaluaci|E[PF][0-9]", ev$summary, ignore.case = TRUE)) "red"
              else "blue"

            event_divs <- c(event_divs, list(
              tags$div(class = paste0("cal-event cal-ev-", clr),
                style = paste0("top:", top_px, "px; height:", height_px, "px; width:", evt_width, "; left:", evt_left, ";"),
                tags$div(class = "cal-ev-name", ev$summary),
                tags$div(class = "cal-ev-time", time_label),
                if (nchar(ev$location) > 0) tags$div(class = "cal-ev-room", ev$location)
              )
            ))
          }
        }
      }

      tags$div(class = "cal-day-col",
        style = paste0("height:", total_hours * HOUR_H, "px;"),
        hour_lines, event_divs
      )
    })

    # Collect all-day events and render as spanning banners
    has_allday <- FALSE
    allday_spans <- list()
    week_sun <- week_start - 1  # Sunday
    week_sat <- week_start + 5  # Saturday
    if (!is.null(gcal_events) && nrow(gcal_events) > 0 && !"error" %in% names(gcal_events)) {
      allday_evts <- gcal_events[nchar(gcal_events$start) <= 10, ]
      if (nrow(allday_evts) > 0) {
        # Deduplicate by start date (not by summary — allows same label in different semesters)
        allday_evts <- allday_evts[!duplicated(paste0(allday_evts$summary, "|", allday_evts$start)), ]
        for (j in seq_len(nrow(allday_evts))) {
          ev_start <- as.Date(substr(allday_evts$start[j], 1, 10))
          ev_end <- as.Date(substr(allday_evts$end[j], 1, 10))
          if (is.na(ev_start) || is.na(ev_end)) next
          # Clip to visible week
          vis_start <- max(ev_start, week_sun)
          vis_end <- min(ev_end - 1, week_sat)  # end is exclusive in ICS
          if (vis_start > week_sat || vis_end < week_sun) next
          # Grid columns: 2=Sun, 3=Mon, ..., 8=Sat
          col_start <- as.integer(vis_start - week_sun) + 2
          col_end <- as.integer(vis_end - week_sun) + 3  # +1 because grid-column end is exclusive
          has_allday <- TRUE
          allday_spans <- c(allday_spans, list(
            tags$div(style = paste0(
              "grid-column:", col_start, "/", col_end, ";",
              "background:#fff3cd;color:#856404;border-left:3px solid #ffc107;",
              "padding:3px 8px;border-radius:4px;font-size:0.75rem;font-weight:700;",
              "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;"),
              allday_evts$summary[j])
          ))
        }
      }
    }

    tags$div(
      # Headers row (fixed)
      tags$div(class = "cal-wrapper cal-header-row", headers),
      # All-day events row (if any) — spanning banners
      if (has_allday) tags$div(class = "cal-allday-grid",
        style = "display:grid;grid-template-columns:55px repeat(7,1fr);border:1px solid #e2e8f0;border-top:none;border-bottom:none;background:#fffbeb;padding:3px 0;gap:2px 0;",
        tags$div(),  # spacer for time column
        allday_spans),
      # Scrollable body with time labels + day columns
      tags$div(class = "cal-scroll-container",
        style = "max-height:550px; overflow-y:auto; position:relative;",
        tags$div(class = "cal-wrapper cal-body-row",
          time_col,
          day_cols
        )
      )
    )
  })

  # ---- COURSES ----
  output$course_cards <- renderUI({
    rv$refresh
    cached <- all_grades_cache()
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
          tags$div(class = "small text-muted", paste0(c_info$id, " · ", c_info$credits, " cr · ", c_info$professor)),
          tags$div(class = "mt-2", paste0("Promedio: "),
            tags$b(class = paste0("text-", col), avg$partial), paste0(" (", avg$pct_graded, "% evaluado)")
          ),
          div(class = "progress mt-1 mb-2", style = "height:6px",
            div(class = paste0("progress-bar bg-", col), style = paste0("width:", avg$pct_graded, "%"))
          ),
          if (nrow(pending) > 0) tagList(
            tags$div(class = "small fw-bold", "Pendientes:"),
            lapply(seq_len(min(nrow(pending), 4)), function(j) {
              tags$div(class = "small text-muted", paste0("→ ", pending$label[j], " (", pending$weight[j], "%) S", pending$week[j]))
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

  # (Registration handled in custom auth section above)

  # ---- SYLLABUS UPLOAD ----
  observeEvent(input$syllabus_upload_btn, {
    req(input$syllabus_file)
    f <- input$syllabus_file
    ext <- tolower(tools::file_ext(f$name))
    cid <- input$syllabus_course
    text <- ""
    tryCatch({
      if (ext == "txt") text <- paste(readLines(f$datapath, warn=FALSE), collapse="\n")
      else if (ext == "pdf" && requireNamespace("pdftools", quietly=TRUE)) text <- paste(pdftools::pdf_text(f$datapath), collapse="\n")
      else if (ext == "docx" && requireNamespace("readtext", quietly=TRUE)) text <- readtext::readtext(f$datapath)$text
      if (nchar(text) > 0) {
        mg_syllabus_add(uid(), cid, f$name, text)
        rv$refresh <- rv$refresh + 1
        output$syllabus_status <- renderPrint(cat(paste0("✅ Sílabo '", f$name, "' subido (", nchar(text), " car.)")))
      }
    }, error = function(e) output$syllabus_status <- renderPrint(cat(paste0("❌ ", e$message))))
  })

  # Generate AI study guide from syllabus
  observeEvent(input$syllabus_gen_guide, {
    cid <- input$syllabus_course
    cname <- courses$name[courses$id == cid]
    topics <- course_topics[[cid]]
    syllabi <- mg_syllabi_get(uid(), cid)
    syllabus_text <- if (nrow(syllabi) > 0) paste(syllabi$content, collapse = "\n\n") else NULL

    if (is.null(topics) || length(topics) == 0) {
      output$syllabus_status <- renderPrint(cat("⚠ Este curso no tiene temas definidos."))
      return()
    }

    output$syllabus_status <- renderPrint(cat("⏳ Generando guía de estudio con IA... (30-60 seg)"))

    tryCatch({
      guide_text <- ai_generate_study_guide(cname, topics, syllabus_text)
      mg_note_add(uid(), cid, guide_text, paste0("Guía IA — ", courses$short[courses$id == cid]))
      rv$refresh <- rv$refresh + 1
      output$syllabus_status <- renderPrint(cat(paste0("✅ Guía generada para ", cname, ". Ve a la pestaña IA para verla.")))
    }, error = function(e) output$syllabus_status <- renderPrint(cat(paste0("❌ ", e$message))))
  })

  # ---- EXTRACT COURSE FROM SYLLABUS WITH AI ----
  rv_extract <- reactiveValues(data = NULL, text = NULL)

  # Helper to read file content
  read_syllabus_file <- function(filepath, ext) {
    if (ext == "txt") paste(readLines(filepath, warn = FALSE), collapse = "\n")
    else if (ext == "pdf" && requireNamespace("pdftools", quietly = TRUE)) paste(pdftools::pdf_text(filepath), collapse = "\n")
    else if (ext == "docx" && requireNamespace("readtext", quietly = TRUE)) readtext::readtext(filepath)$text
    else ""
  }

  observeEvent(input$syllabus_extract_btn, {
    req(input$syllabus_file)
    f <- input$syllabus_file
    ext <- tolower(tools::file_ext(f$name))

    shinyjs::disable("syllabus_extract_btn")
    shinyjs::html("syllabus_status_div",
      '<div class="alert alert-info py-2 small"><span class="spinner-border spinner-border-sm me-2"></span><b>Extrayendo curso con IA...</b> Esto puede tomar 15-30 segundos.</div>')

    # Read file
    text <- tryCatch(read_syllabus_file(f$datapath, ext), error = function(e) "")
    if (nchar(text) < 50) {
      shinyjs::html("syllabus_status_div", '<div class="alert alert-danger py-2 small">No se pudo leer el archivo o está vacío.</div>')
      shinyjs::enable("syllabus_extract_btn")
      return()
    }
    rv_extract$text <- text

    session$onFlushed(function() {
      tryCatch({
        result <- ai_extract_syllabus(text)
        if (!is.null(result$error)) {
          shinyjs::html("syllabus_status_div", paste0('<div class="alert alert-danger py-2 small">', result$error, '</div>'))
          shinyjs::enable("syllabus_extract_btn")
          return()
        }
        rv_extract$data <- result
        shinyjs::html("syllabus_status_div",
          '<div class="alert alert-success py-2 small">✅ Curso extraído. Revisa el preview y confirma para guardar.</div>')

        # Render preview
        evals <- result$evaluaciones
        eval_rows <- ""
        if (!is.null(evals) && (is.data.frame(evals) || is.list(evals))) {
          if (is.data.frame(evals)) {
            for (i in seq_len(nrow(evals))) {
              eval_rows <- paste0(eval_rows, '<tr><td>', evals$nombre[i], '</td><td>', evals$codigo[i],
                '</td><td>', evals$peso[i], '%</td><td>S', evals$semana[i], '</td><td>', evals$tipo[i], '</td></tr>')
            }
          }
        }
        temas_html <- ""
        if (!is.null(result$temas)) {
          temas_html <- paste0('<li>', result$temas, '</li>', collapse = "")
          temas_html <- paste0('<ul class="small mb-0">', temas_html, '</ul>')
        }

        preview_html <- paste0(
          '<div class="card shadow-sm"><div class="card-body py-3">',
          '<h6 class="fw-bold mb-1">', ifelse(is.null(result$nombre_curso), "Curso", result$nombre_curso), '</h6>',
          '<div class="small text-muted mb-2">',
            'Código: <b>', ifelse(is.null(result$codigo), "—", result$codigo), '</b> | ',
            'Créditos: <b>', ifelse(is.null(result$creditos), "—", result$creditos), '</b> | ',
            'Profesor: <b>', ifelse(is.null(result$profesor), "—", result$profesor), '</b>',
          '</div>',
          if (nchar(eval_rows) > 0) paste0(
            '<b class="small">Evaluaciones:</b>',
            '<table class="table table-sm table-bordered small mt-1 mb-2"><thead><tr>',
            '<th>Evaluación</th><th>Código</th><th>Peso</th><th>Semana</th><th>Tipo</th></tr></thead><tbody>',
            eval_rows, '</tbody></table>'
          ) else "",
          if (nchar(temas_html) > 0) paste0('<b class="small">Temas:</b>', temas_html) else "",
          '</div></div>'
        )

        output$syllabus_preview <- renderUI(HTML(preview_html))
        shinyjs::show("syllabus_confirm_div")
        shinyjs::enable("syllabus_confirm")
        shinyjs::enable("syllabus_cancel")
      }, error = function(e) {
        shinyjs::html("syllabus_status_div", paste0('<div class="alert alert-danger py-2 small">Error: ', e$message, '</div>'))
      })
      shinyjs::enable("syllabus_extract_btn")
    }, once = TRUE)
  })

  # Confirm: save extracted course + evaluations
  observeEvent(input$syllabus_confirm, {
    message("[StudyPilot] Confirm button clicked")
    req(rv_extract$data)
    d <- rv_extract$data
    message("[StudyPilot] Saving course: ", d$nombre_curso, " (", d$codigo, ")")

    # Show loading state
    shinyjs::disable("syllabus_confirm")
    shinyjs::disable("syllabus_cancel")
    shinyjs::html("syllabus_status_div",
      '<div class="alert alert-info py-2 small"><span class="spinner-border spinner-border-sm text-success me-2"></span><b>Guardando curso y evaluaciones...</b></div>')

    tryCatch({
      ensure_init()
      # Generate a course ID
      cid <- if (!is.null(d$codigo) && nchar(d$codigo) > 0) d$codigo else paste0("C", format(Sys.time(), "%H%M%S"))
      # Check for duplicate — if course already exists, delete old data first
      existing <- mg_custom_courses_get(uid())
      if (nrow(existing) > 0 && cid %in% existing$id) {
        mg_custom_course_delete(uid(), cid)
        message("[StudyPilot] Replaced existing course: ", cid)
      }
      cname <- if (!is.null(d$nombre_curso)) d$nombre_curso else "Curso sin nombre"
      short <- substr(gsub("[^A-Za-z0-9 ]", "", cname), 1, 12)
      credits <- if (!is.null(d$creditos)) as.integer(d$creditos) else 3L
      prof <- if (!is.null(d$profesor)) d$profesor else ""
      formula <- if (!is.null(d$formula)) d$formula else ""
      colors <- c("#2563eb", "#16a34a", "#7c3aed", "#ea580c", "#db2777", "#0891b2", "#d97706", "#059669")
      clr <- sample(colors, 1)

      # Save course to MongoDB
      mg_custom_course_add(uid(), cid, cname, short, credits, prof, formula, 5L, clr)

      # Save evaluations as activities
      evals <- d$evaluaciones
      if (!is.null(evals) && is.data.frame(evals) && nrow(evals) > 0) {
        for (i in seq_len(nrow(evals))) {
          ev_name <- evals$nombre[i]
          ev_weight <- suppressWarnings(as.numeric(evals$peso[i]))
          ev_week <- suppressWarnings(as.integer(evals$semana[i]))
          ev_type <- if (!is.null(evals$tipo[i])) evals$tipo[i] else "ec"
          ev_date <- if (!is.na(ev_week)) as.character(week_to_date(ev_week, 5L)) else as.character(Sys.Date())
          if (is.na(ev_weight)) ev_weight <- 0
          mg_activity_add(uid(), cid, ev_type, ev_name, ev_date, ev_weight, paste0("Semana ", ev_week))
        }
      }

      # Save syllabus text
      if (!is.null(rv_extract$text) && nchar(rv_extract$text) > 0) {
        mg_syllabus_add(uid(), cid, paste0("Sílabo_", short, ".pdf"), rv_extract$text)
      }

      # Save topics to study notes
      if (!is.null(d$temas) && length(d$temas) > 0) {
        topics_text <- paste0("Temas del curso ", cname, ":\n", paste(seq_along(d$temas), d$temas, sep = ". ", collapse = "\n"))
        mg_note_add(uid(), cid, topics_text, "Temas extraídos del sílabo")
        # Update course_topics in memory
        ct <- get0("course_topics", envir = globalenv())
        if (is.null(ct)) ct <- list()
        ct[[cid]] <- d$temas
        assign("course_topics", ct, envir = globalenv())
      }

      # Refresh global courses from MongoDB
      db_courses <- tryCatch(mg_custom_courses_get(uid()), error = function(e) data.frame())
      if (nrow(db_courses) > 0) assign("courses", tibble::as_tibble(db_courses), envir = globalenv())
      rv$refresh <- rv$refresh + 1
      rv_extract$data <- NULL
      rv_extract$text <- NULL
      output$syllabus_preview <- renderUI(NULL)
      shinyjs::hide("syllabus_confirm_div")
      shinyjs::html("syllabus_status_div",
        paste0('<div class="alert alert-success py-2 small">✅ Curso <b>', cname, '</b> creado con ',
          if (!is.null(evals) && is.data.frame(evals)) nrow(evals) else 0,
          ' evaluaciones. Ve al Dashboard para ver las actividades.</div>'))
    }, error = function(e) {
      shinyjs::html("syllabus_status_div", paste0('<div class="alert alert-danger py-2 small">❌ Error al guardar: ', e$message, '</div>'))
      shinyjs::enable("syllabus_confirm")
      shinyjs::enable("syllabus_cancel")
    })
  })

  # Cancel extraction
  observeEvent(input$syllabus_cancel, {
    rv_extract$data <- NULL
    rv_extract$text <- NULL
    output$syllabus_preview <- renderUI(NULL)
    shinyjs::hide("syllabus_confirm_div")
    shinyjs::html("syllabus_status_div", "")
  })

  output$syllabi_list <- renderUI({
    rv$refresh
    syllabi <- mg_syllabi_get(uid())
    if (nrow(syllabi) == 0) return(tags$p(class = "text-muted small", "Sin sílabos subidos."))
    lapply(seq_len(nrow(syllabi)), function(i) {
      s <- syllabi[i,]
      cname <- courses$short[match(s$course_id, courses$id)]
      div(class = "d-flex align-items-center gap-2 py-1 border-bottom small",
        tags$span(class = "badge bg-primary", cname),
        tags$span(s$filename),
        tags$span(class = "text-muted", paste0(nchar(s$content), " car."))
      )
    })
  })

  # ---- EMAIL NOTIFICATIONS ----
  observeEvent(input$send_notif, {
    email <- trimws(input$notif_email)
    if (nchar(email) < 5) {
      output$notif_status <- renderUI(tags$div(class = "alert alert-warning py-1 small mt-1", "Ingresa un email válido."))
      return()
    }

    a <- acts() |> filter(done == 0) |>
      mutate(days = as.integer(as.Date(date) - Sys.Date()),
             course = courses$short[match(course_id, courses$id)]) |>
      filter(days >= 0, days <= 14) |>
      arrange(date) |>
      select(course, name, weight, date, days)

    if (nrow(a) == 0) {
      output$notif_status <- renderUI(tags$div(class = "alert alert-info py-1 small mt-1", "No hay evaluaciones en las próximas 2 semanas."))
      return()
    }

    output$notif_status <- renderUI(tags$div(class = "alert alert-info py-1 small mt-1", "⏳ Enviando email..."))
    result <- send_eval_reminder(email, "Estudiante", a)
    output$notif_status <- renderUI(tags$div(class = paste0("alert py-1 small mt-1 ", if(grepl("✅", result)) "alert-success" else "alert-danger"), result))
  })

  # ---- CALENDAR NAVIGATION ----
  observeEvent(input$cal_prev, { rv$cal_week <- rv$cal_week - 1 })
  observeEvent(input$cal_next, { rv$cal_week <- rv$cal_week + 1 })
  observeEvent(input$cal_today, {
    days_diff <- as.integer(Sys.Date() - SEMESTER_START)
    rv$cal_week <- floor((days_diff + 1) / 7) + 1
  })

  # ---- SCHEDULE FROM PDF (AI) ----
  observeEvent(input$schedule_extract_btn, {
    f <- input$schedule_file
    if (is.null(f)) {
      shinyjs::html("schedule_status_div", '<div class="alert alert-warning py-1 small">Sube un PDF de horario primero.</div>')
      return()
    }
    shinyjs::disable("schedule_extract_btn")
    shinyjs::html("schedule_status_div",
      '<div class="alert alert-info py-1 small"><span class="spinner-border spinner-border-sm me-2"></span>Extrayendo horario con IA...</div>')

    # Capture reactive values BEFORE onFlushed (no reactive context inside)
    current_uid <- uid()

    session$onFlushed(function() {
      tryCatch({
        ext <- tolower(tools::file_ext(f$name))
        text <- if (ext == "pdf") paste(pdftools::pdf_text(f$datapath), collapse = "\n")
                else readLines(f$datapath, warn = FALSE) |> paste(collapse = "\n")

        message("[StudyPilot] Schedule PDF text length: ", nchar(text))
        if (nchar(text) < 50) {
          shinyjs::html("schedule_status_div", '<div class="alert alert-danger py-1 small">❌ El PDF no tiene texto extraíble (puede ser una imagen escaneada). Intenta con un PDF de texto.</div>')
          shinyjs::enable("schedule_extract_btn")
          return()
        }
        sched <- ai_extract_schedule(text)
        if (nrow(sched) == 0) {
          shinyjs::html("schedule_status_div", '<div class="alert alert-warning py-1 small">⚠️ No se encontraron bloques de horario en el PDF. Verifica que el documento contenga horarios con días y horas.</div>')
        } else {
          mg_schedule_set(current_uid, sched)
          rv$refresh <- isolate(rv$refresh) + 1
          shinyjs::html("schedule_status_div",
            paste0('<div class="alert alert-success py-1 small">✅ Horario extraído: ', nrow(sched), ' bloques de clase.</div>'))
        }
      }, error = function(e) {
        err_msg <- e$message
        if (grepl("429", err_msg)) err_msg <- "Demasiadas solicitudes al API de IA. Espera 1-2 minutos e intenta de nuevo."
        shinyjs::html("schedule_status_div", paste0('<div class="alert alert-danger py-1 small">❌ ', err_msg, '</div>'))
      })
      shinyjs::enable("schedule_extract_btn")
    }, once = TRUE)
  })

  observeEvent(input$schedule_clear_btn, {
    mg_schedule_set(uid(), data.frame())
    rv$refresh <- rv$refresh + 1
    shinyjs::html("schedule_status_div", '<div class="alert alert-info py-1 small">Horario limpiado.</div>')
  })

  # Render schedule grid (Google Calendar style)
  output$schedule_grid <- renderUI({
    rv$refresh
    sched <- tryCatch(mg_schedule_get(uid()), error = function(e) data.frame())
    if (nrow(sched) == 0) return(NULL)

    days <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    colors <- c("#3b82f6", "#22c55e", "#8b5cf6", "#f97316", "#ec4899", "#06b6d4", "#eab308", "#10b981")
    cursos_uniq <- unique(sched$curso)
    color_map <- setNames(colors[seq_along(cursos_uniq) %% length(colors) + 1], cursos_uniq)

    # Find time range
    all_hours <- c(
      as.integer(substr(sched$hora_inicio, 1, 2)),
      as.integer(substr(sched$hora_fin, 1, 2))
    )
    min_h <- max(7, min(all_hours, na.rm = TRUE))
    max_h <- min(23, max(all_hours, na.rm = TRUE) + 1)
    hours <- min_h:max_h

    # Build grid: each cell is 1 hour x 1 day
    grid_rows <- lapply(hours, function(h) {
      cells <- lapply(days, function(d) {
        # Find classes that START at this hour on this day
        matches <- which(sched$dia == d & as.integer(substr(sched$hora_inicio, 1, 2)) == h)
        if (length(matches) > 0) {
          s <- sched[matches[1], ]
          h_start <- as.integer(substr(s$hora_inicio, 1, 2))
          h_end <- as.integer(substr(s$hora_fin, 1, 2))
          span <- max(1, h_end - h_start)
          col <- color_map[s$curso]
          aula <- if (!is.null(s$aula) && nchar(s$aula) > 0 && s$aula != "TBD") s$aula else ""
          short_name <- if (nchar(s$curso) > 20) paste0(substr(s$curso, 1, 18), "...") else s$curso
          tags$td(
            rowspan = span,
            style = paste0("background:", col, ";color:white;font-size:11px;padding:4px 6px;",
                          "border-radius:6px;vertical-align:top;min-width:120px;"),
            tags$div(class = "fw-bold", short_name),
            tags$div(style = "opacity:0.9;font-size:10px", paste0(s$hora_inicio, "-", s$hora_fin)),
            if (nchar(aula) > 0) tags$div(style = "opacity:0.8;font-size:10px", paste0("📍 ", aula))
          )
        } else {
          # Check if this cell is covered by a rowspan from a previous hour
          covered <- FALSE
          for (prev_h in min_h:(h-1)) {
            prev_matches <- which(sched$dia == d & as.integer(substr(sched$hora_inicio, 1, 2)) == prev_h)
            if (length(prev_matches) > 0) {
              ps <- sched[prev_matches[1], ]
              ph_end <- as.integer(substr(ps$hora_fin, 1, 2))
              if (h < ph_end) { covered <- TRUE; break }
            }
          }
          if (!covered) tags$td(style = "min-width:120px;height:40px;") else NULL
        }
      })
      cells <- Filter(Negate(is.null), cells)
      tags$tr(
        tags$td(style = "font-size:11px;font-weight:bold;color:#64748b;white-space:nowrap;padding:4px 8px;vertical-align:top;",
                paste0(sprintf("%02d", h), ":00")),
        cells
      )
    })

    div(class = "card mt-2",
      div(class = "card-header py-2 d-flex align-items-center gap-2",
        tags$b("📋 Mi Horario de Clases"),
        tags$span(class = "badge bg-primary", paste0(nrow(sched), " bloques"))
      ),
      div(class = "card-body p-0", style = "overflow-x:auto;",
        tags$table(class = "table table-bordered mb-0", style = "border-collapse:collapse;",
          tags$thead(style = "background:#f1f5f9;",
            tags$tr(
              tags$th(style = "width:60px;font-size:12px;", "Hora"),
              lapply(days, function(d) {
                tags$th(style = "text-align:center;font-size:12px;min-width:120px;", d)
              })
            )
          ),
          tags$tbody(grid_rows)
        )
      )
    )
  })

  # ---- DOWNLOAD ICS ----
  output$download_ics <- downloadHandler(
    filename = function() {
      paste0("StudyPilot_actividades_", format(Sys.Date(), "%Y%m%d"), ".ics")
    },
    content = function(file) {
      a <- acts()
      # Get all courses (hardcoded + custom)
      all_courses <- courses
      custom <- tryCatch(mg_custom_courses_get(uid()), error = function(e) data.frame())
      if (nrow(custom) > 0) {
        custom_df <- data.frame(
          id = custom$id, name = custom$name, short = custom$short,
          credits = custom$credits, professor = if ("professor" %in% names(custom)) custom$professor else "",
          formula = if ("formula" %in% names(custom)) custom$formula else "",
          eval_day = if ("eval_day" %in% names(custom)) custom$eval_day else 5L,
          color = if ("color" %in% names(custom)) custom$color else "#666",
          stringsAsFactors = FALSE
        )
        all_courses <- rbind(all_courses, custom_df)
      }
      # Filter pending activities
      pending <- a[a$done == 0, ]
      if (nrow(pending) == 0) pending <- a
      ics_content <- generate_ics(pending, all_courses)
      writeLines(ics_content, file)
    },
    contentType = "text/calendar"
  )

  output$cal_week_label <- renderUI({
    w <- rv$cal_week
    ws <- SEMESTER_START + (w - 1) * 7
    we <- ws + 6
    tags$h5(class = "mb-0 fw-bold",
      paste0(format(ws, "%d %b"), " – ", format(we, "%d %b %Y"))
    )
  })

  # ---- GOOGLE CALENDAR ----
  rv_gcal <- reactiveValues(events = NULL)

  observeEvent(input$gcal_sync, {
    email <- trimws(input$gcal_email)
    if (nchar(email) < 5) {
      shinyjs::html("gcal_status_div", '<div class="alert alert-warning mt-2 small">Ingresa un email válido</div>')
      return()
    }

    # Clear old events FIRST before syncing new account
    rv_gcal$events <- NULL
    output$gcal_events_table <- renderUI(NULL)
    shinyjs::disable("gcal_sync")
    shinyjs::html("gcal_status_div",
      '<div class="alert alert-info mt-2 small py-2"><span class="spinner-border spinner-border-sm me-2" role="status"></span><b>Sincronizando calendario...</b> Esto puede tomar unos segundos.</div>')

    events <- gcal_get_events(email)

    shinyjs::enable("gcal_sync")
    if ("error" %in% names(events)) {
      shinyjs::html("gcal_status_div", paste0(
        '<div class="alert alert-danger mt-2 small">',
        '<b>❌ No se pudo conectar.</b> Tu calendario debe ser público.<br>',
        '<span class="text-muted">Ve a Google Calendar → Configuración → tu calendario → "Hacer disponible para el público"</span><br>',
        '<span class="text-muted small">Error: ', events$error, '</span></div>'))
    } else if (nrow(events) == 0) {
      shinyjs::html("gcal_status_div", '<div class="alert alert-warning mt-2 small">No se encontraron eventos en las próximas semanas.</div>')
    } else {
      rv_gcal$events <- events
      parsed <- gcal_parse_to_activities(events)
      n_exams <- sum(parsed$is_exam)
      shinyjs::html("gcal_status_div", paste0(
        '<div class="alert alert-success mt-2 small">✅ ', nrow(events),
        ' eventos encontrados (', n_exams, ' evaluaciones detectadas)</div>'))
    }
  })

  observeEvent(input$gcal_clear, {
    rv_gcal$events <- NULL
    shinyjs::html("gcal_status_div", '<div class="alert alert-secondary mt-2 small">Calendario limpiado. Sincroniza para ver eventos.</div>')
    output$gcal_events_table <- renderUI(NULL)
  })

  # Auto-clear events when email changes (user switching accounts)
  observeEvent(input$gcal_email, {
    rv_gcal$events <- NULL
    output$gcal_events_table <- renderUI(NULL)
  }, ignoreInit = TRUE)

  output$gcal_events_table <- renderUI({
    events <- rv_gcal$events
    if (is.null(events) || nrow(events) == 0) return(NULL)

    parsed <- gcal_parse_to_activities(events)

    rows <- lapply(seq_len(nrow(parsed)), function(i) {
      e <- parsed[i, ]
      is_exam <- e$is_exam
      bg <- if (is_exam) "bg-danger bg-opacity-10" else ""
      icon <- if (is_exam) "🔴" else "📌"
      tags$tr(class = bg,
        tags$td(class = "small", icon),
        tags$td(class = "small fw-bold", e$summary),
        tags$td(class = "small", format(e$date, "%a %d %b")),
        tags$td(class = "small text-muted", substr(e$start, 12, 16)),
        tags$td(class = "small text-muted", e$location)
      )
    })

    tags$div(class = "mt-3",
      tags$h6("Eventos del calendario:"),
      tags$table(class = "table table-sm table-hover",
        tags$thead(tags$tr(
          tags$th(""), tags$th("Evento"), tags$th("Fecha"), tags$th("Hora"), tags$th("Lugar")
        )),
        tags$tbody(rows)
      )
    )
  })

  # ---- COURSE MANAGEMENT (dynamic from MongoDB) ----
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

  # Update all course dropdowns when courses change
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

  observeEvent(input$add_course_btn, {
    id <- trimws(input$new_course_id)
    name <- trimws(input$new_course_name)
    short <- trimws(input$new_course_short)
    if (nchar(id) < 2 || nchar(name) < 3) {
      showNotification("Completa código y nombre del curso", type = "error")
      return()
    }
    tryCatch({
      mg_custom_course_add(uid(), id, name, short, input$new_course_credits,
        input$new_course_prof, "", as.integer(input$new_course_day), input$new_course_color)
      showNotification(paste0("✅ Curso '", short, "' agregado"), type = "message")
      rv$refresh <- rv$refresh + 1
    }, error = function(e) showNotification(paste0("❌ Error: ", e$message), type = "error"))
  })

  observeEvent(input$del_course_btn, {
    cid <- input$del_course_id
    if (is.null(cid) || nchar(cid) == 0) return()
    tryCatch({
      mg_custom_course_delete(uid(), cid)
      # Refresh global courses
      db_courses <- tryCatch(mg_custom_courses_get(uid()), error = function(e) data.frame())
      if (nrow(db_courses) > 0) {
        assign("courses", tibble::as_tibble(db_courses), envir = globalenv())
      } else {
        assign("courses", tibble::tibble(id=character(), name=character(), short=character(),
          credits=integer(), professor=character(), formula=character(), eval_day=integer(), color=character()), envir = globalenv())
      }
      showNotification(paste0("🗑 Curso '", cid, "' eliminado"), type = "warning")
      rv$refresh <- rv$refresh + 1
    }, error = function(e) showNotification(paste0("❌ Error: ", e$message), type = "error"))
  })

  # ---- POMODORO (JS bridge) ----
  observeEvent(input$pomo_session_done, {
    cid <- input$pomo_course
    dur <- as.integer(input$pomo_duration)
    mg_pomo_add(uid(), cid, dur)
  })

  # ---- PRACTICE EXAM ----
  rv_quiz <- reactiveValues(exam = NULL, submitted = FALSE, results = NULL, loading = FALSE, error_msg = NULL)

  observeEvent(input$quiz_generate, {
    shinyjs::disable("quiz_generate")
    course_id <- input$quiz_course
    n_q <- input$quiz_n
    q_type <- input$quiz_type

    # Use reactive state instead of overwriting output
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
          rv_quiz$error_msg <- "No se pudieron generar preguntas. Verifica que el curso tenga temas (sube un sílabo primero)."
        }
      }, error = function(e) {
        rv_quiz$error_msg <- paste0("Error: ", e$message)
      })
      rv_quiz$loading <- FALSE
      shinyjs::enable("quiz_generate")
    }, once = TRUE)
  })

  output$quiz_content <- renderUI({
    # Loading state
    if (isTRUE(rv_quiz$loading)) {
      return(tags$div(class = "text-center py-5",
        tags$div(class = "spinner-border text-primary"),
        tags$p(class = "mt-2 text-muted", "Generando preguntas con IA... (15-30 seg)")
      ))
    }
    # Error state
    if (!is.null(rv_quiz$error_msg)) {
      return(tags$div(class = "alert alert-warning", rv_quiz$error_msg))
    }
    # Empty state
    exam <- rv_quiz$exam
    if (is.null(exam)) return(tags$div(class = "text-center text-muted py-5",
      tags$h5("🎲 Haz clic en 'Generar' para comenzar"),
      tags$p("Se generarán preguntas basadas en los temas del curso.")
    ))

    cname <- courses$short[courses$id == input$quiz_course]
    questions_ui <- lapply(seq_along(exam), function(i) {
      q <- exam[[i]]
      div(class = "mb-4 p-3 border rounded",
        tags$h6(class = "fw-bold", paste0("Pregunta ", i, " — ", q$topic)),
        tags$p(q$q),
        if (q$type == "mc") {
          radioButtons(paste0("quiz_ans_", i), NULL,
                      choices = setNames(seq_along(q$opts), q$opts),
                      selected = character(0), width = "100%")
        } else {
          textAreaInput(paste0("quiz_ans_", i), "Tu respuesta:",
                       rows = 4, width = "100%", placeholder = "Escribe tu respuesta aquí...")
        }
      )
    })

    tagList(
      tags$div(class = "alert alert-info py-2",
        paste0("📝 ", cname, " — ", length(exam), " preguntas")
      ),
      questions_ui
    )
  })

  observeEvent(input$quiz_submit, {
    exam <- rv_quiz$exam
    if (is.null(exam)) return()

    answers <- lapply(seq_along(exam), function(i) {
      input[[paste0("quiz_ans_", i)]]
    })

    rv_quiz$results <- grade_exam(answers, exam)
    rv_quiz$submitted <- TRUE
    updateActionButton(session, "quiz_submit", disabled = TRUE)
  })

  output$quiz_score_summary <- renderUI({
    res <- rv_quiz$results
    if (is.null(res)) return(NULL)

    col <- if (!is.na(res$pct) && res$pct >= 70) "success" else if (!is.na(res$pct) && res$pct >= 50) "warning" else "danger"
    n_correct <- sum(sapply(res$results, function(r) isTRUE(r$correct)))
    n_wrong <- sum(sapply(res$results, function(r) isFALSE(r$correct)))
    n_open <- sum(sapply(res$results, function(r) is.na(r$correct)))
    div(class = paste0("alert alert-", col, " mt-3"),
      div(class = "text-center",
        tags$h2(class = "mb-1 fw-bold", paste0(res$score, "/", res$total)),
        if (!is.na(res$pct)) tags$h5(paste0(res$pct, "% de acierto")) else NULL
      ),
      tags$hr(class = "my-2"),
      div(class = "d-flex justify-content-around text-center",
        div(tags$span(class = "fs-4 text-success fw-bold", n_correct), tags$br(), tags$small("✅ Correctas")),
        div(tags$span(class = "fs-4 text-danger fw-bold", n_wrong), tags$br(), tags$small("❌ Incorrectas")),
        if (n_open > 0) div(tags$span(class = "fs-4 text-primary fw-bold", n_open), tags$br(), tags$small("📝 Abiertas"))
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
      icon <- if (is_open) "📝" else if (is_correct) "✅" else "❌"
      bg <- if (is_open) "light" else if (is_correct) "success" else "danger"

      div(class = paste0("alert alert-", bg, " py-2 mb-2"),
        tags$div(class = "fw-bold", paste0(icon, " Pregunta ", i, ": ", r$question)),
        if (!is_open) tagList(
          tags$div(class = "small", paste0("Tu respuesta: ", r$user_answer)),
          if (!is_correct) tags$div(class = "small fw-bold", paste0("Respuesta correcta: ", r$right_answer))
        ) else tagList(
          tags$div(class = "small", paste0("Tu respuesta: ", substr(r$user_answer, 1, 200))),
          tags$div(class = "small fw-bold text-primary", paste0("Guía: ", r$right_answer))
        ),
        tags$div(class = "small text-muted fst-italic mt-1", r$explanation)
      )
    })

    tagList(items)
  })

  # ---- AI TAB HANDLERS ----
  rv_ai <- reactiveValues(result = NULL, text = NULL)

  observeEvent(input$ai_study_file, {
    req(input$ai_study_file)
    f <- input$ai_study_file
    ext <- tolower(tools::file_ext(f$name))
    text <- ""
    tryCatch({
      if (ext %in% c("txt","csv")) text <- paste(readLines(f$datapath, warn=FALSE), collapse="\n")
      else if (ext == "pdf" && requireNamespace("pdftools", quietly=TRUE)) text <- paste(pdftools::pdf_text(f$datapath), collapse="\n")
      else if (ext == "docx" && requireNamespace("readtext", quietly=TRUE)) text <- readtext::readtext(f$datapath)$text
      else if (ext %in% c("xlsx","xls") && requireNamespace("readxl", quietly=TRUE)) {
        sheets <- readxl::excel_sheets(f$datapath)
        text <- paste(sapply(sheets, function(s) paste(capture.output(print(readxl::read_excel(f$datapath, sheet=s))), collapse="\n")), collapse="\n\n")
      }
      rv_ai$text <- text
      output$ai_upload_status <- renderPrint(cat(paste0("✅ '", f$name, "' cargado (", nchar(text), " caracteres). Listo para procesar con IA.")))
    }, error = function(e) output$ai_upload_status <- renderPrint(cat(paste0("❌ Error: ", e$message))))
  })

  observeEvent(input$ai_gen_summary, {
    req(rv_ai$text)
    if (nchar(rv_ai$text) < 50) { output$ai_result_content <- renderUI(tags$div(class="alert alert-warning","Sube un archivo primero.")); return() }

    # Capture reactive values BEFORE onFlushed (which runs outside reactive context)
    text_val <- rv_ai$text
    cid <- input$ai_upload_course
    cname <- courses$name[courses$id == cid]

    shinyjs::disable("ai_gen_summary")
    current_uid <- uid()
    output$ai_result_content <- renderUI(tags$div(class="alert alert-info py-3 text-center",
      HTML('<span class="spinner-border spinner-border-sm me-2" role="status"></span><b>Generando resumen con IA...</b><br><small class="text-muted">Esto puede tomar 15-30 segundos</small>')))

    session$onFlushed(function() {
      tryCatch({
        response <- ai_generate_summary(text_val, cname)
        if (grepl("^Error al generar", response)) {
          output$ai_result_content <- renderUI(tags$div(class="alert alert-danger", paste0("❌ ", response)))
        } else {
          mg_note_add(current_uid, cid, response, "Resumen IA")
          isolate({ rv$refresh <- rv$refresh + 1 })
          # Convert mermaid code blocks to renderable divs
          html_response <- markdown::markdownToHTML(text = response, fragment.only = TRUE)
          # Replace <code class="mermaid"> or <pre><code> mermaid blocks
          html_response <- gsub(
            '<pre><code class="mermaid">(.+?)</code></pre>',
            '<div class="mermaid">\\1</div>',
            html_response, perl = TRUE
          )
          # Also handle ```mermaid blocks that markdown might render as plain <pre><code>
          html_response <- gsub(
            '<pre><code>\\s*(graph\\s+(?:TD|LR|TB|BT|RL)[\\s\\S]*?)</code></pre>',
            '<div class="mermaid">\\1</div>',
            html_response, perl = TRUE
          )
          output$ai_result_content <- renderUI(tags$div(
            tags$div(class="alert alert-success py-2","✅ Resumen generado y guardado"),
            tags$div(class="ai-formatted", style="max-height:600px;overflow-y:auto",
              HTML(html_response)),
            tags$script(HTML("setTimeout(function(){try{mermaid.run()}catch(e){}},300);"))
          ))
        }
      }, error = function(e) {
        output$ai_result_content <- renderUI(tags$div(class="alert alert-danger", paste0("❌ ", e$message)))
      })
      shinyjs::enable("ai_gen_summary")
    }, once = TRUE)
  })

  observeEvent(input$ai_gen_questions2, {
    req(rv_ai$text)
    if (nchar(rv_ai$text) < 50) { output$ai_result_content <- renderUI(tags$div(class="alert alert-warning","Sube un archivo primero.")); return() }

    # Capture reactive values BEFORE onFlushed
    text_val <- rv_ai$text
    cid <- input$ai_upload_course
    cname <- courses$name[courses$id == cid]

    shinyjs::disable("ai_gen_questions2")
    output$ai_result_content <- renderUI(tags$div(class="alert alert-info py-3 text-center",
      HTML('<span class="spinner-border spinner-border-sm me-2" role="status"></span><b>Generando preguntas con IA...</b><br><small class="text-muted">Esto puede tomar 15-30 segundos</small>')))

    session$onFlushed(function() {
      tryCatch({
        ai_qs <- ai_generate_questions(text_val, cname, n_questions=8)
        if (!is.null(ai_qs$error)) { output$ai_result_content <- renderUI(tags$div(class="alert alert-danger", ai_qs$error)); shinyjs::enable("ai_gen_questions2"); return() }
        if (is.null(exam_questions[[cid]])) exam_questions[[cid]] <<- list()
        exam_questions[[cid]] <<- c(exam_questions[[cid]], ai_qs)
        output$ai_result_content <- renderUI(tags$div(
          tags$div(class="alert alert-success py-2", paste0("✅ ", length(ai_qs), " preguntas generadas. Ve a 'Examen Práctica' para usarlas.")),
          lapply(seq_along(ai_qs), function(i) {
            q <- ai_qs[[i]]
            tipo_badge <- if (q$type == "mc") {
              tags$span(class="badge bg-primary ms-2", "Opción Múltiple")
            } else {
              tags$span(class="badge bg-info ms-2", "Abierta")
            }
            tema_badge <- if (!is.null(q$topic) && nchar(q$topic) > 0 && nchar(q$topic) < 60) {
              tags$span(class="badge bg-secondary ms-2", style="max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;", q$topic)
            }

            opciones_ui <- NULL
            if (q$type == "mc" && length(q$opts) > 0) {
              letras <- c("a", "b", "c", "d", "e", "f")
              opciones_ui <- tags$div(class="mt-2 ps-3",
                lapply(seq_along(q$opts), function(j) {
                  is_correct <- (!is.null(q$ans) && j == q$ans)
                  tags$div(class = paste0("py-1", if(is_correct) " fw-bold text-success" else ""),
                    tags$span(class="me-2", paste0(letras[j], ")")),
                    q$opts[j],
                    if(is_correct) tags$span(class="ms-1", "✓")
                  )
                })
              )
            }

            resp_text <- if (q$type == "mc" && !is.null(q$ans)) {
              paste0("Respuesta correcta: opción ", q$ans)
            } else if (!is.null(q$ans_guide)) {
              q$ans_guide
            } else ""

            collapse_id <- paste0("q_collapse_", i)
            tags$div(class="card mb-3 shadow-sm", style="border-left:4px solid #2563eb;",
              tags$div(class="card-body py-2 px-3",
                tags$div(class="d-flex align-items-center mb-1",
                  tags$span(class="fw-bold", style="color:#2563eb;font-size:0.95rem;", paste0("Pregunta ", i)),
                  tipo_badge, tema_badge
                ),
                tags$p(class="mb-1", style="font-size:0.9rem;", q$q),
                opciones_ui,
                tags$div(class="mt-2",
                  tags$a(class="btn btn-sm btn-outline-secondary py-0", `data-bs-toggle`="collapse",
                    href=paste0("#", collapse_id), "Ver explicación"),
                  tags$div(id=collapse_id, class="collapse mt-2",
                    tags$div(class="alert alert-light py-2 small mb-0",
                      if (nchar(resp_text) > 0) tags$div(tags$b("Respuesta: "), resp_text),
                      if (!is.null(q$expl) && nchar(q$expl) > 0) tags$div(class="mt-1", tags$b("Explicación: "), q$expl)
                    )
                  )
                )
              )
            )
          })
        ))
      }, error = function(e) {
        output$ai_result_content <- renderUI(tags$div(class="alert alert-danger", paste0("❌ ", e$message)))
      })
      shinyjs::enable("ai_gen_questions2")
    }, once = TRUE)
  })

  output$ai_processed_notes <- renderUI({
    rv$refresh
    notes <- mg_notes_all(uid())
    ai_notes <- notes[grepl("IA|RESUMEN|organizado", notes$source, ignore.case=TRUE), ]
    if (nrow(ai_notes) == 0) return(tags$p(class="text-muted small","Aún no has procesado material con IA."))

    items <- lapply(seq_len(nrow(ai_notes)), function(i) {
      n <- ai_notes[i,]
      cname <- courses$short[match(n$course_id, courses$id)]
      nid <- if("note_id" %in% names(n)) n$note_id else i
      preview <- substr(gsub("\\s+", " ", n$text_content), 1, 80)

      tags$details(class = "mb-1 border rounded",
        tags$summary(class = "ai-note-compact",
          tags$span(class = "badge bg-primary", cname),
          tags$span(class = "fw-bold", n$source),
          tags$span(class = "note-preview", preview),
          actionButton(paste0("del_ainote_", nid), "✕", class = "btn-sm btn-outline-danger py-0 px-1")
        ),
        tags$div(class = "ai-formatted p-3", style = "max-height:400px;overflow-y:auto",
          HTML(tryCatch(markdown::markdownToHTML(text = substr(n$text_content, 1, 5000), fragment.only = TRUE),
                       error = function(e) paste0("<pre>", substr(n$text_content, 1, 5000), "</pre>")))
        )
      )
    })
    tagList(items)
  })

  # Delete individual AI notes
  observe({
    rv$refresh
    notes <- mg_notes_all(uid())
    ai_notes <- notes[grepl("IA|RESUMEN|organizado", notes$source, ignore.case=TRUE), ]
    if (nrow(ai_notes) == 0) return()
    note_ids <- if("note_id" %in% names(ai_notes)) ai_notes$note_id else seq_len(nrow(ai_notes))
    lapply(note_ids, function(nid) {
      observeEvent(input[[paste0("del_ainote_", nid)]], {
        mg_note_delete(uid(), nid)
        rv$refresh <- rv$refresh + 1
        showNotification("Nota eliminada", type="warning")
      }, ignoreInit=TRUE, once=TRUE)
    })
  })

  # Delete ALL notes
  observeEvent(input$del_all_notes, {
    notes <- mg_notes_all(uid())
    if (nrow(notes) == 0) return()
    note_ids <- if("note_id" %in% names(notes)) notes$note_id else seq_len(nrow(notes))
    for (nid in note_ids) mg_note_delete(uid(), nid)
    rv$refresh <- rv$refresh + 1
    showNotification("Todas las notas eliminadas", type="warning")
  })

  # ---- AI CHAT ----
  rv_chat <- reactiveValues(messages = list(
    list(role = "ai", text = "¡Hola! Soy tu asistente de estudio. Pregúntame sobre cualquier tema de tus cursos.")
  ))

  observeEvent(input$chat_send, {
    q <- trimws(input$chat_input)
    if (nchar(q) < 2) return()

    rv_chat$messages <- c(rv_chat$messages, list(list(role = "user", text = q)))
    updateTextInput(session, "chat_input", value = "")

    # Show thinking indicator
    rv_chat$messages <- c(rv_chat$messages, list(list(role = "ai", text = "⏳ Pensando...")))

    tryCatch({
      chat <- get_gemini()
      prompt <- paste0(
        "Eres un tutor académico de apoyo universitario. ",
        "Responde de forma clara, concisa y en español. ",
        "Si es un concepto, da definición + ejemplo práctico. ",
        "Pregunta del estudiante: ", q
      )
      response <- chat$chat(prompt)
      message("[StudyPilot] AI chat response length: ", nchar(response))
      msgs <- rv_chat$messages
      msgs[[length(msgs)]] <- list(role = "ai", text = as.character(response))
      rv_chat$messages <- msgs
    }, error = function(e) {
      message("[StudyPilot] AI chat ERROR: ", e$message)
      err_msg <- if (grepl("429", e$message)) {
        "⏳ La IA está temporalmente saturada (demasiadas solicitudes). Espera 1-2 minutos e intenta de nuevo."
      } else if (grepl("503", e$message)) {
        "⏳ El servicio de IA no está disponible momentáneamente. Intenta de nuevo en unos segundos."
      } else {
        paste0("❌ Error: ", e$message)
      }
      msgs <- rv_chat$messages
      msgs[[length(msgs)]] <- list(role = "ai", text = err_msg)
      rv_chat$messages <- msgs
    })
  })

  output$chat_display <- renderUI({
    msgs <- rv_chat$messages
    msg_divs <- lapply(msgs, function(m) {
      cls <- if (m$role == "user") "chat-msg chat-user" else "chat-msg chat-ai"
      if (m$role == "ai") {
        tags$div(class = cls, HTML(tryCatch(
          markdown::markdownToHTML(text = m$text, fragment.only = TRUE),
          error = function(e) m$text
        )))
      } else {
        tags$div(class = cls, m$text)
      }
    })
    tags$div(class = "chat-container", id = "chat_messages_inner", msg_divs,
      tags$script(HTML("var c=document.getElementById('chat_messages_inner');if(c)c.scrollTop=c.scrollHeight;")))
  })
  # Render even when chat panel is hidden (display:none)
  outputOptions(output, "chat_display", suspendWhenHidden = FALSE)

  # ---- AI QUESTION GENERATION ----
  observeEvent(input$ai_gen_questions, {
    cid <- input$upload_course
    notes <- mg_notes_all(uid())
    course_notes <- notes[notes$course_id == cid, ]

    if (nrow(course_notes) == 0) {
      output$ai_questions_status <- renderUI(
        tags$div(class = "alert alert-warning py-2 small mt-2", "Primero sube material para este curso.")
      )
      return()
    }

    output$ai_questions_status <- renderUI(
      tags$div(class = "alert alert-info py-2 small mt-2", "⏳ Generando preguntas con IA... (puede tardar 15-30 segundos)")
    )

    # Combine all notes text for this course
    all_text <- paste(course_notes$text_content, collapse = "\n\n---\n\n")
    course_name <- courses$name[courses$id == cid]

    tryCatch({
      ai_qs <- ai_generate_questions(all_text, course_name, n_questions = 8)

      if (!is.null(ai_qs$error)) {
        output$ai_questions_status <- renderUI(
          tags$div(class = "alert alert-danger py-2 small mt-2", ai_qs$error)
        )
      } else if (length(ai_qs) > 0) {
        # Add to exam bank temporarily (in session)
        if (is.null(exam_questions[[cid]])) exam_questions[[cid]] <<- list()
        exam_questions[[cid]] <<- c(exam_questions[[cid]], ai_qs)

        output$ai_questions_status <- renderUI(
          tags$div(class = "alert alert-success py-2 small mt-2",
            paste0("✅ ", length(ai_qs), " preguntas generadas con IA y añadidas al banco de ", courses$short[courses$id == cid],
                   ". Ve a 'Examen Práctica' para usarlas."))
        )
      }
    }, error = function(e) {
      output$ai_questions_status <- renderUI(
        tags$div(class = "alert alert-danger py-2 small mt-2", paste0("❌ Error: ", e$message))
      )
    })
  })

  # ---- TEXT ORGANIZATION (enhanced file upload) ----
  observeEvent(input$study_file, {
    req(input$study_file)
    f <- input$study_file
    ext <- tolower(tools::file_ext(f$name))
    cid <- input$upload_course
    text <- ""

    tryCatch({
      if (ext == "txt" || ext == "csv") {
        text <- paste(readLines(f$datapath, warn = FALSE), collapse = "\n")
      } else if (ext == "pdf") {
        if (requireNamespace("pdftools", quietly = TRUE)) {
          text <- paste(pdftools::pdf_text(f$datapath), collapse = "\n")
        } else text <- "Error: instala pdftools"
      } else if (ext == "docx") {
        if (requireNamespace("readtext", quietly = TRUE)) {
          text <- readtext::readtext(f$datapath)$text
        } else text <- "Error: instala readtext"
      } else if (ext %in% c("xlsx", "xls")) {
        if (requireNamespace("readxl", quietly = TRUE)) {
          sheets <- readxl::excel_sheets(f$datapath)
          text <- paste(sapply(sheets, function(s) {
            df <- readxl::read_excel(f$datapath, sheet = s)
            paste0("[", s, "]\n", paste(capture.output(print(df)), collapse = "\n"))
          }), collapse = "\n\n")
        } else text <- "Error: instala readxl"
      }

      if (nchar(text) > 0 && !startsWith(text, "Error")) {
        # Organize by topics
        organized <- organize_text_by_topics(text, cid)

        # Save organized version
        summary_parts <- sapply(organized$by_topic, function(r) {
          if (length(r$paragraphs) > 0) {
            paste0("=== ", r$topic, " ===\n", paste(r$paragraphs, collapse = "\n\n"))
          } else NULL
        })
        summary_parts <- Filter(Nonnull <- function(x) !is.null(x), summary_parts)

        organized_text <- if (length(summary_parts) > 0) {
          paste0("📚 MATERIAL ORGANIZADO POR TEMAS\n\n",
                 paste(summary_parts, collapse = "\n\n---\n\n"),
                 if (length(organized$unmatched) > 0) paste0("\n\n=== SIN CLASIFICAR ===\n", paste(organized$unmatched, collapse = "\n\n")) else "")
        } else {
          paste0("📄 TEXTO COMPLETO (sin coincidencias por tema)\n\n", text)
        }

        mg_note_add(uid(), cid, organized_text, paste0(f$name, " [organizado]"))

        # Generate AI summary if text is substantial
        if (nchar(text) > 100) {
          tryCatch({
            course_name <- courses$name[courses$id == cid]
            ai_summary <- ai_generate_summary(text, course_name)
            if (!startsWith(ai_summary, "Error")) {
              mg_note_add(uid(), cid, ai_summary, paste0(f$name, " [RESUMEN IA]"))
            }
          }, error = function(e) {
            # AI failed, just continue with organized text
          })
        }
        rv$refresh <- rv$refresh + 1

        n_matched <- sum(sapply(organized$by_topic, function(r) length(r$paragraphs)))
        output$upload_status <- renderPrint(cat(paste0(
          "✅ '", f$name, "' procesado\n",
          "📊 ", n_matched, " secciones clasificadas por tema\n",
          "📝 ", length(organized$unmatched), " secciones sin clasificar\n",
          "💾 Guardado como nota organizada"
        )))
      } else {
        output$upload_status <- renderPrint(cat(text))
      }
    }, error = function(e) {
      output$upload_status <- renderPrint(cat(paste0("❌ Error: ", e$message)))
    })
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
