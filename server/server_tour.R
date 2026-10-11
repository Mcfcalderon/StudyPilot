# ============ server_tour.R — Visita guiada para usuarios nuevos ============
# Sourced con local=TRUE. Modal paso a paso (sin dependencias externas).
# Se abre automaticamente en el primer login y desde el boton "Guia" del navbar.

.tour_steps <- list(
  list(t = "\U0001F44B \u00a1Bienvenido a StudyPilot!",
       b = "Tu copiloto acad\u00e9mico: organiza cursos, notas, actividades y calendario, y estudia con ayuda de IA. Te muestro lo esencial en 1 minuto."),
  list(t = "\U0001F4DA Cursos",
       b = "Empieza aqu\u00ed: sube el PDF de tus s\u00edlabos y StudyPilot extrae autom\u00e1ticamente tus <b>evaluaciones, pesos, semanas y temas</b> de cada curso."),
  list(t = "\U0001F4DD Notas",
       b = "Registra tus calificaciones por evaluaci\u00f3n. La app calcula tu <b>promedio ponderado</b> y cu\u00e1nto necesitas para aprobar cada curso."),
  list(t = "\u2705 Actividades",
       b = "Todas tus evaluaciones y entregas con su <b>semana, fecha y d\u00edas restantes</b>. Filtra por curso, tipo, estado o prioridad."),
  list(t = "\U0001F4C5 Calendario",
       b = "Vista de tu ciclo por semanas. Fija el <b>Inicio del ciclo (Semana 1)</b> para que las fechas y semanas se ordenen correctamente."),
  list(t = "\U0001F4D6 Ex\u00e1menes",
       b = "Genera <b>preguntas de pr\u00e1ctica</b> y gu\u00edas de estudio por curso a partir de los temas de tus s\u00edlabos."),
  list(t = "\U0001F345 Estudio (Pomodoro)",
       b = "Sesiones de enfoque con temporizador <b>Pomodoro</b> para estudiar sin distracciones y registrar tu tiempo."),
  list(t = "\U0001F4CA Analytics",
       b = "M\u00e9tricas de tu preparaci\u00f3n y progreso acad\u00e9mico de un vistazo, para saber en qu\u00e9 enfocarte."),
  list(t = "\U0001F916 Asistente IA",
       b = "El bot\u00f3n flotante abre un chat que responde <b>sobre TUS datos</b>: tus notas, tus pr\u00f3ximas evaluaciones y qu\u00e9 priorizar."),
  list(t = "\U0001F680 \u00a1Listo!",
       b = "Sugerencia: ve a <b>Cursos</b> y sube tu primer s\u00edlabo. Puedes reabrir esta gu\u00eda cuando quieras con el bot\u00f3n <b>\u2753 Gu\u00eda</b> del men\u00fa.")
)

rv_tour <- reactiveVal(1L)

show_tour_at <- function(step) {
  n <- length(.tour_steps)
  step <- max(1L, min(as.integer(step), n))
  s <- .tour_steps[[step]]
  showModal(modalDialog(
    title = s$t, easyClose = TRUE, fade = TRUE,
    tags$div(style = "font-size:0.95rem;line-height:1.6", HTML(s$b)),
    tags$div(class = "text-muted small mt-3", paste0("Paso ", step, " de ", n)),
    tags$div(class = "progress mt-1", style = "height:5px",
      tags$div(class = "progress-bar bg-primary",
        style = paste0("width:", round(step / n * 100), "%"))),
    footer = tagList(
      if (step > 1L) actionButton("tour_prev", "\u2190 Anterior", class = "btn-sm btn-outline-secondary"),
      if (step < n) actionButton("tour_next", "Siguiente \u2192", class = "btn-sm btn-primary")
      else actionButton("tour_finish", "Empezar \U0001F680", class = "btn-sm btn-success"),
      modalButton("Cerrar")
    )
  ))
}

observeEvent(input$show_tour, { rv_tour(1L); show_tour_at(1L) })
observeEvent(input$tour_next, { rv_tour(rv_tour() + 1L); show_tour_at(rv_tour()) })
observeEvent(input$tour_prev, { rv_tour(rv_tour() - 1L); show_tour_at(rv_tour()) })
observeEvent(input$tour_finish, {
  removeModal()
  tryCatch(updateTabsetPanel(session, "main_nav", selected = "Cursos"),
           error = function(e) NULL)
})

# Auto-mostrar en el PRIMER login (una sola vez; se marca en settings del usuario)
observeEvent(auth_user(), {
  if (is.null(auth_user())) return()
  seen <- tryCatch(mg_settings_get(uid())$tour_seen, error = function(e) NULL)
  if (is.null(seen) || length(seen) == 0) {
    rv_tour(1L); show_tour_at(1L)
    tryCatch(mg_settings_set(uid(), "tour_seen", "1"), error = function(e) NULL)
  }
}, once = TRUE, ignoreInit = TRUE)
