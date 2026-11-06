# ============ server_canvas.R — Entregas de Canvas en el calendario (feed ICS, sin token) ============
# Sourced con local=TRUE. Logica pura en R/canvas_feed.R. Las entregas quedan en
# rv_gcal$canvas_events (aparte de Google: no cuentan como tiempo ocupado para el scheduler).
# El enlace se guarda en settings$canvas_ics_url y se resincroniza solo al iniciar sesion.

canvas_status <- function(html) shinyjs::html("canvas_status_div", html)

# url ya validada o no; user_id capturado antes (puede correr fuera de contexto reactivo)
canvas_sync <- function(url, user_id, save = TRUE, quiet = FALSE) {
  ev <- canvas_get_events(url)
  if ("error" %in% names(ev)) {
    if (!quiet) canvas_status(paste0('<div class="alert alert-warning py-1 small mt-1">',
                                     esc(ev$error[1]), '</div>'))
    return(invisible(FALSE))
  }
  rv_gcal$canvas_events <- ev
  if (save) tryCatch(mg_settings_set(user_id, "canvas_ics_url", trimws(url)),
                     error = function(e) message("[StudyPilot] canvas url save: ", conditionMessage(e)))
  futuras <- sum(substr(ev$start, 1, 10) >= as.character(Sys.Date()))
  canvas_status(paste0('<div class="alert alert-success py-1 small mt-1">\U0001F4CC Canvas: ',
                       futuras, ' entrega(s) pendientes en el calendario.</div>'))
  rv$refresh <- isolate(rv$refresh) + 1
  invisible(TRUE)
}

observeEvent(input$canvas_sync, {
  url <- input$canvas_ics_url
  if (!valid_canvas_feed_url(url)) {
    canvas_status(paste0('<div class="alert alert-warning py-1 small mt-1">Pega el enlace de ',
      '<b>Canvas → Calendario → Fuente del calendario</b> ',
      '(https://…instructure.com/feeds/calendars/user_….ics).</div>'))
    return()
  }
  shinyjs::disable("canvas_sync")
  canvas_status('<div class="alert alert-info py-1 small mt-1"><span class="spinner-border spinner-border-sm me-2"></span>Leyendo Canvas...</div>')
  current_uid <- uid()
  session$onFlushed(function() {
    tryCatch(canvas_sync(url, current_uid),
             error = function(e) canvas_status(paste0('<div class="alert alert-danger py-1 small mt-1">',
                                                      esc(user_error(e, "canvas_sync")), '</div>')))
    shinyjs::enable("canvas_sync")
  }, once = TRUE)
})

observeEvent(input$canvas_clear, {
  rv_gcal$canvas_events <- NULL
  tryCatch(mg_settings_set(uid(), "canvas_ics_url", ""), error = function(e) NULL)
  updateTextInput(session, "canvas_ics_url", value = "")
  canvas_status('<div class="alert alert-secondary py-1 small mt-1">Canvas desconectado: se olvido el enlace.</div>')
  rv$refresh <- rv$refresh + 1
})

# Al iniciar sesion: si hay un enlace guardado, rellenar el campo y sincronizar en segundo plano
observeEvent(auth_user(), {
  if (is.null(auth_user())) { rv_gcal$canvas_events <- NULL; return() }
  current_uid <- uid()
  url <- tryCatch(mg_settings_get(current_uid)$canvas_ics_url, error = function(e) NULL)
  if (!valid_canvas_feed_url(url)) return()
  updateTextInput(session, "canvas_ics_url", value = url)
  session$onFlushed(function() {
    tryCatch(canvas_sync(url, current_uid, save = FALSE, quiet = TRUE),
             error = function(e) message("[StudyPilot] canvas auto-sync: ", conditionMessage(e)))
  }, once = TRUE)
}, ignoreNULL = FALSE, ignoreInit = TRUE)
