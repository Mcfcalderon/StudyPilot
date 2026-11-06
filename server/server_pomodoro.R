# ============ server_pomodoro.R — Pomodoro session tracking ============
# Depends on: rv, uid, acts, input$pomo_course, input$pomo_duration
# JS handles ALL timer logic (delta-time in pomodoro.js).
# R only records completed sessions to MongoDB.

# ---- Record completed Pomodoro session ----
observeEvent(input$pomo_session_done, {
  cid <- input$pomo_course
  dur <- valid_pomo_duration(input$pomo_duration)   # 1..180 o NA (R/validation.R)
  if (is.na(dur)) { message("[StudyPilot] Pomodoro: duracion invalida, no se guarda"); return() }
  tryCatch({
    mg_pomo_add(uid(), cid, dur)
    message("[StudyPilot] Pomodoro: +", dur, "min for ", cid)
  }, error = function(e) message("[StudyPilot] Pomo save error: ", e$message))
})

# ---- "Estudiar con Pomodoro" from exam panel ----
# input$exam_select es la POSICION en la lista de examenes pendientes, no un act_id:
# exam_at() (R/analytics_calc.R) resuelve la actividad correcta. (selected_exam() de
# server_examen.R no se ve aqui: cada archivo de server/ tiene su propio entorno.)
observeEvent(input$exam_pomo, {
  ex <- exam_at(acts(), input$exam_select)
  if (is.null(ex)) return()
  if (ex$course_id %in% courses$id)
    updateSelectInput(session, "pomo_course", selected = ex$course_id)
  bslib::nav_select("main_nav", selected = "\U0001F4D6 Estudio")
})

# ---- Load today's Pomodoro sessions from MongoDB ----
observe({
  req(uid())
  tryCatch({
    today_sessions <- mg_pomo_get_today(uid())
    n_sessions <- nrow(today_sessions)
    total_min <- if (n_sessions > 0) sum(today_sessions$duration_min, na.rm = TRUE) else 0
    # Send to JS to update the counters
    session$sendCustomMessage("pomo_restore", list(
      sessions = n_sessions,
      total_min = total_min
    ))
    message("[StudyPilot] Pomo restored: ", n_sessions, " sessions, ", total_min, " min today")
  }, error = function(e) message("[StudyPilot] Pomo restore error: ", e$message))
}) |> bindEvent(uid(), once = TRUE)
