# ============ server_pomodoro.R — Pomodoro session tracking ============
# Depends on: rv, uid, acts, input$pomo_course, input$pomo_duration
# JS handles ALL timer logic (delta-time in pomodoro.js).
# R only records completed sessions to MongoDB.

# ---- Record completed Pomodoro session ----
observeEvent(input$pomo_session_done, {
  cid <- input$pomo_course
  dur <- as.integer(input$pomo_duration)
  tryCatch({
    mg_pomo_add(uid(), cid, dur)
    message("[StudyPilot] Pomodoro: +", dur, "min for ", cid)
  }, error = function(e) message("[StudyPilot] Pomo save error: ", e$message))
})

# ---- "Estudiar con Pomodoro" from exam panel ----
observeEvent(input$exam_pomo, {
  ex <- input$exam_select
  if (is.null(ex) || ex == "0") return()
  a <- acts()
  match_a <- a[a$act_id == ex, ]
  if (nrow(match_a) > 0) {
    updateSelectInput(session, "pomo_course", selected = match_a$course_id[1])
  }
  bslib::nav_select("main_nav", selected = "\U0001F4D6 Estudio")
})
