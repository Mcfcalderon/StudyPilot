# ============ MONGODB FUNCTIONS ============
# All data functions require user_id for per-user isolation
# Filtros y updates se construyen con uf()/mq()/mq_set() de R/db_query.R (JSON escapado):
# nunca armar JSON con paste0 sobre texto del usuario.
library(mongolite)

# URI is read fresh via Sys.getenv("MONGODB_URI") at each connection

# Helper: get a collection connection with reuse (returns NULL if not initialized)
.mongo_cache <- new.env(parent = emptyenv())

mongo_col <- function(collection) {
  if (!isTRUE(get0(".app_initialized", envir = globalenv()))) return(NULL)
  # Reuse existing connection if alive
  if (exists(collection, envir = .mongo_cache)) {
    conn <- get(collection, envir = .mongo_cache)
    # Test if connection is still alive
    alive <- tryCatch({ conn$count(); TRUE }, error = function(e) FALSE)
    if (alive) return(conn)
  }
  conn <- mongolite::mongo(collection = collection, url = Sys.getenv("MONGODB_URI"))
  assign(collection, conn, envir = .mongo_cache)
  conn
}

# ============ GRADES ============
mg_grades_all <- function(uid) {
  g <- mongo_col("grades")
  empty <- data.frame(course_id = character(), code = character(), grade = numeric())
  if (is.null(g)) return(empty)
  result <- g$find(uf(uid))
  if (nrow(result) == 0) return(empty)
  result
}

mg_grades_course <- function(uid, cid) {
  g <- mongo_col("grades")
  if (is.null(g)) return(data.frame(code = character(), grade = numeric()))
  result <- g$find(uf(uid, course_id = q_str(cid)))
  if (nrow(result) == 0) return(data.frame(code = character(), grade = numeric()))
  result[, c("code", "grade")]
}

mg_grade_set <- function(uid, cid, code, grade_val) {
  g <- mongo_col("grades")
  if (is.null(g)) return(invisible(NULL))
  g$update(
    uf(uid, course_id = q_str(cid), code = q_str(code)),
    mq_set(user_id = q_str(uid), course_id = q_str(cid), code = q_str(code), grade = q_num(grade_val)),
    upsert = TRUE
  )
}

mg_grade_delete <- function(uid, cid, code) {
  g <- mongo_col("grades")
  if (is.null(g)) return(invisible(NULL))
  g$remove(uf(uid, course_id = q_str(cid), code = q_str(code)))
}

# ============ ACTIVITIES ============
mg_activities_all <- function(uid) {
  a <- mongo_col("activities")
  empty <- data.frame(
    act_id=integer(), course_id=character(), type=character(), name=character(),
    code=character(), date=character(), week=integer(), weight=numeric(),
    done=integer(), done_date=character(), notes=character()
  )
  if (is.null(a)) return(empty)
  result <- a$find(query = uf(uid), sort = '{"date":1}')
  if (nrow(result) == 0) return(empty)
  result
}

mg_activity_toggle <- function(uid, act_id, new_done) {
  a <- mongo_col("activities")
  if (is.null(a)) return(invisible(NULL))
  done_date <- if (new_done == 1) as.character(Sys.time()) else NA_character_
  a$update(
    uf(uid, act_id = q_num(act_id)),
    mq_set(done = as.integer(q_num(new_done)), done_date = done_date)
  )
}

mg_activity_add <- function(uid, cid, type, name, date, weight, notes, temas = NULL, is_calificada = TRUE, week = 0L) {
  a <- mongo_col("activities")
  if (is.null(a)) return(invisible(NULL))
  max_id <- 0
  existing <- a$find(query = uf(uid), fields = '{"act_id":1}', sort = '{"act_id":-1}', limit = 1)
  if (nrow(existing) > 0) max_id <- existing$act_id[1]
  doc <- list(
    user_id = q_str(uid), act_id = max_id + 1L, course_id = cid, type = type, name = name,
    code = "", date = date, week = as.integer(week), weight = as.numeric(weight),
    done = 0L, done_date = NA_character_, notes = notes,
    is_calificada = isTRUE(is_calificada)
  )
  if (!is.null(temas) && length(temas) > 0) doc$temas_vinculados <- I(as.character(temas))
  a$insert(jsonlite::toJSON(doc, auto_unbox = TRUE, na = "null", digits = NA))
}

# ============ EXAM CHECKS ============
mg_exam_checks_get <- function(uid, cid) {
  e <- mongo_col("exam_checks")
  if (is.null(e)) return(data.frame(topic_key = character(), checked = integer()))
  result <- e$find(uf(uid, course_id = q_str(cid)))
  if (nrow(result) == 0) return(data.frame(topic_key = character(), checked = integer()))
  result[, c("topic_key", "checked")]
}

mg_exam_checks_get_checked <- function(uid, cid) {
  e <- mongo_col("exam_checks")
  if (is.null(e)) return(data.frame(topic_key = character(), checked = integer()))
  result <- e$find(uf(uid, course_id = q_str(cid), checked = 1L))
  if (nrow(result) == 0) return(data.frame(topic_key = character(), checked = integer()))
  result[, c("topic_key", "checked")]
}

mg_exam_check_set <- function(uid, cid, key, checked_val) {
  e <- mongo_col("exam_checks")
  if (is.null(e)) return(invisible(NULL))
  e$update(
    uf(uid, course_id = q_str(cid), topic_key = q_str(key)),
    mq_set(user_id = q_str(uid), course_id = q_str(cid), topic_key = q_str(key), checked = q_num(checked_val)),
    upsert = TRUE
  )
}

# ============ STUDY NOTES ============
mg_notes_all <- function(uid) {
  n <- mongo_col("study_notes")
  empty <- data.frame(note_id=integer(), course_id=character(), text_content=character(),
    source=character(), created_at=character())
  if (is.null(n)) return(empty)
  result <- n$find(query = uf(uid), sort = '{"created_at":-1}')
  if (nrow(result) == 0) return(empty)
  result
}

mg_note_add <- function(uid, cid, text_content, source_name) {
  n <- mongo_col("study_notes")
  if (is.null(n)) return(invisible(NULL))
  max_id <- 0
  existing <- n$find(query = uf(uid), fields = '{"note_id":1}', sort = '{"note_id":-1}', limit = 1)
  if (nrow(existing) > 0 && "note_id" %in% names(existing)) max_id <- existing$note_id[1]
  n$insert(data.frame(
    user_id = q_str(uid), note_id = max_id + 1, course_id = cid, text_content = text_content,
    source = source_name, created_at = as.character(Sys.time())
  ))
}

mg_note_delete <- function(uid, note_id) {
  n <- mongo_col("study_notes")
  if (is.null(n)) return(invisible(NULL))
  n$remove(uf(uid, note_id = q_num(note_id)))
}

# ============ COURSES (per-user) ============
mg_custom_courses_get <- function(uid) {
  c <- mongo_col("courses")
  if (is.null(c)) return(data.frame())
  c$find(uf(uid))
}

mg_custom_course_add <- function(uid, id, name, short, credits, professor, formula, eval_day, color) {
  c <- mongo_col("courses")
  if (is.null(c)) return(invisible(NULL))
  c$remove(uf(uid, id = q_str(id)))
  c$insert(data.frame(
    user_id=q_str(uid), id=id, name=name, short=short, credits=as.integer(credits),
    professor=professor, formula=formula, eval_day=as.integer(eval_day), color=color,
    stringsAsFactors=FALSE))
}

mg_custom_course_delete <- function(uid, course_id) {
  c <- mongo_col("courses")
  if (is.null(c)) return(invisible(NULL))
  c$remove(uf(uid, id = q_str(course_id)))
  for (col_name in c("grades", "activities", "study_notes", "syllabi", "exam_checks", "pomo_sessions")) {
    m <- mongo_col(col_name)
    if (!is.null(m)) m$remove(uf(uid, course_id = q_str(course_id)))
  }
}

# ============ USERS (no user_id filter needed) ============
# Devuelve el documento del usuario (data.frame de 0 o 1 fila).
mg_user_find <- function(username) {
  u <- mongo_col("users")
  if (is.null(u)) stop("Base de datos no disponible")
  u$find(mq(user = q_str(username)), limit = 1)
}

mg_user_create <- function(user, password_hash, name) {
  u <- mongo_col("users")
  if (is.null(u)) stop("Base de datos no disponible")
  u$insert(data.frame(user = q_str(user), password = q_str(password_hash), name = name,
                      admin = FALSE, stringsAsFactors = FALSE))
}

mg_user_set_password <- function(user, password_hash) {
  u <- mongo_col("users")
  if (is.null(u)) stop("Base de datos no disponible")
  u$update(mq(user = q_str(user)), mq_set(password = q_str(password_hash)))
}

mg_user_exists <- function(username) {
  u <- mongo_col("users")
  if (is.null(u)) return(FALSE)
  u$count(mq(user = q_str(username))) > 0
}

# ============ SYLLABI ============
mg_syllabus_add <- function(uid, course_id, filename, content) {
  s <- mongo_col("syllabi")
  if (is.null(s)) return(invisible(NULL))
  s$insert(data.frame(user_id=q_str(uid), course_id=course_id, filename=filename, content=content,
    uploaded_at=as.character(Sys.time()), stringsAsFactors=FALSE))
}

mg_syllabi_get <- function(uid, course_id = NULL) {
  s <- mongo_col("syllabi")
  empty <- data.frame(course_id=character(), filename=character(), content=character(), uploaded_at=character())
  if (is.null(s)) return(empty)
  q <- if (is.null(course_id)) uf(uid) else uf(uid, course_id = q_str(course_id))
  result <- s$find(q)
  if (nrow(result) == 0) return(empty)
  result
}

mg_syllabus_delete <- function(uid, course_id, filename) {
  s <- mongo_col("syllabi")
  if (is.null(s)) return(invisible(NULL))
  s$remove(uf(uid, course_id = q_str(course_id), filename = q_str(filename)))
}

# ============ POMODORO SESSIONS ============

mg_pomo_get_today <- function(uid) {
  p <- mongo_col("pomo_sessions")
  if (is.null(p)) return(data.frame())
  today_str <- as.character(Sys.Date())
  tryCatch({
    all_sessions <- p$find(uf(uid))
    if (nrow(all_sessions) == 0) return(data.frame())
    all_sessions[grepl(today_str, all_sessions$completed_at), ]
  }, error = function(e) data.frame())
}

mg_pomo_add <- function(uid, cid, duration) {
  p <- mongo_col("pomo_sessions")
  if (is.null(p)) return(invisible(NULL))
  p$insert(data.frame(user_id = q_str(uid), course_id = cid, duration_min = duration, completed_at = as.character(Sys.time())))
}

# ============ UPDATE ACTIVITY ============
mg_activity_update <- function(uid, act_id, name, weight, date, type = NULL, temas = NULL, is_calificada = NULL, week = NULL) {
  a <- mongo_col("activities")
  if (is.null(a)) return(invisible(NULL))
  set_fields <- list(name = q_str(name), weight = q_num(weight), date = q_str(date))
  if (!is.null(type) && nchar(type) > 0) set_fields$type <- q_str(type)
  if (!is.null(is_calificada)) set_fields$is_calificada <- isTRUE(is_calificada)
  if (!is.null(week) && !is.na(week)) set_fields$week <- as.integer(week)
  if (!is.null(temas)) set_fields$temas_vinculados <- I(as.character(temas))
  a$update(uf(uid, act_id = q_num(act_id)), do.call(mq_set, set_fields))
}

mg_activity_get_topics <- function(uid, act_id) {
  a <- mongo_col("activities")
  if (is.null(a)) return(character(0))
  result <- a$find(uf(uid, act_id = q_num(act_id)), fields = '{"temas_vinculados":1}')
  if (nrow(result) > 0 && "temas_vinculados" %in% names(result)) {
    tv <- result$temas_vinculados[[1]]
    if (!is.null(tv) && length(tv) > 0) return(as.character(tv))
  }
  character(0)
}

mg_activity_delete <- function(uid, act_id) {
  a <- mongo_col("activities")
  if (is.null(a)) return(invisible(NULL))
  a$remove(uf(uid, act_id = q_num(act_id)))
}

# ============ SCHEDULE (per-user) ============
mg_schedule_get <- function(uid) {
  s <- mongo_col("schedules")
  if (is.null(s)) return(data.frame())
  s$find(uf(uid))
}

mg_schedule_set <- function(uid, schedule_df) {
  s <- mongo_col("schedules")
  if (is.null(s)) return(invisible(NULL))
  s$remove(uf(uid))
  if (nrow(schedule_df) > 0) {
    schedule_df$user_id <- q_str(uid)
    s$insert(schedule_df)
  }
}

# ============ CALENDAR OVERRIDES (per-user: ediciones locales de eventos) ============
mg_cal_overrides_get <- function(uid) {
  col <- mongo_col("cal_overrides")
  if (is.null(col)) return(data.frame())
  result <- tryCatch(col$find(uf(uid)), error = function(e) data.frame())
  if (nrow(result) > 0 && "user_id" %in% names(result)) result$user_id <- NULL
  if (nrow(result) > 0 && "_id" %in% names(result)) result[["_id"]] <- NULL
  result
}

mg_cal_overrides_set <- function(uid, overrides_df) {
  col <- mongo_col("cal_overrides")
  if (is.null(col)) return(invisible(NULL))
  col$remove(uf(uid))
  if (!is.null(overrides_df) && is.data.frame(overrides_df) && nrow(overrides_df) > 0) {
    overrides_df$user_id <- q_str(uid)
    col$insert(overrides_df)
  }
}

# ============ CALENDAR HIDDEN EVENTS (per-user: eventos ocultados) ============
mg_cal_hidden_get <- function(uid) {
  col <- mongo_col("cal_hidden")
  if (is.null(col)) return(character())
  result <- tryCatch(col$find(uf(uid)), error = function(e) data.frame())
  if (nrow(result) > 0 && "key" %in% names(result)) return(result$key)
  character()
}

mg_cal_hidden_set <- function(uid, hidden_keys) {
  col <- mongo_col("cal_hidden")
  if (is.null(col)) return(invisible(NULL))
  col$remove(uf(uid))
  if (length(hidden_keys) > 0) {
    df <- data.frame(user_id = q_str(uid), key = hidden_keys, stringsAsFactors = FALSE)
    col$insert(df)
  }
}

# ============ AI BLOCKS (per-user: bloques de estudio generados por Smart Scheduler) ============
mg_ai_blocks_get <- function(uid) {
  col <- mongo_col("ai_blocks")
  if (is.null(col)) return(data.frame())
  result <- tryCatch(col$find(uf(uid)), error = function(e) data.frame())
  if (nrow(result) > 0 && "user_id" %in% names(result)) result$user_id <- NULL
  if (nrow(result) > 0 && "_id" %in% names(result)) result[["_id"]] <- NULL
  result
}

mg_ai_blocks_set <- function(uid, ai_blocks_df) {
  col <- mongo_col("ai_blocks")
  if (is.null(col)) return(invisible(NULL))
  col$remove(uf(uid))
  if (!is.null(ai_blocks_df) && is.data.frame(ai_blocks_df) && nrow(ai_blocks_df) > 0) {
    ai_blocks_df$user_id <- q_str(uid)
    col$insert(ai_blocks_df)
  }
}


# ============ SETTINGS (preferencias por usuario) ============
mg_settings_get <- function(uid) {
  s <- mongo_col("settings"); if (is.null(s)) return(list())
  r <- tryCatch(s$find(uf(uid)), error = function(e) data.frame())
  if (nrow(r) == 0) return(list())
  as.list(r[1, , drop = FALSE])
}

mg_settings_set <- function(uid, key, value) {
  s <- mongo_col("settings"); if (is.null(s)) return(invisible(NULL))
  val <- if (is.numeric(value)) q_num(value) else q_str(value)
  s$update(uf(uid), do.call(mq_set, setNames(list(val), q_field(key))), upsert = TRUE)
}


# ============ USAGE METRICS (best-effort, no bloquea la app) ============
mg_log_event <- function(uid, event, detail = "") {
  col <- mongo_col("usage_events"); if (is.null(col)) return(invisible(NULL))
  tryCatch(col$insert(data.frame(user_id = q_str(uid), event = event,
    detail = as.character(detail), ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    stringsAsFactors = FALSE)), error = function(e) NULL)
  invisible(NULL)
}

mg_usage_all <- function(uid) {
  col <- mongo_col("usage_events"); if (is.null(col)) return(data.frame())
  tryCatch(col$find(uf(uid)), error = function(e) data.frame())
}
