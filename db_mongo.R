# ============ MONGODB FUNCTIONS ============
# All data functions require user_id for per-user isolation
library(mongolite)

MONGO_URI <- Sys.getenv("MONGODB_URI")
if (nchar(MONGO_URI) == 0) stop("MONGODB_URI environment variable not set. See .Renviron.example")

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
  conn <- mongolite::mongo(collection = collection, url = MONGO_URI)
  assign(collection, conn, envir = .mongo_cache)
  conn
}

# Helper: build user filter JSON
uf <- function(uid, extra = "") {
  if (nchar(extra) > 0) paste0('{"user_id":"', uid, '",', extra, '}')
  else paste0('{"user_id":"', uid, '"}')
}

# ============ GRADES ============
mg_grades_all <- function(uid) {
  g <- mongo_col("grades")
  empty <- data.frame(course_id = character(), code = character(), grade = numeric())
  if (is.null(g)) return(empty)
  result <- g$find(uf(uid))
  g# connection reused
  if (nrow(result) == 0) return(empty)
  result
}

mg_grades_course <- function(uid, cid) {
  g <- mongo_col("grades")
  if (is.null(g)) return(data.frame(code = character(), grade = numeric()))
  result <- g$find(uf(uid, paste0('"course_id":"', cid, '"')))
  g# connection reused
  if (nrow(result) == 0) return(data.frame(code = character(), grade = numeric()))
  result[, c("code", "grade")]
}

mg_grade_set <- function(uid, cid, code, grade_val) {
  g <- mongo_col("grades")
  if (is.null(g)) return(invisible(NULL))
  g$update(
    uf(uid, paste0('"course_id":"', cid, '","code":"', code, '"')),
    paste0('{"$set":{"user_id":"', uid, '","course_id":"', cid, '","code":"', code, '","grade":', grade_val, '}}'),
    upsert = TRUE
  )
  g# connection reused
}

mg_grade_delete <- function(uid, cid, code) {
  g <- mongo_col("grades")
  if (is.null(g)) return(invisible(NULL))
  g$remove(uf(uid, paste0('"course_id":"', cid, '","code":"', code, '"')))
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
  a# connection reused
  if (nrow(result) == 0) return(empty)
  result
}

mg_activity_toggle <- function(uid, act_id, new_done) {
  a <- mongo_col("activities")
  if (is.null(a)) return(invisible(NULL))
  done_date <- if (new_done == 1) as.character(Sys.time()) else NA
  a$update(
    uf(uid, paste0('"act_id":', act_id)),
    paste0('{"$set":{"done":', new_done, ',"done_date":', if(is.na(done_date)) 'null' else paste0('"', done_date, '"'), '}}')
  )
  a# connection reused
}

mg_activity_add <- function(uid, cid, type, name, date, weight, notes, temas = NULL, is_calificada = TRUE) {
  a <- mongo_col("activities")
  if (is.null(a)) return(invisible(NULL))
  max_id <- 0
  existing <- a$find(query = uf(uid), fields = '{"act_id":1}', sort = '{"act_id":-1}', limit = 1)
  if (nrow(existing) > 0) max_id <- existing$act_id[1]
  doc <- list(
    user_id = uid, act_id = max_id + 1L, course_id = cid, type = type, name = name,
    code = "", date = date, week = 0L, weight = as.numeric(weight),
    done = 0L, done_date = NA_character_, notes = notes,
    is_calificada = isTRUE(is_calificada)
  )
  if (!is.null(temas) && length(temas) > 0) doc$temas_vinculados <- temas
  a$insert(jsonlite::toJSON(doc, auto_unbox = TRUE))
  a# connection reused
}

# ============ EXAM CHECKS ============
mg_exam_checks_get <- function(uid, cid) {
  e <- mongo_col("exam_checks")
  if (is.null(e)) return(data.frame(topic_key = character(), checked = integer()))
  result <- e$find(uf(uid, paste0('"course_id":"', cid, '"')))
  e# connection reused
  if (nrow(result) == 0) return(data.frame(topic_key = character(), checked = integer()))
  result[, c("topic_key", "checked")]
}

mg_exam_checks_get_checked <- function(uid, cid) {
  e <- mongo_col("exam_checks")
  if (is.null(e)) return(data.frame(topic_key = character(), checked = integer()))
  result <- e$find(uf(uid, paste0('"course_id":"', cid, '","checked":1')))
  e# connection reused
  if (nrow(result) == 0) return(data.frame(topic_key = character(), checked = integer()))
  result[, c("topic_key", "checked")]
}

mg_exam_check_set <- function(uid, cid, key, checked_val) {
  e <- mongo_col("exam_checks")
  if (is.null(e)) return(invisible(NULL))
  e$update(
    uf(uid, paste0('"course_id":"', cid, '","topic_key":"', key, '"')),
    paste0('{"$set":{"user_id":"', uid, '","course_id":"', cid, '","topic_key":"', key, '","checked":', checked_val, '}}'),
    upsert = TRUE
  )
  e# connection reused
}

# ============ STUDY NOTES ============
mg_notes_all <- function(uid) {
  n <- mongo_col("study_notes")
  empty <- data.frame(note_id=integer(), course_id=character(), text_content=character(),
    source=character(), created_at=character())
  if (is.null(n)) return(empty)
  result <- n$find(query = uf(uid), sort = '{"created_at":-1}')
  n# connection reused
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
    user_id = uid, note_id = max_id + 1, course_id = cid, text_content = text_content,
    source = source_name, created_at = as.character(Sys.time())
  ))
  n# connection reused
}

mg_note_delete <- function(uid, note_id) {
  n <- mongo_col("study_notes")
  if (is.null(n)) return(invisible(NULL))
  n$remove(uf(uid, paste0('"note_id":', note_id)))
  n# connection reused
}

# ============ COURSES (per-user) ============
mg_custom_courses_get <- function(uid) {
  c <- mongo_col("courses")
  if (is.null(c)) return(data.frame())
  result <- c$find(uf(uid))
  c# connection reused
  result
}

mg_custom_course_add <- function(uid, id, name, short, credits, professor, formula, eval_day, color) {
  c <- mongo_col("courses")
  if (is.null(c)) return(invisible(NULL))
  c$remove(uf(uid, paste0('"id":"', id, '"')))
  c$insert(data.frame(
    user_id=uid, id=id, name=name, short=short, credits=as.integer(credits),
    professor=professor, formula=formula, eval_day=as.integer(eval_day), color=color,
    stringsAsFactors=FALSE))
  c# connection reused
}

mg_custom_course_delete <- function(uid, course_id) {
  c <- mongo_col("courses")
  if (is.null(c)) return(invisible(NULL))
  c$remove(uf(uid, paste0('"id":"', course_id, '"')))
  c# connection reused
  for (col_name in c("grades", "activities", "study_notes", "syllabi", "exam_checks", "pomo_sessions")) {
    m <- mongo_col(col_name)
    if (!is.null(m)) {
      m$remove(uf(uid, paste0('"course_id":"', course_id, '"')))
      m# connection reused
    }
  }
}

# ============ USERS (no user_id filter needed) ============
mg_users_get <- function() {
  u <- mongo_col("users")
  if (is.null(u)) return(data.frame(user=character(), password=character(), admin=logical(), name=character()))
  result <- u$find()
  u# connection reused
  if (nrow(result) == 0) return(data.frame(user=character(), password=character(), admin=logical(), name=character()))
  result
}

mg_user_add <- function(user, password, name, admin = FALSE) {
  u <- mongo_col("users")
  if (is.null(u)) return(invisible(NULL))
  u$insert(data.frame(user=user, password=password, name=name, admin=admin, stringsAsFactors=FALSE))
  u# connection reused
}

mg_user_exists <- function(username) {
  u <- mongo_col("users")
  if (is.null(u)) return(FALSE)
  n <- u$count(paste0('{"user":"', username, '"}'))
  u# connection reused
  n > 0
}

# ============ SYLLABI ============
mg_syllabus_add <- function(uid, course_id, filename, content) {
  s <- mongo_col("syllabi")
  if (is.null(s)) return(invisible(NULL))
  s$insert(data.frame(user_id=uid, course_id=course_id, filename=filename, content=content,
    uploaded_at=as.character(Sys.time()), stringsAsFactors=FALSE))
  s# connection reused
}

mg_syllabi_get <- function(uid, course_id = NULL) {
  s <- mongo_col("syllabi")
  empty <- data.frame(course_id=character(), filename=character(), content=character(), uploaded_at=character())
  if (is.null(s)) return(empty)
  q <- if (is.null(course_id)) uf(uid) else uf(uid, paste0('"course_id":"', course_id, '"'))
  result <- s$find(q)
  s# connection reused
  if (nrow(result) == 0) return(empty)
  result
}

mg_syllabus_delete <- function(uid, course_id, filename) {
  s <- mongo_col("syllabi")
  if (is.null(s)) return(invisible(NULL))
  s$remove(uf(uid, paste0('"course_id":"', course_id, '","filename":"', filename, '"')))
  s# connection reused
}

# ============ POMODORO SESSIONS ============
mg_pomo_add <- function(uid, cid, duration) {
  p <- mongo_col("pomo_sessions")
  if (is.null(p)) return(invisible(NULL))
  p$insert(data.frame(user_id = uid, course_id = cid, duration_min = duration, completed_at = as.character(Sys.time())))
  p# connection reused
}

# ============ UPDATE ACTIVITY ============
mg_activity_update <- function(uid, act_id, name, weight, date, type = NULL, temas = NULL, is_calificada = NULL) {
  a <- mongo_col("activities")
  if (is.null(a)) return(invisible(NULL))
  set_fields <- paste0('"name":"', name, '","weight":', as.numeric(weight), ',"date":"', date, '"')
  if (!is.null(type) && nchar(type) > 0) set_fields <- paste0(set_fields, ',"type":"', type, '"')
  if (!is.null(is_calificada)) set_fields <- paste0(set_fields, ',"is_calificada":', tolower(as.character(isTRUE(is_calificada))))
  if (!is.null(temas)) {
    temas_json <- jsonlite::toJSON(temas, auto_unbox = FALSE)
    set_fields <- paste0(set_fields, ',"temas_vinculados":', temas_json)
  }
  a$update(
    uf(uid, paste0('"act_id":', act_id)),
    paste0('{"$set":{', set_fields, '}}')
  )
}

mg_activity_get_topics <- function(uid, act_id) {
  a <- mongo_col("activities")
  if (is.null(a)) return(character(0))
  result <- a$find(uf(uid, paste0('"act_id":', act_id)), fields = '{"temas_vinculados":1}')
  if (nrow(result) > 0 && "temas_vinculados" %in% names(result)) {
    tv <- result$temas_vinculados[[1]]
    if (!is.null(tv) && length(tv) > 0) return(as.character(tv))
  }
  character(0)
}

mg_activity_delete <- function(uid, act_id) {
  a <- mongo_col("activities")
  if (is.null(a)) return(invisible(NULL))
  a$remove(uf(uid, paste0('"act_id":', act_id)))
}

# ============ SCHEDULE (per-user) ============
mg_schedule_get <- function(uid) {
  s <- mongo_col("schedules")
  if (is.null(s)) return(data.frame())
  result <- s$find(uf(uid))
  s# connection reused
  result
}

mg_schedule_set <- function(uid, schedule_df) {
  s <- mongo_col("schedules")
  if (is.null(s)) return(invisible(NULL))
  s$remove(uf(uid))
  if (nrow(schedule_df) > 0) {
    schedule_df$user_id <- uid
    s$insert(schedule_df)
  }
  s# connection reused
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
    overrides_df$user_id <- uid
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
    df <- data.frame(user_id = uid, key = hidden_keys, stringsAsFactors = FALSE)
    col$insert(df)
  }
}
