# ============ server_auth.R — Login, Register, Auto-Login, Logout ============
# Depends on: auth_user, uid, rv, rv_gcal, hardcoded_users, session

# Helper: update auth message divs
auth_msg <- function(div_id, text, type = "error") {
  cls <- switch(type, error = "auth-msg error", success = "auth-msg success", loading = "auth-msg")
  style <- if (type == "loading") ' style="background:#eff6ff;color:#2563eb;border:1px solid #bfdbfe;"' else ""
  icon <- if (type == "loading") '<span class="spinner-border spinner-border-sm me-2" role="status"></span>' else ""
  shinyjs::html(div_id, html = paste0('<div class="', cls, '"', style, '>', icon, text, '</div>'))
}

# ---- Heartbeat receiver ----
observeEvent(input$keep_alive, { invisible() })

# ---- Tab tracking for session restore ----
observeEvent(input$main_nav, {
  session$sendCustomMessage("tab_changed", list(tab = input$main_nav))
})

# ---- Offline detection ----
observeEvent(input$app_online, {
  if (!isTRUE(input$app_online)) {
    showNotification("\U0001F4E1 Sin conexión — Modo Offline activo.", type = "warning", duration = 5)
  }
})

# ============ MANUAL LOGIN ============
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
    u <- mongolite::mongo(collection = "users", url = Sys.getenv("MONGODB_URI"))
    db_users <- u$find(paste0('{"user":"', user, '"}'))
    message("[StudyPilot] Login: found ", nrow(db_users), " matching users")
    result <- FALSE
    if (nrow(db_users) > 0) {
      stored_pass <- db_users$password[1]
      verified <- FALSE
      if (grepl("^\\$7\\$", stored_pass)) {
        verified <- tryCatch(sodium::password_verify(stored_pass, pass), error = function(e) FALSE)
      }
      # Legacy plaintext migration
      if (!verified && stored_pass == pass) {
        verified <- TRUE
        hashed <- sodium::password_store(pass)
        u$update(paste0('{"user":"', user, '"}'), paste0('{"$set":{"password":"', hashed, '"}}'))
        message("[StudyPilot] Migrated password to hash for: ", user)
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
    message("[StudyPilot] Login error: ", e$message)
    FALSE
  })

  shinyjs::enable("login_btn")
  if (!login_ok) auth_msg("login_msg_div", "Usuario o contraseña incorrectos.")
})

# ============ REGISTER ============
observeEvent(input$register_btn, {
  name <- trimws(input$reg_name)
  user <- trimws(input$reg_user)
  pass <- input$reg_pass
  pass2 <- input$reg_pass2

  if (nchar(name) < 2 || nchar(user) < 3 || nchar(pass) < 4) {
    auth_msg("register_msg_div", "Completa todos los campos (usuario mín. 3, contraseña mín. 4).")
    return()
  }
  if (!grepl("^[a-zA-Z0-9._]+$", user)) {
    auth_msg("register_msg_div", "El usuario solo puede contener letras, números, puntos y guiones bajos.")
    return()
  }
  if (pass != pass2) { auth_msg("register_msg_div", "Las contraseñas no coinciden."); return() }
  if (user %in% hardcoded_users$user) { auth_msg("register_msg_div", "Usuario no disponible."); return() }

  shinyjs::disable("register_btn")
  auth_msg("register_msg_div", "Creando cuenta...", "loading")

  tryCatch({
    ensure_init()
    if (!.app_initialized) stop("No se pudo conectar a la base de datos.")
    u <- mongolite::mongo(collection = "users", url = Sys.getenv("MONGODB_URI"))
    if (u$count(paste0('{"user":"', user, '"}')) > 0) {
      u$disconnect()
      auth_msg("register_msg_div", "Ese usuario ya existe.")
      shinyjs::enable("register_btn")
      return()
    }
    hashed_pass <- sodium::password_store(pass)
    u$insert(data.frame(user = user, password = hashed_pass, name = name,
                         admin = FALSE, stringsAsFactors = FALSE))
    saved <- u$count(paste0('{"user":"', user, '"}'))
    u$disconnect()
    if (saved > 0) {
      message("[StudyPilot] Register: '", user, "' saved to MongoDB")
      auth_msg("register_msg_div",
        paste0("\u2705 Cuenta creada. Inicia sesión con <b>", user, "</b>."), "success")
      updateTextInput(session, "reg_name", value = "")
      updateTextInput(session, "reg_user", value = "")
      updateTextInput(session, "reg_pass", value = "")
      updateTextInput(session, "reg_pass2", value = "")
    } else {
      auth_msg("register_msg_div", "Error: la cuenta no se pudo guardar.")
    }
  }, error = function(e) {
    message("[StudyPilot] Register error: ", e$message)
    auth_msg("register_msg_div", paste0("Error: ", e$message))
  })
  shinyjs::enable("register_btn")
})

# ============ AUTO-LOGIN from localStorage ============
observeEvent(input$auto_login, {
  req(is.null(auth_user()))
  al <- input$auto_login
  if (is.null(al) || is.null(al$user) || nchar(al$user) == 0) {
    shinyjs::runjs("$('#login_form_container').fadeIn(200);")
    return()
  }

  expected_token <- digest::digest(paste0(al$user, "_studypilot_session"), algo = "md5")
  if (!identical(al$token, expected_token)) {
    message("[StudyPilot] Auto-login: invalid token for ", al$user)
    session$sendCustomMessage("clear_session", list())
    shinyjs::runjs("$('#login_form_container').fadeIn(200);")
    return()
  }

  login_ok <- FALSE
  tryCatch({
    ensure_init()
    u <- mongolite::mongo(collection = "users", url = Sys.getenv("MONGODB_URI"))
    db_users <- u$find(paste0('{"user":"', al$user, '"}'))
    u$disconnect()
    if (nrow(db_users) > 0) {
      auth_user(list(user = al$user, name = db_users$name[1]))
      login_ok <- TRUE
    } else {
      match <- hardcoded_users[hardcoded_users$user == al$user, ]
      if (nrow(match) > 0) {
        auth_user(list(user = al$user, name = match$name[1]))
        login_ok <- TRUE
      }
    }
  }, error = function(e) message("[StudyPilot] Auto-login error: ", e$message))

  if (login_ok) {
    message("[StudyPilot] Auto-login SUCCESS: ", al$user)
    shinyjs::hide("auth_overlay", anim = TRUE, animType = "fade")
    shinyjs::show("main_app", anim = TRUE, animType = "fade")
    if (!is.null(al$tab) && nchar(al$tab) > 0) {
      session$onFlushed(function() {
        bslib::nav_select("main_nav", selected = al$tab)
      }, once = TRUE)
    }
  } else {
    session$sendCustomMessage("clear_session", list())
    shinyjs::runjs("$('#login_form_container').fadeIn(200);")
  }
}, ignoreInit = TRUE)

# ============ POST-LOGIN: Load all user data ============
observeEvent(auth_user(), {
  req(auth_user())
  shinyjs::hide("auth_overlay", anim = TRUE, animType = "fade")
  shinyjs::show("main_app", anim = TRUE, animType = "fade")

  # Save session to localStorage
  token <- digest::digest(paste0(uid(), "_studypilot_session"), algo = "md5")
  session$sendCustomMessage("save_session", list(user = uid(), token = token))

  ensure_init()

  # Load courses from MongoDB
  db_courses <- tryCatch(mg_custom_courses_get(uid()), error = function(e) {
    message("[StudyPilot] Error loading courses: ", e$message); data.frame()
  })
  message("[StudyPilot] Post-login: ", nrow(db_courses), " courses")
  if (nrow(db_courses) > 0) {
    assign("courses", tibble::as_tibble(db_courses), envir = globalenv())
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
        topic_lines <- strsplit(raw, "\n")[[1]]
        topic_lines <- topic_lines[grepl("^\\d+\\.", topic_lines)]
        topic_lines <- sub("^\\d+\\.\\s*", "", topic_lines)
        if (length(topic_lines) > 0) topics_list[[cid_t]] <- topic_lines
      }
      assign("course_topics", topics_list, envir = globalenv())
      message("[StudyPilot] Loaded topics for ", length(topics_list), " courses")
    }
  }, error = function(e) message("[StudyPilot] Topics load error: ", e$message))

  # Load PDF schedule
  tryCatch({
    sched_base <- mg_schedule_get(uid())
    if (nrow(sched_base) > 0) message("[StudyPilot] Auto-loaded ", nrow(sched_base), " schedule blocks")
  }, error = function(e) message("[StudyPilot] Schedule load error: ", e$message))

  # Load AI blocks
  tryCatch({
    saved_ai <- mg_ai_blocks_get(uid())
    if (nrow(saved_ai) > 0) {
      rv_gcal$ai_blocks <- saved_ai
      message("[StudyPilot] Auto-loaded ", nrow(saved_ai), " AI blocks")
    }
  }, error = function(e) message("[StudyPilot] AI blocks load error: ", e$message))

  # Load calendar overrides
  tryCatch({
    saved_ov <- mg_cal_overrides_get(uid())
    if (nrow(saved_ov) > 0) {
      rv_gcal$overrides <- saved_ov
      message("[StudyPilot] Auto-loaded ", nrow(saved_ov), " overrides")
    }
  }, error = function(e) message("[StudyPilot] Overrides load error: ", e$message))

  # Load hidden events
  tryCatch({
    saved_hidden <- mg_cal_hidden_get(uid())
    if (length(saved_hidden) > 0) {
      rv_gcal$hidden_events <- saved_hidden
      message("[StudyPilot] Auto-loaded ", length(saved_hidden), " hidden events")
    }
  }, error = function(e) message("[StudyPilot] Hidden load error: ", e$message))

  rv$refresh <- isolate(rv$refresh) + 1
})

# ============ LOGOUT ============
observeEvent(input$logout_btn, {
  auth_user(NULL)
  rv_gcal$events <- NULL
  rv_gcal$ai_blocks <- NULL
  rv_gcal$overrides <- NULL
  rv_gcal$hidden_events <- NULL
  session$sendCustomMessage("execute_logout", list())
})
