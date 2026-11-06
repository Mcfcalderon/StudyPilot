# ============ StudyPilot app.R ============

# ==== ZONA HORARIA ====
# shinyapps.io corre en UTC: sin esto Sys.Date() pasaba al dia siguiente a las 19:00 de Lima
# (dias restantes, atrasadas, Pomodoro de "hoy", prioridades). Los sellos de tiempo guardados
# con as.character(Sys.time()) quedan en hora de Lima desde el despliegue de este cambio
# (antes, UTC): ver docs/MONGO_SCHEMA.md.
Sys.setenv(TZ = "America/Lima")

# ==== LOAD ENV VARS (before anything else) ====
for (.ef in c("app_env", ".Renviron")) {
  if (file.exists(.ef)) {
    for (.ln in readLines(.ef, warn = FALSE)) {
      .ln <- trimws(.ln)
      if (nchar(.ln) > 0 && !startsWith(.ln, "#") && grepl("=", .ln)) {
        .k <- sub("=.*", "", .ln)
        .v <- sub("^[^=]+=", "", .ln)
        .v <- gsub("\r", "", .v)
        do.call(Sys.setenv, setNames(list(.v), .k))
      }
    }
    message("[StudyPilot] Env loaded from: ", .ef)
    break
  }
}
message("[StudyPilot] MONGODB_URI = ", nchar(Sys.getenv("MONGODB_URI")), " chars")

# ---- Load global: libraries + R/ + ui/ ----
source("global.R")

# ---- Build UI ----
ui <- ui_navbar()

# ---- Server ----
server <- function(input, output, session) {

  auth_user <- reactiveVal(NULL)
  # Umbral de aprobacion POR SESION (no global): cada usuario tiene el suyo
  session$userData$pass_grade <- reactiveVal(PASS_GRADE)
  # Fecha de inicio del ciclo POR SESION (configurable/persistente)
  .lunes_actual <- Sys.Date() - (as.integer(format(Sys.Date(), "%u")) - 1)  # lunes de esta semana
  session$userData$semester_start <- reactiveVal(.lunes_actual)
  # Estado por-sesion (privado): cursos, temas y preguntas de examen de ESTE usuario
  session$userData$courses        <- reactiveVal(.empty_courses())
  session$userData$course_topics  <- reactiveVal(list())
  session$userData$exam_questions <- reactiveVal(list())
  # Active bindings: las lecturas de 'courses'/'course_topics'/'exam_questions' en los
  # modulos resuelven a los datos de ESTA sesion (no al entorno global compartido).
  makeActiveBinding("courses",        function(v) if (missing(v)) get_courses()        else set_courses(v),        environment())
  makeActiveBinding("course_topics",  function(v) if (missing(v)) get_course_topics()  else set_course_topics(v),  environment())
  makeActiveBinding("exam_questions", function(v) if (missing(v)) get_exam_questions() else set_exam_questions(v), environment())
  uid <- reactive({ req(auth_user()); auth_user()$user })

  rv <- reactiveValues(
    refresh = 0, grades_refresh = 0,
    view_week = 1L, cal_week = 1L
  )

  rv_gcal <- reactiveValues(
    events = NULL, ai_blocks = NULL,
    overrides = NULL, hidden_events = NULL
  )

  rv_extract <- reactiveValues(data = NULL, text = NULL)
  rv_quiz <- reactiveValues(
    exam = NULL, submitted = FALSE,
    results = NULL, loading = FALSE, error_msg = NULL
  )

  # Admins: usuario:HASH:Nombre (ver R/auth_passwords.R y tools/hash_password.R)
  hardcoded_users <- parse_admin_users(Sys.getenv("STUDYPILOT_ADMIN_USERS"))

  save_ai_blocks_mongo <- function() {
    tryCatch(mg_ai_blocks_set(uid(), rv_gcal$ai_blocks),
             error = function(e) message("[StudyPilot] AI blocks save error: ", e$message))
  }

  # ---- Shared reactives ----
  acts <- reactive({
    rv$refresh
    tryCatch(mg_activities_all(uid()), error = function(e) {
      message("[StudyPilot] acts() error: ", e$message)
      data.frame(act_id = integer(), course_id = character(), type = character(),
        name = character(), date = character(), weight = numeric(),
        week = character(), done = integer(), stringsAsFactors = FALSE)
    })
  })

  all_grades_cache <- reactive({
    rv$grades_refresh
    rv$refresh
    tryCatch(mg_grades_all(uid()), error = function(e) {
      data.frame(course_id = character(), code = character(), grade = numeric(), stringsAsFactors = FALSE)
    })
  })

  calc_avg_fast <- function(cid, cached) {
    empty <- list(partial = 0, pct_graded = 0, earned = 0, needed = NA,
                  remaining = 100, remaining_evals = data.frame())
    g <- cached[cached$course_id == cid, ]
    if (nrow(g) == 0) return(empty)
    evals_c <- tryCatch({
      a <- mg_activities_all(uid())
      if (nrow(a) == 0) return(data.frame())
      e <- a[a$course_id == cid, ]
      if ("is_calificada" %in% names(e)) e <- e[is.na(e$is_calificada) | e$is_calificada == TRUE, ]
      e <- e[!is.na(e$weight) & e$weight > 0, ]
      if (nrow(e) > 0) e <- e[order(e$date), ]
      e
    }, error = function(err) data.frame())
    if (nrow(evals_c) == 0) return(empty)
    for (j in seq_len(nrow(evals_c))) {
      if (!"code" %in% names(evals_c) || is.null(evals_c$code[j]) || is.na(evals_c$code[j]) || nchar(as.character(evals_c$code[j])) == 0) {
        if (!"code" %in% names(evals_c)) evals_c$code <- ""
        evals_c$code[j] <- paste0("E", j)
      }
    }
    # Delegar el calculo aritmetico en la funcion pura testeable
    compute_course_avg(evals_c, g[, c("code", "grade")], get_pass_grade())
  }

  # ---- Server modules (local=TRUE shares reactives) ----
  lapply(list.files("server", pattern = "\\.R$", full.names = TRUE), function(f) {
    source(f, local = TRUE)
    message("[StudyPilot] Server loaded: ", f)
  })
}

shinyApp(ui, server)
