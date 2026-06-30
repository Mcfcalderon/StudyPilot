# ============ StudyPilot app.R ============

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
  uid <- reactive({ req(auth_user()); auth_user()$user })

  rv <- reactiveValues(
    refresh = 0, grades_refresh = 0,
    view_week = current_week(), cal_week = current_week()
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

  hardcoded_users <- {
    raw <- Sys.getenv("STUDYPILOT_ADMIN_USERS")
    if (nchar(raw) > 0) {
      parts <- strsplit(trimws(strsplit(raw, ",")[[1]]), ":")
      data.frame(user = sapply(parts, `[`, 1), password = sapply(parts, `[`, 2),
                 name = sapply(parts, `[`, 3), stringsAsFactors = FALSE)
    } else data.frame(user = character(), password = character(), name = character(), stringsAsFactors = FALSE)
  }

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
    total_w <- sum(evals_c$weight, na.rm = TRUE)
    if (total_w == 0) return(empty)
    # Merge grades by code (keep name for remaining list)
    name_col <- if ("name" %in% names(evals_c)) "name" else "code"
    merged <- merge(evals_c[, c("code", "weight", name_col)], g[, c("code", "grade")], by = "code", all.x = TRUE)
    graded <- merged[!is.na(merged$grade), ]
    # Puntos absolutos de 20 ya asegurados
    earned <- if (nrow(graded) > 0) sum(graded$grade * graded$weight / 100, na.rm = TRUE) else 0
    pct <- if (nrow(graded) > 0) sum(graded$weight, na.rm = TRUE) else 0
    partial <- if (pct > 0) earned / (pct / 100) else 0
    remaining <- total_w - pct
    # Evaluaciones restantes (sin nota)
    rem <- merged[is.na(merged$grade), c(name_col, "weight")]
    if (nrow(rem) > 0) names(rem) <- c("name", "weight")
    # Nota constante necesaria en CADA evaluacion restante para aprobar (umbral 10.5)
    needed <- if (remaining > 0) (10.5 - earned) / (remaining / 100) else 0
    list(partial = round(partial, 2), pct_graded = round(pct),
         earned = round(earned, 2), needed = round(max(0, needed), 2),
         remaining = round(remaining), remaining_evals = rem)
  }

  # ---- Server modules (local=TRUE shares reactives) ----
  lapply(list.files("server", pattern = "\\.R$", full.names = TRUE), function(f) {
    source(f, local = TRUE)
    message("[StudyPilot] Server loaded: ", f)
  })
}

shinyApp(ui, server)
