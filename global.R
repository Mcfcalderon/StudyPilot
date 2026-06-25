# ============ StudyPilot — global.R ============
# Carga TODAS las librerias, funciones puras (R/) y modulos UI (ui/).
# Los modulos server/ se cargan en app.R con local=TRUE (necesitan contexto reactivo).

.startup_time <- proc.time()

options(shiny.maxRequestSize = 50 * 1024^2)  # 50 MB para batch upload

# ---- Librerias ----
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(DT)
library(htmltools)
library(markdown)
library(shinyjs)
library(sodium)
library(digest)

# ---- Funciones puras (carpeta R/) ----
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), function(f) {
  source(f)
  message("[StudyPilot] Loaded: ", f)
})

# ---- UI modules (carpeta ui/) ----
lapply(list.files("ui", pattern = "\\.R$", full.names = TRUE), function(f) {
  source(f)
  message("[StudyPilot] UI loaded: ", f)
})

# ============ SEMESTER CONFIG ============
SEMESTER_START <- as.Date("2026-03-23")
TOTAL_WEEKS <- 16
current_week <- function() {
  w <- as.integer(difftime(Sys.Date(), SEMESTER_START, units = "weeks")) + 1L
  max(1L, min(w, TOTAL_WEEKS))
}
week_to_date <- function(w, eval_day = 5) {
  SEMESTER_START + (w - 1) * 7 + (eval_day - 1)
}

date_to_week <- function(date_val) {
  d <- tryCatch(as.Date(date_val), error = function(e) NA)
  if (is.na(d)) return(0L)
  w <- as.integer(difftime(d, SEMESTER_START, units = "weeks")) + 1L
  max(1L, min(w, TOTAL_WEEKS))
}

# ============ CALENDAR CONSTANTS ============
CAL_FIRST_HOUR <- 0
CAL_LAST_HOUR  <- 23
CAL_SCROLL_TO  <- 7
HOUR_H         <- 50  # pixels per hour — MUST match CSS .cal-hour-label height

# ============ EMPTY DATA TEMPLATES ============
courses <- tibble::tibble(
  id = character(), name = character(), short = character(),
  credits = integer(), professor = character(), formula = character(),
  eval_day = integer(), color = character()
)

evaluations <- tibble::tibble(
  course_id = character(), code = character(), label = character(),
  weight = numeric(), week = integer(), type = character(), grade = numeric()
)

course_topics <- list()

# ============ LAZY INIT ============
assign(".app_initialized", FALSE, envir = globalenv())
assign("courses", courses, envir = globalenv())

ensure_init <- function() {
  if (isTRUE(get0(".app_initialized", envir = globalenv()))) return(invisible(TRUE))
  tryCatch({
    # Direct connection (bypass mongo_col which checks .app_initialized)
    uri <- Sys.getenv("MONGODB_URI")
    if (nchar(uri) == 0) { message("[StudyPilot] ensure_init: MONGODB_URI empty"); return(invisible(FALSE)) }
    test <- mongolite::mongo(collection = "users", url = uri)
    test$count()
    assign(".app_initialized", TRUE, envir = globalenv())
    message("[StudyPilot] MongoDB initialized OK")
    test$disconnect()
    invisible(TRUE)
  }, error = function(e) {
    message("[StudyPilot] ensure_init FAILED: ", e$message)
    invisible(FALSE)
  })
}

# ============ HELPER FUNCTIONS ============
calc_course_avg <- function(cid) {
  g <- mg_grades_course(cid)
  evals <- evaluations |> filter(course_id == cid) |> select(code, weight)
  if (nrow(g) == 0) return(list(partial = 0, pct_graded = 0, earned = 0, needed = NA, remaining = 100))
  merged <- evals |> left_join(g |> rename(actual_grade = grade), by = "code")
  graded <- merged |> filter(!is.na(actual_grade))
  earned <- sum(graded$actual_grade * graded$weight / 100, na.rm = TRUE)
  pct_graded <- sum(graded$weight, na.rm = TRUE)
  partial <- if (pct_graded > 0) earned / (pct_graded / 100) else 0
  remaining <- 100 - pct_graded
  needed <- if (remaining > 0) (10.5 - earned) / (remaining / 100) else 0
  list(partial = round(partial, 2), pct_graded = pct_graded,
       earned = round(earned, 2), needed = round(max(0, needed), 2), remaining = remaining)
}

render_diagram <- function(diagram_data) {
  sections <- lapply(diagram_data, function(sec) {
    items_html <- lapply(sec$items, function(item) {
      if (is.list(item)) {
        tagList(
          tags$div(class = "dg-item fw-bold", style = paste0("color:", sec$color),
            if (!is.null(item$icon)) paste0(item$icon, " ") else "", item$label),
          if (!is.null(item$children)) {
            tags$div(class = "dg-sub ms-3",
              lapply(item$children, function(ch) tags$div(class = "dg-child", paste0("-> ", ch))))
          }
        )
      } else tags$div(class = "dg-item", paste0("-> ", item))
    })
    tags$div(class = "dg-section", style = paste0("border-left:4px solid ", sec$color, ";"),
      tags$div(class = "dg-title", style = paste0("background:", sec$color, "15;color:", sec$color),
        if (!is.null(sec$icon)) paste0(sec$icon, " ") else "", sec$title),
      tags$div(class = "dg-body", items_html))
  })
  title_sec <- diagram_data[[1]]
  tags$div(class = "dg-container",
    tags$div(class = "dg-main-title", style = paste0("background:", title_sec$color, ";color:#fff;"),
      if (!is.null(title_sec$main_title)) title_sec$main_title else title_sec$title),
    tags$div(class = "dg-grid", sections))
}

priority_class <- function(w) case_when(w >= 20 ~ "high", w >= 10 ~ "medium", TRUE ~ "low")
days_until <- function(date_str) as.integer(as.Date(date_str) - Sys.Date())

calcular_prioridades_estudio <- function(df_cursos, df_actividades, df_grades = NULL) {
  if (nrow(df_actividades) == 0) return(data.frame())
  today <- Sys.Date()
  pending <- df_actividades[df_actividades$done == 0 & as.Date(df_actividades$date) >= today - 3, ]
  if (nrow(pending) == 0) return(data.frame())
  max_credits <- max(df_cursos$credits, na.rm = TRUE)
  if (max_credits == 0) max_credits <- 1
  result <- do.call(rbind, lapply(seq_len(nrow(pending)), function(i) {
    act <- pending[i, ]
    cid <- act$course_id
    credits <- 0
    if (cid %in% df_cursos$id) credits <- df_cursos$credits[df_cursos$id == cid]
    f_credits <- credits / max_credits
    f_deficit <- 0.5
    if (!is.null(df_grades) && nrow(df_grades) > 0) {
      cg <- df_grades[df_grades$course_id == cid, ]
      if (nrow(cg) > 0) f_deficit <- (20 - mean(cg$grade, na.rm = TRUE)) / 20
    }
    act_weight <- if (!is.null(act$weight) && !is.na(act$weight)) act$weight else 0
    f_weight <- min(act_weight / 100, 1)
    is_cal <- TRUE
    if ("is_calificada" %in% names(act)) is_cal <- isTRUE(act$is_calificada)
    else if (act_weight == 0) is_cal <- FALSE
    days_left <- max(as.integer(as.Date(act$date) - today), 1)
    f_urgency <- 1 / (1 + days_left / 7)
    priority_score <- if (is_cal) {
      f_credits * 0.20 + f_deficit * 0.25 + f_weight * 0.25 + f_urgency * 0.30
    } else {
      f_credits * 0.15 + f_deficit * 0.35 + f_urgency * 0.50
    }
    temas <- if ("temas_vinculados" %in% names(act) && !is.null(act$temas_vinculados[[1]])) {
      paste(act$temas_vinculados[[1]], collapse = " | ")
    } else ""
    data.frame(course_id = cid,
      course_name = if (cid %in% df_cursos$id) df_cursos$short[df_cursos$id == cid] else cid,
      activity = act$name, act_id = act$act_id, type = act$type, weight = act_weight,
      is_calificada = is_cal, date = act$date, days_left = days_left, credits = credits,
      f_credits = round(f_credits, 3), f_deficit = round(f_deficit, 3),
      f_weight = round(f_weight, 3), f_urgency = round(f_urgency, 3),
      priority_score = round(priority_score, 3), temas = temas, stringsAsFactors = FALSE)
  }))
  result[order(-result$priority_score), ]
}

# ============ STARTUP COMPLETE ============
.elapsed <- (proc.time() - .startup_time)["elapsed"]
message(sprintf("[StudyPilot] global.R loaded in %.1fs - %d R/ + %d ui/ modules",
  .elapsed,
  length(list.files("R", pattern = "\\.R$")),
  length(list.files("ui", pattern = "\\.R$"))))

