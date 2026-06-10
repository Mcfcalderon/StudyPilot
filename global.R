library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(DT)
library(htmltools)
library(markdown)

# ============ SEMESTER CONFIG ============
SEMESTER_START <- as.Date("2026-03-23")
TOTAL_WEEKS <- 16
current_week <- function() {
  w <- as.integer(difftime(Sys.Date(), SEMESTER_START, units = "weeks")) + 1L
  max(1L, min(w, TOTAL_WEEKS))
}
week_to_date <- function(w, eval_day = 5) {
  # eval_day: 1=Mon,...,6=Sat
  SEMESTER_START + (w - 1) * 7 + (eval_day - 1)
}

# ============ COURSE DATA (empty — courses are created via AI syllabus extraction) ============
courses <- tibble::tibble(
  id = character(), name = character(), short = character(),
  credits = integer(), professor = character(), formula = character(),
  eval_day = integer(), color = character()
)

# ============ EVALUATIONS (empty — created via AI syllabus extraction) ============
evaluations <- tibble::tibble(
  course_id = character(), code = character(), label = character(),
  weight = numeric(), week = integer(), type = character(), grade = numeric()
)

# ============ TOPICS PER COURSE (empty — populated from AI syllabus extraction) ============
course_topics <- list()

# No course-specific data — everything is created dynamically via AI

# ============ SCHEDULE ============
# Fixed schedule removed — calendar now syncs from Google Calendar only

# ============ DATABASE (MongoDB) ============
# Functions are in db_mongo.R, sourced from app.R

# Auto-complete old activities without grades (older than 2 weeks)
auto_complete_old <- function() {
  tryCatch({
    a <- mg_activities_all()
    cutoff <- as.character(Sys.Date() - 14)
    old_pending <- a |> filter(done == 0, date < cutoff)
    if (nrow(old_pending) > 0) {
      for (i in seq_len(nrow(old_pending))) {
        aid <- old_pending$act_id[i]
        mg_activity_toggle(aid, 1L)
      }
    }
  }, error = function(e) NULL)
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

  list(
    partial = round(partial, 2),
    pct_graded = pct_graded,
    earned = round(earned, 2),
    needed = round(max(0, needed), 2),
    remaining = remaining
  )
}

# ============ RENDER DIAGRAM AS HTML ============
render_diagram <- function(diagram_data) {
  # diagram_data = list of sections: list(title, color, icon, items)
  # items = character vector of bullet points
  sections <- lapply(diagram_data, function(sec) {
    items_html <- lapply(sec$items, function(item) {
      if (is.list(item)) {
        # Sub-item with its own children
        tagList(
          tags$div(class = "dg-item fw-bold", style = paste0("color:", sec$color),
            if (!is.null(item$icon)) paste0(item$icon, " ") else "", item$label
          ),
          if (!is.null(item$children)) {
            tags$div(class = "dg-sub ms-3",
              lapply(item$children, function(ch) {
                tags$div(class = "dg-child", paste0("→ ", ch))
              })
            )
          }
        )
      } else {
        tags$div(class = "dg-item", paste0("→ ", item))
      }
    })

    tags$div(class = "dg-section",
      style = paste0("border-left:4px solid ", sec$color, ";"),
      tags$div(class = "dg-title", style = paste0("background:", sec$color, "15;color:", sec$color),
        if (!is.null(sec$icon)) paste0(sec$icon, " ") else "",
        sec$title
      ),
      tags$div(class = "dg-body", items_html)
    )
  })

  # Main title
  title_sec <- diagram_data[[1]]
  tags$div(class = "dg-container",
    tags$div(class = "dg-main-title",
      style = paste0("background:", title_sec$color, ";color:#fff;"),
      if (!is.null(title_sec$main_title)) title_sec$main_title else title_sec$title
    ),
    tags$div(class = "dg-grid", sections)
  )
}

priority_class <- function(w) {
  case_when(w >= 20 ~ "high", w >= 10 ~ "medium", TRUE ~ "low")
}

days_until <- function(date_str) {
  as.integer(as.Date(date_str) - Sys.Date())
}
