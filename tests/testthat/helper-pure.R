# Carga funciones puras de global.R y R/ para tests (sin Shiny/Mongo)
suppressWarnings(suppressMessages({
  library(dplyr); library(lubridate)
}))
# Constantes y helpers de negocio (replican global.R)
SEMESTER_START <- as.Date("2026-03-23"); TOTAL_WEEKS <- 16
PASS_GRADE <- 13; WARN_BAND <- 2.5
current_week <- function() { w <- as.integer(difftime(Sys.Date(), SEMESTER_START, units="weeks"))+1L; max(1L,min(w,TOTAL_WEEKS)) }
week_to_date <- function(w, eval_day=5) SEMESTER_START + (w-1)*7 + (eval_day-1)
date_to_week <- function(date_val){ d <- tryCatch(as.Date(date_val), error=function(e) NA); if(is.na(d)) return(0L); w <- as.integer(difftime(d,SEMESTER_START,units="weeks"))+1L; max(1L,min(w,TOTAL_WEEKS)) }
grade_tier <- function(avg){ if(is.null(avg)||avg$pct_graded==0) return("none"); if(avg$partial>=PASS_GRADE) return("ok"); if(avg$partial>=PASS_GRADE-WARN_BAND) return("warn"); "bad" }
grade_class <- function(avg) unname(c(none="secondary",ok="success",warn="warning",bad="danger")[grade_tier(avg)])
grade_hex   <- function(avg) unname(c(none="#94a3b8",ok="#16a34a",warn="#d97706",bad="#dc2626")[grade_tier(avg)])
priority_class <- function(w) dplyr::case_when(w>=20~"high", w>=10~"medium", TRUE~"low")


# Cargar el codigo REAL de las funciones puras (no copias)
for (.p in c("R/pure_calc.R", "../../R/pure_calc.R")) if (file.exists(.p)) { source(.p); break }
