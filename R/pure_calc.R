# ============ R/pure_calc.R — Calculo puro y testeable (Fase D+) ============
# Funciones SIN dependencia de Shiny/Mongo: probadas por tests/testthat.

# Nucleo del calculo de promedio por curso (extraido de calc_avg_fast).
# evals_c: data.frame(code, weight, name) ya filtrado/ordenado con codigos.
# grades_g: data.frame(code, grade).  pass_grade: umbral de aprobacion.
compute_course_avg <- function(evals_c, grades_g, pass_grade = 13) {
  empty <- list(partial = 0, pct_graded = 0, earned = 0, needed = NA,
                remaining = 100, remaining_evals = data.frame())
  if (is.null(evals_c) || nrow(evals_c) == 0) return(empty)
  if (is.null(grades_g) || nrow(grades_g) == 0)
    grades_g <- data.frame(code = character(), grade = numeric())
  total_w <- sum(evals_c$weight, na.rm = TRUE)
  if (total_w == 0) return(empty)
  name_col <- if ("name" %in% names(evals_c)) "name" else "code"
  merged <- merge(evals_c[, c("code", "weight", name_col)],
                  grades_g[, c("code", "grade")], by = "code", all.x = TRUE)
  graded <- merged[!is.na(merged$grade), ]
  earned <- if (nrow(graded) > 0) sum(graded$grade * graded$weight / 100, na.rm = TRUE) else 0
  pct <- if (nrow(graded) > 0) sum(graded$weight, na.rm = TRUE) else 0
  partial <- if (pct > 0) earned / (pct / 100) else 0
  remaining <- total_w - pct
  rem <- merged[is.na(merged$grade), c(name_col, "weight")]
  if (nrow(rem) > 0) names(rem) <- c("name", "weight")
  needed <- if (remaining > 0) (pass_grade - earned) / (remaining / 100) else 0
  list(partial = round(partial, 2), pct_graded = round(pct),
       earned = round(earned, 2), needed = round(max(0, needed), 2),
       remaining = round(remaining), remaining_evals = rem)
}

# Repaso espaciado: bloques de repaso en intervalos crecientes antes de cada examen
# (fundamento: efecto de espaciamiento / practica distribuida, Cepeda et al. 2006)
generar_repaso_espaciado <- function(all_a, courses_df, horizon_end,
                                     offsets = c(1, 3, 7, 14),
                                     hora_inicio = "19:00", dur_min = 90) {
  out <- data.frame(summary = character(), fecha = character(), hora_inicio = character(),
                    hora_fin = character(), color = character(), stringsAsFactors = FALSE)
  if (is.null(all_a) || nrow(all_a) == 0) return(out)
  hoy <- Sys.Date()
  ex <- all_a[all_a$done == 0 & !is.na(all_a$date) &
              all_a$type %in% c("examen", "quiz") &
              !is.na(all_a$weight) & all_a$weight >= 15, ]
  if (nrow(ex) == 0) return(out)
  hi <- as.integer(substr(hora_inicio, 1, 2)); mi <- as.integer(substr(hora_inicio, 4, 5))
  tot <- hi * 60 + mi + dur_min
  hora_fin <- sprintf("%02d:%02d:00", (tot %/% 60) %% 24, tot %% 60)
  for (i in seq_len(nrow(ex))) {
    d_ex <- as.Date(ex$date[i])
    cname <- if (ex$course_id[i] %in% courses_df$id) courses_df$short[courses_df$id == ex$course_id[i]] else ex$course_id[i]
    for (off in offsets) {
      d <- d_ex - off
      if (d < hoy || d > as.Date(horizon_end)) next
      out <- rbind(out, data.frame(
        summary = paste0("Repaso ", cname, ": ", ex$name[i]),
        fecha = as.character(d), hora_inicio = paste0(hora_inicio, ":00"),
        hora_fin = hora_fin, color = "teal", stringsAsFactors = FALSE))
    }
  }
  out[order(out$fecha), ]
}
