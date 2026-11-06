# ============ R/analytics_calc.R — Deuda academica (metricas de preparacion por examen) ============
# Movido desde server/server_analytics.R: cada archivo de server/ se carga en su PROPIO entorno
# (source(local = TRUE) dentro de lapply), asi que server_dashboard.R no veia esta funcion y su
# alerta de deuda academica siempre daba 0. Aqui la ven todos los modulos.
#
# Logica: para cada examen/quiz pendiente con peso >= 15 %:
#   1. Temas objetivo = max(temas vinculados a la actividad, temas del curso, 1).
#   2. Bloques IA de estudio del curso (se adivina el curso buscando su codigo, nombre o nombre
#      corto dentro del titulo del bloque; se excluyen los "Descanso").
#   3. ratio = min(bloques / temas objetivo, 1) -> alerta critica (< 0.3 y <= 7 dias), alta (< 0.5),
#      media (< 0.75) u ok.
calc_prep_metrics <- function(all_acts, ai_blocks, course_topics_list, courses_df = get_courses(),
                              today = Sys.Date()) {
  if (is.null(all_acts) || nrow(all_acts) == 0) return(data.frame())
  if (is.null(courses_df)) courses_df <- data.frame(id = character(), name = character(), short = character())
  fechas <- suppressWarnings(as.Date(all_acts$date))
  # Examenes pendientes con peso >= 15% (las filas sin fecha o sin peso se descartan)
  keep <- all_acts$done == 0 & all_acts$type %in% c("examen", "quiz") &
          all_acts$weight >= 15 & fechas >= today
  exams <- all_acts[!is.na(keep) & keep, ]
  if (nrow(exams) == 0) return(data.frame())

  exams <- exams[order(as.Date(exams$date)), ]

  metrics <- do.call(rbind, lapply(seq_len(nrow(exams)), function(i) {
    ex <- exams[i, ]
    cid <- ex$course_id

    # Temas vinculados al curso
    topics <- course_topics_list[[cid]]
    n_topics <- if (!is.null(topics)) length(topics) else 0

    # Temas vinculados a esta actividad especifica
    act_topics <- if ("temas_vinculados" %in% names(ex) && !is.null(ex$temas_vinculados[[1]])) {
      ex$temas_vinculados[[1]]
    } else character(0)
    n_act_topics <- length(act_topics)

    # Usar temas de la actividad si existen, sino del curso
    n_target <- max(n_act_topics, n_topics, 1)

    # Bloques de estudio IA para este curso
    n_blocks <- 0
    if (!is.null(ai_blocks) && is.data.frame(ai_blocks) && nrow(ai_blocks) > 0) {
      cname_full <- if (cid %in% courses_df$id) courses_df$name[courses_df$id == cid][1] else ""
      cshort <- if (cid %in% courses_df$id) courses_df$short[courses_df$id == cid][1] else ""
      course_blocks <- ai_blocks[
        grepl(cid, ai_blocks$summary, fixed = TRUE) |
        (nchar(cname_full) > 3 & grepl(cname_full, ai_blocks$summary, fixed = TRUE)) |
        (nchar(cshort) > 3 & grepl(cshort, ai_blocks$summary, fixed = TRUE)), , drop = FALSE]

      # Excluir bloques de descanso
      course_blocks <- course_blocks[!grepl("Descanso|descanso", course_blocks$summary), , drop = FALSE]
      n_blocks <- nrow(course_blocks)
    }

    # Ratio: bloques completados vs temas objetivo
    # Cada tema necesita al menos 1 bloque de estudio
    prep_ratio <- min(n_blocks / n_target, 1.0)
    days_left <- as.integer(as.Date(ex$date) - today)

    # Nivel de alerta
    alert <- if (prep_ratio < 0.3 && days_left <= 7) "critica"
             else if (prep_ratio < 0.5) "alta"
             else if (prep_ratio < 0.75) "media"
             else "ok"

    cname <- if (cid %in% courses_df$id) courses_df$short[courses_df$id == cid][1] else cid

    data.frame(
      course_id = cid, course_name = cname, exam_name = ex$name,
      weight = ex$weight, date = ex$date, days_left = days_left,
      n_topics = n_target, n_blocks = n_blocks,
      prep_ratio = round(prep_ratio, 2), alert = alert,
      stringsAsFactors = FALSE
    )
  }))

  metrics
}

# ============ EXAMENES PENDIENTES (selector de "Modo examen") ============
# Examen/quiz pendiente con peso >= 15 %, ordenado por fecha, con dias restantes.
# Lo usan server_examen.R (selector) y server_pomodoro.R ("Estudiar con Pomodoro").
pending_exams <- function(all_acts, today = Sys.Date()) {
  if (is.null(all_acts) || nrow(all_acts) == 0) return(all_acts)
  a <- dplyr::filter(all_acts, done == 0, weight >= 15, type %in% c("examen", "quiz"))
  a <- dplyr::mutate(a, days_left = as.integer(as.Date(date) - today))
  dplyr::arrange(a, date)
}

# input$exam_select es la POSICION (1-based) en pending_exams(), no un act_id.
exam_at <- function(all_acts, val, today = Sys.Date()) {
  if (is.null(val) || length(val) != 1 || is.na(val) || val == "" || val == "0") return(NULL)
  idx <- suppressWarnings(as.integer(val))
  if (is.na(idx) || idx < 1) return(NULL)
  a <- pending_exams(all_acts, today)
  if (is.null(a) || idx > nrow(a)) return(NULL)
  a[idx, ]
}
