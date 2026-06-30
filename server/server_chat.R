# ============ server_chat.R — Chat flotante IA con contexto del usuario ============
# Sourced con local=TRUE. rv_chat definido aqui.
# El chat inyecta los datos reales del usuario (cursos, notas, actividades,
# horario) en el prompt para responder preguntas especificas.

rv_chat <- reactiveValues(messages = list())

# --------------------------------------------------------------------
# build_user_context(): arma un resumen de texto con TODOS los datos
# del usuario para que Gemini responda preguntas especificas.
# Replica la logica de calc_avg_fast para mapear notas (E1,E2...) a evaluaciones.
# --------------------------------------------------------------------
build_user_context <- function() {
  ctx <- c("=== DATOS ACADEMICOS DEL ESTUDIANTE (fuente: base de datos de la plataforma) ===")

  cached <- tryCatch(all_grades_cache(), error = function(e) data.frame())
  all_acts <- tryCatch(acts(), error = function(e) data.frame())
  today <- Sys.Date()

  # ---- Promedio global ponderado ----
  sw <- 0; sn <- 0
  for (cid in courses$id) {
    a <- tryCatch(calc_avg_fast(cid, cached), error = function(e) NULL)
    cr <- courses$credits[courses$id == cid]
    if (!is.null(a) && a$partial > 0) { sw <- sw + cr; sn <- sn + a$partial * cr }
  }
  prom_global <- if (sw > 0) round(sn / sw, 2) else NA
  ctx <- c(ctx, paste0("Promedio ponderado global estimado: ",
                       if (is.na(prom_global)) "sin notas aun" else prom_global,
                       " (sobre 20, basado en ", sum(courses$credits), " creditos)"))

  # ---- Detalle por curso ----
  if (nrow(courses) == 0) {
    ctx <- c(ctx, "El estudiante no tiene cursos registrados aun.")
    return(paste(ctx, collapse = "\n"))
  }

  for (i in seq_len(nrow(courses))) {
    c_info <- courses[i, ]
    cid <- c_info$id
    avg <- tryCatch(calc_avg_fast(cid, cached), error = function(e) NULL)

    ctx <- c(ctx, "",
      paste0("CURSO: ", c_info$name, " (codigo: ", cid, ")"),
      paste0("  Creditos: ", c_info$credits,
             " | Profesor: ", if (nchar(c_info$professor) > 0) c_info$professor else "no registrado"),
      if (!is.null(c_info$formula) && nchar(c_info$formula) > 0) paste0("  Formula de evaluacion: ", c_info$formula) else NULL)

    if (!is.null(avg) && avg$pct_graded > 0) {
      ctx <- c(ctx,
        paste0("  Puntos acumulados (asegurados de 20): ", avg$earned, "/20"),
        paste0("  Promedio parcial (sobre lo evaluado): ", avg$partial,
               "/20 | ", avg$pct_graded, "% del curso evaluado"))
      # Nota necesaria por evaluacion restante para aprobar (umbral 10.5)
      if (avg$remaining > 0) {
        rem_names <- if (is.data.frame(avg$remaining_evals) && nrow(avg$remaining_evals) > 0)
          paste0(avg$remaining_evals$name, " (", avg$remaining_evals$weight, "%)", collapse = ", ")
          else paste0(avg$remaining, "% restante")
        if (avg$needed <= 0) {
          ctx <- c(ctx, paste0("  Ya aseguro la aprobacion: necesita 0 en lo restante (", rem_names, ")"))
        } else if (avg$needed <= 20) {
          ctx <- c(ctx, paste0("  Para aprobar necesita ", avg$needed,
                               "/20 en cada evaluacion restante: ", rem_names))
        } else {
          ctx <- c(ctx, paste0("  Aprobar es muy dificil: necesitaria ", avg$needed,
                               "/20 (>20) en cada evaluacion restante: ", rem_names))
        }
      } else {
        ctx <- c(ctx, "  Curso completamente evaluado.")
      }
    } else {
      ctx <- c(ctx, "  Sin notas registradas aun.")
    }

    # Evaluaciones del curso con sus notas (replica calc_avg_fast)
    evals <- all_acts[all_acts$course_id == cid, ]
    if (nrow(evals) > 0) {
      if ("is_calificada" %in% names(evals)) {
        evals <- evals[is.na(evals$is_calificada) | evals$is_calificada == TRUE, ]
      }
      evals <- evals[!is.na(evals$weight) & evals$weight > 0, ]
      if (nrow(evals) > 0) {
        evals <- evals[order(evals$date), ]
        for (j in seq_len(nrow(evals))) {
          if (!"code" %in% names(evals) || is.null(evals$code[j]) ||
              is.na(evals$code[j]) || nchar(as.character(evals$code[j])) == 0) {
            if (!"code" %in% names(evals)) evals$code <- ""
            evals$code[j] <- paste0("E", j)
          }
        }
        g <- cached[cached$course_id == cid, ]
        ctx <- c(ctx, "  Evaluaciones:")
        for (j in seq_len(nrow(evals))) {
          ev <- evals[j, ]
          nota <- g$grade[g$code == ev$code]
          nota_txt <- if (length(nota) > 0 && !is.na(nota[1])) paste0(nota[1], "/20") else "sin nota aun"
          ctx <- c(ctx, paste0("    - ", ev$name, " (", ev$type, ", peso ", ev$weight, "%): ", nota_txt))
        }
      }
    }

    # Temas del curso
    topics <- tryCatch(course_topics[[cid]], error = function(e) NULL)
    if (!is.null(topics) && length(topics) > 0) {
      ctx <- c(ctx, paste0("  Temas: ", paste(head(topics, 12), collapse = "; ")))
    }
  }

  # ---- Actividades pendientes proximas ----
  if (nrow(all_acts) > 0) {
    pend <- all_acts[all_acts$done == 0 & !is.na(all_acts$date) &
                     as.Date(all_acts$date) >= today, ]
    if (nrow(pend) > 0) {
      pend <- pend[order(as.Date(pend$date)), ]
      pend <- pend[seq_len(min(8, nrow(pend))), ]
      ctx <- c(ctx, "", "PROXIMAS ENTREGAS/EVALUACIONES PENDIENTES:")
      for (k in seq_len(nrow(pend))) {
        r <- pend[k, ]
        cname <- if (r$course_id %in% courses$id) courses$short[courses$id == r$course_id] else r$course_id
        dl <- as.integer(as.Date(r$date) - today)
        ctx <- c(ctx, paste0("  - ", cname, ": ", r$name, " el ", r$date,
                             " (en ", dl, " dias, peso ", r$weight, "%)"))
      }
    }
  }

  # ---- Horario de clases ----
  sched <- tryCatch(mg_schedule_get(uid()), error = function(e) data.frame())
  if (is.data.frame(sched) && nrow(sched) > 0) {
    ctx <- c(ctx, "", "HORARIO DE CLASES:")
    for (k in seq_len(nrow(sched))) {
      s <- sched[k, ]
      ctx <- c(ctx, paste0("  - ", s$curso, ": ", s$dia, " ", s$hora_inicio, "-", s$hora_fin,
                           if (!is.null(s$aula) && nchar(s$aula) > 0) paste0(" (", s$aula, ")") else ""))
    }
  }

  ctx <- c(ctx, "", "=== FIN DE LOS DATOS ===")
  paste(ctx, collapse = "\n")
}

observeEvent(input$chat_send, {
  q <- trimws(input$chat_input)
  if (nchar(q) < 2) return()

  rv_chat$messages <- c(rv_chat$messages, list(list(role = "user", text = q)))
  updateTextInput(session, "chat_input", value = "")
  rv_chat$messages <- c(rv_chat$messages, list(list(role = "ai", text = "Pensando...")))

  # Construir contexto del usuario (dentro del contexto reactivo)
  user_ctx <- tryCatch(build_user_context(),
                       error = function(e) {
                         message("[StudyPilot] Context build error: ", e$message)
                         "(No se pudo cargar el contexto del estudiante.)"
                       })

  tryCatch({
    chat <- get_gemini()
    prompt <- paste0(
      "Eres el asistente academico personal de StudyPilot. Tienes acceso a los datos ",
      "reales del estudiante (abajo). Responde SIEMPRE en espanol, de forma clara y concisa.\n\n",
      "REGLAS:\n",
      "- Si la pregunta es sobre sus notas, cursos, promedios, evaluaciones, horario o ",
      "actividades, responde con los DATOS EXACTOS que aparecen abajo. Cita la nota o el ",
      "dato preciso (ej: 'En tu Examen Parcial de Data Analytics tienes 14/20').\n",
      "- Si pregunta por un curso, identificalo aunque use abreviaturas (ej: 'Data' = Data Analytics, ",
      "'Etica' = Etica y Tecnologia, 'PCO' = Planificacion y Control de Operaciones).\n",
      "- Distingue dos metricas: 'Puntos acumulados /20' son los puntos REALES ya asegurados del ",
      "curso (suma ponderada de notas obtenidas); 'Promedio parcial' es el promedio solo sobre lo ya ",
      "evaluado. Si preguntan 'cuantos puntos llevo' o 'como voy', usa los Puntos acumulados.\n",
      "- Si preguntan que nota necesitan para aprobar, usa el dato 'Para aprobar necesita X/20 en cada ",
      "evaluacion restante' y nombra las evaluaciones pendientes.\n",
      "- Si el dato no esta registrado, dilo claramente (ej: 'aun no tienes nota en esa evaluacion').\n",
      "- Si es una pregunta conceptual o de estudio, responde como tutor con definicion + ejemplo.\n",
      "- No inventes notas ni datos que no aparezcan abajo.\n\n",
      user_ctx, "\n\n",
      "PREGUNTA DEL ESTUDIANTE: ", q
    )
    response <- chat$chat(prompt)
    message("[StudyPilot] AI chat response length: ", nchar(response))
    msgs <- rv_chat$messages
    msgs[[length(msgs)]] <- list(role = "ai", text = as.character(response))
    rv_chat$messages <- msgs
  }, error = function(e) {
    message("[StudyPilot] AI chat ERROR: ", e$message)
    err_msg <- if (grepl("429", e$message)) {
      "La IA esta temporalmente saturada. Espera 1-2 minutos."
    } else if (grepl("503", e$message)) {
      "El servicio de IA no esta disponible. Intenta de nuevo."
    } else {
      paste0("Error: ", e$message)
    }
    msgs <- rv_chat$messages
    msgs[[length(msgs)]] <- list(role = "ai", text = err_msg)
    rv_chat$messages <- msgs
  })
})

output$chat_display <- renderUI({
  msgs <- rv_chat$messages
  msg_divs <- lapply(msgs, function(m) {
    cls <- if (m$role == "user") "chat-msg chat-user" else "chat-msg chat-ai"
    if (m$role == "ai") {
      tags$div(class = cls, HTML(tryCatch(
        markdown::markdownToHTML(text = m$text, fragment.only = TRUE),
        error = function(e) m$text
      )))
    } else {
      tags$div(class = cls, m$text)
    }
  })
  tags$div(class = "chat-container", id = "chat_messages_inner", msg_divs,
    tags$script(HTML(
      "var c=document.getElementById('chat_messages_inner');if(c)c.scrollTop=c.scrollHeight;"
    )))
})

outputOptions(output, "chat_display", suspendWhenHidden = FALSE)
