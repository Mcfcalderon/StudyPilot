# ============ AI FUNCTIONS (Google Gemini via ellmer) ============
library(ellmer)
if (requireNamespace("blastula", quietly = TRUE)) library(blastula)

# ============ API KEY ROTATION ============
# Keys loaded from environment variable GEMINI_KEYS (comma-separated)
.gemini_keys <- {
  raw <- Sys.getenv("GEMINI_KEYS")
  if (nchar(raw) > 0) trimws(strsplit(raw, ",")[[1]])
  else {
    single <- Sys.getenv("GEMINI_API_KEY")
    if (nchar(single) > 0) single
    else {
      warning("[StudyPilot] GEMINI_KEYS not set. AI functions will fail. See .Renviron.example")
      "PLACEHOLDER_KEY"
    }
  }
}
assign(".gemini_key_idx", 1L, envir = globalenv())

get_gemini <- function() {
  env_key <- Sys.getenv("GEMINI_API_KEY")
  idx <- get0(".gemini_key_idx", envir = globalenv(), ifnotfound = 1L)
  key <- if (nchar(env_key) > 0) env_key else .gemini_keys[idx]
  # Set env var so ellmer picks it up automatically (avoids credentials type issues)
  Sys.setenv(GOOGLE_API_KEY = key)
  chat_google_gemini(model = "gemini-2.5-flash")
}

# Rotate to next key (called on 429 errors)
rotate_key <- function() {
  idx <- get0(".gemini_key_idx", envir = globalenv(), ifnotfound = 1L)
  new_idx <- (idx %% length(.gemini_keys)) + 1L
  assign(".gemini_key_idx", new_idx, envir = globalenv())
  message("[StudyPilot] Rotated API key to #", new_idx)
  new_idx
}

# Smart AI call with automatic key rotation on 429/503
ai_call <- function(prompt) {
  last_error <- "Error desconocido"
  for (attempt in seq_along(.gemini_keys)) {
    result <- tryCatch({
      chat <- get_gemini()
      response <- chat$chat(prompt)
      return(response)
    }, error = function(e) {
      if (grepl("429|503|502|rate", e$message, ignore.case = TRUE)) {
        message("[StudyPilot] Key #", get0(".gemini_key_idx", envir = globalenv()), " error: ", substr(e$message, 1, 60), " - rotating...")
        rotate_key()
        Sys.sleep(2)
        return(NULL)
      }
      last_error <<- e$message
      message("[StudyPilot] AI non-retryable error: ", e$message)
      rotate_key()
      return(NULL)
    })
    if (!is.null(result)) return(result)
  }
  stop(paste0("Error de IA tras ", length(.gemini_keys), " intentos: ", last_error))
}

# ============ GENERATE STUDY SUMMARY FROM TEXT ============
ai_generate_summary <- function(text, course_name, topic_hint = NULL) {
  prompt <- paste0(
    "Eres un tutor universitario experto de apoyo académico.\n\n",
    "CURSO: ", course_name, "\n",
    if (!is.null(topic_hint)) paste0("TEMA RELACIONADO: ", topic_hint, "\n") else "",
    "\nA partir del siguiente material de estudio, genera:\n\n",
    "1. **RESUMEN** (máximo 150 palabras, claro y conciso)\n",
    "2. **CONCEPTOS CLAVE** (lista de 4-8 conceptos con definición breve de 1-2 líneas cada uno, formato: '**CONCEPTO**: definición')\n",
    "3. **MAPA CONCEPTUAL** — Genera un diagrama en formato Mermaid dentro de un bloque de código ```mermaid. ",
    "Usa 'graph TD' (top-down). Usa IDs cortos sin espacios (ej: A, B, C1). Pon las etiquetas entre corchetes [\"texto\"]. ",
    "No uses paréntesis ni caracteres especiales en las etiquetas. Máximo 12 nodos. Ejemplo:\n",
    "```mermaid\ngraph TD\n  A[\"Tema Principal\"] --> B[\"Subtema 1\"]\n  A --> C[\"Subtema 2\"]\n  B --> D[\"Detalle\"]\n```\n",
    "4. **PREGUNTAS DE REPASO** (5 preguntas que el estudiante debería poder responder)\n\n",
    "Responde EN ESPAÑOL. Usa formato claro con los encabezados exactos: RESUMEN, CONCEPTOS CLAVE, MAPA CONCEPTUAL, PREGUNTAS DE REPASO.\n\n",
    "--- MATERIAL ---\n",
    # Limit text to ~4000 chars to avoid token limits
    substr(text, 1, 4000),
    if (nchar(text) > 4000) "\n[... material truncado ...]" else ""
  )

  tryCatch({
    ai_call(prompt)
  }, error = function(e) {
    paste0("Error al generar resumen con IA: ", e$message)
  })
}

# ============ GENERATE PRACTICE QUESTIONS FROM TEXT ============
ai_generate_questions <- function(text, course_name, n_questions = 5) {
  prompt <- paste0(
    "Eres un profesor universitario experto en ", course_name, ".\n\n",
    "A partir del siguiente material, genera exactamente ", n_questions, " preguntas de examen.\n\n",
    "Para cada pregunta usa EXACTAMENTE este formato (incluye los marcadores):\n",
    "---PREGUNTA---\n",
    "TIPO: [MC o ABIERTA]\n",
    "TEMA: [tema de la pregunta]\n",
    "PREGUNTA: [la pregunta]\n",
    "OPCIONES: [solo para MC, 4 opciones separadas por |]\n",
    "RESPUESTA: [número 1-4 para MC, o guía de respuesta para ABIERTA]\n",
    "EXPLICACION: [por qué esa es la respuesta correcta]\n\n",
    "Genera una mezcla de preguntas MC y ABIERTAS. Responde EN ESPAÑOL.\n\n",
    "--- MATERIAL ---\n",
    substr(text, 1, 4000),
    if (nchar(text) > 4000) "\n[... material truncado ...]" else ""
  )

  tryCatch({
    response <- ai_call(prompt)
    parse_ai_questions(response)
  }, error = function(e) {
    list(error = paste0("Error: ", e$message))
  })
}

# ============ PARSE AI-GENERATED QUESTIONS ============
parse_ai_questions <- function(response_text) {
  parts <- strsplit(response_text, "---PREGUNTA---")[[1]]
  parts <- parts[nchar(trimws(parts)) > 10]

  # Field markers in order — each field ends where the next begins
  markers <- c("TIPO", "TEMA", "PREGUNTA", "OPCIONES", "RESPUESTA", "EXPLICACION")

  questions <- lapply(parts, function(part) {
    # Extract field by capturing text between this marker and the next one
    get_field <- function(field) {
      idx <- which(markers == field)
      if (idx < length(markers)) {
        remaining <- paste(markers[(idx+1):length(markers)], collapse = "|")
        pattern <- paste0("(?s)", field, ":\\s*(.*?)\\s*(?=(?:", remaining, "):|$)")
      } else {
        pattern <- paste0("(?s)", field, ":\\s*(.*?)\\s*$")
      }
      m <- regmatches(part, regexpr(pattern, part, perl = TRUE))
      if (length(m) > 0) {
        trimws(sub(paste0("(?s)", field, ":\\s*"), "", m, perl = TRUE))
      } else ""
    }

    type_raw <- toupper(get_field("TIPO"))
    type <- if (grepl("MC", type_raw)) "mc" else "open"
    tema <- trimws(get_field("TEMA"))
    pregunta <- trimws(get_field("PREGUNTA"))
    opciones_raw <- trimws(get_field("OPCIONES"))
    respuesta <- trimws(get_field("RESPUESTA"))
    explicacion <- trimws(get_field("EXPLICACION"))

    if (nchar(pregunta) < 5) return(NULL)

    if (type == "mc" && nchar(opciones_raw) > 0) {
      opts <- trimws(strsplit(opciones_raw, "\\|")[[1]])
      # Remove leading numbers like "1. " or "1) "
      opts <- sub("^\\d+[.)\\s]+", "", opts)
      ans_num <- suppressWarnings(as.integer(sub("\\D.*", "", respuesta)))
      if (is.na(ans_num) || ans_num < 1 || ans_num > length(opts)) ans_num <- 1
      list(topic = tema, type = "mc", q = pregunta, opts = opts, ans = ans_num, expl = explicacion)
    } else {
      list(topic = tema, type = "open", q = pregunta, ans_guide = respuesta, expl = explicacion)
    }
  })

  Filter(Negate(is.null), questions)
}

# ============ GENERATE STUDY GUIDE FOR ANY COURSE ============
ai_generate_study_guide <- function(course_name, topics, syllabus_text = NULL) {
  topics_text <- paste(seq_along(topics), topics, sep = ". ", collapse = "\n")
  extra <- if (!is.null(syllabus_text) && nchar(syllabus_text) > 50)
    paste0("\n\nMATERIAL DEL SÍLABO:\n", substr(syllabus_text, 1, 3000)) else ""

  prompt <- paste0(
    "Eres un tutor experto de ", course_name, ".\n\n",
    "Genera una GUÍA DE ESTUDIO COMPLETA para cada tema del curso.\n\n",
    "TEMAS:\n", topics_text, extra, "\n\n",
    "Para CADA tema genera:\n",
    "## [Número]. [Nombre del tema]\n",
    "### Resumen\n[2-3 párrafos claros]\n",
    "### Conceptos Clave\n[4-6 conceptos con definición de 1-2 líneas, formato: **Concepto**: definición]\n",
    "### Mapa Conceptual\n[Diagrama con → mostrando relaciones jerárquicas]\n",
    "### Preguntas de Repaso\n[3-4 preguntas clave]\n\n",
    "Responde EN ESPAÑOL. Usa formato Markdown."
  )

  tryCatch({
    ai_call(prompt)
  }, error = function(e) paste0("Error: ", e$message))
}

# ============ EXTRACT SYLLABUS WITH AI ============
ai_extract_syllabus <- function(syllabus_text) {
  lines <- strsplit(syllabus_text, "\n")[[1]]

  # 1. COURSE HEADER: find "DATOS GENERALES" or course code pattern
  header_idx <- grep("DATOS GENERALES|^\\s*[A-Z]{2}\\d{4}\\s*[–-]", lines, ignore.case = FALSE)
  header_text <- ""
  if (length(header_idx) > 0) {
    h_start <- max(1, min(header_idx) - 3)
    h_end <- min(length(lines), min(header_idx) + 20)
    header_text <- paste(lines[h_start:h_end], collapse = "\n")
  } else {
    header_text <- substr(syllabus_text, 1, 2000)
  }

  # 2. TOPICS: find "TEMAS" or "CONTENIDO" section
  tema_idx <- grep("TEMAS$|TEMAS\\s*$|CONTENIDO|UNIDADES TEM|SUMILLA|DESCRIPCI.N DEL CURSO", lines, ignore.case = TRUE)
  topic_text <- ""
  if (length(tema_idx) > 0) {
    t_start <- max(1, min(tema_idx) - 1)
    t_end <- min(length(lines), min(tema_idx) + 60)
    topic_text <- paste(lines[t_start:t_end], collapse = "\n")
  }

  # 3. EVALUATION: find "SISTEMA DE EVALUACIÓN"
  eval_idx <- grep("SISTEMA DE EVALUACI|EVALUACI.N CONTINUA.*%|EVALUACI.N PARCIAL.*%", lines, ignore.case = TRUE)
  eval_text <- ""
  if (length(eval_idx) > 0) {
    e_start <- max(1, min(eval_idx) - 3)
    e_end <- min(length(lines), min(eval_idx) + 60)
    eval_text <- paste(lines[e_start:e_end], collapse = "\n")
  }

  combined <- paste0(
    "--- INFORMACIÓN DEL CURSO ---\n\n", header_text,
    "\n\n--- TEMAS DEL CURSO ---\n\n", topic_text,
    "\n\n--- SISTEMA DE EVALUACIÓN ---\n\n", eval_text)

  prompt <- paste0(
    "Extrae la información de este sílabo universitario en formato JSON EXACTO (sin markdown, sin ```json):\n\n",
    '{\n',
    '  "nombre_curso": "Nombre completo del curso",\n',
    '  "codigo": "Código del curso (ej: IN3012)",\n',
    '  "creditos": 4,\n',
    '  "profesor": "Nombre del profesor",\n',
    '  "formula": "Fórmula de evaluación (ej: EF 30% + EP 20% + EC 50%)",\n',
    '  "evaluaciones": [\n',
    '    {"nombre": "Examen Parcial", "codigo": "EP1", "peso": 20, "semana": 8, "tipo": "examen"},\n',
    '    {"nombre": "Trabajo Final", "codigo": "TF1", "peso": 30, "semana": 16, "tipo": "proyecto"}\n',
    '  ],\n',
    '  "temas": ["Tema 1", "Tema 2", "Tema 3"]\n',
    '}\n\n',
    "REGLAS:\n",
    "- tipo debe ser: examen, quiz, ec (evaluación continua), o proyecto\n",
    "- semana es el número de semana del ciclo (1-16) donde se realiza la evaluación\n",
    "- peso es el porcentaje (número sin el símbolo %)\n",
    "- codigo: abreviatura corta (EP1, EF1, EC1, TF1, etc.)\n",
    "- INCLUYE TODAS las evaluaciones que aparezcan con su peso y semana\n",
    "- Si no encuentras un dato, pon null\n",
    "- Responde SOLO el JSON, sin texto adicional\n\n",
    "--- SÍLABO ---\n",
    combined
  )

  tryCatch({
    response <- ai_call(prompt)
    response <- gsub("```json\\s*", "", response)
    response <- gsub("```\\s*", "", response)
    response <- trimws(response)
    parsed <- jsonlite::fromJSON(response, simplifyVector = TRUE)
    parsed
  }, error = function(e) {
    list(error = paste0("Error al extraer: ", e$message))
  })
}

# ============ SEND EMAIL NOTIFICATION ============
# ============ SEND VERIFICATION CODE ============
get_smtp_creds <- function() {
  Sys.setenv(SMTP_PASSWORD = Sys.getenv("SMTP_PASS"))
  user <- Sys.getenv("SMTP_USER")
  list(user = user, cred = blastula::creds_envvar(user = user, pass_envvar = "SMTP_PASSWORD",
    host = "smtp.gmail.com", port = 465, use_ssl = TRUE))
}

send_verification_code <- function(to_email, code) {
  body <- paste0(
    "<div style='font-family:sans-serif;max-width:400px;margin:auto;padding:20px'>",
    "<h2 style='color:#2563eb'>🚀 StudyPilot</h2>",
    "<p>Tu código de verificación es:</p>",
    "<div style='background:#f1f5f9;padding:20px;text-align:center;border-radius:10px;margin:16px 0'>",
    "<span style='font-size:2rem;font-weight:800;letter-spacing:8px;color:#1e293b'>", code, "</span></div>",
    "<p style='color:#64748b;font-size:12px'>Este código expira en 10 minutos.</p></div>"
  )
  tryCatch({
    if (!requireNamespace("blastula", quietly = TRUE)) return("❌ blastula no disponible")
    s <- get_smtp_creds()
    email <- blastula::compose_email(body = blastula::md(body))
    blastula::smtp_send(email, from = s$user, to = to_email, subject = "🚀 StudyPilot — Código de Verificación", credentials = s$cred)
    "✅ Código enviado"
  }, error = function(e) paste0("❌ ", e$message))
}

send_eval_reminder <- function(to_email, student_name, upcoming_evals, subject_extra = NULL) {
  if (nrow(upcoming_evals) == 0) return("No hay evaluaciones próximas.")

  rows <- paste(apply(upcoming_evals, 1, function(r) {
    paste0("<tr><td>", r["course"], "</td><td>", r["name"], "</td><td><b>", r["weight"], "%</b></td><td>", r["date"], "</td><td>", r["days"], "d</td></tr>")
  }), collapse = "\n")

  body <- paste0(
    "<h2>🚀 StudyPilot — Recordatorio de Evaluaciones</h2>",
    "<p>Hola <b>", student_name, "</b>, tienes estas evaluaciones próximas:</p>",
    "<table border='1' cellpadding='8' style='border-collapse:collapse;font-family:sans-serif;font-size:14px'>",
    "<tr style='background:#2563eb;color:#fff'><th>Curso</th><th>Evaluación</th><th>Peso</th><th>Fecha</th><th>Días</th></tr>",
    rows, "</table>",
    "<p style='color:#64748b;font-size:12px;margin-top:16px'>Enviado desde StudyPilot</p>"
  )

  tryCatch({
    if (!requireNamespace("blastula", quietly = TRUE)) return("❌ blastula no disponible")
    s <- get_smtp_creds()
    email <- blastula::compose_email(body = blastula::md(body))
    blastula::smtp_send(email, from = s$user, to = to_email,
      subject = "🚀 StudyPilot — Evaluaciones Próximas", credentials = s$cred)
    "✅ Email enviado"
  }, error = function(e) paste0("❌ ", e$message))
}

# ============ EXTRACT SCHEDULE FROM PDF ============
ai_extract_schedule <- function(schedule_text) {
  # For short documents (< 10K chars), send everything — no filtering needed
  # For longer docs, extract schedule-relevant sections
  if (nchar(schedule_text) <= 10000) {
    relevant_text <- schedule_text
  } else {
    lines <- strsplit(schedule_text, "\n")[[1]]
    sched_idx <- grep("Horario|Lun|Mar|Mi[eé]|Jue|Vie|S[aá]b|Teor[ií]a|Laboratorio|\\d{2}:\\d{2}", lines, ignore.case = TRUE)
    if (length(sched_idx) > 0) {
      s_start <- max(1, min(sched_idx) - 5)
      s_end <- min(length(lines), max(sched_idx) + 5)
      relevant_text <- paste(lines[s_start:s_end], collapse = "\n")
    } else {
      relevant_text <- substr(schedule_text, 1, 10000)
    }
  }
  if (nchar(relevant_text) > 10000) relevant_text <- substr(relevant_text, 1, 10000)

  prompt <- paste0(
    "Eres un asistente que extrae horarios de clases universitarias de documentos.\n",
    "Del siguiente texto, extrae TODOS los bloques de clase en formato JSON.\n",
    "Responde SOLO el JSON (array), sin markdown ni ```json.\n\n",
    "Formato: [{\"dia\":\"Lunes\",\"hora_inicio\":\"08:00\",\"hora_fin\":\"10:00\",\"curso\":\"Nombre\",\"codigo\":\"XX0000\",\"aula\":\"A-101\"}]\n\n",
    "REGLAS:\n",
    "- dia: Lunes, Martes, Miércoles, Jueves, Viernes, Sábado (SIEMPRE nombre completo)\n",
    "- Convierte abreviaturas: Lun.=Lunes, Mar.=Martes, Mié.=Miércoles, Jue.=Jueves, Vie.=Viernes, Sáb.=Sábado\n",
    "- hora_inicio/hora_fin: formato HH:MM (24h)\n",
    "- Busca patrones como 'Teoría:15:00-18:00 A906' o '15:00 - 18:00'\n",
    "- Si no hay aula, pon \"TBD\"\n",
    "- Si no hay hora_fin, calcula sumando 2 horas a hora_inicio\n",
    "- Incluye TODAS las clases: Teoría, Laboratorio, Práctica — cada bloque como entrada separada\n",
    "- Si un curso tiene Teoría el Lunes y Laboratorio el Viernes, son 2 entradas separadas\n",
    "- Si el documento es un consolidado de matrícula (con columna Horario), extrae cada bloque de horario\n",
    "- Si no encuentras horarios, devuelve array vacío []\n\n",
    "--- DOCUMENTO ---\n",
    relevant_text
  )

  message("[StudyPilot] Schedule extraction: sending ", nchar(relevant_text), " chars to AI")

  tryCatch({
    response <- ai_call(prompt)
    message("[StudyPilot] AI schedule response length: ", nchar(response))
    response <- gsub("```json\\s*", "", response)
    response <- gsub("```\\s*", "", response)
    response <- trimws(response)
    if (response == "[]" || nchar(response) < 5) return(data.frame())
    parsed <- jsonlite::fromJSON(response, simplifyVector = TRUE)
    if (is.data.frame(parsed) && nrow(parsed) > 0) {
      for (col in c("dia", "hora_inicio", "hora_fin", "curso", "codigo", "aula")) {
        if (!col %in% names(parsed)) parsed[[col]] <- "TBD"
      }
      return(parsed)
    }
    data.frame()
  }, error = function(e) {
    message("[StudyPilot] Schedule extraction error: ", e$message)
    data.frame()
  })
}

# ============ SMART SCHEDULER: LLM STUDY BLOCK GENERATION ============
generate_smart_schedule_llm <- function(df_prioridades, df_free_time,
                                       weeks_remaining = 1, semester_end = NULL) {
  if (nrow(df_prioridades) == 0 || nrow(df_free_time) == 0) return(data.frame())

  # Build JSON inputs for the LLM
  study_items <- lapply(seq_len(min(nrow(df_prioridades), 15)), function(i) {
    r <- df_prioridades[i, ]
    list(
      curso = r$course_name,
      actividad = r$activity,
      tipo = r$type,
      peso = r$weight,
      dias_restantes = r$days_left,
      prioridad = r$priority_score,
      temas = if (nchar(r$temas) > 0) strsplit(r$temas, " \\| ")[[1]] else list(),
      es_calificada = r$is_calificada
    )
  })

  free_slots <- lapply(seq_len(min(nrow(df_free_time), 40)), function(i) {
    r <- df_free_time[i, ]
    list(fecha = r$date, dia = r$day, inicio = r$start_time,
         fin = r$end_time, duracion_min = r$duration_min)
  })

  study_json <- jsonlite::toJSON(study_items, auto_unbox = TRUE, pretty = FALSE)
  slots_json <- jsonlite::toJSON(free_slots, auto_unbox = TRUE, pretty = FALSE)

  prompt <- paste0(
    "Eres un planificador académico inteligente. Asigna bloques de estudio ",
    "en los huecos libres del calendario del estudiante.\n\n",
    "REGLAS ESTRICTAS:\n",
    "1. Solo usa los huecos libres proporcionados, NUNCA sobrepongas eventos existentes\n",
    "2. Cada bloque de estudio debe durar MÁXIMO 1.5 horas (90 min). Si hay más tiempo libre, fragmenta en sesiones de 60-90 min con descansos de 15 min entre ellas\n",
    "3. MÁXIMO 4 HORAS de estudio generado por día. Si hay más temas, pásalos a los días siguientes\n",
    "4. Prioriza las actividades con mayor score de prioridad\n",
    "5. TÍTULOS: Usa EXACTAMENTE este formato: '🤖 [Nombre del Curso]: [Tema específico]'. Si no hay tema vinculado, usa '🤖 [Curso]: Repaso general'\n",
    "6. Alterna entre cursos para evitar fatiga (no más de 2 bloques seguidos del mismo curso)\n",
    "7. Incluye bloques de descanso de 15 min entre sesiones: titulo='☕ Descanso', tipo_bloque='descanso'\n",
    "8. Para actividades calificadas, sesiones de 75-90 min\n",
    "9. Para actividades formativas (no calificadas), sesiones de 45-60 min\n",
    "10. NO asignes estudio entre 22:00 y 07:00\n",
    "11. Prefiere horarios de mañana (08:00-12:00) y tarde (14:00-18:00)\n\n",
    "RESPONDE SOLO el JSON (sin markdown, sin ```json), con este formato exacto:\n",
    '[{"fecha":"2026-06-12","dia":"Jueves","hora_inicio":"09:00","hora_fin":"10:30",',
    '"titulo":"\\ud83e\\udd16 Data Analytics: Clustering y K-means",',
    '"curso":"Data Analytics","tipo_bloque":"estudio","prioridad":"alta"}]\n\n',
    "--- CONTEXTO DEL SEMESTRE ---\n",
    "Semanas restantes hasta fin de ciclo: ", weeks_remaining, "\n",
    if (!is.null(semester_end)) paste0("Fecha fin del horizonte: ", semester_end, "\n") else "",
    "Distribuye el estudio proporcionalmente: si un examen es en 3 semanas, empieza ",
    "con sesiones ligeras ahora y aumenta la intensidad conforme se acerque.\n\n",
    "--- ACTIVIDADES A ESTUDIAR (ordenadas por prioridad) ---\n", study_json,
    "\n\n--- HUECOS LIBRES DISPONIBLES (esta semana) ---\n", slots_json
  )

  message("[StudyPilot] Smart Scheduler: sending ", nrow(df_prioridades), " activities + ",
          nrow(df_free_time), " free slots to AI")

  tryCatch({
    response <- ai_call(prompt)
    message("[StudyPilot] Smart Scheduler AI response: ", nchar(response), " chars")
    response <- gsub("```json\\s*", "", response)
    response <- gsub("```\\s*", "", response)
    response <- trimws(response)
    if (nchar(response) < 5 || response == "[]") return(data.frame())
    parsed <- jsonlite::fromJSON(response, simplifyVector = TRUE)
    if (is.data.frame(parsed) && nrow(parsed) > 0) {
      # Ensure required columns
      for (col in c("fecha", "dia", "hora_inicio", "hora_fin", "titulo", "curso", "tipo_bloque", "prioridad")) {
        if (!col %in% names(parsed)) parsed[[col]] <- ""
      }
      return(parsed)
    }
    data.frame()
  }, error = function(e) {
    message("[StudyPilot] Smart Scheduler error: ", e$message)
    data.frame()
  })
}
