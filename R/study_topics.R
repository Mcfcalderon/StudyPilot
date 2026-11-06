# ============ R/study_topics.R — Temas del curso guardados en study_notes ============
# Los temas se guardan como una nota de texto (ver docs/MONGO_SCHEMA.md, "Formato N. Tema"):
#   Temas del curso <nombre>:
#   1. Tema uno
#   2. Tema dos
# Funciones puras (sin Shiny/Mongo), extraidas de server_cursos.R y server_auth.R sin cambiar
# su comportamiento. La migracion a v2 (tabla course_topics) debe reproducir este parseo.

# Texto de la nota a partir de los temas
build_topics_note <- function(course_name, temas) {
  paste0("Temas del curso ", course_name, ":\n",
         paste(seq_along(temas), temas, sep = ". ", collapse = "\n"))
}

# Lineas que empiezan con "N." -> texto sin el prefijo "N. "
parse_topics_note <- function(raw) {
  topic_lines <- strsplit(raw, "\n")[[1]]
  topic_lines <- topic_lines[grepl("^\\d+\\.", topic_lines)]
  sub("^\\d+\\.\\s*", "", topic_lines)
}

# data.frame de study_notes (course_id, text_content, source) -> list(course_id = temas)
# Solo cuentan las notas cuyo source contiene "Temas". Si un curso tiene varias, cada una
# sobrescribe a la anterior en el orden recibido (mg_notes_all ordena por created_at desc,
# asi que gana la mas antigua).
topics_from_notes <- function(all_notes) {
  topics_list <- list()
  if (is.null(all_notes) || nrow(all_notes) == 0) return(topics_list)
  topic_notes <- all_notes[grepl("Temas", all_notes$source, ignore.case = TRUE), ]
  for (j in seq_len(nrow(topic_notes))) {
    topic_lines <- parse_topics_note(topic_notes$text_content[j])
    if (length(topic_lines) > 0) topics_list[[topic_notes$course_id[j]]] <- topic_lines
  }
  topics_list
}
