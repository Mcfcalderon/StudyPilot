# Formato "N. Tema" de study_notes (R/study_topics.R). Fija el comportamiento actual:
# la migracion a v2 (course_topics) debe dar el mismo resultado.

test_that("build_topics_note arma cabecera + lista numerada", {
  expect_equal(build_topics_note("Fisica I", c("Cinematica", "Dinamica")),
               "Temas del curso Fisica I:\n1. Cinematica\n2. Dinamica")
})

test_that("parse_topics_note es la inversa de build_topics_note", {
  temas <- c("Arquitectura REST", "Autenticación JWT", "Despliegue: Docker y CI/CD")
  expect_equal(parse_topics_note(build_topics_note("Curso X", temas)), temas)
})

test_that("parse_topics_note descarta la cabecera y lineas sin numero", {
  raw <- "Temas del curso X:\n1. Uno\n- suelto\n\n2.Dos\n10.   Diez"
  expect_equal(parse_topics_note(raw), c("Uno", "Dos", "Diez"))
})

test_that("parse_topics_note: casos borde que la migracion debe reproducir", {
  expect_equal(parse_topics_note(""), character(0))
  expect_equal(parse_topics_note(NA_character_), character(0))
  # una linea que empieza con numero decimal pierde el "2." (comportamiento actual)
  expect_equal(parse_topics_note("2.5 horas de laboratorio"), "5 horas de laboratorio")
  # un tema con salto de linea interno se corta
  expect_equal(parse_topics_note(build_topics_note("X", "Linea uno\ncontinua")), "Linea uno")
  # con \r\n se conserva el \r final
  expect_equal(parse_topics_note("1. Uno\r\n2. Dos\r\n"), c("Uno\r", "Dos\r"))
  # "1)" no cuenta como tema
  expect_equal(parse_topics_note("1) Uno"), character(0))
})

test_that("topics_from_notes solo usa notas de Temas y agrupa por curso", {
  notes <- data.frame(
    course_id = c("C1", "C1", "C2", "C3"),
    text_content = c("Temas del curso A:\n1. A1\n2. A2", "## Resumen\n1. no es tema",
                     "Temas del curso B:\n1. B1", "Temas del curso C:\nsin numeros"),
    source = c("Temas extraidos del silabo", "Resumen IA", "Temas extraídos del sílabo",
               "Temas extraidos del silabo"),
    stringsAsFactors = FALSE)
  t <- topics_from_notes(notes)
  expect_equal(t$C1, c("A1", "A2"))
  expect_equal(t$C2, "B1")
  expect_null(t$C3)                        # sin lineas numeradas: no se asigna
  expect_equal(topics_from_notes(notes[0, ]), list())
})

test_that("topics_from_notes: con varias notas del mismo curso gana la ultima recibida", {
  # mg_notes_all ordena por created_at desc -> la ultima recibida es la mas antigua
  notes <- data.frame(course_id = c("C1", "C1"),
                      text_content = c("1. Nuevo", "1. Viejo"),
                      source = c("Temas", "temas"), stringsAsFactors = FALSE)
  expect_equal(topics_from_notes(notes)$C1, "Viejo")
})
