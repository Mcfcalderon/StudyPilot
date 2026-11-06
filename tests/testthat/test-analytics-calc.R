# R/analytics_calc.R — deuda academica y selector de examenes pendientes

HOY <- as.Date("2026-05-04")
acts_demo <- function() {
  a <- data.frame(
    act_id = 1:5, course_id = c("CS1", "CS1", "MA1", "CS1", "MA1"),
    type = c("examen", "quiz", "examen", "tarea", "examen"),
    name = c("Parcial", "Quiz 1", "Final", "Informe", "Pasado"),
    date = as.character(HOY + c(10, 3, 20, 2, -5)),
    weight = c(30, 15, 40, 20, 30), done = c(0, 0, 0, 0, 0), stringsAsFactors = FALSE)
  a$temas_vinculados <- list(c("A", "B", "C", "D"), NULL, NULL, NULL, NULL)
  a
}
cursos_demo <- data.frame(id = c("CS1", "MA1"), name = c("Programacion", "Calculo"),
                          short = c("Progra", "Calculo"), stringsAsFactors = FALSE)

test_that("pending_exams: solo examen/quiz pendientes con peso >= 15, por fecha", {
  p <- pending_exams(acts_demo(), today = HOY)
  expect_equal(p$name, c("Pasado", "Quiz 1", "Parcial", "Final"))   # incluye los atrasados
  expect_equal(p$days_left, c(-5L, 3L, 10L, 20L))
  a <- acts_demo(); a$done[2] <- 1; a$weight[1] <- 10
  expect_equal(pending_exams(a, today = HOY)$name, c("Pasado", "Final"))
  expect_equal(nrow(pending_exams(acts_demo()[0, ])), 0)
})

test_that("exam_at interpreta el valor del select como posicion, no como act_id", {
  a <- acts_demo()
  expect_equal(exam_at(a, "2", today = HOY)$name, "Quiz 1")   # act_id 2 por casualidad
  expect_equal(exam_at(a, "3", today = HOY)$name, "Parcial")  # act_id 3 seria "Final"
  expect_equal(exam_at(a, 4L, today = HOY)$course_id, "MA1")
  for (bad in list(NULL, "", "0", "abc", "-1", "9", NA, c("1", "2"))) expect_null(exam_at(a, bad, today = HOY))
})

test_that("calc_prep_metrics: ratio y alerta por examen futuro", {
  ai <- data.frame(summary = c("Estudio Progra: arrays", "Progra repaso", "☕ Descanso Progra",
                               "Calculo: limites"), stringsAsFactors = FALSE)
  topics <- list(CS1 = c("T1", "T2"), MA1 = c("L1", "L2", "L3", "L4"))
  m <- calc_prep_metrics(acts_demo(), ai, topics, courses_df = cursos_demo, today = HOY)
  expect_equal(m$exam_name, c("Quiz 1", "Parcial", "Final"))   # sin la tarea ni el pasado
  # Quiz 1: 2 bloques / 2 temas del curso -> ok
  expect_equal(m$n_blocks[1], 2); expect_equal(m$prep_ratio[1], 1); expect_equal(m$alert[1], "ok")
  # Parcial: 4 temas vinculados (mas que los del curso) -> 2/4 = 0.5 -> media
  expect_equal(m$n_topics[2], 4); expect_equal(m$prep_ratio[2], 0.5); expect_equal(m$alert[2], "media")
  # Final: 1/4 = 0.25, faltan 20 dias -> alta (critica solo si <= 7 dias)
  expect_equal(m$prep_ratio[3], 0.25); expect_equal(m$alert[3], "alta")
  expect_equal(m$course_name, c("Progra", "Progra", "Calculo"))
})

test_that("calc_prep_metrics: critica a <= 7 dias y robusta ante datos vacios o sin fecha", {
  m <- calc_prep_metrics(acts_demo(), NULL, list(), courses_df = cursos_demo, today = HOY)
  expect_equal(m$alert[m$exam_name == "Quiz 1"], "critica")    # 0 bloques, 3 dias
  expect_equal(nrow(calc_prep_metrics(acts_demo()[0, ], NULL, list(), courses_df = cursos_demo, today = HOY)), 0)
  a <- acts_demo(); a$date[1] <- NA
  expect_false("Parcial" %in% calc_prep_metrics(a, NULL, list(), courses_df = cursos_demo, today = HOY)$exam_name)
  # curso desconocido: usa el codigo como nombre
  a <- acts_demo(); a$course_id[3] <- "ZZ9"
  expect_true("ZZ9" %in% calc_prep_metrics(a, NULL, list(), courses_df = cursos_demo, today = HOY)$course_name)
})
