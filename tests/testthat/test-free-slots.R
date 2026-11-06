# R/free_slots.R — obtener_espacio_libre (huecos >= 30 min, zona America/Lima)

LUNES <- as.Date("2026-05-04")

slots_dia <- function(...) obtener_espacio_libre(start_date = LUNES, end_date = LUNES, ...)

test_that("sin eventos: un hueco entre el fin y el inicio del sueno", {
  s <- slots_dia()
  expect_equal(nrow(s), 1)
  expect_equal(s$start_time, "07:00"); expect_equal(s$end_time, "23:00")
  expect_equal(s$duration_min, 960)
  expect_equal(s$day, "Lunes"); expect_equal(s$date, "2026-05-04")
  expect_equal(s$start_iso, "2026-05-04T07:00:00")
})

test_that("un dia por fecha del rango", {
  s <- obtener_espacio_libre(start_date = LUNES, end_date = LUNES + 6)
  expect_equal(nrow(s), 7)
  expect_equal(s$date, as.character(LUNES + 0:6))
})

test_that("sueno que no cruza medianoche deja libre el final del dia", {
  s <- slots_dia(sleep_start = "01:00", sleep_end = "07:00")
  expect_equal(s$start_time, c("00:00", "07:00"))
  expect_equal(s$end_time, c("01:00", "23:59"))
  expect_equal(s$end_iso[2], "2026-05-04T23:59:59")
})

test_that("las clases del horario semanal (por nombre del dia) parten el dia", {
  sched <- data.frame(dia = c("Lunes", "Lunes", "Martes"), hora_inicio = c("10:00", "11:00", "08:00"),
                      hora_fin = c("12:00", "13:00", "20:00"), stringsAsFactors = FALSE)
  s <- slots_dia(schedule_data = sched)
  expect_equal(s$start_time, c("07:00", "13:00"))
  expect_equal(s$end_time,   c("10:00", "23:00"))
  expect_equal(s$duration_min, c(180, 600))
  # "TBD" (hora que la IA no dio) se ignora sin romper
  sched_tbd <- data.frame(dia = "Lunes", hora_inicio = "TBD", hora_fin = "TBD", stringsAsFactors = FALSE)
  expect_equal(nrow(slots_dia(schedule_data = sched_tbd)), 1)
})

test_that("get_free_time_slots es alias de obtener_espacio_libre", {
  expect_identical(get_free_time_slots, obtener_espacio_libre)
})
