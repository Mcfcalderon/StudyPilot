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
test_that("las clases del horario (por nombre del dia) y los eventos parten el dia", {
  sched <- data.frame(dia = "Lunes", hora_inicio = "10:00", hora_fin = "12:00", stringsAsFactors = FALSE)
  gcal <- data.frame(summary = "Cita", start = "2026-05-04T15:00:00", end = "2026-05-04T16:00:00",
                     stringsAsFactors = FALSE)
  s <- slots_dia(schedule_data = sched, gcal_events = gcal)
  expect_equal(s$start_time, c("07:00", "12:00", "16:00"))
  expect_equal(s$end_time,   c("10:00", "15:00", "23:00"))
  expect_equal(s$duration_min, c(180, 180, 420))
})

test_that("eventos solapados se unen y los huecos < 30 min se descartan", {
  gcal <- data.frame(start = c("2026-05-04T08:00", "2026-05-04T09:00", "2026-05-04T10:20"),
                     end   = c("2026-05-04T09:30", "2026-05-04T10:00", "2026-05-04T22:50"),
                     stringsAsFactors = FALSE)
  s <- slots_dia(gcal_events = gcal)
  # 07:00-08:00 (60), 10:00-10:20 (20, descartado), 22:50-23:00 (10, descartado)
  expect_equal(nrow(s), 1)
  expect_equal(s$start_time, "07:00"); expect_equal(s$duration_min, 60)
})

test_that("los bloques IA existentes tambien ocupan tiempo", {
  ai <- data.frame(summary = "Estudio", start = "2026-05-04T07:00:00", end = "2026-05-04T22:00:00",
                   color = "purple", stringsAsFactors = FALSE)
  s <- slots_dia(ai_blocks = ai)
  expect_equal(s$start_time, "22:00"); expect_equal(s$duration_min, 60)
})

test_that("eventos de otro dia, sin hora o con error se ignoran", {
  gcal <- data.frame(start = c("2026-05-05T08:00", "2026-05-04", "2026-05-04T12:00"),
                     end   = c("2026-05-05T20:00", "2026-05-04", "2026-05-04T11:00"),  # fin < inicio
                     stringsAsFactors = FALSE)
  expect_equal(nrow(slots_dia(gcal_events = gcal)), 1)
  expect_equal(nrow(slots_dia(gcal_events = data.frame(error = "x"))), 1)
})

test_that("sin huecos devuelve un data.frame vacio con las columnas esperadas", {
  gcal <- data.frame(start = "2026-05-04T06:00", end = "2026-05-04T23:30", stringsAsFactors = FALSE)
  s <- slots_dia(gcal_events = gcal)
  expect_equal(nrow(s), 0)
  expect_equal(names(s), c("date", "day", "start_time", "end_time", "start_iso", "end_iso", "duration_min"))
})

test_that("parse_local_datetime entiende la T, el espacio y rechaza lo que no tiene hora", {
  expect_equal(format(parse_local_datetime("2026-05-04T15:30:00"), "%H:%M"), "15:30")
  expect_equal(format(parse_local_datetime("2026-05-04T15:30"), "%H:%M"), "15:30")
  expect_equal(format(parse_local_datetime("2026-05-04 15:30:00"), "%H:%M"), "15:30")
  expect_true(is.na(parse_local_datetime("2026-05-04")))
  expect_true(is.na(parse_local_datetime("2026-05-04TTBD")))
  expect_true(is.na(parse_local_datetime(NA)))
  expect_true(is.na(parse_local_datetime(NULL)))
})

test_that("el bloque PASADO del scheduler (00:00 -> ahora) recorta el dia", {
  pasado <- data.frame(summary = "PASADO", start = "2026-05-04T00:00:00",
                       end = "2026-05-04T15:12:40", stringsAsFactors = FALSE)
  s <- slots_dia(gcal_events = pasado)
  expect_equal(s$start_time, "15:12")
  expect_true(all(s$start_iso >= "2026-05-04T15:12"))
})

test_that("el horario bloquea su dia aunque la IA escriba el nombre sin tilde", {
  mie <- LUNES + 2; sab <- LUNES + 5
  sched <- data.frame(dia = c("Miercoles", " S\u00c1BADO "), hora_inicio = "10:00", hora_fin = "12:00",
                      stringsAsFactors = FALSE)
  s <- obtener_espacio_libre(start_date = mie, end_date = mie, schedule_data = sched)
  expect_equal(s$start_time, c("07:00", "12:00"))
  s <- obtener_espacio_libre(start_date = sab, end_date = sab, schedule_data = sched)
  expect_equal(s$start_time, c("07:00", "12:00"))
  expect_equal(norm_dia(c("Mi\u00e9rcoles", "miercoles ", "MI\u00c9RCOLES")), rep("miercoles", 3))
})
