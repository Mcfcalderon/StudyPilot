# aplicar_arrastre() (R/calendar_events.R) — mover eventos del calendario arrastrando

bloques <- function() data.frame(
  summary = c("☕ Descanso", "☕ Descanso", "Estudio Fisica"),
  start = c("2026-05-04T10:00:00", "2026-05-04T15:00:00", "2026-05-04T11:00:00"),
  end   = c("2026-05-04T10:15:00", "2026-05-04T15:15:00", "2026-05-04T12:30:00"),
  color = "gray", stringsAsFactors = FALSE)

test_that("mueve el bloque IA correcto aunque haya otro con el mismo titulo", {
  r <- aplicar_arrastre(bloques(), NULL, list(title = "☕ Descanso", old_start = "2026-05-04T15:00",
                                              new_start = "16:00", new_end = "16:15"))
  expect_equal(r$kind, "ai")
  expect_equal(r$ai_blocks$start, c("2026-05-04T10:00:00", "2026-05-04T16:00:00", "2026-05-04T11:00:00"))
  expect_equal(r$ai_blocks$end[2], "2026-05-04T16:15:00")
})

test_that("sin old_start (JS viejo en cache) se mantiene el comportamiento anterior: por titulo", {
  r <- aplicar_arrastre(bloques(), NULL, list(title = "☕ Descanso", new_start = "09:00", new_end = "09:15"))
  expect_equal(r$kind, "ai"); expect_equal(r$ai_blocks$start[1], "2026-05-04T09:00:00")
})

test_that("evento del horario/Google: override con la clave del modal (titulo + inicio original)", {
  r <- aplicar_arrastre(NULL, NULL, list(title = "Fisica", old_start = "2026-05-04T08:00",
                                         new_start = "09:00", new_end = "11:00"))
  expect_equal(r$kind, "override")
  expect_equal(r$overrides$orig_title, "Fisica")
  expect_equal(r$overrides$orig_start, "2026-05-04T08:00")
  expect_equal(r$overrides$new_start_time, "09:00"); expect_equal(r$overrides$new_end_time, "11:00")
})

test_that("arrastrar de nuevo un evento ya movido reemplaza su override y conserva la clave original", {
  ov <- data.frame(orig_title = "Fisica", orig_start = "2026-05-04T08:00:00", new_title = "Fisica (lab)",
                   new_start_time = "09:00", new_end_time = "11:00", new_color = "#16a34a",
                   stringsAsFactors = FALSE)
  # lo que se ve ahora: "Fisica (lab)" a las 09:00
  r <- aplicar_arrastre(NULL, ov, list(title = "Fisica (lab)", old_start = "2026-05-04T09:00",
                                       new_start = "14:00", new_end = "16:00"))
  expect_equal(nrow(r$overrides), 1)
  expect_equal(r$overrides$orig_title, "Fisica")
  expect_equal(r$overrides$orig_start, "2026-05-04T08:00")
  expect_equal(r$overrides$new_title, "Fisica (lab)")    # conserva lo editado en el modal
  expect_equal(r$overrides$new_color, "#16a34a")
  expect_equal(r$overrides$new_start_time, "14:00")
})

test_that("otros overrides (incluidos los viejos 'any_T') se conservan", {
  ov <- data.frame(orig_title = c("Quimica", "Fisica"), orig_start = c("2026-05-05T08:00", "any_T08:00 - 10:00"),
                   new_title = c("Quimica", "Fisica"), new_start_time = c("10:00", "09:00"),
                   new_end_time = c("12:00", "11:00"), new_color = "", stringsAsFactors = FALSE)
  r <- aplicar_arrastre(NULL, ov, list(title = "Fisica", old_start = "2026-05-04T08:00",
                                       new_start = "09:00", new_end = "11:00"))
  expect_equal(nrow(r$overrides), 3)
  expect_equal(r$overrides$orig_start[3], "2026-05-04T08:00")
})

test_that("rechaza horas invalidas, titulos vacios y eventos sin inicio original", {
  ok <- list(title = "Fisica", old_start = "2026-05-04T08:00", new_start = "09:00", new_end = "11:00")
  expect_null(aplicar_arrastre(NULL, NULL, modifyList(ok, list(new_end = "24:30"))))
  expect_null(aplicar_arrastre(NULL, NULL, modifyList(ok, list(new_end = "08:00"))))
  expect_null(aplicar_arrastre(NULL, NULL, modifyList(ok, list(title = ""))))
  expect_null(aplicar_arrastre(NULL, NULL, modifyList(ok, list(old_start = ""))))       # no hay a que atarlo
  expect_null(aplicar_arrastre(NULL, NULL, modifyList(ok, list(old_start = "any_T08:00"))))
  expect_null(aplicar_arrastre(NULL, NULL, NULL))
  expect_null(aplicar_arrastre(NULL, NULL, modifyList(ok, list(title = list(`$ne` = 1)))))
})
