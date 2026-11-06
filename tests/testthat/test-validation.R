# R/validation.R — entradas de formularios y mensajes de error para el usuario

test_that("validar_actividad acepta una actividad calificada valida", {
  v <- validar_actividad("  PC1  ", 20, as.Date("2026-05-08"), TRUE)
  expect_true(v$ok)
  expect_equal(v$name, "PC1"); expect_equal(v$weight, 20); expect_equal(v$date, "2026-05-08")
  expect_equal(validar_actividad("PC1", "15.5", "2026-05-08", TRUE)$weight, 15.5)
})

test_that("validar_actividad: formativa guarda peso 0 aunque el campo este vacio", {
  v <- validar_actividad("Lectura", NA, as.Date("2026-05-08"), FALSE)
  expect_true(v$ok); expect_equal(v$weight, 0)
  expect_equal(validar_actividad("Lectura", NULL, "2026-05-08", FALSE)$weight, 0)
})

test_that("validar_actividad rechaza nombre, fecha o peso invalidos (antes rompian la sesion)", {
  expect_false(validar_actividad("", 10, "2026-05-08")$ok)
  expect_false(validar_actividad("   ", 10, "2026-05-08")$ok)
  expect_false(validar_actividad(NULL, 10, "2026-05-08")$ok)
  expect_false(validar_actividad(strrep("a", 201), 10, "2026-05-08")$ok)
  expect_false(validar_actividad("PC1", NA, "2026-05-08", TRUE)$ok)      # peso vacio
  expect_false(validar_actividad("PC1", 120, "2026-05-08", TRUE)$ok)
  expect_false(validar_actividad("PC1", -1, "2026-05-08", TRUE)$ok)
  expect_false(validar_actividad("PC1", "abc", "2026-05-08", TRUE)$ok)
  expect_false(validar_actividad("PC1", 10, NULL)$ok)                    # fecha borrada
  expect_false(validar_actividad("PC1", 10, as.Date(NA))$ok)
  expect_false(validar_actividad("PC1", 10, "no-es-fecha")$ok)
  expect_false(validar_actividad(list(`$ne` = 1), 10, "2026-05-08")$ok)  # objeto desde el navegador
  expect_match(validar_actividad("PC1", 120, "2026-05-08")$msg, "entre 0 y 100")
})

test_that("valid_hhmm y validar_rango_horas", {
  expect_true(valid_hhmm("00:00")); expect_true(valid_hhmm("23:59")); expect_true(valid_hhmm("08:30"))
  for (bad in list("24:00", "8:30", "08:60", "08:30:00", "abc", "", NA, NULL, c("08:00", "09:00")))
    expect_false(valid_hhmm(bad))
  expect_true(validar_rango_horas("08:00", "09:30")$ok)
  expect_false(validar_rango_horas("09:30", "09:30")$ok)
  expect_false(validar_rango_horas("10:00", "09:00")$ok)
  expect_false(validar_rango_horas("23:30", "24:30")$ok)   # arrastre que pasa la medianoche
  expect_match(validar_rango_horas("x", "09:00")$msg, "HH:MM")
})

test_that("user_error_text no filtra detalles internos", {
  internos <- c("Error in mongo_col(...): No suitable servers found (`serverSelectionTryOnce` set): [connection refused calling ismaster on 'cluster0.abcd.mongodb.net:27017']",
                "object 'evals_c' not found", "subscript out of bounds",
                "q_num: valor no numerico", "Error de IA tras 3 intentos: HTTP 400 Bad Request. API key not valid",
                "cannot open URL 'https://calendar.google.com/calendar/ical/ana%40gmail.com/public/basic.ics'")
  for (m in internos) {
    out <- user_error_text(m)
    expect_false(grepl("mongodb\\.net|evals_c|subscript|q_num|API key|gmail|https?://", out))
  }
  expect_match(user_error_text("HTTP 429 Too Many Requests"), "saturada")
  expect_match(user_error_text("Error de IA tras 2 intentos: RESOURCE_EXHAUSTED"), "saturada")
  expect_match(user_error_text("HTTP 503 Service Unavailable"), "no está disponible")
  expect_match(user_error_text("Timeout was reached"), "tardó")
  expect_match(user_error_text("No se pudo conectar a la base de datos."), "servidor")
  expect_match(user_error_text(NULL), "inesperado")
})

test_that("user_error registra el detalle y devuelve el texto seguro", {
  expect_message(out <- user_error(simpleError("object 'x' not found"), "prueba"), "object 'x' not found")
  expect_match(out, "inesperado")
  expect_message(out2 <- user_error("HTTP 429"), "429")
  expect_match(out2, "saturada")
})

test_that("valid_pomo_duration acepta 1..180 minutos y rechaza lo demas", {
  expect_equal(valid_pomo_duration(25), 25L)
  expect_equal(valid_pomo_duration("50"), 50L)
  expect_equal(valid_pomo_duration(24.6), 25L)
  expect_equal(valid_pomo_duration(180), 180L)
  for (bad in list(0, -5, 181, NA, NULL, "abc", c(25, 50), list(25))) expect_true(is.na(valid_pomo_duration(bad)))
})

test_that("exam_is_final reconoce el final por codigo (regla original) o por nombre", {
  expect_true(exam_is_final("EF", ""))                 # por codigo, como antes
  expect_true(exam_is_final("EXM2", NA))
  expect_true(exam_is_final("", "Examen Final"))       # code vacio: antes daba FALSE
  expect_true(exam_is_final("", "EF - Teoria"))
  expect_true(exam_is_final(NA, "final"))
  expect_false(exam_is_final("", "Examen Parcial"))
  expect_false(exam_is_final("", "Refuerzo de efectos"))   # 'ef' dentro de palabra no cuenta
  expect_false(exam_is_final("", "Semifinales de debate")) # 'final' dentro de palabra no cuenta
  expect_false(exam_is_final(NULL, NULL))
})
