# R/canvas_feed.R — feed ICS de Canvas (sin red: fetch simulado)

URL_OK <- "https://utec.instructure.com/feeds/calendars/user_AbCdEf1234567890xyz.ics"

# Feed con el formato de Canvas: entrega con hora (UTC, inicio == fin), una de dia completo,
# titulo largo partido en dos lineas (folding) y caracteres escapados.
ics_demo <- function(d = Sys.Date() + 10) {
  ymd <- format(d, "%Y%m%d"); ymd2 <- format(d + 1, "%Y%m%d")
  c("BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//Instructure//Canvas",
    "BEGIN:VEVENT", paste0("DTSTART:", ymd2, "T045900Z"), paste0("DTEND:", ymd2, "T045900Z"),
    "SUMMARY:Tarea 2: API REST\\, pruebas [CS2031-1 Desarrollo Basado en Plataf",
    " ormas]", "UID:event-assignment-101", "END:VEVENT",
    "BEGIN:VEVENT", paste0("DTSTART;VALUE=DATE:", ymd), paste0("DTEND;VALUE=DATE:", ymd),
    "SUMMARY:Lectura semana 9 [MA1001 Calculo]", "UID:event-assignment-102", "END:VEVENT",
    "END:VCALENDAR")
}

test_that("valid_canvas_feed_url solo acepta feeds https de Canvas", {
  expect_true(valid_canvas_feed_url(URL_OK))
  expect_true(valid_canvas_feed_url(paste0("  ", URL_OK, " ")))
  expect_true(valid_canvas_feed_url("https://canvas.instructure.com/feeds/calendars/user_abcdefghij12.ics"))
  bad <- c(sub("https", "http", URL_OK),                                    # sin TLS
           "https://evil.com/feeds/calendars/user_AbCdEf1234567890xyz.ics",  # otro host
           "https://utec.instructure.com.evil.com/feeds/calendars/user_AbCdEf1234567890.ics",
           "https://utec.instructure.com/api/v1/users/self",                  # otra ruta
           "https://utec.instructure.com/feeds/calendars/course_AbCdEf1234567890.ics",
           paste0(URL_OK, "?x=1"), "https://127.0.0.1/feeds/calendars/user_AbCdEf1234567890.ics",
           "", NA)
  for (b in bad) expect_false(valid_canvas_feed_url(b, extra_hosts = ""), info = b)
  expect_false(valid_canvas_feed_url(NULL)); expect_false(valid_canvas_feed_url(list(URL_OK)))
  # dominio propio de la universidad, si se configura
  propio <- "https://canvas.utec.edu.pe/feeds/calendars/user_AbCdEf1234567890.ics"
  expect_false(valid_canvas_feed_url(propio, extra_hosts = ""))
  expect_true(valid_canvas_feed_url(propio, extra_hosts = "canvas.utec.edu.pe, otro.edu"))
})

test_that("unfold_ics_lines une las lineas partidas e ics_unescape limpia escapes", {
  expect_equal(unfold_ics_lines(c("SUMMARY:Hola mun\r", " do\r", "UID:1")), c("SUMMARY:Hola mundo", "UID:1"))
  expect_equal(unfold_ics_lines(c("A", "\tB")), "AB")
  expect_equal(ics_unescape("API REST\\, pruebas\\; parte\\nB \\\\ x"), "API REST, pruebas; parte B \\ x")
})

test_that("canvas_get_events convierte las entregas sin tratarlas como tiempo ocupado", {
  d <- Sys.Date() + 10
  ev <- canvas_get_events(URL_OK, fetch = function(u) ics_demo(d))
  expect_equal(nrow(ev), 2)
  expect_true(all(ev$source == "canvas"))
  expect_equal(names(ev), CALENDAR_SCHEMA)
  tarea <- ev[grepl("Tarea 2", ev$summary), ]
  expect_equal(tarea$summary, "\U0001F4CC Tarea 2: API REST, pruebas [CS2031-1 Desarrollo Basado en Plataformas]")
  # 04:59Z del dia siguiente = 23:59 en Lima; bloque de 30 min que termina en la hora limite
  expect_equal(tarea$start, paste0(format(d, "%Y-%m-%d"), "T23:29"))
  expect_equal(tarea$end,   paste0(format(d, "%Y-%m-%d"), "T23:59"))
  lectura <- ev[grepl("Lectura", ev$summary), ]
  expect_equal(lectura$start, format(d, "%Y-%m-%d"))        # dia completo se mantiene
})

test_that("canvas_get_events: errores claros y sin filtrar la URL", {
  expect_match(canvas_get_events("https://evil.com/x.ics")$error, "no parece")
  expect_match(canvas_get_events(URL_OK, fetch = function(u) stop("HTTP 404"))$error, "No se pudo leer")
  expect_match(canvas_get_events(URL_OK, fetch = function(u) c("<html>login</html>"))$error, "No se pudo leer")
  vacio <- c("BEGIN:VCALENDAR", "END:VCALENDAR")
  expect_match(canvas_get_events(URL_OK, fetch = function(u) vacio)$error, "no tiene entregas")
  # el log no incluye la URL privada
  expect_message(canvas_get_events(URL_OK, fetch = function(u) stop("HTTP 500")), "HTTP 500")
  msgs <- character(0)
  withCallingHandlers(canvas_get_events(URL_OK, fetch = function(u) stop("fallo")),
                      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_false(any(grepl("user_AbCdEf", msgs)))
})

test_that("canvas_feed_to_events tolera entradas vacias", {
  expect_equal(nrow(canvas_feed_to_events(NULL)), 0)
  expect_equal(nrow(canvas_feed_to_events(data.frame(error = "x"))), 0)
})
