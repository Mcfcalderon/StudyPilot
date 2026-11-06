# R/calendar_events.R — timestamps, esquema, colores, horario PDF y fusion con Google

ev <- function(summary, start, end, source = "gcal", color = "", is_ai = FALSE)
  data.frame(summary = summary, start = start, end = end, location = "", color = color,
             is_ai = is_ai, source = source, stringsAsFactors = FALSE)

test_that("estandarizar_timestamps quita Z y desfase, y agrega segundos", {
  df <- data.frame(start = c("2026-05-04T10:00:00Z", "2026-05-04T10:00:00-05:00", "2026-05-04T10:00", "2026-05-04"),
                   end = NA_character_, stringsAsFactors = FALSE)
  out <- estandarizar_timestamps(df)
  # el desfase "-05:00" NO se quita (la regex solo cubre "+HH:MM"): comportamiento actual
  expect_equal(out$start, c("2026-05-04T10:00:00", "2026-05-04T10:00:00-05:00",
                            "2026-05-04T10:00:00", "2026-05-04"))
  expect_true(all(is.na(out$end)))
  expect_equal(estandarizar_timestamps(data.frame(start = "2026-05-04T08:00:00+00:00"))$start,
               "2026-05-04T08:00:00")
  px <- data.frame(start = as.POSIXct("2026-05-04 08:30:00", tz = "UTC"))
  expect_equal(estandarizar_timestamps(px)$start, "2026-05-04T08:30:00")
  expect_null(estandarizar_timestamps(NULL))
})

test_that("estandarizar_evento devuelve siempre el esquema y limpia NA", {
  e0 <- estandarizar_evento(NULL)
  expect_equal(names(e0), CALENDAR_SCHEMA); expect_equal(nrow(e0), 0)
  e <- estandarizar_evento(data.frame(summary = c("A", NA), start = c("x", NA),
                                      extra = 1:2, stringsAsFactors = FALSE), "pdf")
  expect_equal(names(e), CALENDAR_SCHEMA)
  expect_equal(e$summary, c("A", "")); expect_equal(e$start, c("x", ""))
  expect_equal(e$end, c("", "")); expect_equal(e$color, c("", ""))
  expect_equal(e$is_ai, c(FALSE, FALSE)); expect_equal(e$source, c("pdf", "pdf"))
})

test_that("pdf_schedule_to_events: week_start es el LUNES (Lunes cae en week_start)", {
  sched <- data.frame(dia = c("Lunes", "Miércoles", "Miercoles", "Sábado", "Domingo", "Feriado"),
                      hora_inicio = "08:00", hora_fin = "10:00", curso = "Fisica", aula = "A1",
                      stringsAsFactors = FALSE)
  lunes <- as.Date("2026-05-04")   # lunes
  out <- pdf_schedule_to_events(sched, lunes)
  expect_equal(nrow(out), 5)       # "Feriado" se descarta
  expect_equal(substr(out$start, 1, 10),
               c("2026-05-04", "2026-05-06", "2026-05-06", "2026-05-09", "2026-05-03"))
  expect_equal(out$start[1], "2026-05-04T08:00")
  expect_equal(out$location[1], "A1"); expect_equal(out$color[1], "")
  expect_true(all(out$source == "pdf"))
  expect_equal(nrow(pdf_schedule_to_events(sched[0, ], lunes)), 0)
  # color por curso si se pasa un mapa
  expect_equal(pdf_schedule_to_events(sched[1, ], lunes, list(Fisica = "#111111"))$color, "#111111")
})

test_that("fusionar_horarios descarta la clase del PDF que se cruza >= 30 min con Google", {
  pdf <- ev(c("Fisica", "Quimica"), c("2026-05-04T08:00", "2026-05-04T14:00"),
            c("2026-05-04T10:00", "2026-05-04T16:00"), source = "pdf")
  g_cruce  <- ev("Clase Fisica (Google)", "2026-05-04T09:00:00", "2026-05-04T11:00:00")
  out <- fusionar_horarios(pdf, g_cruce)
  expect_equal(sort(out$summary), c("Clase Fisica (Google)", "Quimica"))
  # cruce de 20 min: se conservan ambos
  g_corto <- ev("Reunion", "2026-05-04T09:40:00", "2026-05-04T10:30:00")
  expect_equal(nrow(fusionar_horarios(pdf, g_corto)), 3)
  # otro dia: no hay cruce
  g_otro <- ev("Otro", "2026-05-05T08:00:00", "2026-05-05T10:00:00")
  expect_equal(nrow(fusionar_horarios(pdf, g_otro)), 3)
  # entradas vacias
  expect_equal(nrow(fusionar_horarios(NULL, NULL)), 0)
  expect_equal(fusionar_horarios(pdf, NULL)$summary, pdf$summary)
  expect_equal(fusionar_horarios(NULL, g_otro)$summary, "Otro")
})

test_that("aplicar_colores_cursos: respeta colores dados, usa el curso, heuristica y paleta", {
  get_courses <- function() data.frame(id = "CS2031", name = "Desarrollo Web", short = "DesWeb",
                                       color = "#123456", stringsAsFactors = FALSE)
  environment(aplicar_colores_cursos) <- environment()   # usa este get_courses()
  df <- ev(c("Bloque IA", "Clase Desarrollo Web", "Examen parcial", "Almuerzo", "Club de ajedrez", "Club de ajedrez martes"),
           "2026-05-04T08:00", "2026-05-04T09:00", source = "pdf",
           color = c("purple", "", "", "", "", ""))
  out <- aplicar_colores_cursos(df)
  expect_equal(out$color[1], "purple")              # nunca se sobrescribe
  expect_equal(out$color[2], "#123456")             # por nombre del curso
  expect_equal(out$color[3], "#dc2626")             # heuristica: examen
  expect_equal(out$color[4], "#94a3b8")             # heuristica: comida
  expect_equal(out$color[5], COURSE_COLOR_PALETTE[1])
  expect_equal(out$color[6], COURSE_COLOR_PALETTE[1])  # mismas 3 primeras palabras -> mismo color
})

test_that("aplicar_colores_cursos evita que un evento del PDF repita un color de Google", {
  get_courses <- function() NULL
  environment(aplicar_colores_cursos) <- environment()
  df <- rbind(ev("Google", "2026-05-04T08:00", "2026-05-04T09:00", "gcal", COURSE_COLOR_PALETTE[1]),
              ev("Taller libre", "2026-05-04T10:00", "2026-05-04T11:00", "pdf"))
  out <- aplicar_colores_cursos(df)
  expect_equal(out$color[1], COURSE_COLOR_PALETTE[1])
  expect_false(out$color[2] == COURSE_COLOR_PALETTE[1])
})

test_that("week_monday devuelve el lunes de la semana (lunes a domingo)", {
  expect_equal(week_monday(as.Date("2026-05-04")), as.Date("2026-05-04"))  # lunes
  expect_equal(week_monday(as.Date("2026-05-07")), as.Date("2026-05-04"))  # jueves
  expect_equal(week_monday(as.Date("2026-05-10")), as.Date("2026-05-04"))  # domingo
  # junto con pdf_schedule_to_events, la clase del Lunes cae en lunes
  ev_l <- pdf_schedule_to_events(data.frame(dia = "Lunes", hora_inicio = "08:00", hora_fin = "10:00",
                                            curso = "X", stringsAsFactors = FALSE),
                                 week_monday(as.Date("2026-05-07")))
  expect_equal(weekdays(as.Date(substr(ev_l$start, 1, 10))), weekdays(as.Date("2026-05-04")))
})

payload_of <- function(js) {
  pre <- "Shiny.setInputValue('cal_event_click',"; post <- ",{priority:'event'})"
  expect_true(startsWith(js, pre)); expect_true(endsWith(js, post))
  body <- substr(js, nchar(pre) + 1, nchar(js) - nchar(post))
  expect_true(jsonlite::validate(body))          # todo el medio es UN objeto JSON valido
  jsonlite::fromJSON(body)
}

test_that("cal_event_onclick serializa el evento como JSON sin dejar escapar el titulo", {
  p <- payload_of(cal_event_onclick("Fisica", "2026-05-04T08:00", "2026-05-04T10:00",
                                    "#2563eb", TRUE, FALSE, "ai", 3L))
  expect_equal(p$title, "Fisica"); expect_equal(p$start, "2026-05-04T08:00")
  expect_true(p$is_ai); expect_false(p$is_gcal); expect_equal(p$source, "ai"); expect_equal(p$idx, 3)

  malo <- "x',a:alert(document.cookie),b:'\"</div><script>alert(1)</script>\\\n"
  js <- cal_event_onclick(malo, "s", "e", "#fff", FALSE, TRUE, NULL, 1)
  p <- payload_of(js)
  expect_equal(p$title, malo)                    # llega intacto como texto, sin claves extra
  expect_setequal(names(p), c("title", "start", "end", "color", "is_ai", "is_gcal", "source", "idx"))
  expect_false(grepl("\n", js, fixed = TRUE))    # salto de linea escapado
  expect_equal(p$source, "unknown")
  expect_equal(payload_of(cal_event_onclick(NA, NULL, "e", "", NA, NA, NA, 1))$title, "")
})

test_that("safe_css_color solo deja pasar colores hex", {
  expect_equal(safe_css_color("#2563eb"), "#2563eb")
  expect_equal(safe_css_color("#FFF"), "#FFF")
  expect_equal(safe_css_color("red;background:url(x)"), "#3b82f6")
  expect_equal(safe_css_color(NA), "#3b82f6")
  expect_equal(safe_css_color(NULL, "#000000"), "#000000")
  expect_equal(safe_css_color(c("#111", "#222")), "#3b82f6")
})
