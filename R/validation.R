# ============ R/validation.R — Validacion de entradas y mensajes de error seguros ============
# Funciones puras (sin Shiny/Mongo), probadas en tests/testthat/test-validation.R.
# Un error no atrapado dentro de un observer de Shiny cierra la sesion del usuario, y
# e$message puede traer detalles internos (Mongo, rutas, respuestas HTTP): validar antes
# de escribir y mostrar solo mensajes de user_error_text().

.scalar_chr <- function(x) {
  if (is.null(x) || is.list(x) || length(x) != 1 || is.na(x)) return(NA_character_)
  as.character(x)
}

# Actividad del formulario (nueva o editada).
# Devuelve list(ok, msg, name, weight, date) con los valores listos para guardar.
validar_actividad <- function(name, weight, date, is_calificada = TRUE) {
  fail <- function(msg) list(ok = FALSE, msg = msg)
  nm <- trimws(.scalar_chr(name))
  if (is.na(nm) || !nzchar(nm)) return(fail("Escribe un nombre para la actividad"))
  if (nchar(nm) > 200) return(fail("El nombre es muy largo (máx. 200 caracteres)"))
  d <- if (inherits(date, "Date") && length(date) == 1) date
       else tryCatch(as.Date(.scalar_chr(date)), error = function(e) as.Date(NA))
  if (length(d) != 1 || is.na(d)) return(fail("Elige una fecha válida"))
  w <- 0
  if (isTRUE(is_calificada)) {
    w <- suppressWarnings(as.numeric(.scalar_chr(weight)))
    if (is.na(w) || w < 0 || w > 100) return(fail("El peso debe ser un número entre 0 y 100"))
  }
  list(ok = TRUE, msg = "", name = nm, weight = w, date = format(d, "%Y-%m-%d"))
}

# Hora "HH:MM" de 00:00 a 23:59
valid_hhmm <- function(x) {
  x <- .scalar_chr(x)
  !is.na(x) && grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", x)
}

# Rango de horas del editor del calendario: ambas validas y fin > inicio
validar_rango_horas <- function(start, end) {
  if (!valid_hhmm(start) || !valid_hhmm(end))
    return(list(ok = FALSE, msg = "Usa el formato HH:MM (por ejemplo 08:30)"))
  if (trimws(end) <= trimws(start))
    return(list(ok = FALSE, msg = "La hora de fin debe ser posterior a la de inicio"))
  list(ok = TRUE, msg = "")
}

# Texto de error apto para el usuario a partir del mensaje interno (que se registra aparte).
user_error_text <- function(msg) {
  m <- .scalar_chr(msg); if (is.na(m)) m <- ""
  if (grepl("429|RESOURCE_EXHAUSTED|quota", m, ignore.case = TRUE))
    return("La IA está saturada. Espera 1-2 minutos e intenta de nuevo.")
  if (grepl("503|UNAVAILABLE|overloaded", m, ignore.case = TRUE))
    return("El servicio de IA no está disponible ahora. Intenta de nuevo en unos minutos.")
  if (grepl("timed? ?out|timeout", m, ignore.case = TRUE))
    return("La operación tardó demasiado. Intenta de nuevo.")
  if (grepl("base de datos|mongo|socket|server selection|No suitable servers|connect",
            m, ignore.case = TRUE))
    return("No se pudo conectar con el servidor. Intenta de nuevo en unos minutos.")
  "Ocurrió un error inesperado. Intenta de nuevo; si persiste, recarga la página."
}

# Registra el detalle en el log del servidor y devuelve el texto para el usuario.
user_error <- function(e, where = "") {
  m <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)
  message("[StudyPilot] Error", if (nzchar(where)) paste0(" (", where, ")") else "", ": ", m)
  user_error_text(m)
}
