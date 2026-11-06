# ============ R/canvas_feed.R — Entregas de Canvas por el feed de calendario (ICS) ============
# Sin token: el alumno copia en Canvas  Calendario -> "Fuente del calendario"  un enlace como
#   https://utec.instructure.com/feeds/calendars/user_XXXXXXXX.ics
# y StudyPilot muestra todas sus entregas (de todos los cursos) en el calendario.
# El enlace no vence (los tokens de alumno de UTEC caducan a los 30 dias). Quien lo tenga puede
# ver el calendario: se guarda solo en settings del propio usuario y nunca se muestra en logs.
# Ver docs/CANVAS_INTEGRACION.md.
#
# Las entregas NO son tiempo ocupado: van aparte de Google (rv_gcal$canvas_events) para que ni el
# Smart Scheduler ni fusionar_horarios las traten como clases o reuniones.

CANVAS_FEED_PATH_RE <- "^/feeds/calendars/user_[A-Za-z0-9]{10,80}\\.ics$"

# Solo https, host *.instructure.com (o los de CANVAS_FEED_HOSTS, separados por coma, para un
# dominio propio de la universidad) y la ruta de feed de usuario: el servidor descarga esta URL,
# asi que no puede apuntar a cualquier sitio.
valid_canvas_feed_url <- function(url, extra_hosts = Sys.getenv("CANVAS_FEED_HOSTS")) {
  if (is.null(url) || is.list(url) || length(url) != 1 || is.na(url)) return(FALSE)
  u <- trimws(url)
  m <- regmatches(u, regexec("^https://([A-Za-z0-9.-]+)(/[^?#]*)$", u))[[1]]
  if (length(m) != 3) return(FALSE)
  host <- tolower(m[2]); path <- m[3]
  hosts_ok <- grepl("(^|\\.)instructure\\.com$", host)
  extra <- tolower(trimws(strsplit(if (is.null(extra_hosts)) "" else extra_hosts, ",", fixed = TRUE)[[1]]))
  hosts_ok <- hosts_ok || host %in% extra[nzchar(extra)]
  hosts_ok && grepl(CANVAS_FEED_PATH_RE, path)
}

# RFC 5545: una linea que empieza con espacio o tab continua la anterior (Canvas corta los
# titulos largos asi; sin esto el nombre de la tarea queda truncado).
unfold_ics_lines <- function(lines) {
  lines <- sub("\r$", "", lines)
  out <- character(0)
  for (ln in lines) {
    if (length(out) && grepl("^[ \t]", ln)) out[length(out)] <- paste0(out[length(out)], substring(ln, 2))
    else out <- c(out, ln)
  }
  out
}

# "\," "\;" "\n" "\\" de ICS -> texto
ics_unescape <- function(x) {
  x <- gsub("\\\\[nN]", " ", x)
  x <- gsub("\\\\([,;\\\\])", "\\1", x)
  trimws(x)
}

# Salida de parse_ics_events() -> eventos del calendario (CALENDAR_SCHEMA, source = "canvas").
# Una entrega con hora (inicio == fin) se dibuja como un bloque de 30 min que TERMINA a la hora
# limite; una de dia completo queda como evento de todo el dia.
canvas_feed_to_events <- function(df, tz = "America/Lima") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || "error" %in% names(df))
    return(estandarizar_evento(NULL))
  out <- estandarizar_evento(df, "canvas")
  out$summary <- paste0("\U0001F4CC ", ics_unescape(out$summary))
  out$location <- ics_unescape(out$location)
  for (i in seq_len(nrow(out))) {
    s <- out$start[i]; e <- out$end[i]
    if (nchar(s) >= 16 && (!nzchar(e) || substr(e, 1, 16) <= substr(s, 1, 16))) {
      due <- parse_local_datetime(s, tz)
      if (!is.na(due)) {
        out$start[i] <- format(due - 30 * 60, "%Y-%m-%dT%H:%M")
        out$end[i]   <- format(due, "%Y-%m-%dT%H:%M")
      }
    }
  }
  out
}

# Descarga y convierte. fetch(url) -> vector de lineas (inyectable para tests).
# Devuelve el data.frame de eventos, o data.frame(error = "<texto para el usuario>").
canvas_get_events <- function(url, fetch = canvas_fetch_lines) {
  if (!valid_canvas_feed_url(url))
    return(data.frame(error = "El enlace no parece un feed de calendario de Canvas (https://…instructure.com/feeds/calendars/user_….ics)."))
  lines <- tryCatch(fetch(trimws(url)), error = function(e) {
    message("[StudyPilot] canvas feed: ", conditionMessage(e))   # sin la URL: es privada
    NULL
  })
  if (is.null(lines) || !any(grepl("BEGIN:VCALENDAR", lines, fixed = TRUE)))
    return(data.frame(error = "No se pudo leer el calendario de Canvas. Copia de nuevo el enlace de \"Fuente del calendario\"."))
  parsed <- parse_ics_events(unfold_ics_lines(lines))
  if ("error" %in% names(parsed)) return(data.frame(error = "Tu calendario de Canvas no tiene entregas en los proximos meses."))
  canvas_feed_to_events(parsed)
}

# Descarga con timeout corto (la sesion de Shiny espera mientras tanto) y tope de tamano.
canvas_fetch_lines <- function(url, timeout_s = 15, max_bytes = 5 * 1024^2) {
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle(timeout = timeout_s, connecttimeout = 8, followlocation = FALSE,
                          maxfilesize = max_bytes)
    r <- curl::curl_fetch_memory(url, handle = h)
    if (r$status_code != 200) stop("HTTP ", r$status_code)
    txt <- rawToChar(r$content); Encoding(txt) <- "UTF-8"
    return(strsplit(txt, "\n", fixed = TRUE)[[1]])
  }
  old <- options(timeout = timeout_s); on.exit(options(old))
  tmp <- tempfile(fileext = ".ics"); on.exit(unlink(tmp), add = TRUE)
  utils::download.file(url, tmp, quiet = TRUE, method = "libcurl")
  readLines(tmp, warn = FALSE, encoding = "UTF-8")
}
