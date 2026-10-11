# ============ R/db_query.R — Construccion SEGURA de consultas Mongo ============
# Funciones puras (sin Mongo): convierten listas de R a JSON con jsonlite, que
# escapa comillas y barras. Un texto del usuario SIEMPRE queda como valor string;
# nunca puede convertirse en un operador ($ne, $gt, $where...). Reemplaza el
# armado manual con paste0, que permitia inyeccion NoSQL.

# Valor string escalar. Rechaza listas (Shiny::setInputValue puede enviar objetos
# como {"$ne": 1}), vectores y NA.
q_str <- function(x) {
  if (is.null(x) || is.list(x) || length(x) != 1 || is.na(x))
    stop("q_str: se esperaba un texto simple", call. = FALSE)
  as.character(x)
}

# Valor numerico escalar.
q_num <- function(x) {
  if (is.null(x) || is.list(x) || length(x) != 1)
    stop("q_num: se esperaba un numero simple", call. = FALSE)
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) stop("q_num: valor no numerico", call. = FALSE)
  v
}

# Nombre de campo seguro (para $set con clave dinamica).
q_field <- function(x) {
  x <- q_str(x)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", x))
    stop("q_field: nombre de campo invalido", call. = FALSE)
  x
}

# Lista con nombres -> JSON. Vectores de largo 1 salen como escalares;
# para forzar arreglo usar I(vector).
mq <- function(...) {
  x <- list(...)
  if (length(x) == 0) return("{}")
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null",
                                digits = NA))
}

# {"$set": {...}}
mq_set <- function(...) mq(`$set` = list(...))

# Filtro por usuario + campos extra (pares con nombre).
uf <- function(uid, ...) mq(user_id = q_str(uid), ...)
