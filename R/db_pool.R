# ============ R/db_pool.R — Conexiones Mongo reutilizables ============
# Una conexion por coleccion, creada UNA vez por proceso R y compartida por
# todas las sesiones (antes se hacia un count() de "salud" antes de cada
# consulta y el login abria conexiones nuevas). Si una operacion falla por
# un error de red, se descarta la conexion, se crea otra y se reintenta UNA vez.
# El conector se inyecta para poder testear sin Mongo.

.db_pool <- new.env(parent = emptyenv())

DB_METHODS <- c("find", "update", "remove", "insert", "count", "run",
                "index", "aggregate", "distinct", "iterate")

is_network_error <- function(e) {
  grepl("socket|connect|timed? ?out|server selection|No suitable servers|network",
        conditionMessage(e), ignore.case = TRUE)
}

pool_reset <- function() rm(list = ls(.db_pool, all.names = TRUE), envir = .db_pool)

pool_get <- function(collection, connect) {
  conn <- get0(collection, envir = .db_pool, inherits = FALSE)
  if (is.null(conn)) {
    conn <- connect(collection)
    assign(collection, conn, envir = .db_pool)
  }
  conn
}

pool_call <- function(collection, method, connect, ...) {
  tryCatch(
    pool_get(collection, connect)[[method]](...),
    error = function(e) {
      if (!is_network_error(e)) stop(e)
      message("[StudyPilot] Mongo '", collection, "': reconectando (", conditionMessage(e), ")")
      if (exists(collection, envir = .db_pool, inherits = FALSE)) rm(list = collection, envir = .db_pool)
      pool_get(collection, connect)[[method]](...)
    }
  )
}

# Objeto con los mismos metodos que mongolite (col$find(...), col$update(...)...)
pool_collection <- function(collection, connect) {
  stats::setNames(lapply(DB_METHODS, function(m) {
    force(m)
    function(...) pool_call(collection, m, connect, ...)
  }), DB_METHODS)
}
