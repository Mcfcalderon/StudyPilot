# ============ tools/export_mongo.R — Exportacion de SOLO LECTURA de MongoDB ============
# Vuelca cada coleccion de StudyPilot a JSON para la migracion a v2 (ver docs/MONGO_SCHEMA.md).
#
# Uso (desde la carpeta de la app):
#   Rscript tools/export_mongo.R --dry-run          # solo cuenta documentos, no escribe nada
#   Rscript tools/export_mongo.R                    # escribe export/<YYYY-MM-DD>/<coleccion>.json
#   Opciones: --out=DIR  --env=ARCHIVO  --include-sessions
#
# Garantias:
#   - Solo usa find(), count() y el comando de lectura listCollections. La conexion se envuelve
#     en read_only_collection(), que NO expone insert/update/remove/drop/run arbitrario.
#   - Usa la misma variable MONGODB_URI que la app, leida de app_env (o .Renviron), sin
#     imprimirla nunca.
#   - 'sessions' (hashes de tokens de auto-login) se omite salvo --include-sessions: no se migra.
#   - La salida contiene hashes de contrasena y datos de alumnos: export/ esta en .gitignore.

EXPORT_COLLECTIONS <- c("users", "settings", "courses", "activities", "grades", "study_notes",
                        "syllabi", "schedules", "ai_blocks", "exam_checks", "pomo_sessions",
                        "cal_overrides", "cal_hidden", "sessions", "usage_events")

parse_export_args <- function(args) {
  opt <- list(dry_run = FALSE, out = NULL, env = NULL, include_sessions = FALSE)
  for (a in args) {
    if (a == "--dry-run") opt$dry_run <- TRUE
    else if (a == "--include-sessions") opt$include_sessions <- TRUE
    else if (startsWith(a, "--out=")) opt$out <- sub("^--out=", "", a)
    else if (startsWith(a, "--env=")) opt$env <- sub("^--env=", "", a)
    else stop("Argumento desconocido: ", a, call. = FALSE)
  }
  opt
}

# Lee SOLO la clave pedida de un archivo KEY=VALUE (mismo formato que carga app.R).
read_env_value <- function(files, key = "MONGODB_URI") {
  for (f in files) {
    if (!file.exists(f)) next
    for (ln in trimws(readLines(f, warn = FALSE))) {
      if (!nzchar(ln) || startsWith(ln, "#") || !grepl("=", ln, fixed = TRUE)) next
      if (sub("=.*", "", ln) == key) return(gsub("\r", "", sub("^[^=]+=", "", ln)))
    }
  }
  v <- Sys.getenv(key)
  if (nzchar(v)) v else NA_character_
}

# Envoltorio de solo lectura sobre una coleccion de mongolite (o un doble de prueba).
read_only_collection <- function(conn) {
  list(
    count = function() conn$count("{}"),
    find  = function() conn$find(query = "{}", fields = "{}")   # fields = {} incluye _id
  )
}

# Lista las colecciones de la base con un comando de solo lectura.
list_db_collections <- function(conn) {
  r <- tryCatch(conn$run('{"listCollections": 1, "nameOnly": true}'), error = function(e) NULL)
  b <- r$cursor$firstBatch
  if (is.null(b)) return(character(0))
  if (is.data.frame(b)) return(as.character(b$name))
  vapply(b, function(x) as.character(x$name), character(1))
}

# Fechas a ISO-8601 UTC y list-columns intactas, para que el JSON sea estable.
to_export_json <- function(df) {
  for (nm in names(df))
    if (inherits(df[[nm]], "POSIXt"))
      df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE, na = "null",
                   null = "null", digits = NA, pretty = TRUE)
}

# connect(collection) -> objeto con count/find/run (mongolite o un doble de prueba).
run_export <- function(connect, collections = EXPORT_COLLECTIONS, out_dir = NULL,
                       dry_run = FALSE, include_sessions = FALSE, log = message) {
  if (!include_sessions) collections <- setdiff(collections, "sessions")
  found <- list_db_collections(connect(collections[1]))
  unknown <- setdiff(found, c(EXPORT_COLLECTIONS, "system.views"))
  if (length(unknown))
    log("AVISO: colecciones no documentadas en docs/MONGO_SCHEMA.md: ", paste(unknown, collapse = ", "))
  if (!dry_run) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  counts <- list()
  for (coll in c(collections, unknown)) {
    ro <- read_only_collection(connect(coll))
    n <- ro$count()
    if (!dry_run && n > 0) {
      df <- ro$find()
      writeLines(to_export_json(df), file.path(out_dir, paste0(coll, ".json")), useBytes = TRUE)
      if (nrow(df) != n) log("AVISO: ", coll, " conto ", n, " pero find() devolvio ", nrow(df))
      n <- nrow(df)
    } else if (!dry_run) {
      writeLines("[]", file.path(out_dir, paste0(coll, ".json")))
    }
    counts[[coll]] <- n
    log(sprintf("  %-15s %6d documento(s)", coll, n))
  }

  summary <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    dry_run = dry_run, counts = counts,
    total = sum(unlist(counts)), undocumented_collections = I(unknown),
    skipped = I(if (include_sessions) character(0) else "sessions"))
  if (!dry_run)
    writeLines(jsonlite::toJSON(summary, auto_unbox = TRUE, pretty = TRUE),
               file.path(out_dir, "_resumen.json"))
  invisible(summary)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  opt <- parse_export_args(args)
  env_files <- if (!is.null(opt$env)) opt$env else c("app_env", ".Renviron")
  uri <- read_env_value(env_files)
  if (is.na(uri) || !nzchar(uri)) stop("No se encontro MONGODB_URI en ", paste(env_files, collapse = " / "), call. = FALSE)
  out <- if (!is.null(opt$out)) opt$out else file.path("export", format(Sys.Date(), "%Y-%m-%d"))
  message("[export_mongo] ", if (opt$dry_run) "DRY-RUN (solo conteo)" else paste0("Destino: ", out))
  conns <- list()
  connect <- function(coll) {
    if (is.null(conns[[coll]])) conns[[coll]] <<- mongolite::mongo(collection = coll, url = uri)
    conns[[coll]]
  }
  s <- run_export(connect, out_dir = out, dry_run = opt$dry_run,
                  include_sessions = opt$include_sessions)
  message("[export_mongo] Total: ", s$total, " documento(s)")
  for (cn in conns) try(cn$disconnect(), silent = TRUE)
  invisible(s)
}

# Solo corre al invocarse con Rscript (al hacer source() desde los tests no hace nada)
if (sys.nframe() == 0L) main()
