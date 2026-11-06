# tools/export_mongo.R con un doble de Mongo en memoria (nunca toca la base real)
for (.p in c("tools/export_mongo.R", "../../tools/export_mongo.R")) if (file.exists(.p)) { source(.p); break }

fake_db <- function(data, extra_collections = character(0)) {
  calls <- new.env(); calls$log <- character(0)
  forbid <- function(name) function(...) stop("ESCRITURA PROHIBIDA: ", name)
  connect <- function(coll) {
    df <- data[[coll]]
    if (is.null(df)) df <- data.frame()
    list(
      count  = function(q = "{}") { calls$log <- c(calls$log, paste0(coll, ".count")); nrow(df) },
      find   = function(query = "{}", fields = "{}") {
        calls$log <- c(calls$log, paste0(coll, ".find(", fields, ")")); df },
      run    = function(cmd) {
        calls$log <- c(calls$log, paste0(coll, ".run"))
        stopifnot(grepl("listCollections", cmd))
        list(cursor = list(firstBatch = data.frame(name = c(names(data), extra_collections))))
      },
      insert = forbid("insert"), update = forbid("update"),
      remove = forbid("remove"), drop = forbid("drop")
    )
  }
  list(connect = connect, calls = calls)
}

sample_data <- function() list(
  users = data.frame(`_id` = c("a1", "a2"), user = c("ana.p", "luis"),
                     password = c("$7$x", "$7$y"), name = c("Ana", "Luis"),
                     check.names = FALSE, stringsAsFactors = FALSE),
  activities = {
    d <- data.frame(user_id = "ana.p", act_id = 1:2, course_id = "CS2031",
                    name = c("PC1", "Examen \"Parcial\""), date = c("2026-04-10", "2026-05-15"),
                    stringsAsFactors = FALSE)
    d$temas_vinculados <- list(c("REST", "JWT"), character(0)); d
  },
  sessions = data.frame(token_hash = "h", user = "ana.p",
                        expires_at = as.POSIXct("2026-12-01 10:00:00", tz = "UTC"))
)

test_that("parse_export_args reconoce las opciones y rechaza las desconocidas", {
  o <- parse_export_args(c("--dry-run", "--out=x/y", "--include-sessions"))
  expect_true(o$dry_run); expect_true(o$include_sessions); expect_equal(o$out, "x/y")
  expect_false(parse_export_args(character(0))$dry_run)
  expect_error(parse_export_args("--borrar"), "desconocido")
})

test_that("read_env_value lee solo la clave pedida y respeta '=' dentro del valor", {
  f <- tempfile(); on.exit(unlink(f))
  writeLines(c("# comentario", "OTRA=1", "MONGODB_URI=mongodb+srv://u:p@h/db?a=b\r"), f)
  expect_equal(read_env_value(f), "mongodb+srv://u:p@h/db?a=b")
  expect_true(is.na(read_env_value(tempfile(), key = "NO_EXISTE_STUDYPILOT")))
})

test_that("--dry-run solo cuenta: no escribe archivos ni llama find()", {
  db <- fake_db(sample_data())
  out <- file.path(tempdir(), "exp_dry"); unlink(out, recursive = TRUE)
  s <- run_export(db$connect, out_dir = out, dry_run = TRUE, log = function(...) NULL)
  expect_false(dir.exists(out))
  expect_false(any(grepl("\\.find", db$calls$log)))
  expect_equal(s$counts$users, 2); expect_equal(s$counts$activities, 2)
  expect_equal(s$counts$grades, 0)
  expect_null(s$counts$sessions)            # omitida por defecto
})

test_that("la exportacion escribe un JSON por coleccion y un resumen con los conteos", {
  db <- fake_db(sample_data(), extra_collections = "connection_test")
  out <- file.path(tempdir(), "exp_full"); unlink(out, recursive = TRUE)
  msgs <- character(0)
  s <- run_export(db$connect, out_dir = out, log = function(...) msgs <<- c(msgs, paste0(...)))
  expect_true(file.exists(file.path(out, "users.json")))
  expect_true(file.exists(file.path(out, "grades.json")))   # vacia -> []
  expect_equal(readLines(file.path(out, "grades.json")), "[]")
  expect_false(file.exists(file.path(out, "sessions.json")))
  # coleccion no documentada: se avisa y se exporta igual
  expect_true(any(grepl("connection_test", msgs)))
  expect_true("connection_test" %in% names(s$counts))
  # _id incluido (fields = {}) y arrays conservados
  expect_true(any(grepl('find\\(\\{\\}\\)', db$calls$log)))
  acts <- jsonlite::fromJSON(file.path(out, "activities.json"), simplifyVector = FALSE)
  expect_equal(length(acts), 2)
  expect_equal(unlist(acts[[1]]$temas_vinculados), c("REST", "JWT"))
  expect_equal(acts[[2]]$name, "Examen \"Parcial\"")
  users <- jsonlite::fromJSON(file.path(out, "users.json"))
  expect_equal(users$`_id`, c("a1", "a2"))
  res <- jsonlite::fromJSON(file.path(out, "_resumen.json"))
  expect_equal(res$counts$users, 2); expect_equal(res$total, 4)
  expect_equal(res$skipped, "sessions")
})

test_that("--include-sessions exporta sessions con fechas en ISO UTC", {
  db <- fake_db(sample_data())
  out <- file.path(tempdir(), "exp_sess"); unlink(out, recursive = TRUE)
  run_export(db$connect, out_dir = out, include_sessions = TRUE, log = function(...) NULL)
  ss <- jsonlite::fromJSON(file.path(out, "sessions.json"))
  expect_equal(ss$expires_at, "2026-12-01T10:00:00.000Z")
})

test_that("read_only_collection no expone metodos de escritura", {
  ro <- read_only_collection(fake_db(sample_data())$connect("users"))
  expect_setequal(names(ro), c("count", "find"))
})
