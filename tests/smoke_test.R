# Smoke test de arranque de StudyPilot:  source("tests/smoke_test.R")
# Verifica que TODOS los .R parsean y que las funciones clave existen.
# Atrapa los errores de sintaxis/carga que tumbaban el despliegue.

.smoke <- function() {
  files <- c("app.R","global.R",
             list.files("R", "\\.R$", full.names=TRUE),
             list.files("server", "\\.R$", full.names=TRUE),
             list.files("ui", "\\.R$", full.names=TRUE))
  fails <- 0
  cat("== 1) Parse de", length(files), "archivos ==\n")
  for (f in files) {
    r <- tryCatch({ parse(f); TRUE }, error=function(e){ cat("  X", f, "->", conditionMessage(e), "\n"); FALSE })
    if (!r) fails <- fails + 1
  }
  if (fails == 0) cat("  OK: todos parsean\n")

  cat("== 2) Funciones clave definidas (via global.R) ==\n")
  ok <- tryCatch({
    e <- new.env()
    suppressWarnings(suppressMessages(sys.source("global.R", envir=e)))
    req_fns <- c("current_week","week_to_date","date_to_week","PASS_GRADE",
                 "grade_class","grade_hex","priority_class","compute_course_avg","generar_repaso_espaciado","get_semester_start")
    faltan <- req_fns[!vapply(req_fns, function(x) exists(x, envir=e, inherits=TRUE), logical(1))]
    if (length(faltan)) { cat("  X faltan:", paste(faltan, collapse=", "), "\n"); FALSE } else { cat("  OK: presentes\n"); TRUE }
  }, error=function(e){ cat("  X global.R fallo:", conditionMessage(e), "\n"); FALSE })
  if (!ok) fails <- fails + 1

  cat(if (fails==0) "\n== SMOKE TEST: PASO ==\n" else paste0("\n== SMOKE TEST: ", fails, " FALLO(S) ==\n"))
  invisible(fails == 0)
}
.smoke()

