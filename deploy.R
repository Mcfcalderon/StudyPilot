# ============ deploy.R — Despliegue modular StudyPilot ============
# EJECUTAR DESDE TU CONSOLA DE RSTUDIO (no desde Posit Assistant):
#   source("deploy.R")

library(rsconnect)

app_files <- c(
  "app.R",
  "global.R",
  ".Renviron",
  list.files("ui",     pattern = "\\.R$", full.names = TRUE, recursive = FALSE),
  list.files("server", pattern = "\\.R$", full.names = TRUE, recursive = FALSE),
  list.files("R",      pattern = "\\.R$", full.names = TRUE, recursive = FALSE),
  list.files("www",    full.names = TRUE, recursive = FALSE)
)

# Filter OneDrive temp files
app_files <- app_files[!grepl("~\\$|\\.~|Thumbs\\.db|\\.tmp$|desktop\\.ini", app_files)]
app_files <- unique(app_files)

cat("\n========================================\n")
cat("  StudyPilot \u2014 Deploying", length(app_files), "files\n")
cat("========================================\n")
cat(paste0("  ", app_files), sep = "\n")
cat("\n")

rsconnect::deployApp(
  appDir       = getwd(),
  appFiles     = app_files,
  # App en produccion: https://studypilot.shinyapps.io/plataforma/ (appId fijo para no crear otra)
  appName      = "plataforma",
  appId        = 17534798,
  appTitle     = "StudyPilot",
  account      = "studypilot",
  server       = "shinyapps.io",
  forceUpdate  = TRUE,
  launch.browser = interactive(),
  lint         = FALSE
)

