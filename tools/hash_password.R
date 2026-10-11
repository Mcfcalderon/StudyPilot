# ============ tools/hash_password.R — Genera el hash de una contraseña de admin ============
# Uso (en RStudio o consola R, desde la carpeta del proyecto):
#   source("tools/hash_password.R")
# Te pide usuario, nombre y contraseña, y muestra la entrada lista para pegar en
# STUDYPILOT_ADMIN_USERS de tu .Renviron (varias entradas se separan con coma):
#   STUDYPILOT_ADMIN_USERS=usuario:HASH_HEX:Nombre
# La contraseña no se guarda en ningun lado.

local({
  user <- readline("Usuario admin: ")
  name <- readline("Nombre a mostrar: ")
  pass <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    rstudioapi::askForPassword("Contraseña del admin") else readline("Contraseña: ")
  if (!nzchar(user) || !nzchar(pass)) stop("Usuario y contraseña son obligatorios")
  if (nchar(pass) < 8) stop("Usa al menos 8 caracteres")
  h <- sodium::bin2hex(charToRaw(sodium::password_store(pass)))
  cat("\nAgrega esta entrada a STUDYPILOT_ADMIN_USERS:\n\n",
      user, ":", h, ":", name, "\n\n", sep = "")
})
