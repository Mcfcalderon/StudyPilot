# ============ R/auth_passwords.R — Verificacion de contraseñas y admins ============
# Contraseñas: hash scrypt de sodium ("$7$..."). Los admins se definen en
# STUDYPILOT_ADMIN_USERS como  usuario:HASH:Nombre  (separados por coma).
# HASH puede ir tal cual ("$7$...") o en hexadecimal (recomendado: evita que
# el "$" choque con .Renviron). Genera el valor con tools/hash_password.R.
# Ya NO se aceptan contraseñas de admin en texto plano.

is_password_hash <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && startsWith(x, "$7$")
}

verify_password <- function(stored, pass) {
  if (!is_password_hash(stored) || !is.character(pass) || length(pass) != 1) return(FALSE)
  isTRUE(tryCatch(sodium::password_verify(stored, pass), error = function(e) FALSE))
}

# "$7$..." o su version en hex -> "$7$..." ; cualquier otra cosa -> NA
decode_admin_hash <- function(x) {
  x <- trimws(x)
  if (is_password_hash(x)) return(x)
  if (grepl("^([0-9a-f]{2})+$", x)) {
    dec <- tryCatch(rawToChar(sodium::hex2bin(x)), error = function(e) NA_character_)
    if (is_password_hash(dec)) return(dec)
  }
  NA_character_
}

parse_admin_users <- function(raw) {
  out <- data.frame(user = character(), password = character(), name = character(),
                    stringsAsFactors = FALSE)
  if (is.null(raw) || !nzchar(trimws(raw))) return(out)
  for (entry in trimws(strsplit(raw, ",", fixed = TRUE)[[1]])) {
    if (!nzchar(entry)) next
    p <- strsplit(entry, ":", fixed = TRUE)[[1]]
    if (length(p) < 2) next
    hash <- decode_admin_hash(p[2])
    if (is.na(hash)) {
      message("[StudyPilot] Admin '", p[1], "' ignorado: la contraseña no es un hash (usa tools/hash_password.R)")
      next
    }
    name <- if (length(p) >= 3) paste(p[-(1:2)], collapse = ":") else p[1]
    out[nrow(out) + 1, ] <- list(p[1], hash, name)
  }
  out
}

# Nombre del admin si usuario y contraseña coinciden; NULL si no.
check_admin_login <- function(admins, user, pass) {
  i <- which(admins$user == user)
  if (length(i) == 1 && verify_password(admins$password[i], pass)) admins$name[i] else NULL
}
