# ============ R/auth_session.R — Tokens de sesion (auto-login) ============
# El token es aleatorio (32 bytes) y solo lo conoce el navegador del usuario.
# En la BD se guarda unicamente su hash SHA-256, con fecha de expiracion:
# si la BD se filtra, los hashes no sirven para entrar.

SESSION_DAYS <- 30

session_token_new <- function() sodium::bin2hex(sodium::random(32))

session_token_valid_format <- function(token) {
  is.character(token) && length(token) == 1 && !is.na(token) && grepl("^[0-9a-f]{64}$", token)
}

session_token_hash <- function(token) {
  if (!session_token_valid_format(token)) stop("token con formato invalido", call. = FALSE)
  digest::digest(token, algo = "sha256", serialize = FALSE)
}

session_expiry <- function(now = Sys.time()) now + SESSION_DAYS * 86400

session_is_valid <- function(expires_at, now = Sys.time()) {
  length(expires_at) == 1 && !is.na(expires_at) && as.POSIXct(expires_at) > now
}
