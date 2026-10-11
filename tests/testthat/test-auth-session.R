
test_that("el token es aleatorio de 64 hex y no se repite", {
  t1 <- session_token_new(); t2 <- session_token_new()
  expect_true(session_token_valid_format(t1))
  expect_false(identical(t1, t2))
})

test_that("en BD se guarda un hash distinto del token y estable", {
  t <- session_token_new()
  h <- session_token_hash(t)
  expect_false(identical(h, t))
  expect_identical(h, session_token_hash(t))
  expect_match(h, "^[0-9a-f]{64}$")
})

test_that("el token viejo md5(usuario + texto fijo) y objetos se rechazan", {
  viejo <- digest::digest("maria_studypilot_session", algo = "md5")
  expect_false(session_token_valid_format(viejo))
  expect_false(session_token_valid_format(list(`$ne` = "")))
  expect_false(session_token_valid_format(NULL))
  expect_error(session_token_hash(viejo))
})

test_that("la expiracion se respeta", {
  ahora <- as.POSIXct("2026-10-11 12:00:00", tz = "UTC")
  expect_true(session_is_valid(session_expiry(ahora), now = ahora))
  expect_false(session_is_valid(ahora - 1, now = ahora))
  expect_false(session_is_valid(NA, now = ahora))
  expect_false(session_is_valid(session_expiry(ahora), now = ahora + 31 * 86400))
})
