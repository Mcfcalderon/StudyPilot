
h <- sodium::password_store("secreto-admin")
h_hex <- sodium::bin2hex(charToRaw(h))

test_that("verify_password acepta la contraseña correcta y rechaza el resto", {
  expect_true(verify_password(h, "secreto-admin"))
  expect_false(verify_password(h, "otra"))
  expect_false(verify_password(h, h))                 # el hash no sirve como contraseña
  expect_false(verify_password("texto-plano", "texto-plano"))
  expect_false(verify_password(h, list("secreto-admin")))
})

test_that("parse_admin_users acepta hash directo o en hex y descarta texto plano", {
  raw <- paste0("ana:", h, ":Ana Perez, beto:", h_hex, ":Beto, carla:clave123:Carla")
  a <- suppressMessages(parse_admin_users(raw))
  expect_equal(a$user, c("ana", "beto"))
  expect_equal(a$name, c("Ana Perez", "Beto"))
  expect_true(all(vapply(a$password, is_password_hash, logical(1))))
  expect_message(parse_admin_users("carla:clave123:Carla"), "no es un hash")
  expect_equal(nrow(parse_admin_users("")), 0)
})

test_that("check_admin_login devuelve el nombre solo con la contraseña correcta", {
  a <- parse_admin_users(paste0("ana:", h_hex, ":Ana"))
  expect_equal(check_admin_login(a, "ana", "secreto-admin"), "Ana")
  expect_null(check_admin_login(a, "ana", "mala"))
  expect_null(check_admin_login(a, "nadie", "secreto-admin"))
})

test_that("el login se bloquea tras 5 fallos en 10 minutos y se libera despues", {
  st <- new.env()
  t0 <- as.POSIXct("2026-10-11 12:00:00", tz = "UTC")
  for (i in 1:4) login_fail_record("ana", t0 + i, store = st)
  expect_false(login_blocked("ana", t0 + 5, store = st))
  login_fail_record("ana", t0 + 5, store = st)
  expect_true(login_blocked("ana", t0 + 6, store = st))
  expect_false(login_blocked("beto", t0 + 6, store = st))
  expect_false(login_blocked("ana", t0 + 700, store = st))
  login_fail_clear("ana", store = st)
  expect_false(login_blocked("ana", t0 + 6, store = st))
})
