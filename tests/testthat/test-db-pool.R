
# Conector falso: cuenta cuantas conexiones se crean y permite simular fallos.
fake_connector <- function(fail_first = NULL) {
  state <- new.env()
  state$connects <- 0
  state$fail <- fail_first
  connect <- function(collection) {
    state$connects <- state$connects + 1
    list(find = function(q) {
      if (!is.null(state$fail)) { msg <- state$fail; state$fail <- NULL; stop(msg) }
      paste0(collection, ":", q)
    })
  }
  list(connect = connect, state = state)
}

test_that("la conexion se crea una sola vez y se reutiliza", {
  pool_reset()
  f <- fake_connector()
  col <- pool_collection("grades", f$connect)
  expect_equal(col$find("a"), "grades:a")
  expect_equal(col$find("b"), "grades:b")
  expect_equal(pool_collection("grades", f$connect)$find("c"), "grades:c")
  expect_equal(f$state$connects, 1)
})

test_that("ante un error de red se reconecta y reintenta una vez", {
  pool_reset()
  f <- fake_connector(fail_first = "Failed to read 4 bytes: socket error or timeout")
  col <- pool_collection("activities", f$connect)
  expect_equal(col$find("x"), "activities:x")
  expect_equal(f$state$connects, 2)
})

test_that("un error que no es de red no se reintenta", {
  pool_reset()
  f <- fake_connector(fail_first = "Invalid JSON object")
  col <- pool_collection("activities", f$connect)
  expect_error(col$find("x"), "Invalid JSON")
  expect_equal(f$state$connects, 1)
})
