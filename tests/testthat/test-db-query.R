
parse_q <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)

test_that("uf arma el filtro por usuario como JSON valido", {
  j <- uf("maria")
  expect_true(jsonlite::validate(j))
  expect_equal(parse_q(j), list(user_id = "maria"))
  j2 <- uf("maria", course_id = "CS101", act_id = q_num(3))
  expect_equal(parse_q(j2), list(user_id = "maria", course_id = "CS101", act_id = 3L))
})

test_that("texto con comillas u operadores queda como un unico string (sin inyeccion)", {
  ataques <- c('x","user_id":{"$ne":"x',
               '{"$ne": ""}',
               'a\\"b',
               '"}, "$where": "sleep(1000)", "a": "')
  for (a in ataques) {
    j <- uf(a, name = a)
    expect_true(jsonlite::validate(j))
    p <- parse_q(j)
    expect_identical(names(p), c("user_id", "name"))
    expect_identical(p$user_id, a)
    expect_identical(p$name, a)
  }
})

test_that("q_str rechaza objetos enviados desde el navegador", {
  expect_error(q_str(list(`$ne` = 1)))
  expect_error(q_str(c("a", "b")))
  expect_error(q_str(NULL))
  expect_error(q_str(NA))
  expect_error(uf(list(`$ne` = "x")))
  expect_identical(q_str("ok"), "ok")
})

test_that("q_num valida numeros y q_field nombres de campo", {
  expect_equal(q_num("12.5"), 12.5)
  expect_error(q_num("1; drop"))
  expect_error(q_num(list(`$gt` = 0)))
  expect_identical(q_field("pass_grade"), "pass_grade")
  expect_error(q_field("$where"))
  expect_error(q_field("a.b"))
})

test_that("mq_set arma updates validos; I() conserva arreglos y NA sale null", {
  j <- mq_set(name = 'Lab "1"', weight = 20, temas = I("Unico tema"), done_date = NA_character_)
  expect_true(jsonlite::validate(j))
  p <- parse_q(j)
  expect_identical(p$`$set`$name, 'Lab "1"')
  expect_identical(p$`$set`$temas, list("Unico tema"))
  expect_null(p$`$set`$done_date)
  expect_identical(mq(), "{}")
})
