
test_that("grade_tier clasifica correctamente segun PASS_GRADE", {
  expect_equal(grade_tier(list(pct_graded=0,  partial=0)),  "none")
  expect_equal(grade_tier(list(pct_graded=70, partial=15)), "ok")
  expect_equal(grade_tier(list(pct_graded=70, partial=13)), "ok")   # borde exacto
  expect_equal(grade_tier(list(pct_graded=70, partial=11)), "warn") # dentro de WARN_BAND
  expect_equal(grade_tier(list(pct_graded=70, partial=8)),  "bad")
  expect_equal(grade_tier(NULL), "none")
})

test_that("grade_class y grade_hex son consistentes con el tier", {
  ok <- list(pct_graded=70, partial=16)
  expect_equal(grade_class(ok), "success")
  expect_equal(grade_hex(ok),   "#16a34a")
  bad <- list(pct_graded=70, partial=5)
  expect_equal(grade_class(bad), "danger")
})

test_that("week_to_date y date_to_week son inversas", {
  expect_equal(date_to_week(week_to_date(5)), 5L)
  expect_equal(date_to_week(week_to_date(1)), 1L)
  expect_equal(date_to_week(week_to_date(16)), 16L)
})

test_that("date_to_week es robusto ante fechas invalidas o vacias", {
  expect_equal(date_to_week(NA), 0L)
  expect_equal(date_to_week(""), 0L)
  expect_true(date_to_week("2026-03-23") >= 1L)
})

test_that("date_to_week hace clamp dentro de 1..TOTAL_WEEKS", {
  expect_equal(date_to_week(SEMESTER_START - 30), 1L)  # antes del ciclo
  expect_equal(date_to_week(SEMESTER_START + 400), 16L) # muy despues
})

test_that("priority_class segmenta por peso", {
  expect_equal(as.character(priority_class(25)), "high")
  expect_equal(as.character(priority_class(15)), "medium")
  expect_equal(as.character(priority_class(5)),  "low")
})

