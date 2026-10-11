
test_that("safe_md no deja pasar HTML ni enlaces javascript", {
  x <- "**hola** <script>alert(1)</script> <img src=x onerror=alert(1)> [a](javascript:alert(1))"
  h <- as.character(safe_md(x))
  expect_match(h, "<strong>hola</strong>", fixed = TRUE)
  expect_false(grepl("<script", h, fixed = TRUE))
  expect_false(grepl("<img", h, fixed = TRUE))
  expect_false(grepl("javascript:", h, ignore.case = TRUE))
})

test_that("safe_md conserva bloques mermaid y codigo", {
  h <- as.character(safe_md("Mapa:\n\n```mermaid\ngraph TD\n  A[\"x\"] --> B\n```\n\nUsa `a<b` aqui"))
  expect_match(h, "mermaid", fixed = TRUE)
  expect_match(h, "A[&quot;x&quot;] --&gt; B", fixed = TRUE)
  expect_match(h, "<code>a&lt;b</code>", fixed = TRUE)
})

test_that("safe_md muestra el texto '<' y '&' literalmente", {
  h <- as.character(safe_md("si x < 3 & y > 2"))
  expect_match(h, "x &lt; 3 &amp; y", fixed = TRUE)
  expect_equal(as.character(safe_md(NULL)), "")
})

test_that("esc escapa HTML", {
  expect_equal(as.character(esc("<b>\"x\"</b>")), "&lt;b&gt;&quot;x&quot;&lt;/b&gt;")
})
