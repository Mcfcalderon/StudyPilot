
test_that("compute_course_avg calcula puntos y promedio (datos reales Estrategia)", {
  ev <- data.frame(code=paste0("E",1:6), weight=c(20,5,25,20,5,25),
                   name=c("PC1","EC1","Parcial","PC2","EC2","Final"), stringsAsFactors=FALSE)
  gr <- data.frame(code=c("E1","E2","E3","E4"), grade=c(16,17,14,16))
  r <- compute_course_avg(ev, gr, 13)
  expect_equal(r$earned, 10.75)
  expect_equal(r$partial, 15.36)
  expect_equal(r$pct_graded, 70)
  expect_equal(r$remaining, 30)
  expect_equal(r$needed, 7.5)          # (13-10.75)/0.30
  expect_equal(nrow(r$remaining_evals), 2)
})

test_that("compute_course_avg respeta el umbral configurable", {
  ev <- data.frame(code=paste0("E",1:6), weight=c(20,5,25,20,5,25),
                   name=letters[1:6], stringsAsFactors=FALSE)
  gr <- data.frame(code=c("E1","E2","E3","E4"), grade=c(16,17,14,16))
  expect_equal(compute_course_avg(ev, gr, 11)$needed, 0.83)  # (11-10.75)/0.30
})

test_that("compute_course_avg sin notas devuelve vacio seguro", {
  ev <- data.frame(code=c("E1","E2"), weight=c(50,50), name=c("a","b"), stringsAsFactors=FALSE)
  r <- compute_course_avg(ev, data.frame(code=character(), grade=numeric()), 13)
  expect_equal(r$pct_graded, 0)
  expect_equal(r$earned, 0)
  expect_equal(r$remaining, 100)
})

test_that("generar_repaso_espaciado crea repasos en offsets validos", {
  aa <- data.frame(done=0, date=as.character(Sys.Date()+10), type="examen",
                   weight=25, name="Parcial", course_id="X1", stringsAsFactors=FALSE)
  cc <- data.frame(id="X1", short="Data", stringsAsFactors=FALSE)
  r <- generar_repaso_espaciado(aa, cc, Sys.Date()+30)
  expect_equal(nrow(r), 3)                       # offsets 1,3,7 (14 cae en el pasado)
  expect_true(all(as.Date(r$fecha) < Sys.Date()+10))
  expect_true(all(as.Date(r$fecha) >= Sys.Date()))
})

test_that("generar_repaso_espaciado ignora examenes de bajo peso o sin examenes", {
  cc <- data.frame(id="X1", short="Data", stringsAsFactors=FALSE)
  baja <- data.frame(done=0, date=as.character(Sys.Date()+10), type="examen",
                     weight=10, name="Quiz", course_id="X1", stringsAsFactors=FALSE)
  expect_equal(nrow(generar_repaso_espaciado(baja, cc, Sys.Date()+30)), 0)
  expect_equal(nrow(generar_repaso_espaciado(data.frame(), cc, Sys.Date()+30)), 0)
})

