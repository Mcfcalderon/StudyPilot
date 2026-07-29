# Ejecuta la suite de tests de StudyPilot:  source("tests/run_tests.R")
library(testthat)
cat("== StudyPilot: tests de funciones puras ==\n")
test_dir("tests/testthat", reporter = "progress")

