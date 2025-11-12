# tests/testthat/test-parallel.R

test_that("run_tests_sequential returns correct structure", {
  student_fun <- function(x) x * 2
  instructor_fun <- function(x) x * 2
  test_data <- list(
    inputs = list(list(1), list(2), list(3))
  )
  
  results <- run_tests_sequential(student_fun, instructor_fun, test_data, 1e-10)
  
  expect_equal(length(results), 3)
  expect_true(all(sapply(results, function(r) "student" %in% names(r))))
  expect_true(all(sapply(results, function(r) "expected" %in% names(r))))
  expect_true(all(sapply(results, function(r) "index" %in% names(r))))
})

test_that("run_tests_sequential handles errors", {
  student_fun <- function(x) stop("Student error")
  instructor_fun <- function(x) x * 2
  test_data <- list(
    inputs = list(list(1))
  )
  
  results <- run_tests_sequential(student_fun, instructor_fun, test_data, 1e-10)
  
  expect_equal(length(results), 1)
  expect_true(inherits(results[[1]]$student, "error"))
  expect_equal(results[[1]]$expected, 2)
})

test_that("run_tests_parallel switches to sequential for small tests", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  
  student_fun <- function(x) x * 2
  instructor_fun <- function(x) x * 2
  test_data <- list(
    inputs = list(list(1), list(2))  # Only 2 tests
  )
  
  # Should use sequential for < 10 tests
  results <- run_tests_parallel(student_fun, instructor_fun, test_data, 1e-10, TRUE)
  
  expect_equal(length(results), 2)
})

test_that("run_tests_parallel works with many tests", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  
  student_fun <- function(x) x * 2
  instructor_fun <- function(x) x * 2
  test_data <- list(
    inputs = lapply(1:15, function(i) list(i))  # 15 tests
  )
  
  results <- run_tests_parallel(student_fun, instructor_fun, test_data, 1e-10, TRUE)
  
  expect_equal(length(results), 15)
  expect_true(all(sapply(results, function(r) r$student == r$expected)))
})

test_that("parallel and sequential give same results", {
  skip_on_cran()
  skip_if_not_installed("parallel")
  
  student_fun <- function(x) x^2 + 2*x + 1
  instructor_fun <- function(x) x^2 + 2*x + 1
  test_data <- list(
    inputs = lapply(1:12, function(i) list(i))
  )
  
  results_seq <- run_tests_sequential(student_fun, instructor_fun, test_data, 1e-10)
  results_par <- run_tests_parallel(student_fun, instructor_fun, test_data, 1e-10, TRUE)
  
  # Extract just the results, not indices
  seq_values <- sapply(results_seq, function(r) r$student)
  par_values <- sapply(results_par, function(r) r$student)
  
  expect_equal(seq_values, par_values)
})
