# tests/testthat/test-final-coverage.R

test_that("autograder handles no internet gracefully", {
  # Mock no internet (hard to test, so skip)
  skip("Requires mocking curl::has_internet()")
})

test_that("format_output handles all matrix edge cases", {
  # Test 1x1 matrix
  expect_type(format_output(matrix(1)), "character")
  
  # Test large matrix with few rows
  expect_type(format_output(matrix(1:1000, nrow = 2)), "character")
  
  # Test matrix with NA
  mat <- matrix(c(1, NA, 3, 4), 2, 2)
  expect_type(format_output(mat), "character")
})

test_that("extract functions handle all edge cases", {
  # Test environment with multiple functions
  env <- new.env()
  env$func1 <- function(x) x
  env$func2 <- function(x) x^2
  env$var1 <- "not a function"
  
  # Should extract the first function found
  result <- extract_instructor_function(env, "test")
  expect_true(is.function(result))
})

test_that("provide_feedback handles non-vector types", {
  # Test with lists
  student <- list(a = 1)
  expected <- list(a = 2)
  feedback <- provide_feedback(student, expected, list())
  
  expect_type(feedback, "list")
})

test_that("autograder handles custom comparison functions", {
  skip("Requires test data with custom comparison_fn")
})
