# ============================================================================
# AUTOGRADER - CORE FUNCTIONALITY TESTS
# ============================================================================
#
# Tests for the main autograder workflow and entry points:
#   - autograder() main function
#   - list_problems()
#   - preview_tests()
#   - Package loading (.onAttach)
#
# ============================================================================

# ============================================================================
# AUTOGRADER MAIN FUNCTION TESTS
# ============================================================================

test_that("autograder returns correct structure for correct answers", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  
  result <- suppressMessages(autograder("fibonacci", verbose = FALSE))
  
  expect_type(result, "list")
  expect_true("passed" %in% names(result))
  expect_true("total" %in% names(result))
  expect_true("score" %in% names(result))
  expect_true("max_score" %in% names(result))
  expect_equal(result$passed, result$total)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder handles wrong answers correctly", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    rep(1, n)  # Wrong implementation
  }, envir = .GlobalEnv)
  
  result <- suppressMessages(autograder("fibonacci", verbose = FALSE))
  
  expect_type(result, "list")
  expect_true(result$passed < result$total)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder handles missing student function", {
  skip_on_cran()
  skip_if_offline()
  
  # Ensure function doesn't exist
  if (exists("student_fibonacci", envir = .GlobalEnv)) {
    rm(student_fibonacci, envir = .GlobalEnv)
  }
  
  expect_error(
    suppressMessages(autograder("fibonacci")),
    "not found"
  )
})

test_that("autograder handles runtime errors in student code", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    stop("Intentional error for testing")
  }, envir = .GlobalEnv)
  
  result <- suppressMessages(autograder("fibonacci", verbose = FALSE))
  
  expect_type(result, "list")
  expect_equal(result$passed, 0)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder detects type errors", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    as.character(1:n)  # Returns character instead of numeric
  }, envir = .GlobalEnv)
  
  result <- suppressMessages(autograder("fibonacci", verbose = FALSE))
  
  expect_type(result, "list")
  expect_true(result$passed < result$total)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder shows progress bar when requested", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  
  suppressMessages({
    result <- autograder("fibonacci", show_progress = TRUE, verbose = FALSE)
  })
  
  expect_type(result, "list")
  expect_equal(result$passed, 6)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder validates function_name parameter", {
  expect_error(autograder(123), "string|character")
})

test_that("autograder validates logical parameters", {
  # Test with non-logical value - should get informative error
  expect_error(autograder("test", verbose = "yes"), "logical flag|logical")
  expect_error(autograder("test", show_progress = "yes"), "logical flag|logical")
})

# ============================================================================
# LIST_PROBLEMS TESTS
# ============================================================================

test_that("list_problems returns expected format", {
  skip_on_cran()
  skip_if_offline()
  
  result <- suppressMessages(list_problems())
  
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

# ============================================================================
# PREVIEW_TESTS TESTS
# ============================================================================

test_that("preview_tests returns test data structure", {
  skip_on_cran()
  skip_if_offline()
  
  result <- suppressMessages(preview_tests("fibonacci"))
  
  expect_type(result, "list")
  # The function returns test_data with these fields:
  expect_true("inputs" %in% names(result))
  expect_true("descriptions" %in% names(result))
  expect_true("hidden" %in% names(result))
  expect_true("points" %in% names(result))
})

test_that("preview_tests validates function name parameter", {
  expect_error(preview_tests(123), "character")
})

# ============================================================================
# PACKAGE LOADING TESTS
# ============================================================================

test_that(".onAttach produces startup message", {
  msg <- capture.output(autograder:::.onAttach("autograder", "autograder"), type = "message")
  expect_true(any(grepl("Autograder", msg)))
})

# ============================================================================
# SCORE FEEDBACK MESSAGES
# ============================================================================

test_that("autograder provides appropriate feedback based on score", {
  skip_on_cran()
  skip_if_offline()
  
  # Test with wrong answers to trigger feedback
  assign("student_fibonacci", function(n) {
    if (n == 1) return(1)  # Only first test passes
    rep(0, n)
  }, envir = .GlobalEnv)
  
  output <- capture.output({
    result <- autograder("fibonacci", verbose = TRUE)
  }, type = "message")
  
  expect_true(result$passed < result$total)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

# ============================================================================
# ADDITIONAL AUTOGRADER TESTS
# ============================================================================

test_that("autograder result contains core fields", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  
  result <- suppressMessages(autograder("fibonacci", verbose = FALSE))
  
  # Check for core result fields
  expect_true("passed" %in% names(result))
  expect_true("total" %in% names(result))
  expect_true("score" %in% names(result))
  expect_true("max_score" %in% names(result))
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder handles show_hidden parameter", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  
  # Both values should work
  result1 <- suppressMessages(autograder("fibonacci", verbose = FALSE, show_hidden = FALSE))
  result2 <- suppressMessages(autograder("fibonacci", verbose = FALSE, show_hidden = TRUE))
  
  expect_type(result1, "list")
  expect_type(result2, "list")
  # Score should be the same regardless of show_hidden
  expect_equal(result1$score, result2$score)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder handles verbose = FALSE", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  
  result <- suppressMessages(autograder("fibonacci", verbose = FALSE))
  
  expect_type(result, "list")
  expect_true(result$passed > 0)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder with different problems", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_factorial", function(n) {
    if (n <= 1) return(1)
    prod(1:n)
  }, envir = .GlobalEnv)
  
  result <- suppressMessages(autograder("factorial", verbose = FALSE))
  
  expect_type(result, "list")
  
  rm(student_factorial, envir = .GlobalEnv)
})

# ============================================================================
# PREVIEW_TESTS ADDITIONAL TESTS
# ============================================================================

test_that("preview_tests shows hidden test count", {
  skip_on_cran()
  skip_if_offline()
  
  result <- suppressMessages(preview_tests("fibonacci"))
  
  # Hidden should exist
  expect_true("hidden" %in% names(result))
  # Some tests may be hidden
  expect_true(is.logical(result$hidden) || length(result$hidden) > 0)
})

test_that("preview_tests handles nonexistent problem", {
  skip_on_cran()
  skip_if_offline()
  
  expect_error(
    suppressMessages(preview_tests("nonexistent_problem_xyz123")),
    class = "function_not_found_error"
  )
})

# ============================================================================
# LIST_PROBLEMS ADDITIONAL TESTS
# ============================================================================

test_that("list_problems includes known problems", {
  skip_on_cran()
  skip_if_offline()
  
  problems <- suppressMessages(list_problems())
  
  expect_true("fibonacci" %in% problems)
  expect_true("factorial" %in% problems)
})

test_that("list_problems returns character vector", {
  skip_on_cran()
  skip_if_offline()
  
  # The function should work and return a result
  problems <- suppressMessages(list_problems())
  expect_type(problems, "character")
  expect_true(length(problems) > 0)
})

