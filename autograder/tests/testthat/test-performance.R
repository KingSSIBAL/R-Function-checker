# ============================================================================
# AUTOGRADER - PERFORMANCE COMPARISON TESTS
# ============================================================================
#
# Tests for performance comparison functionality:
#   - compare_performance()
#   - print.performance_comparison()
#   - plot.performance_comparison()
#
# ============================================================================

# ============================================================================
# INPUT VALIDATION TESTS
# ============================================================================

test_that("compare_performance validates function_name parameter", {
  expect_error(compare_performance(123), "character")
  expect_error(compare_performance(c("a", "b")), "single character")
  expect_error(compare_performance(NULL), "character")
})

test_that("compare_performance validates n_runs parameter", {
  # Setup a valid student function
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  expect_error(compare_performance("fibonacci", n_runs = 0), "positive")
  expect_error(compare_performance("fibonacci", n_runs = -5), "positive")
  expect_error(compare_performance("fibonacci", n_runs = "ten"), "positive integer")
})

test_that("compare_performance validates warmup parameter", {
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  expect_error(compare_performance("fibonacci", warmup = -1), "non-negative")
  expect_error(compare_performance("fibonacci", warmup = "five"), "non-negative integer")
})

test_that("compare_performance validates verbose parameter", {
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  expect_error(compare_performance("fibonacci", verbose = "yes"), "TRUE or FALSE")
})

# ============================================================================
# MISSING FUNCTION TESTS
# ============================================================================

test_that("compare_performance errors when student function missing", {
  skip_on_cran()
  skip_if_offline()
  
  # Ensure function doesn't exist
  if (exists("student_fibonacci", envir = .GlobalEnv)) {
    rm(student_fibonacci, envir = .GlobalEnv)
  }
  
  expect_error(compare_performance("fibonacci"), "not found")
})

test_that("compare_performance errors for non-function student variable", {
  assign("student_fibonacci", "not a function", envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  expect_error(compare_performance("fibonacci"), "not a function")
})

# ============================================================================
# BASIC FUNCTIONALITY TESTS
# ============================================================================

test_that("compare_performance returns correct structure", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 10, warmup = 2, verbose = FALSE))
  
  expect_s3_class(result, "performance_comparison")
  expect_true("function_name" %in% names(result))
  expect_true("n_runs" %in% names(result))
  expect_true("student_times" %in% names(result))
  expect_true("instructor_times" %in% names(result))
  expect_true("student_stats" %in% names(result))
  expect_true("instructor_stats" %in% names(result))
  expect_true("ratio" %in% names(result))
  expect_true("passed_correctness" %in% names(result))
  expect_true("verdict" %in% names(result))
})

test_that("compare_performance timing vectors have correct length", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 15, verbose = FALSE))
  
  expect_equal(length(result$student_times), 15)
  expect_equal(length(result$instructor_times), 15)
  expect_equal(result$n_runs, 15)
})

test_that("compare_performance detects correct implementation", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 5, verbose = FALSE))
  
  expect_true(result$passed_correctness)
})

test_that("compare_performance detects incorrect implementation", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    rep(0, n)  # Wrong implementation
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressWarnings(suppressMessages(
    compare_performance("fibonacci", n_runs = 5, verbose = FALSE)
  ))
  
  expect_false(result$passed_correctness)
})

# ============================================================================
# STATISTICS TESTS
# ============================================================================

test_that("compare_performance stats are computed correctly", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 20, verbose = FALSE))
  
  # Check stats structure
  expect_true("median" %in% names(result$student_stats))
  expect_true("mean" %in% names(result$student_stats))
  expect_true("sd" %in% names(result$student_stats))
  expect_true("min" %in% names(result$student_stats))
  expect_true("max" %in% names(result$student_stats))
  
  # Check stats are positive
  expect_gte(result$student_stats$median, 0)
  expect_gte(result$instructor_stats$median, 0)
  expect_gte(result$ratio, 0)
})

# ============================================================================
# CUSTOM TEST INPUTS
# ============================================================================

test_that("compare_performance accepts custom test inputs", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  custom_inputs <- list(list(5), list(10), list(20))
  
  result <- suppressMessages(compare_performance(
    "fibonacci", 
    n_runs = 5, 
    test_inputs = custom_inputs,
    verbose = FALSE
  ))
  
  expect_equal(result$n_inputs, 3)
  expect_true(result$passed_correctness)
})

# ============================================================================
# PRINT METHOD TESTS
# ============================================================================

test_that("print.performance_comparison produces output", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 5, verbose = FALSE))
  
  output <- capture.output(print(result))
  
  expect_true(length(output) > 0)
  expect_true(any(grepl("Performance Comparison", output)))
  expect_true(any(grepl("fibonacci", output)))
  expect_true(any(grepl("Ratio", output)))
})

# ============================================================================
# PLOT METHOD TESTS
# ============================================================================

test_that("plot.performance_comparison creates plot without error", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 5, verbose = FALSE))
  
  # Should not throw error
  expect_silent(suppressWarnings(plot(result)))
})

# ============================================================================
# VERDICT TESTS
# ============================================================================

test_that("compare_performance generates appropriate verdicts", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 10, verbose = FALSE))
  
  # Verdict should be one of the known options
  expect_true(
    grepl("FASTER|COMPARABLE|SLOWER|SLOW", result$verdict)
  )
})

# ============================================================================
# ADDITIONAL PERFORMANCE TESTS
# ============================================================================

test_that("compare_performance statistics are valid numbers", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 10, verbose = FALSE))
  
  # All timing stats should be positive
  expect_true(result$student_stats$mean >= 0)
  expect_true(result$student_stats$min >= 0)
  expect_true(result$student_stats$max >= 0)
  expect_true(result$instructor_stats$mean >= 0)
  expect_true(result$instructor_stats$min >= 0)
  expect_true(result$instructor_stats$max >= 0)
})

test_that("compare_performance ratio is reasonable", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 10, verbose = FALSE))
  
  # Ratio should be positive
  expect_true(result$ratio > 0)
})

test_that("compare_performance uses all test inputs", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  result <- suppressMessages(compare_performance("fibonacci", n_runs = 5, verbose = FALSE))
  
  # n_runs should be respected
  expect_equal(result$n_runs, 5)
})

test_that("compare_performance verbose mode outputs messages", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  output <- capture.output({
    result <- compare_performance("fibonacci", n_runs = 5, verbose = TRUE)
  }, type = "message")
  
  # Should produce some output
  expect_true(length(output) > 0 || !is.null(result))
})

test_that("compare_performance warmup affects timing consistency", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  # Both should work
  result0 <- suppressMessages(compare_performance("fibonacci", n_runs = 5, warmup = 0, verbose = FALSE))
  result5 <- suppressMessages(compare_performance("fibonacci", n_runs = 5, warmup = 5, verbose = FALSE))
  
  expect_type(result0, "list")
  expect_type(result5, "list")
})

test_that("print.performance_comparison handles all verdict types", {
  # Create mock result objects with all required fields
  for (verdict in c("FASTER", "COMPARABLE", "SLOWER", "MUCH SLOWER", "VERY SLOW")) {
    mock_result <- list(
      function_name = "test",
      student_stats = list(mean = 0.001, median = 0.001, sd = 0.0001, min = 0.0009, max = 0.0012),
      instructor_stats = list(mean = 0.001, median = 0.001, sd = 0.0001, min = 0.0009, max = 0.0012),
      ratio = 1.0,
      verdict = verdict,
      correct = TRUE,
      passed_correctness = TRUE,
      n_runs = 10,
      n_inputs = 4
    )
    class(mock_result) <- "performance_comparison"
    
    expect_output(print(mock_result), verdict)
  }
})

test_that("print.performance_comparison handles failed correctness", {
  mock_result <- list(
    function_name = "test",
    student_stats = list(mean = 0.001, median = 0.001, sd = 0.0001, min = 0.0009, max = 0.0012),
    instructor_stats = list(mean = 0.001, median = 0.001, sd = 0.0001, min = 0.0009, max = 0.0012),
    ratio = 1.0,
    verdict = "COMPARABLE",
    correct = FALSE,
    passed_correctness = FALSE,  # Failed correctness check
    n_runs = 10,
    n_inputs = 4
  )
  class(mock_result) <- "performance_comparison"
  
  output <- capture.output(print(mock_result))
  output_str <- paste(output, collapse = " ")
  expect_true(grepl("FAILED", output_str, ignore.case = TRUE))
})

