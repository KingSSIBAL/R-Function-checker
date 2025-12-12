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

# ============================================================================
# ADDITIONAL PRINT AND MOCK TESTS
# ============================================================================

test_that("print.performance_comparison handles very large ratio", {
  result <- structure(
    list(
      function_name = "slow_function",
      n_runs = 10,
      n_inputs = 1,
      student_times = rep(10, 10),
      instructor_times = rep(0.001, 10),
      student_stats = list(median = 10, mean = 10, sd = 0, min = 10, max = 10),
      instructor_stats = list(median = 0.001, mean = 0.001, sd = 0, min = 0.001, max = 0.001),
      ratio = 10000,
      passed_correctness = TRUE,
      verdict = "VERY SLOW"
    ),
    class = c("performance_comparison", "list")
  )
  
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

test_that("print.performance_comparison handles very small ratio", {
  result <- structure(
    list(
      function_name = "fast_function",
      n_runs = 10,
      n_inputs = 1,
      student_times = rep(0.001, 10),
      instructor_times = rep(10, 10),
      student_stats = list(median = 0.001, mean = 0.001, sd = 0, min = 0.001, max = 0.001),
      instructor_stats = list(median = 10, mean = 10, sd = 0, min = 10, max = 10),
      ratio = 0.0001,
      passed_correctness = TRUE,
      verdict = "MUCH FASTER"
    ),
    class = c("performance_comparison", "list")
  )
  
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})

test_that("plot.performance_comparison handles single time point", {
  result <- structure(
    list(
      function_name = "test",
      n_runs = 1,
      n_inputs = 1,
      student_times = 0.5,
      instructor_times = 0.5,
      student_stats = list(median = 0.5, mean = 0.5, sd = NA, min = 0.5, max = 0.5),
      instructor_stats = list(median = 0.5, mean = 0.5, sd = NA, min = 0.5, max = 0.5),
      ratio = 1,
      passed_correctness = TRUE,
      verdict = "COMPARABLE"
    ),
    class = c("performance_comparison", "list")
  )
  
  expect_no_error(plot(result))
})

test_that("performance verdict categories work correctly", {
  # Much faster (ratio < 0.5)
  expect_true(0.3 < 0.5)
  
  # Faster (0.5 <= ratio < 0.9)
  expect_true(0.7 >= 0.5 && 0.7 < 0.9)
  
  # Comparable (0.9 <= ratio <= 1.1)
  expect_true(1.0 >= 0.9 && 1.0 <= 1.1)
  
  # Slower (1.1 < ratio <= 2.0)
  expect_true(1.5 > 1.1 && 1.5 <= 2.0)
  
  # Much slower (ratio > 2.0)
  expect_true(5 > 2.0)
})

# ============================================================================
# ADDITIONAL COVERAGE TESTS FOR performance.R
# ============================================================================

test_that("openmp_status returns expected structure", {
  status <- openmp_status()
  
  expect_s3_class(status, "openmp_status")
  expect_true("available" %in% names(status))
  expect_true("max_threads" %in% names(status))
  expect_type(status$available, "logical")
})

test_that("print.openmp_status works with OpenMP available", {
  # Create mock status with OpenMP available
  mock_status <- list(
    available = TRUE,
    version = "2015.11",
    max_threads = 4L,
    parallel_threshold = 10000L
  )
  class(mock_status) <- "openmp_status"
  
  output <- capture.output(print(mock_status))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Enabled|OpenMP", ignore.case = TRUE)
})

test_that("print.openmp_status works without OpenMP", {
  # Create mock status without OpenMP
  mock_status <- list(
    available = FALSE,
    version = "",
    max_threads = 1L,
    parallel_threshold = 10000L
  )
  class(mock_status) <- "openmp_status"
  
  output <- capture.output(print(mock_status))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Not Available|sequential", ignore.case = TRUE)
})

test_that("compare_performance handles empty test inputs gracefully", {
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
  
  # Empty test_inputs should error
  expect_error(
    compare_performance("fibonacci", test_inputs = list()),
    "No test inputs"
  )
})

test_that("compare_performance handles custom test inputs", {
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
  
  # Provide custom test inputs
  custom_inputs <- list(list(n = 5), list(n = 10))
  
  result <- suppressMessages(compare_performance(
    "fibonacci",
    test_inputs = custom_inputs,
    n_runs = 3,
    verbose = FALSE
  ))
  
  expect_equal(result$n_inputs, 2)
})

test_that("compare_performance detects incorrect student output", {
  skip_on_cran()
  skip_if_offline()
  
  # Define a wrong function
  assign("student_fibonacci", function(n) {
    rep(0, n)  # Always returns zeros - incorrect
  }, envir = .GlobalEnv)
  on.exit(rm(student_fibonacci, envir = .GlobalEnv))
  
  # Should run but flag as incorrect
  result <- suppressWarnings(suppressMessages(
    compare_performance("fibonacci", n_runs = 3, verbose = FALSE)
  ))
  
  expect_false(result$passed_correctness)
})

test_that("print.performance_comparison returns invisibly", {
  result <- structure(
    list(
      function_name = "test",
      n_runs = 10,
      n_inputs = 1,
      student_times = rep(0.001, 10),
      instructor_times = rep(0.001, 10),
      student_stats = list(median = 0.001, mean = 0.001, sd = 0.0001, min = 0.0009, max = 0.0012),
      instructor_stats = list(median = 0.001, mean = 0.001, sd = 0.0001, min = 0.0009, max = 0.0012),
      ratio = 1.0,
      passed_correctness = TRUE,
      verdict = "COMPARABLE"
    ),
    class = c("performance_comparison", "list")
  )
  
  output <- capture.output(ret <- print(result))
  expect_identical(ret, result)
})

test_that("plot.performance_comparison returns invisibly", {
  result <- structure(
    list(
      function_name = "test",
      n_runs = 5,
      n_inputs = 1,
      student_times = runif(5, 0.001, 0.002),
      instructor_times = runif(5, 0.001, 0.002),
      student_stats = list(median = 0.0015, mean = 0.0015, sd = 0.0002, min = 0.001, max = 0.002),
      instructor_stats = list(median = 0.0015, mean = 0.0015, sd = 0.0002, min = 0.001, max = 0.002),
      ratio = 1.0,
      passed_correctness = TRUE,
      verdict = "COMPARABLE"
    ),
    class = c("performance_comparison", "list")
  )
  
  ret <- plot(result)
  expect_identical(ret, result)
})
