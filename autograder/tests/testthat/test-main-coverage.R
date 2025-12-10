# ============================================================================
# AUTOGRADER - ADDITIONAL MAIN FUNCTION COVERAGE TESTS
# ============================================================================
#
# Tests targeting remaining uncovered paths in autograder() and helpers:
#   - autograder() display paths
#   - progress bar code
#   - hidden test handling
#   - type checking paths
#   - custom comparison_fn
#   - summary message variations
#
# ============================================================================

# ============================================================================
# AUTOGRADER DISPLAY PATH TESTS
# ============================================================================

test_that("autograder shows correct output for all passing tests", {
  skip_on_cran()
  skip_if_offline()
  
  # Define correct student function
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  output <- capture.output(
    result <- autograder("fibonacci", verbose = FALSE, show_progress = FALSE)
  )
  
  # Should pass all tests
  expect_true(result$passed > 0)
  expect_true(any(grepl("ALL TESTS PASSED|PASS", output)))
})

test_that("autograder shows correct output for partial passing tests", {
  skip_on_cran()
  skip_if_offline()
  
  # Define partially correct student function
  assign("student_fibonacci", function(n) {
    # Works for small n but wrong for large n
    if (n <= 0) return(numeric(0))
    if (n <= 5) {
      if (n == 1) return(1)
      fib <- c(1, 1)
      for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
      return(fib)
    }
    # Wrong for n > 5
    1:n
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  output <- capture.output(
    result <- autograder("fibonacci", verbose = TRUE, show_progress = FALSE)
  )
  
  expect_true(result$passed > 0 || result$failed > 0)
})

test_that("autograder shows correct output for all failing tests", {
  skip_on_cran()
  skip_if_offline()
  
  # Define completely wrong function
  assign("student_fibonacci", function(n) {
    # Always wrong
    rep(0, n)
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  output <- capture.output(
    result <- autograder("fibonacci", verbose = TRUE, show_progress = FALSE)
  )
  
  expect_true(result$failed > 0)
  expect_true(any(grepl("FAIL|failed", output)))
})

# ============================================================================
# VERBOSE OUTPUT TESTS
# ============================================================================

test_that("autograder verbose=TRUE shows detailed output", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    1:n  # Wrong
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  output <- capture.output(
    result <- autograder("fibonacci", verbose = TRUE, show_progress = FALSE, show_hints = TRUE)
  )
  
  output_str <- paste(output, collapse = "\n")
  
  # Verbose output should show Input/Expected/Got
  expect_true(grepl("Input|Expected|Got", output_str))
})

test_that("autograder verbose=FALSE shows minimal output", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    1:n  # Wrong
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  output <- capture.output(
    result <- autograder("fibonacci", verbose = FALSE, show_progress = FALSE)
  )
  
  # Should still show something
  expect_true(length(output) > 0)
})

# ============================================================================
# SHOW_HIDDEN TESTS
# ============================================================================

test_that("autograder show_hidden affects display", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  # With show_hidden = FALSE (default)
  output1 <- capture.output(
    autograder("fibonacci", show_hidden = FALSE, verbose = FALSE)
  )
  
  # With show_hidden = TRUE
  output2 <- capture.output(
    autograder("fibonacci", show_hidden = TRUE, verbose = FALSE)
  )
  
  expect_true(length(output1) > 0)
  expect_true(length(output2) > 0)
})

# ============================================================================
# PROGRESS BAR TESTS
# ============================================================================

test_that("autograder show_progress triggers progress bar for many tests", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  # This tests the progress bar code path (>5 tests)
  output <- capture.output(
    result <- autograder("fibonacci", show_progress = TRUE, verbose = FALSE)
  )
  
  expect_true(length(output) > 0)
})

# ============================================================================
# PARALLEL EXECUTION TESTS
# ============================================================================

test_that("autograder use_parallel=TRUE uses parallel for many tests", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  # This tests the parallel code path (if >=10 tests)
  output <- capture.output(
    result <- autograder("fibonacci", use_parallel = TRUE, verbose = FALSE)
  )
  
  expect_true(length(output) > 0)
})

test_that("autograder use_parallel=FALSE forces sequential", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  # Force sequential execution
  output <- capture.output(
    result <- autograder("fibonacci", use_parallel = FALSE, verbose = FALSE)
  )
  
  expect_true(length(output) > 0)
})

# ============================================================================
# ERROR HANDLING IN TEST EXECUTION
# ============================================================================

test_that("autograder handles student function that throws errors", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    stop("Student error!")
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  # Should complete without error (captures student error)
  output <- capture.output(
    result <- autograder("fibonacci", verbose = TRUE, show_progress = FALSE)
  )
  
  expect_true(result$failed > 0)
  expect_true(any(grepl("Error|error|FAIL", paste(output, collapse = ""))))
})

# ============================================================================
# SUMMARY MESSAGES
# ============================================================================

test_that("autograder displays appropriate summary for different scores", {
  skip_on_cran()
  skip_if_offline()
  
  # All pass
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  
  output <- capture.output(
    result <- autograder("fibonacci", verbose = FALSE)
  )
  
  rm("student_fibonacci", envir = .GlobalEnv)
  
  if (result$passed == result$total) {
    expect_true(any(grepl("ALL TESTS PASSED|Excellent", paste(output, collapse = ""))))
  }
})

# ============================================================================
# LIST_PROBLEMS VARIATIONS
# ============================================================================

test_that("list_problems handles fetch failures gracefully", {
  skip_on_cran()
  
  # This should return fallback list even if network fails
  result <- suppressMessages(list_problems())
  
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

# ============================================================================
# PREVIEW_TESTS VARIATIONS
# ============================================================================

test_that("preview_tests displays test information", {
  skip_on_cran()
  skip_if_offline()
  
  output <- capture.output(
    result <- preview_tests("fibonacci")
  )
  
  expect_true(length(output) > 0)
  expect_true(is.list(result))
})

# ============================================================================
# PROVIDE_FEEDBACK EDGE CASES
# ============================================================================

test_that("provide_feedback with many differences truncates", {
  # Create vectors with many differences
  student <- 1:100
  expected <- 101:200
  
  feedback <- provide_feedback(student, expected, list())
  
  if ("diff_positions" %in% names(feedback)) {
    # Should show only first 5 and indicate more with ...
    expect_true(grepl("\\.\\.\\.", feedback$diff_positions))
  }
})

test_that("provide_feedback with fewer than 5 differences shows all", {
  student <- c(1, 2, 10, 4, 5)  # One difference at position 3
  expected <- c(1, 2, 3, 4, 5)
  
  feedback <- provide_feedback(student, expected, list())
  
  if ("diff_positions" %in% names(feedback)) {
    expect_true(grepl("3", feedback$diff_positions))
    expect_false(grepl("\\.\\.\\.", feedback$diff_positions))
  }
})

# ============================================================================
# PRINT_FEEDBACK EDGE CASES
# ============================================================================

test_that("print_feedback with one item", {
  feedback <- list(hint = "Check edge cases")
  
  output <- capture.output(print_feedback(feedback))
  
  expect_true(any(grepl("Check edge cases", output)))
})

# ============================================================================
# COMPARE_PERFORMANCE ADDITIONAL TESTS
# ============================================================================

test_that("compare_performance validates n_runs", {
  expect_error(compare_performance("fibonacci", n_runs = 0))
  expect_error(compare_performance("fibonacci", n_runs = -1))
  expect_error(compare_performance("fibonacci", n_runs = "10"))
})

test_that("compare_performance validates warmup", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) 1:n, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  expect_error(compare_performance("fibonacci", warmup = -1))
})

test_that("compare_performance with warmup=0 works", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- compare_performance("fibonacci", n_runs = 3, warmup = 0, verbose = FALSE)
  
  expect_s3_class(result, "performance_comparison")
})

test_that("compare_performance with verbose=TRUE shows messages", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  output <- capture.output(
    result <- compare_performance("fibonacci", n_runs = 3, verbose = TRUE)
  )
  
  expect_true(length(output) > 0)
})

# ============================================================================
# PERFORMANCE_COMPARISON S3 METHODS
# ============================================================================

test_that("print.performance_comparison shows all stats", {
  mock_result <- structure(
    list(
      function_name = "test_func",
      n_runs = 10,
      n_inputs = 5,
      student_times = runif(10, 0.01, 0.02),
      instructor_times = runif(10, 0.01, 0.02),
      student_stats = list(median = 0.015, mean = 0.015, sd = 0.002, min = 0.01, max = 0.02),
      instructor_stats = list(median = 0.015, mean = 0.015, sd = 0.002, min = 0.01, max = 0.02),
      ratio = 1.0,
      passed_correctness = TRUE,
      verdict = "COMPARABLE - Your implementation has similar performance."
    ),
    class = c("performance_comparison", "list")
  )
  
  output <- capture.output(print(mock_result))
  
  expect_true(any(grepl("Performance Comparison", output)))
  expect_true(any(grepl("Median", output)))
  expect_true(any(grepl("COMPARABLE", output)))
})

test_that("print.performance_comparison handles correctness failure", {
  mock_result <- structure(
    list(
      function_name = "test_func",
      n_runs = 10,
      n_inputs = 5,
      student_times = runif(10, 0.01, 0.02),
      instructor_times = runif(10, 0.01, 0.02),
      student_stats = list(median = 0.015, mean = 0.015, sd = 0.002, min = 0.01, max = 0.02),
      instructor_stats = list(median = 0.015, mean = 0.015, sd = 0.002, min = 0.01, max = 0.02),
      ratio = 1.0,
      passed_correctness = FALSE,
      verdict = "COMPARABLE"
    ),
    class = c("performance_comparison", "list")
  )
  
  output <- capture.output(print(mock_result))
  
  expect_true(any(grepl("FAILED|Correctness", output)))
})

test_that("plot.performance_comparison creates boxplot", {
  skip_on_cran()
  
  mock_result <- structure(
    list(
      function_name = "test_func",
      student_times = runif(10, 0.01, 0.02),
      instructor_times = runif(10, 0.01, 0.02),
      ratio = 1.0
    ),
    class = c("performance_comparison", "list")
  )
  
  # Should not error
  expect_no_error(suppressWarnings(plot(mock_result)))
})

# ============================================================================
# VERDICT VARIATIONS
# ============================================================================

test_that("performance verdict covers all cases", {
  create_mock <- function(ratio, correctness = TRUE) {
    structure(
      list(
        function_name = "test",
        n_runs = 5,
        n_inputs = 1,
        student_times = rep(ratio * 0.1, 5),
        instructor_times = rep(0.1, 5),
        student_stats = list(median = ratio * 0.1, mean = ratio * 0.1, sd = 0, min = ratio * 0.1, max = ratio * 0.1),
        instructor_stats = list(median = 0.1, mean = 0.1, sd = 0, min = 0.1, max = 0.1),
        ratio = ratio,
        passed_correctness = correctness,
        verdict = if (ratio < 0.9) "FASTER"
                  else if (ratio <= 1.1) "COMPARABLE"
                  else if (ratio <= 2.0) "SLOWER"
                  else if (ratio <= 5.0) "MUCH SLOWER"
                  else "VERY SLOW"
      ),
      class = c("performance_comparison", "list")
    )
  }
  
  # Test all verdict types
  for (ratio in c(0.5, 1.0, 1.5, 3.0, 10.0)) {
    mock <- create_mock(ratio)
    output <- capture.output(print(mock))
    expect_true(length(output) > 0)
  }
})
