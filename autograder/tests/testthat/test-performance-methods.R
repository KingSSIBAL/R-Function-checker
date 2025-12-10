# ============================================================================
# AUTOGRADER - PERFORMANCE COMPARISON METHODS TESTS
# ============================================================================
#
# Tests for print.performance_comparison and plot.performance_comparison
#
# ============================================================================

# ============================================================================
# PRINT METHOD TESTS
# ============================================================================

test_that("print.performance_comparison displays all information", {
  # Create a mock performance_comparison object
  result <- list(
    function_name = "test_func",
    n_runs = 10,
    n_inputs = 3,
    student_times = c(0.001, 0.002, 0.001, 0.002, 0.001, 0.002, 0.001, 0.002, 0.001, 0.002),
    instructor_times = c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001),
    student_stats = list(
      median = 0.0015,
      mean = 0.0015,
      sd = 0.0005,
      min = 0.001,
      max = 0.002
    ),
    instructor_stats = list(
      median = 0.001,
      mean = 0.001,
      sd = 0.0,
      min = 0.001,
      max = 0.001
    ),
    ratio = 1.5,
    passed_correctness = TRUE,
    verdict = "SLOWER - Your implementation is somewhat slower."
  )
  class(result) <- c("performance_comparison", "list")
  
  # Capture output
  output <- capture.output(print(result))
  
  # Check for key elements
  expect_true(any(grepl("Performance Comparison", output)))
  expect_true(any(grepl("test_func", output)))
  expect_true(any(grepl("Runs:", output)))
  expect_true(any(grepl("Correctness: PASSED", output)))
  expect_true(any(grepl("Timing", output)))
  expect_true(any(grepl("Median", output)))
  expect_true(any(grepl("Mean", output)))
  expect_true(any(grepl("Ratio", output)))
  expect_true(any(grepl("SLOWER", output)))
})

test_that("print.performance_comparison shows failure status", {
  result <- list(
    function_name = "failing_func",
    n_runs = 5,
    n_inputs = 2,
    student_times = c(0.01, 0.01, 0.01, 0.01, 0.01),
    instructor_times = c(0.001, 0.001, 0.001, 0.001, 0.001),
    student_stats = list(median = 0.01, mean = 0.01, sd = 0.0, min = 0.01, max = 0.01),
    instructor_stats = list(median = 0.001, mean = 0.001, sd = 0.0, min = 0.001, max = 0.001),
    ratio = 10,
    passed_correctness = FALSE,
    verdict = "VERY SLOW - Your implementation needs significant optimization."
  )
  class(result) <- c("performance_comparison", "list")
  
  output <- capture.output(print(result))
  
  # Check for failure indication
  expect_true(any(grepl("FAILED", output)))
  expect_true(any(grepl("VERY SLOW", output)))
})

test_that("print.performance_comparison returns invisibly", {
  result <- list(
    function_name = "test",
    n_runs = 1,
    n_inputs = 1,
    student_times = 0.001,
    instructor_times = 0.001,
    student_stats = list(median = 0.001, mean = 0.001, sd = NA, min = 0.001, max = 0.001),
    instructor_stats = list(median = 0.001, mean = 0.001, sd = NA, min = 0.001, max = 0.001),
    ratio = 1.0,
    passed_correctness = TRUE,
    verdict = "COMPARABLE"
  )
  class(result) <- c("performance_comparison", "list")
  
  # Should return invisibly
  invisible_result <- print(result)
  expect_identical(invisible_result, result)
})

test_that("print.performance_comparison handles different verdicts", {
  # Test each verdict type
  verdicts <- c(
    "FASTER - Your implementation is faster than the reference!",
    "COMPARABLE - Your implementation has similar performance.",
    "SLOWER - Your implementation is somewhat slower.",
    "MUCH SLOWER - Consider optimizing your implementation.",
    "VERY SLOW - Your implementation needs significant optimization."
  )
  
  for (verdict in verdicts) {
    result <- list(
      function_name = "test",
      n_runs = 1,
      n_inputs = 1,
      student_times = 0.001,
      instructor_times = 0.001,
      student_stats = list(median = 0.001, mean = 0.001, sd = NA, min = 0.001, max = 0.001),
      instructor_stats = list(median = 0.001, mean = 0.001, sd = NA, min = 0.001, max = 0.001),
      ratio = 1.0,
      passed_correctness = TRUE,
      verdict = verdict
    )
    class(result) <- c("performance_comparison", "list")
    
    output <- capture.output(print(result))
    expect_true(any(grepl(strsplit(verdict, " - ")[[1]][1], output)))
  }
})

# ============================================================================
# PLOT METHOD TESTS
# ============================================================================

test_that("plot.performance_comparison creates a plot", {
  result <- list(
    function_name = "test_func",
    n_runs = 10,
    n_inputs = 3,
    student_times = c(0.001, 0.002, 0.001, 0.002, 0.001, 0.002, 0.001, 0.002, 0.001, 0.002),
    instructor_times = c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001),
    student_stats = list(median = 0.0015, mean = 0.0015, sd = 0.0005, min = 0.001, max = 0.002),
    instructor_stats = list(median = 0.001, mean = 0.001, sd = 0.0, min = 0.001, max = 0.001),
    ratio = 1.5,
    passed_correctness = TRUE,
    verdict = "SLOWER"
  )
  class(result) <- c("performance_comparison", "list")
  
  # Plot should run without error
  expect_silent(plot(result))
})

test_that("plot.performance_comparison returns invisibly", {
  result <- list(
    function_name = "test",
    n_runs = 5,
    n_inputs = 1,
    student_times = c(0.001, 0.002, 0.003, 0.002, 0.001),
    instructor_times = c(0.001, 0.001, 0.001, 0.001, 0.001),
    student_stats = list(median = 0.002, mean = 0.0018, sd = 0.0008, min = 0.001, max = 0.003),
    instructor_stats = list(median = 0.001, mean = 0.001, sd = 0.0, min = 0.001, max = 0.001),
    ratio = 2.0,
    passed_correctness = TRUE,
    verdict = "MUCH SLOWER"
  )
  class(result) <- c("performance_comparison", "list")
  
  invisible_result <- plot(result)
  expect_identical(invisible_result, result)
})

test_that("plot.performance_comparison handles single run", {
  result <- list(
    function_name = "single_run",
    n_runs = 1,
    n_inputs = 1,
    student_times = 0.005,
    instructor_times = 0.001,
    student_stats = list(median = 0.005, mean = 0.005, sd = NA, min = 0.005, max = 0.005),
    instructor_stats = list(median = 0.001, mean = 0.001, sd = NA, min = 0.001, max = 0.001),
    ratio = 5.0,
    passed_correctness = TRUE,
    verdict = "MUCH SLOWER"
  )
  class(result) <- c("performance_comparison", "list")
  
  # Should not error
  expect_silent(plot(result))
})

test_that("plot.performance_comparison accepts additional arguments", {
  result <- list(
    function_name = "test",
    n_runs = 10,
    n_inputs = 2,
    student_times = rnorm(10, 0.002, 0.0005),
    instructor_times = rnorm(10, 0.001, 0.0002),
    student_stats = list(median = 0.002, mean = 0.002, sd = 0.0005, min = 0.001, max = 0.003),
    instructor_stats = list(median = 0.001, mean = 0.001, sd = 0.0002, min = 0.0008, max = 0.0012),
    ratio = 2.0,
    passed_correctness = TRUE,
    verdict = "MUCH SLOWER"
  )
  class(result) <- c("performance_comparison", "list")
  
  # Test with additional boxplot arguments (suppress warnings since notch may warn)
  expect_no_error(suppressWarnings(plot(result, notch = TRUE)))
  expect_silent(plot(result, horizontal = TRUE))
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("print handles extreme ratios", {
  # Very fast student
  result <- list(
    function_name = "fast_func",
    n_runs = 5,
    n_inputs = 1,
    student_times = rep(0.0001, 5),
    instructor_times = rep(0.01, 5),
    student_stats = list(median = 0.0001, mean = 0.0001, sd = 0.0, min = 0.0001, max = 0.0001),
    instructor_stats = list(median = 0.01, mean = 0.01, sd = 0.0, min = 0.01, max = 0.01),
    ratio = 0.01,
    passed_correctness = TRUE,
    verdict = "FASTER - Your implementation is faster than the reference!"
  )
  class(result) <- c("performance_comparison", "list")
  
  output <- capture.output(print(result))
  expect_true(any(grepl("FASTER", output)))
})

test_that("print handles very slow implementations", {
  result <- list(
    function_name = "slow_func",
    n_runs = 5,
    n_inputs = 1,
    student_times = rep(10.0, 5),
    instructor_times = rep(0.001, 5),
    student_stats = list(median = 10.0, mean = 10.0, sd = 0.0, min = 10.0, max = 10.0),
    instructor_stats = list(median = 0.001, mean = 0.001, sd = 0.0, min = 0.001, max = 0.001),
    ratio = 10000,
    passed_correctness = TRUE,
    verdict = "VERY SLOW - Your implementation needs significant optimization."
  )
  class(result) <- c("performance_comparison", "list")
  
  output <- capture.output(print(result))
  expect_true(any(grepl("VERY SLOW", output)))
  expect_true(any(grepl("10000", output)))
})

test_that("plot handles identical times", {
  result <- list(
    function_name = "identical",
    n_runs = 5,
    n_inputs = 1,
    student_times = rep(0.001, 5),
    instructor_times = rep(0.001, 5),
    student_stats = list(median = 0.001, mean = 0.001, sd = 0.0, min = 0.001, max = 0.001),
    instructor_stats = list(median = 0.001, mean = 0.001, sd = 0.0, min = 0.001, max = 0.001),
    ratio = 1.0,
    passed_correctness = TRUE,
    verdict = "COMPARABLE"
  )
  class(result) <- c("performance_comparison", "list")
  
  expect_silent(plot(result))
})
