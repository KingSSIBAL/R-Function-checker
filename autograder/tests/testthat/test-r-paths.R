# ============================================================================
# AUTOGRADER - R CODE PATH COVERAGE TESTS
# ============================================================================
#
# These tests focus on improving R code coverage by:
#   1. Testing pure R functions that don't require network
#   2. Using mocked environments to test functions that normally need network
#   3. Exercising all code branches in R files
#
# Target files:
#   - api.R: preview_tests, list_problems
#   - autograder-main.R: autograder input validation
#   - performance.R: compare_performance, print/plot methods
#   - utils.R: format_output edge cases
#   - feedback.R: provide_feedback, print_feedback
#
# ============================================================================

# ============================================================================
# PREVIEW_TESTS VALIDATION TESTS (No network required)
# ============================================================================

test_that("preview_tests validates missing function_name", {
  expect_error(preview_tests(), "single character")
})

test_that("preview_tests validates non-character function_name", {
  expect_error(preview_tests(123), "single character")
  expect_error(preview_tests(TRUE), "single character")
  expect_error(preview_tests(list()), "single character")
})

test_that("preview_tests validates multi-element function_name", {
  expect_error(preview_tests(c("a", "b")), "single character")
})

# ============================================================================
# LIST_PROBLEMS FALLBACK TESTS
# ============================================================================

test_that("list_problems fallback works when fetch fails", {
  # Mock the C++ function to simulate failure
  local_mocked_bindings(
    .cpp_fetch_problems_list = function() stop("Network error"),
    .package = "autograder"
  )
  
  output <- capture.output(result <- list_problems())
  
  # Should fall back to default list
  expect_true(any(grepl("fibonacci", output)))
  expect_true(any(grepl("factorial", output)))
  expect_true(any(grepl("sum_vector", output)))
})

test_that("list_problems fallback works when code is empty", {
  local_mocked_bindings(
    .cpp_fetch_problems_list = function() "",
    .package = "autograder"
  )
  
  output <- capture.output(result <- list_problems())
  
  # Should fall back to default list
  expect_true("fibonacci" %in% result)
  expect_true("factorial" %in% result)
  expect_true("sum_vector" %in% result)
})

test_that("list_problems displays usage instructions", {
  local_mocked_bindings(
    .cpp_fetch_problems_list = function() "problems <- c('test1', 'test2')",
    .package = "autograder"
  )
  
  output <- capture.output(result <- list_problems())
  output_str <- paste(output, collapse = "\n")
  
  expect_true(grepl("Usage:", output_str))
  expect_true(grepl("student_", output_str))
  expect_true(grepl("autograder", output_str))
  expect_true(grepl("preview_tests", output_str))
})

# ============================================================================
# AUTOGRADER INPUT VALIDATION TESTS (No network required)
# ============================================================================

test_that("autograder validates missing function_name", {
  expect_error(autograder(), "Missing required argument")
})

test_that("autograder validates non-character function_name", {
  expect_error(autograder(123), "single character string")
  expect_error(autograder(TRUE), "single character string")
  expect_error(autograder(NULL), "single character string")
})

test_that("autograder validates multi-element function_name", {
  expect_error(autograder(c("a", "b")), "single character string")
})

test_that("autograder validates logical parameters", {
  # These should fail fast with validation error before network check
  expect_error(autograder("test", verbose = "yes"), "TRUE or FALSE")
  expect_error(autograder("test", show_hidden = "no"), "TRUE or FALSE")
  expect_error(autograder("test", show_progress = 1), "TRUE or FALSE")
  expect_error(autograder("test", use_parallel = 0), "TRUE or FALSE")
  expect_error(autograder("test", show_hints = "maybe"), "TRUE or FALSE")
})

test_that("autograder validates vector logical parameters", {
  expect_error(autograder("test", verbose = c(TRUE, FALSE)), "TRUE or FALSE")
})

# ============================================================================
# COMPARE_PERFORMANCE INPUT VALIDATION TESTS (No network required)
# ============================================================================

test_that("compare_performance validates all input types", {
  # function_name validation
  expect_error(compare_performance(NULL), "character")
  expect_error(compare_performance(NA), "character")
  expect_error(compare_performance(list()), "character")
  
  # n_runs validation - need student function defined first
  assign("student_test", function(x) x, envir = .GlobalEnv)
  on.exit(rm("student_test", envir = .GlobalEnv), add = TRUE)
  
  expect_error(compare_performance("test", n_runs = NULL), "positive integer")
  expect_error(compare_performance("test", n_runs = NA), "positive integer")
  expect_error(compare_performance("test", n_runs = "ten"), "positive integer")
  
  # warmup validation
  expect_error(compare_performance("test", warmup = NULL), "non-negative integer")
  expect_error(compare_performance("test", warmup = NA), "non-negative integer")
  
  # verbose validation - NULL and vector checked, NA passes is.logical check but fails in if()
  expect_error(compare_performance("test", verbose = NULL), "TRUE or FALSE")
  expect_error(compare_performance("test", verbose = c(TRUE, FALSE)), "TRUE or FALSE")
  expect_error(compare_performance("test", verbose = "yes"), "TRUE or FALSE")
})

# ============================================================================
# PRINT.PERFORMANCE_COMPARISON ALL PATHS
# ============================================================================

test_that("print.performance_comparison handles passed correctness", {
  # Create mock performance object with passed correctness
  mock_result <- list(
    function_name = "test_func",
    n_runs = 10,
    n_inputs = 3,
    student_times = runif(10, 0.001, 0.01),
    instructor_times = runif(10, 0.001, 0.01),
    student_stats = list(median = 0.005, mean = 0.005, sd = 0.001, min = 0.001, max = 0.01),
    instructor_stats = list(median = 0.005, mean = 0.005, sd = 0.001, min = 0.001, max = 0.01),
    ratio = 1.0,
    passed_correctness = TRUE,
    verdict = "COMPARABLE - Your implementation has similar performance."
  )
  class(mock_result) <- c("performance_comparison", "list")
  
  output <- capture.output(print(mock_result))
  output_str <- paste(output, collapse = "\n")
  
  expect_true(grepl("Performance Comparison", output_str))
  expect_true(grepl("test_func", output_str))
  expect_true(grepl("Correctness: PASSED", output_str))
  expect_true(grepl("Median", output_str))
  expect_true(grepl("Ratio", output_str))
  expect_true(grepl("COMPARABLE", output_str))
})

test_that("print.performance_comparison handles failed correctness", {
  mock_result <- list(
    function_name = "test_func",
    n_runs = 10,
    n_inputs = 3,
    student_times = runif(10, 0.001, 0.01),
    instructor_times = runif(10, 0.001, 0.01),
    student_stats = list(median = 0.005, mean = 0.005, sd = 0.001, min = 0.001, max = 0.01),
    instructor_stats = list(median = 0.005, mean = 0.005, sd = 0.001, min = 0.001, max = 0.01),
    ratio = 1.0,
    passed_correctness = FALSE,
    verdict = "COMPARABLE"
  )
  class(mock_result) <- c("performance_comparison", "list")
  
  output <- capture.output(print(mock_result))
  output_str <- paste(output, collapse = "\n")
  
  expect_true(grepl("FAILED", output_str))
  expect_true(grepl("not be meaningful", output_str))
})

test_that("print.performance_comparison shows all verdict types", {
  base_result <- list(
    function_name = "test",
    n_runs = 10,
    n_inputs = 1,
    student_times = runif(10),
    instructor_times = runif(10),
    student_stats = list(median = 0.005, mean = 0.005, sd = 0.001, min = 0.001, max = 0.01),
    instructor_stats = list(median = 0.005, mean = 0.005, sd = 0.001, min = 0.001, max = 0.01),
    ratio = 1.0,
    passed_correctness = TRUE,
    verdict = ""
  )
  class(base_result) <- c("performance_comparison", "list")
  
  # Test each verdict type
  verdicts <- c(
    "FASTER - Your implementation is faster than the reference!",
    "COMPARABLE - Your implementation has similar performance.",
    "SLOWER - Your implementation is somewhat slower.",
    "MUCH SLOWER - Consider optimizing your implementation.",
    "VERY SLOW - Your implementation needs significant optimization."
  )
  
  for (v in verdicts) {
    base_result$verdict <- v
    output <- capture.output(print(base_result))
    expect_true(any(grepl(substr(v, 1, 10), output)))
  }
})

# ============================================================================
# PLOT.PERFORMANCE_COMPARISON TESTS
# ============================================================================

test_that("plot.performance_comparison creates valid plot", {
  mock_result <- list(
    function_name = "test_func",
    n_runs = 10,
    n_inputs = 3,
    student_times = runif(10, 0.001, 0.01),
    instructor_times = runif(10, 0.001, 0.01),
    student_stats = list(median = 0.005, mean = 0.005, sd = 0.001, min = 0.001, max = 0.01),
    instructor_stats = list(median = 0.005, mean = 0.005, sd = 0.001, min = 0.001, max = 0.01),
    ratio = 1.5,
    passed_correctness = TRUE,
    verdict = "SLOWER"
  )
  class(mock_result) <- c("performance_comparison", "list")
  
  # Should not error
  expect_no_error(plot(mock_result))
  
  # Returns invisibly
  ret <- plot(mock_result)
  expect_identical(ret, mock_result)
})

test_that("plot.performance_comparison passes extra args", {
  mock_result <- list(
    function_name = "test",
    n_runs = 5,
    n_inputs = 1,
    student_times = c(0.001, 0.002, 0.003, 0.004, 0.005),
    instructor_times = c(0.001, 0.002, 0.003, 0.004, 0.005),
    student_stats = list(median = 0.003, mean = 0.003, sd = 0.001, min = 0.001, max = 0.005),
    instructor_stats = list(median = 0.003, mean = 0.003, sd = 0.001, min = 0.001, max = 0.005),
    ratio = 1.0,
    passed_correctness = TRUE,
    verdict = "COMPARABLE"
  )
  class(mock_result) <- c("performance_comparison", "list")
  
  # Extra arguments shouldn't cause error
  expect_no_error(plot(mock_result, notch = FALSE))
})

# ============================================================================
# FORMAT_OUTPUT EDGE CASES
# ============================================================================

test_that("format_output handles deeply nested lists", {
  nested <- list(a = list(b = list(c = list(d = 1))))
  result <- format_output(nested)
  expect_true(nchar(result) > 0)
})

test_that("format_output handles empty structures", {
  expect_equal(format_output(NULL), "NULL")
  expect_equal(format_output(list()), "list()")
  expect_equal(format_output(numeric(0)), "numeric(0)")
  expect_equal(format_output(character(0)), "character(0)")
  expect_equal(format_output(logical(0)), "logical(0)")
})

test_that("format_output handles special numeric values", {
  result <- format_output(c(NA, NaN, Inf, -Inf))
  expect_true(grepl("NA|NaN|Inf", result))
})

test_that("format_output handles complex numbers", {
  result <- format_output(complex(real = 1, imaginary = 2))
  expect_true(nchar(result) > 0)
})

test_that("format_output handles expressions", {
  result <- format_output(expression(x + y))
  expect_true(nchar(result) > 0)
})

test_that("format_output handles functions", {
  result <- format_output(function(x) x + 1)
  expect_true(nchar(result) > 0)
})

test_that("format_output handles environments", {
  env <- new.env()
  result <- format_output(env)
  expect_true(nchar(result) > 0)
})

test_that("format_output handles named vectors", {
  result <- format_output(c(a = 1, b = 2, c = 3))
  expect_true(nchar(result) > 0)
})

test_that("format_output handles very long strings", {
  long_str <- paste(rep("a", 1000), collapse = "")
  result <- format_output(long_str, max_length = 50)
  expect_lte(nchar(result), 55)  # Allow for truncation marker
})

# ============================================================================
# PROVIDE_FEEDBACK ADDITIONAL PATHS
# ============================================================================

test_that("provide_feedback handles matrix comparison", {
  student <- matrix(1:4, 2, 2)
  expected <- matrix(5:8, 2, 2)
  fb <- provide_feedback(student, expected, list())
  expect_true(is.list(fb))
})

test_that("provide_feedback handles list comparison", {
  student <- list(a = 1, b = 2)
  expected <- list(a = 1, b = 3)
  fb <- provide_feedback(student, expected, list())
  expect_true(is.list(fb))
})

test_that("provide_feedback handles character vector comparison", {
  student <- c("a", "b", "c")
  expected <- c("a", "x", "c")
  fb <- provide_feedback(student, expected, list())
  expect_true(is.list(fb))
})

test_that("provide_feedback handles single value comparison", {
  fb <- provide_feedback(1, 2, list())
  expect_true(is.list(fb))
  
  fb <- provide_feedback("a", "b", list())
  expect_true(is.list(fb))
})

test_that("provide_feedback handles all NAs", {
  fb <- provide_feedback(c(NA, NA), c(NA, NA), list())
  expect_true(is.list(fb))
})

# ============================================================================
# PRINT_FEEDBACK TESTS
# ============================================================================

test_that("print_feedback handles empty feedback", {
  # Empty feedback returns early, produces no output
  # Use invisible return check instead
  result <- print_feedback(list())
  expect_null(result)
})

test_that("print_feedback handles feedback with all fields", {
  fb <- list(
    type_issue = "Expected numeric, got character",
    length_issue = "Expected length 5, got 3",
    diff_positions = "Positions 1, 2, 3",
    hint = "Check your algorithm"
  )
  
  output <- capture.output(print_feedback(fb))
  output_str <- paste(output, collapse = "\n")
  
  expect_true(grepl("Feedback", output_str))
})

test_that("print_feedback handles partial feedback", {
  fb <- list(hint = "Just a hint")
  output <- capture.output(print_feedback(fb))
  expect_true(length(output) > 0)
})

# ============================================================================
# ERROR CLASS TESTS
# ============================================================================

test_that("network_error creates correct error class", {
  err <- network_error("Test network error")
  expect_true(inherits(err, "network_error"))
  expect_true(inherits(err, "error"))
  expect_true(inherits(err, "condition"))
})

test_that("function_not_found_error creates correct error class", {
  err <- function_not_found_error("test_func")
  expect_true(inherits(err, "function_not_found_error"))
  expect_true(inherits(err, "error"))
})

test_that("test_execution_error creates correct error class", {
  err <- test_execution_error("Test failed", test_num = 5)
  expect_true(inherits(err, "test_execution_error"))
  expect_true(inherits(err, "error"))
})

# ============================================================================
# NULL COALESCING OPERATOR TESTS
# ============================================================================

test_that("%||% works correctly", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(3 %||% 5, 3)
  expect_equal("" %||% "default", "")
  expect_equal(NA %||% "default", NA)
  expect_equal(list() %||% list(a = 1), list())
})

# ============================================================================
# RUN_TESTS_SEQUENTIAL TESTS
# ============================================================================

test_that("run_tests_sequential handles normal execution", {
  student_fn <- function(x) x * 2
  instructor_fn <- function(x) x * 2
  test_data <- list(inputs = list(list(1), list(2), list(3)))
  
  results <- run_tests_sequential(student_fn, instructor_fn, test_data, 1e-10)
  
  expect_equal(length(results), 3)
  expect_equal(results[[1]]$index, 1)
  expect_equal(results[[2]]$index, 2)
  expect_equal(results[[3]]$index, 3)
})

test_that("run_tests_sequential handles student errors", {
  student_fn <- function(x) stop("Student error")
  instructor_fn <- function(x) x * 2
  test_data <- list(inputs = list(list(1)))
  
  results <- run_tests_sequential(student_fn, instructor_fn, test_data, 1e-10)
  
  expect_true(inherits(results[[1]]$student, "error"))
  expect_false(inherits(results[[1]]$expected, "error"))
})

test_that("run_tests_sequential handles instructor errors", {
  student_fn <- function(x) x * 2
  instructor_fn <- function(x) stop("Instructor error")
  test_data <- list(inputs = list(list(1)))
  
  results <- run_tests_sequential(student_fn, instructor_fn, test_data, 1e-10)
  
  expect_false(inherits(results[[1]]$student, "error"))
  expect_true(inherits(results[[1]]$expected, "error"))
})

# ============================================================================
# RUN_TESTS_PARALLEL TESTS
# ============================================================================

test_that("run_tests_parallel falls back to sequential for small test sets", {
  student_fn <- function(x) x * 2
  instructor_fn <- function(x) x * 2
  test_data <- list(inputs = list(list(1), list(2)))
  
  # With < 10 tests, should fall back to sequential
  results <- run_tests_parallel(student_fn, instructor_fn, test_data, 1e-10, TRUE)
  
  expect_equal(length(results), 2)
})

test_that("run_tests_parallel respects use_parallel=FALSE", {
  student_fn <- function(x) x * 2
  instructor_fn <- function(x) x * 2
  test_data <- list(inputs = as.list(1:15))  # More than 10 tests
  
  # With use_parallel=FALSE, should use sequential
  results <- run_tests_parallel(student_fn, instructor_fn, test_data, 1e-10, FALSE)
  
  expect_equal(length(results), 15)
})

# ============================================================================
# VALIDATE_TEST_CASES EDGE CASES
# ============================================================================

test_that("validate_test_cases handles minimal valid input", {
  test_data <- list(
    inputs = list(list(1)),
    descriptions = "Test 1",
    points = 1,
    hidden = FALSE
  )
  
  result <- validate_test_cases(test_data, "test")
  expect_true(is.list(result))
  expect_equal(length(result$inputs), 1)
})

test_that("validate_test_cases fills missing optional fields", {
  test_data <- list(
    inputs = list(list(1), list(2)),
    descriptions = c("Test 1", "Test 2")
    # Missing: points, hidden, tolerance, hints
  )
  
  result <- validate_test_cases(test_data, "test")
  
  expect_equal(length(result$points), 2)
  expect_equal(length(result$hidden), 2)
  expect_true(!is.null(result$tolerance))
})

test_that("validate_test_cases preserves optional fields when present", {
  test_data <- list(
    inputs = list(list(1)),
    descriptions = "Test 1",
    points = 5,
    hidden = TRUE,
    tolerance = 1e-5,
    hints = "Custom hint",
    expected_type = "numeric"
  )
  
  result <- validate_test_cases(test_data, "test")
  
  expect_equal(result$points[1], 5)
  expect_true(result$hidden[1])
  expect_equal(result$tolerance, 1e-5)
  expect_equal(result$hints[1], "Custom hint")
  expect_equal(result$expected_type, "numeric")
})

# ============================================================================
# .ONATTACH TEST
# ============================================================================

test_that(".onAttach produces startup message", {
  output <- capture.output(
    .onAttach("", "autograder"),
    type = "message"
  )
  output_str <- paste(output, collapse = "\n")
  
  expect_true(grepl("Autograder", output_str))
  expect_true(grepl("loaded", output_str))
})
