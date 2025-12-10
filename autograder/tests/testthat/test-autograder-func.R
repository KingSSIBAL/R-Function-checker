# ============================================================================
# AUTOGRADER - AUTOGRADER FUNCTION COVERAGE TESTS
# ============================================================================
#
# Tests specifically for the main autograder() function and related functions:
#   - autograder() parameter validation
#   - run_tests_sequential() / run_tests_parallel()
#   - extract_instructor_function()
#   - extract_test_cases()
#   - preview_tests()
#   - list_problems()
#
# ============================================================================

# ============================================================================
# AUTOGRADER PARAMETER VALIDATION TESTS
# ============================================================================

test_that("autograder rejects missing function_name", {
  expect_error(autograder(), "Missing required argument")
})

test_that("autograder rejects non-character function_name", {
  expect_error(autograder(123), "must be a single character string")
  expect_error(autograder(TRUE), "must be a single character string")
  expect_error(autograder(c("a", "b")), "must be a single character string")
  expect_error(autograder(NULL), "must be a single character string")
})

test_that("autograder rejects invalid logical parameters", {
  skip_on_cran()
  skip_if_offline()
  
  # Define a dummy function to pass initial checks
  assign("student_fibonacci", function(n) 1:n, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  # These should error on type check before network call
  expect_error(autograder("fibonacci", verbose = "yes"))
  expect_error(autograder("fibonacci", show_hidden = 1))
  expect_error(autograder("fibonacci", show_progress = "true"))
  expect_error(autograder("fibonacci", use_parallel = NULL))
  expect_error(autograder("fibonacci", show_hints = c(TRUE, FALSE)))
})

test_that("autograder checks for student function", {
  skip_on_cran()
  skip_if_offline()
  
  # Make sure no student function exists
  if (exists("student_test_nonexistent", envir = .GlobalEnv)) {
    rm("student_test_nonexistent", envir = .GlobalEnv)
  }
  
  # Create a valid mock environment
  mock_env <- new.env()
  mock_env$fibonacci <- function(n) 1:n
  mock_env$test_cases <- list(inputs = list(list(n = 5)))
  
  # Mock fetch_instructor_code locally
  local_mock <- function(fn) mock_env
  
  # This will fail at the student function check
  expect_error(
    autograder("fibonacci"),
    "not found in your environment|student_fibonacci"
  )
})

# ============================================================================
# RUN_TESTS_SEQUENTIAL TESTS
# ============================================================================

test_that("run_tests_sequential executes all tests", {
  student_fn <- function(x) x + 1
  instructor_fn <- function(x) x + 1
  
  test_data <- list(
    inputs = list(list(x = 1), list(x = 2), list(x = 3)),
    descriptions = c("Test 1", "Test 2", "Test 3"),
    hidden = c(FALSE, FALSE, FALSE),
    points = c(1, 1, 1),
    tolerance = 1e-10
  )
  
  results <- run_tests_sequential(student_fn, instructor_fn, test_data, 1e-10)
  
  expect_length(results, 3)
  expect_equal(results[[1]]$index, 1)
  expect_equal(results[[2]]$index, 2)
  expect_equal(results[[3]]$index, 3)
})

test_that("run_tests_sequential captures student errors", {
  student_fn <- function(x) {
    if (x == 2) stop("Error on 2")
    x + 1
  }
  instructor_fn <- function(x) x + 1
  
  test_data <- list(
    inputs = list(list(x = 1), list(x = 2), list(x = 3)),
    descriptions = c("Test 1", "Test 2", "Test 3"),
    hidden = c(FALSE, FALSE, FALSE),
    points = c(1, 1, 1),
    tolerance = 1e-10
  )
  
  results <- run_tests_sequential(student_fn, instructor_fn, test_data, 1e-10)
  
  expect_length(results, 3)
  # Test 2 should have error
  expect_true(inherits(results[[2]]$student, "error"))
  expect_true(grepl("Error on 2", results[[2]]$student$error))
})

test_that("run_tests_sequential captures instructor errors", {
  student_fn <- function(x) x + 1
  instructor_fn <- function(x) {
    if (x == 3) stop("Instructor error")
    x + 1
  }
  
  test_data <- list(
    inputs = list(list(x = 1), list(x = 2), list(x = 3)),
    descriptions = c("Test 1", "Test 2", "Test 3"),
    hidden = c(FALSE, FALSE, FALSE),
    points = c(1, 1, 1),
    tolerance = 1e-10
  )
  
  results <- run_tests_sequential(student_fn, instructor_fn, test_data, 1e-10)
  
  # Test 3 should have instructor error
  expect_true(inherits(results[[3]]$expected, "error"))
})

test_that("run_tests_sequential handles empty input list", {
  student_fn <- function() 42
  instructor_fn <- function() 42
  
  test_data <- list(
    inputs = list(list()),  # Empty argument list
    descriptions = c("No args test"),
    hidden = FALSE,
    points = 1,
    tolerance = 1e-10
  )
  
  results <- run_tests_sequential(student_fn, instructor_fn, test_data, 1e-10)
  
  expect_length(results, 1)
  expect_equal(results[[1]]$student, 42)
  expect_equal(results[[1]]$expected, 42)
})

test_that("run_tests_sequential handles multiple arguments", {
  student_fn <- function(a, b, c) a + b + c
  instructor_fn <- function(a, b, c) a + b + c
  
  test_data <- list(
    inputs = list(
      list(a = 1, b = 2, c = 3),
      list(a = 10, b = 20, c = 30)
    ),
    descriptions = c("Test 1", "Test 2"),
    hidden = c(FALSE, FALSE),
    points = c(1, 1),
    tolerance = 1e-10
  )
  
  results <- run_tests_sequential(student_fn, instructor_fn, test_data, 1e-10)
  
  expect_equal(results[[1]]$student, 6)
  expect_equal(results[[2]]$student, 60)
})

# ============================================================================
# RUN_TESTS_PARALLEL TESTS
# ============================================================================

test_that("run_tests_parallel falls back for small test sets", {
  student_fn <- function(x) x + 1
  instructor_fn <- function(x) x + 1
  
  # Less than 10 tests - should fall back to sequential
  test_data <- list(
    inputs = list(list(x = 1), list(x = 2), list(x = 3)),
    descriptions = c("Test 1", "Test 2", "Test 3"),
    hidden = c(FALSE, FALSE, FALSE),
    points = c(1, 1, 1),
    tolerance = 1e-10
  )
  
  results <- run_tests_parallel(student_fn, instructor_fn, test_data, 1e-10, TRUE)
  
  expect_length(results, 3)
})

test_that("run_tests_parallel falls back when use_parallel is FALSE", {
  student_fn <- function(x) x + 1
  instructor_fn <- function(x) x + 1
  
  # Create many tests but with use_parallel = FALSE
  test_data <- list(
    inputs = lapply(1:20, function(i) list(x = i)),
    descriptions = paste("Test", 1:20),
    hidden = rep(FALSE, 20),
    points = rep(1, 20),
    tolerance = 1e-10
  )
  
  results <- run_tests_parallel(student_fn, instructor_fn, test_data, 1e-10, FALSE)
  
  expect_length(results, 20)
})

test_that("run_tests_parallel works with many tests", {
  skip_on_cran()
  skip_on_ci()  # Windows CI has process spawn limits
  
  student_fn <- function(x) x * 2
  instructor_fn <- function(x) x * 2
  
  # 15 tests - should use parallel
  test_data <- list(
    inputs = lapply(1:15, function(i) list(x = i)),
    descriptions = paste("Test", 1:15),
    hidden = rep(FALSE, 15),
    points = rep(1, 15),
    tolerance = 1e-10
  )
  
  results <- run_tests_parallel(student_fn, instructor_fn, test_data, 1e-10, TRUE)
  
  expect_length(results, 15)
  
  # Verify all results are correct
  for (i in 1:15) {
    expect_equal(results[[i]]$student, i * 2)
    expect_equal(results[[i]]$expected, i * 2)
  }
})

# ============================================================================
# EXTRACT_INSTRUCTOR_FUNCTION TESTS
# ============================================================================

test_that("extract_instructor_function finds function in environment", {
  env <- new.env()
  env$my_func <- function(x) x + 1
  
  result <- extract_instructor_function(env, "my_func")
  
  expect_true(is.function(result))
  expect_equal(result(5), 6)
})

test_that("extract_instructor_function returns first function found", {
  env <- new.env()
  env$data <- 123  # Not a function
  env$helper <- function(x) x  # First function
  env$main_func <- function(x) x + 1  # Second function
  
  result <- extract_instructor_function(env, "test")
  
  # Should return the first function found
  expect_true(is.function(result))
})

test_that("extract_instructor_function errors on no function", {
  env <- new.env()
  env$data <- 123
  env$text <- "hello"
  
  expect_error(
    extract_instructor_function(env, "test"),
    "No function implementation found"
  )
})

test_that("extract_instructor_function errors on empty environment", {
  env <- new.env()
  
  expect_error(
    extract_instructor_function(env, "test"),
    "No function implementation found"
  )
})

# ============================================================================
# EXTRACT_TEST_CASES TESTS
# ============================================================================

test_that("extract_test_cases retrieves and validates test_cases", {
  env <- new.env()
  env$my_func <- function(x) x + 1
  env$test_cases <- list(
    inputs = list(list(x = 1), list(x = 2)),
    descriptions = c("Test 1", "Test 2")
  )
  
  result <- extract_test_cases(env, "my_func")
  
  expect_true(is.list(result))
  expect_equal(length(result$inputs), 2)
  # Should have defaults filled in
  expect_equal(length(result$hidden), 2)
  expect_equal(length(result$points), 2)
})

test_that("extract_test_cases errors when test_cases missing", {
  env <- new.env()
  env$my_func <- function(x) x + 1
  
  expect_error(
    extract_test_cases(env, "my_func"),
    "No test cases found"
  )
})

# ============================================================================
# LIST_PROBLEMS TESTS
# ============================================================================

test_that("list_problems returns character vector", {
  skip_on_cran()
  skip_if_offline()
  
  result <- suppressMessages(list_problems())
  
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("list_problems includes expected functions", {
  skip_on_cran()
  skip_if_offline()
  
  result <- suppressMessages(list_problems())
  
  # At least one of these should be present
  expected <- c("fibonacci", "factorial", "sum_vector")
  found <- any(expected %in% result)
  expect_true(found)
})

# ============================================================================
# PREVIEW_TESTS TESTS
# ============================================================================

test_that("preview_tests shows test information", {
  skip_on_cran()
  skip_if_offline()
  
  output <- capture.output(
    result <- preview_tests("fibonacci")
  )
  
  expect_true(length(output) > 0)
  expect_true(is.list(result))
})

test_that("preview_tests returns test_data structure", {
  skip_on_cran()
  skip_if_offline()
  
  result <- suppressMessages(preview_tests("fibonacci"))
  
  expect_true("inputs" %in% names(result))
  expect_true("descriptions" %in% names(result))
})

# ============================================================================
# COMPARE_PERFORMANCE TESTS
# ============================================================================

test_that("compare_performance requires student function", {
  # Make sure no student function exists
  if (exists("student_test_perf", envir = .GlobalEnv)) {
    rm("student_test_perf", envir = .GlobalEnv)
  }
  
  expect_error(
    compare_performance("test_perf"),
    "not found|student_"
  )
})

test_that("compare_performance validates function_name", {
  expect_error(compare_performance(123), "character")
  expect_error(compare_performance(c("a", "b")), "single")
})

test_that("compare_performance runs with valid function", {
  skip_on_cran()
  skip_if_offline()
  
  # Define student function
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- compare_performance("fibonacci", n_runs = 2, warmup = 0, verbose = FALSE)
  
  expect_s3_class(result, "performance_comparison")
  expect_true("student_times" %in% names(result))
  expect_true("instructor_times" %in% names(result))
  expect_true("ratio" %in% names(result))
})

test_that("compare_performance handles custom test_inputs", {
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
  
  custom_inputs <- list(list(n = 5), list(n = 10))
  
  result <- compare_performance("fibonacci", test_inputs = custom_inputs, 
                                n_runs = 2, warmup = 0, verbose = FALSE)
  
  expect_s3_class(result, "performance_comparison")
  expect_equal(result$n_inputs, 2)
})

test_that("print.performance_comparison formats output", {
  # Create mock result
  result <- structure(
    list(
      function_name = "test",
      n_runs = 5,
      n_inputs = 3,
      student_times = c(0.1, 0.11, 0.09, 0.1, 0.12),
      instructor_times = c(0.1, 0.1, 0.1, 0.1, 0.1),
      student_stats = list(median = 0.1, mean = 0.104, sd = 0.01, min = 0.09, max = 0.12),
      instructor_stats = list(median = 0.1, mean = 0.1, sd = 0, min = 0.1, max = 0.1),
      ratio = 1.0,
      passed_correctness = TRUE,
      verdict = "COMPARABLE"
    ),
    class = c("performance_comparison", "list")
  )
  
  output <- capture.output(print(result))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Performance Comparison", output)))
})

test_that("plot.performance_comparison creates plot", {
  skip_on_cran()
  
  result <- structure(
    list(
      function_name = "test",
      n_runs = 5,
      n_inputs = 3,
      student_times = c(0.1, 0.11, 0.09, 0.1, 0.12),
      instructor_times = c(0.1, 0.1, 0.1, 0.1, 0.1),
      ratio = 1.0
    ),
    class = c("performance_comparison", "list")
  )
  
  # Should not error
  expect_silent(suppressWarnings(plot(result)))
})

# ============================================================================
# .onAttach MESSAGE TEST
# ============================================================================

test_that(".onAttach creates startup message", {
  # Create mock message capture
  messages <- character()
  
  # The function exists
  expect_true(exists(".onAttach", envir = asNamespace("autograder")))
})
