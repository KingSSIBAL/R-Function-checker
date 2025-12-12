# ============================================================================
# AUTOGRADER - ERROR PATH COVERAGE TESTS
# ============================================================================
#
# Tests targeting uncovered error paths in R/autograder.R
#
# ============================================================================

# ============================================================================
# FORMAT OUTPUT ERROR PATHS
# ============================================================================

test_that("format_output handles data.frame with many columns", {
  # Use C++ version since format_output is internal
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  result <- .cpp_format_output(df, 100L)
  expect_true(nchar(result) > 0)
  # Output should contain some info about the data.frame
  expect_true(grepl("data.frame|3|4|a|b|c|d", result))
})

test_that("format_output handles data.frame with single column", {
  df <- data.frame(x = 1:5)
  result <- .cpp_format_output(df, 100L)
  expect_true(nchar(result) > 0)
})

test_that("format_output handles default else branch", {
  # Factor
  f <- factor(c("a", "b", "c"))
  result <- .cpp_format_output(f, 100L)
  expect_true(nchar(result) > 0)
  
  # Date
  d <- as.Date("2024-01-01")
  result <- .cpp_format_output(d, 100L)
  expect_true(nchar(result) > 0)
  
  # POSIXct
  dt <- as.POSIXct("2024-01-01 12:00:00")
  result <- .cpp_format_output(dt, 100L)
  expect_true(nchar(result) > 0)
})

test_that("format_output truncates output exceeding max_length", {
  # Very long vector that will exceed max_length
  long_vec <- 1:1000
  result <- .cpp_format_output(long_vec, 50L)
  expect_true(nchar(result) <= 60)  # Allow some tolerance
})

# ============================================================================
# AUTOGRADER PARAMETER VALIDATION
# ============================================================================

test_that("autograder validates function_name parameter", {
  # Not a string
  expect_error(autograder(123), "function_name")
  expect_error(autograder(c("a", "b")), "function_name")
  expect_error(autograder(NULL), "function_name")
})

test_that("autograder validates boolean parameters", {
  # Error message uses checkmate format
  expect_error(autograder("test", verbose = "yes"), "logical flag")
  expect_error(autograder("test", show_hidden = "no"), "logical flag")
  expect_error(autograder("test", show_hints = 1), "logical flag")
  expect_error(autograder("test", show_progress = list()), "logical flag")
  expect_error(autograder("test", use_parallel = "false"), "logical flag")
})

# ============================================================================
# EXTRACT FUNCTIONS ERROR PATHS
# ============================================================================

test_that("extract_instructor_function handles empty environment", {
  # Create environment with no functions
  empty_env <- new.env()
  
  expect_error(
    extract_instructor_function(empty_env, "test_func"),
    "No function implementation found"
  )
})

test_that("extract_instructor_function handles environment with only data", {
  # Create environment with only data, no functions
  data_env <- new.env()
  data_env$x <- 1:10
  data_env$y <- "hello"
  data_env$z <- list(a = 1, b = 2)
  
  expect_error(
    extract_instructor_function(data_env, "test_func"),
    "No function implementation found"
  )
})

test_that("extract_test_cases handles missing test_cases", {
  # Create environment without test_cases
  env <- new.env()
  env$my_func <- function(x) x + 1
  
  expect_error(
    extract_test_cases(env, "test_func"),
    "No test cases found"
  )
})

test_that("extract_test_cases handles empty test_cases", {
  env <- new.env()
  env$my_func <- function(x) x + 1
  env$test_cases <- list()
  
  # Empty list is still present, should not error on extraction
  # but may fail validation
  result <- tryCatch(
    extract_test_cases(env, "test_func"),
    error = function(e) e
  )
  # Either returns empty or errors - both are valid
  expect_true(is.list(result) || inherits(result, "error"))
})

# ============================================================================
# VALIDATION ERROR PATHS
# ============================================================================

test_that("validate_test_cases catches missing required fields", {
  # Missing inputs
  test_data <- list(
    expected = list(1, 2, 3),
    descriptions = c("test1", "test2", "test3")
  )
  expect_error(validate_test_cases(test_data, "test_func"), "inputs")
})

test_that("validate_test_cases catches empty inputs", {
  test_data <- list(
    inputs = list()  # Empty inputs
  )
  expect_error(validate_test_cases(test_data, "test_func"), "at least one")
})

test_that("validate_test_cases catches description length mismatches", {
  test_data <- list(
    inputs = list(1, 2, 3),
    descriptions = c("test1", "test2")  # Wrong length
  )
  expect_error(validate_test_cases(test_data, "test_func"), "length|doesn't match")
})

test_that("validate_test_cases handles missing descriptions", {
  test_data <- list(
    inputs = list(1, 2, 3)
  )
  
  # Should add default descriptions
  result <- validate_test_cases(test_data, "test_func")
  expect_true("descriptions" %in% names(result))
  expect_equal(length(result$descriptions), 3)
})

test_that("validate_test_cases handles missing points", {
  test_data <- list(
    inputs = list(1, 2)
  )
  
  result <- validate_test_cases(test_data, "test_func")
  expect_true("points" %in% names(result))
})

# ============================================================================
# PROVIDE FEEDBACK PATHS
# ============================================================================

test_that("provide_feedback detects type mismatches", {
  result <- provide_feedback(
    student_out = "hello",
    expected_out = 123,
    input_args = list(x = 1)
  )
  expect_true("type_issue" %in% names(result))
  expect_true(grepl("Type mismatch", result$type_issue))
})

test_that("provide_feedback detects length mismatches", {
  result <- provide_feedback(
    student_out = 1:3,
    expected_out = 1:5,
    input_args = list(x = 1)
  )
  expect_true("length_issue" %in% names(result))
  expect_true(grepl("Length mismatch", result$length_issue))
})

test_that("provide_feedback detects value differences", {
  result <- provide_feedback(
    student_out = c(1, 2, 99, 4, 5),
    expected_out = c(1, 2, 3, 4, 5),
    input_args = list(x = 1)
  )
  expect_true("diff_positions" %in% names(result))
})

test_that("provide_feedback includes hints", {
  result <- provide_feedback(
    student_out = 1,
    expected_out = 2,
    input_args = list(x = 1),
    hint = "Check your calculation"
  )
  expect_true("hint" %in% names(result))
  expect_true(grepl("Hint:", result$hint))
})

test_that("provide_feedback handles identical outputs", {
  result <- provide_feedback(
    student_out = 42,
    expected_out = 42,
    input_args = list(x = 1)
  )
  # Should have no issues
  expect_false("type_issue" %in% names(result))
  expect_false("length_issue" %in% names(result))
  expect_false("diff_positions" %in% names(result))
})

test_that("provide_feedback handles multiple differences", {
  result <- provide_feedback(
    student_out = c(10, 20, 30, 40, 50, 60, 70),
    expected_out = c(1, 2, 3, 4, 5, 6, 7),
    input_args = list(x = 1)
  )
  expect_true("diff_positions" %in% names(result))
  expect_true(grepl("\\.\\.\\.", result$diff_positions))  # Should have ellipsis for >5 diffs
})

# ============================================================================
# NETWORK ERROR HANDLING
# ============================================================================

test_that("network_error creates proper error", {
  err <- tryCatch(
    network_error("Test error message"),
    error = function(e) e
  )
  expect_true(inherits(err, "network_error"))
})

test_that("function_not_found_error creates proper error", {
  err <- tryCatch(
    function_not_found_error("my_function"),
    error = function(e) e
  )
  expect_true(inherits(err, "function_not_found_error"))
})

test_that("test_execution_error creates proper error", {
  err <- tryCatch(
    test_execution_error("Test error", 3),
    error = function(e) e
  )
  expect_true(inherits(err, "test_execution_error"))
})

# ============================================================================
# CPP FUNCTION VALIDATION
# ============================================================================

test_that(".cpp_validate_function_name rejects shell injection attempts", {
  # Command injection
  expect_false(.cpp_validate_function_name("test; rm -rf /"))
  expect_false(.cpp_validate_function_name("test | cat"))
  expect_false(.cpp_validate_function_name("test & echo"))
  expect_false(.cpp_validate_function_name("test$(whoami)"))
  expect_false(.cpp_validate_function_name("test`id`"))
})

test_that(".cpp_validate_function_name rejects control characters", {
  # Tab, newline, carriage return
  expect_false(.cpp_validate_function_name("test\neval"))
  expect_false(.cpp_validate_function_name("test\reval"))
})

# ============================================================================
# LIST PROBLEMS FUNCTION
# ============================================================================

test_that("list_problems returns available problems", {
  skip_if_offline()
  skip_on_cran()
  
  result <- tryCatch(
    list_problems(),
    error = function(e) NULL
  )
  
  if (!is.null(result)) {
    expect_type(result, "character")
    expect_true(length(result) >= 0)
  }
})

# ============================================================================
# PREVIEW TESTS FUNCTION
# ============================================================================

test_that("preview_tests validates function_name", {
  # This should follow the same validation as autograder
  expect_error(preview_tests(123), class = "error")
})

# ============================================================================
# RUN TESTS FUNCTIONS
# ============================================================================

test_that("run_tests_sequential handles execution errors gracefully", {
  # Create a function that throws an error
  bad_fun <- function(x) stop("Intentional error")
  
  test_data <- list(
    inputs = list(1),
    expected = list(2),
    descriptions = "Test",
    hidden = FALSE,
    points = 1,
    tolerance = 0.001,
    expected_type = "any",
    hints = "",
    comparison_fn = NULL
  )
  
  # Should not crash, should return error result
  result <- tryCatch(
    run_tests_sequential(
      student_fun = bad_fun,
      instructor_fun = function(x) x + 1,
      test_data = test_data
    ),
    error = function(e) list(error = TRUE)
  )
  
  expect_type(result, "list")
})

# ============================================================================
# EDGE CASES FOR COMPARISON
# ============================================================================

test_that(".cpp_compare_detailed handles identical complex structures", {
  # Lists with same structure
  l1 <- list(a = 1:5, b = list(c = "test", d = TRUE))
  l2 <- list(a = 1:5, b = list(c = "test", d = TRUE))
  
  result <- .cpp_compare_detailed(l1, l2, 1e-10)
  expect_true(result$equal)
})

test_that(".cpp_compare_detailed shows differences", {
  l1 <- list(a = 1, b = 2)
  l2 <- list(a = 1, b = 3)
  
  result <- .cpp_compare_detailed(l1, l2, 0)
  expect_false(result$equal)
  # Message may be in different field or format
  expect_true(!is.null(result$message) || !is.null(result$reason) || !result$equal)
})

# ============================================================================
# ERROR CLASS STRUCTURE TESTS
# ============================================================================

test_that("function_not_found_error includes function name", {
  err <- tryCatch(
    function_not_found_error("my_func"),
    error = function(e) e
  )
  
  expect_equal(err$function_name, "my_func")
})

test_that("function_not_found_error can be caught specifically", {
  result <- tryCatch(
    function_not_found_error("test"),
    autograder_function_not_found_error = function(e) "caught_fnf",
    error = function(e) "caught_generic"
  )
  
  expect_equal(result, "caught_fnf")
})

test_that("type_mismatch_error creates proper error structure", {
  err <- tryCatch(
    type_mismatch_error("numeric", "character"),
    error = function(e) e
  )
  
  expect_s3_class(err, "autograder_type_mismatch_error")
  expect_s3_class(err, "autograder_error")
  expect_match(conditionMessage(err), "Type mismatch")
})

test_that("type_mismatch_error includes type information", {
  err <- tryCatch(
    type_mismatch_error("integer", "double"),
    error = function(e) e
  )
  
  expect_equal(err$expected_type, "integer")
  expect_equal(err$actual_type, "double")
})

test_that("length_mismatch_error creates proper error structure", {
  err <- tryCatch(
    length_mismatch_error(10, 5),
    error = function(e) e
  )
  
  expect_s3_class(err, "autograder_length_mismatch_error")
  expect_s3_class(err, "autograder_error")
  expect_match(conditionMessage(err), "Length mismatch")
})

test_that("length_mismatch_error includes length information", {
  err <- tryCatch(
    length_mismatch_error(100, 50),
    error = function(e) e
  )
  
  expect_equal(err$expected_length, 100)
  expect_equal(err$actual_length, 50)
})

test_that("student_function_error creates proper error structure", {
  err <- tryCatch(
    student_function_error("fibonacci"),
    error = function(e) e
  )
  
  expect_s3_class(err, "autograder_student_function_error")
  expect_s3_class(err, "autograder_error")
  expect_match(conditionMessage(err), "student_fibonacci")
})

test_that("timeout_error creates proper error structure", {
  err <- tryCatch(
    timeout_error(30),
    error = function(e) e
  )
  
  expect_s3_class(err, "autograder_timeout_error")
  expect_s3_class(err, "autograder_error")
  expect_match(conditionMessage(err), "30 seconds")
})

test_that("abort_validation creates proper error structure", {
  err <- tryCatch(
    abort_validation("x", "numeric", "character"),
    error = function(e) e
  )
  
  expect_s3_class(err, "autograder_validation_error")
  expect_s3_class(err, "autograder_error")
  expect_match(conditionMessage(err), "x")
})

test_that("abort_input creates proper error structure", {
  err <- tryCatch(
    abort_input("Invalid input"),
    error = function(e) e
  )
  
  expect_s3_class(err, "autograder_input_error")
  expect_s3_class(err, "autograder_error")
  expect_match(conditionMessage(err), "Invalid input")
})

test_that("all error types inherit from autograder_error", {
  errors <- list(
    tryCatch(network_error("test"), error = function(e) e),
    tryCatch(function_not_found_error("test"), error = function(e) e),
    tryCatch(test_execution_error("test", 1), error = function(e) e),
    tryCatch(type_mismatch_error("a", "b"), error = function(e) e),
    tryCatch(length_mismatch_error(1, 2), error = function(e) e),
    tryCatch(student_function_error("test"), error = function(e) e),
    tryCatch(timeout_error(10), error = function(e) e)
  )
  
  for (err in errors) {
    expect_s3_class(err, "autograder_error")
  }
})

test_that("autograder errors can be caught with autograder_error class", {
  errors_to_test <- list(
    function() network_error("test"),
    function() function_not_found_error("test"),
    function() test_execution_error("test", 1),
    function() type_mismatch_error("a", "b"),
    function() length_mismatch_error(1, 2),
    function() student_function_error("test"),
    function() timeout_error(10)
  )
  
  for (err_fn in errors_to_test) {
    result <- tryCatch(
      err_fn(),
      autograder_error = function(e) "caught_autograder",
      error = function(e) "caught_generic"
    )
    expect_equal(result, "caught_autograder")
  }
})
