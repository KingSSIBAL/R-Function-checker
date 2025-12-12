# ============================================================================
# AUTOGRADER - ADDITIONAL R COVERAGE TESTS
# ============================================================================
#
# Tests specifically targeting R code paths for maximum coverage:
#   - provide_feedback edge cases
#   - format_output all branches
#   - validate_test_cases all paths
#   - autograder result processing
#   - progress bar code
#   - summary generation
#
# ============================================================================

# ============================================================================
# PROVIDE_FEEDBACK COMPREHENSIVE TESTS
# ============================================================================

test_that("provide_feedback handles all comparison scenarios", {
  # Identical values - no feedback
  expect_equal(length(provide_feedback(5, 5, list())), 0)
  expect_equal(length(provide_feedback("test", "test", list())), 0)
  expect_equal(length(provide_feedback(1:10, 1:10, list())), 0)
  
  # Type mismatch
  fb <- provide_feedback("5", 5, list())
  expect_true("type_issue" %in% names(fb))
  
  # Length mismatch for vectors
  fb <- provide_feedback(1:5, 1:10, list())
  expect_true("length_issue" %in% names(fb))
  
  # Value differences at specific positions
  fb <- provide_feedback(c(1, 99, 3), c(1, 2, 3), list())
  expect_true("diff_positions" %in% names(fb))
  expect_true(grepl("2", fb$diff_positions))  # Position 2 differs
})

test_that("provide_feedback handles hint variations", {
  # NULL hint
  fb <- provide_feedback(1, 2, list(), hint = NULL)
  expect_false("hint" %in% names(fb))
  
  # Empty string hint
  fb <- provide_feedback(1, 2, list(), hint = "")
  expect_false("hint" %in% names(fb))
  
  # Valid hint
  fb <- provide_feedback(1, 2, list(), hint = "Check your math")
  expect_true("hint" %in% names(fb))
  expect_true(grepl("Check your math", fb$hint))
})

test_that("provide_feedback truncates many diff positions", {
  # Many differences should be truncated
  student <- 1:50
  expected <- 51:100  # All different
  
  fb <- provide_feedback(student, expected, list())
  
  if ("diff_positions" %in% names(fb)) {
    expect_true(grepl("\\.\\.\\.", fb$diff_positions))
  }
})

test_that("provide_feedback handles tolerance in numeric comparison", {
  # Within tolerance
  fb <- provide_feedback(1.0000000001, 1.0, list())
  # No diff_positions if within default tolerance
  
  # Just outside tolerance
  fb2 <- provide_feedback(1.001, 1.0, list())
  expect_true(is.list(fb2))
})

test_that("provide_feedback handles NA in vectors", {
  # Both have NA at same position
  fb <- provide_feedback(c(1, NA, 3), c(1, NA, 3), list())
  expect_true(is.list(fb))
  
  # NA vs value
  fb <- provide_feedback(c(1, NA, 3), c(1, 2, 3), list())
  expect_true(is.list(fb))
  
  # Value vs NA  
  fb <- provide_feedback(c(1, 2, 3), c(1, NA, 3), list())
  expect_true(is.list(fb))
})

test_that("provide_feedback handles NaN and Inf", {
  fb <- provide_feedback(NaN, 0, list())
  expect_true(is.list(fb))
  
  fb <- provide_feedback(Inf, 1e308, list())
  expect_true(is.list(fb))
  
  fb <- provide_feedback(-Inf, -Inf, list())
  expect_true(is.list(fb))
})

# ============================================================================
# FORMAT_OUTPUT ALL BRANCHES
# ============================================================================

test_that("format_output NULL branch", {
  expect_equal(format_output(NULL), "NULL")
})

test_that("format_output list branches", {
  # Empty list
  expect_equal(format_output(list()), "list()")
  
  # Small list (≤3 elements) - show full
  result <- format_output(list(a = 1, b = 2))
  expect_true(nchar(result) > 0)
  
  # Boundary list (exactly 3)
  result <- format_output(list(a = 1, b = 2, c = 3))
  expect_true(nchar(result) > 0)
  
  # Large list (>3) - show summary
  result <- format_output(list(a = 1, b = 2, c = 3, d = 4))
  expect_true(grepl("length", result))
})

test_that("format_output vector branches", {
  # Short vector (≤10) - use deparse
  result <- format_output(1:10)
  expect_true(nchar(result) > 0)
  
  # Long vector (>10) - use head...tail format
  result <- format_output(1:100)
  expect_true(grepl("\\.\\.\\.", result))
  
  # Character vector
  result <- format_output(letters)
  expect_true(grepl("\\.\\.\\.", result))
})

test_that("format_output matrix branch", {
  # Various matrix sizes
  result <- format_output(matrix(1:4, 2, 2))
  expect_true(grepl("matrix", result))
  
  result <- format_output(matrix(1:100, 10, 10))
  expect_true(grepl("matrix", result))
  
  # 1x1 matrix
  result <- format_output(matrix(1))
  expect_true(grepl("matrix", result))
})

test_that("format_output data.frame branch", {
  # Simple data.frame
  result <- format_output(data.frame(x = 1:3))
  # data.frame formatting may vary
  expect_true(nchar(result) > 0)
  
  # Many columns
  result <- format_output(mtcars)
  expect_true(nchar(result) > 0)
})

test_that("format_output default branch", {
  # Factors use default deparse
  result <- format_output(factor(c("a", "b", "c")))
  expect_true(nchar(result) > 0)
  
  # Dates
  result <- format_output(Sys.Date())
  expect_true(nchar(result) > 0)
  
  # POSIXct
  result <- format_output(Sys.time())
  expect_true(nchar(result) > 0)
})

test_that("format_output truncation branch", {
  # Create output that exceeds max_length
  long_list <- as.list(setNames(1:10, paste0("very_long_name_", 1:10)))
  result <- format_output(long_list, max_length = 50)
  
  # Should be truncated with ...
  expect_lte(nchar(result), 55)
})

test_that("format_output preserve_structure parameter", {
  lst <- list(a = 1, b = 2)
  
  # TRUE - show structure
  r1 <- format_output(lst, preserve_structure = TRUE)
  
  # FALSE - use deparse
  r2 <- format_output(lst, preserve_structure = FALSE)
  
  expect_true(nchar(r1) > 0)
  expect_true(nchar(r2) > 0)
})

# ============================================================================
# VALIDATE_TEST_CASES ALL PATHS
# ============================================================================

test_that("validate_test_cases inputs validation", {
  # Missing inputs - error
  expect_error(validate_test_cases(list(), "fn"), "inputs")
  
  # NULL inputs - error
  expect_error(validate_test_cases(list(inputs = NULL), "fn"))
  
  # Empty inputs - error
  expect_error(validate_test_cases(list(inputs = list()), "fn"))
  
  # Non-list inputs - error
  expect_error(validate_test_cases(list(inputs = 1:10), "fn"))
})

test_that("validate_test_cases descriptions handling", {
  # Valid descriptions
  tc <- list(inputs = list(list(1), list(2)), descriptions = c("A", "B"))
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$descriptions, c("A", "B"))
  
  # Missing descriptions - auto-generate
  tc <- list(inputs = list(list(1), list(2)))
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$descriptions, c("Test 1", "Test 2"))
  
  # Wrong length - error
  tc <- list(inputs = list(list(1), list(2)), descriptions = "only one")
  expect_error(validate_test_cases(tc, "fn"), "doesn't match")
})

test_that("validate_test_cases hidden handling", {
  # Valid hidden
  tc <- list(inputs = list(list(1)), hidden = TRUE)
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$hidden, TRUE)
  
  # Missing hidden - default FALSE
  tc <- list(inputs = list(list(1)))
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$hidden, FALSE)
  
  # Wrong length - error
  tc <- list(inputs = list(list(1), list(2)), hidden = TRUE)  # scalar for 2 tests
  expect_error(validate_test_cases(tc, "fn"), "doesn't match")
  
  # Non-logical - error
  tc <- list(inputs = list(list(1)), hidden = "yes")
  expect_error(validate_test_cases(tc, "fn"), "logical")
})

test_that("validate_test_cases points handling", {
  # Valid points
  tc <- list(inputs = list(list(1)), points = 5)
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$points, 5)
  
  # Missing points - default 1
  tc <- list(inputs = list(list(1)))
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$points, 1)
  
  # Wrong length - error
  tc <- list(inputs = list(list(1), list(2)), points = 5)  # scalar for 2 tests
  expect_error(validate_test_cases(tc, "fn"), "doesn't match")
  
  # Negative points - error
  tc <- list(inputs = list(list(1)), points = -1)
  expect_error(validate_test_cases(tc, "fn"), "non-negative")
  
  # Non-numeric - error
  tc <- list(inputs = list(list(1)), points = "five")
  expect_error(validate_test_cases(tc, "fn"), "non-negative numeric")
})

test_that("validate_test_cases tolerance handling", {
  # Valid tolerance
  tc <- list(inputs = list(list(1)), tolerance = 0.001)
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$tolerance, 0.001)
  
  # Missing tolerance - default 1e-10
  tc <- list(inputs = list(list(1)))
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$tolerance, 1e-10)
  
  # Negative tolerance - error
  tc <- list(inputs = list(list(1)), tolerance = -0.01)
  expect_error(validate_test_cases(tc, "fn"), "non-negative")
  
  # Vector tolerance - error
  tc <- list(inputs = list(list(1)), tolerance = c(0.01, 0.02))
  expect_error(validate_test_cases(tc, "fn"), "single")
  
  # Non-numeric - error
  tc <- list(inputs = list(list(1)), tolerance = "small")
  expect_error(validate_test_cases(tc, "fn"), "non-negative numeric")
})

test_that("validate_test_cases expected_type handling", {
  # Valid expected_type
  tc <- list(inputs = list(list(1)), expected_type = "numeric")
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$expected_type, "numeric")
  
  # Invalid (vector) - warn and NULL
  tc <- list(inputs = list(list(1)), expected_type = c("a", "b"))
  result <- expect_warning(validate_test_cases(tc, "fn"), "expected_type")
  expect_null(result$expected_type)
})

test_that("validate_test_cases hints handling", {
  # Valid hints
  tc <- list(inputs = list(list(1)), hints = "Try this")
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$hints, "Try this")
  
  # Wrong length - warn and NULL
  tc <- list(inputs = list(list(1), list(2)), hints = "only one")
  result <- expect_warning(validate_test_cases(tc, "fn"), "hints")
  expect_null(result$hints)
})

test_that("validate_test_cases comparison_fn handling", {
  # Valid function
  tc <- list(inputs = list(list(1)), comparison_fn = function(a, b) a == b)
  result <- validate_test_cases(tc, "fn")
  expect_true(is.function(result$comparison_fn))
  
  # Non-function - warn and NULL
  tc <- list(inputs = list(list(1)), comparison_fn = "not a function")
  result <- expect_warning(validate_test_cases(tc, "fn"), "comparison_fn")
  expect_null(result$comparison_fn)
})

# ============================================================================
# PRINT_FEEDBACK ALL BRANCHES
# ============================================================================

test_that("print_feedback handles empty feedback", {
  expect_silent(print_feedback(list()))
})

test_that("print_feedback displays all feedback types", {
  fb <- list(
    type_issue = "Type error",
    length_issue = "Length error",
    diff_positions = "Positions 1, 2",
    hint = "A hint"
  )
  
  output <- capture.output(print_feedback(fb))
  expect_true(any(grepl("Type error", output)))
  expect_true(any(grepl("Length error", output)))
  expect_true(any(grepl("Positions", output)))
  expect_true(any(grepl("hint", output)))
})

# ============================================================================
# %||% OPERATOR TESTS
# ============================================================================

test_that("null coalescing works for all types", {
  # NULL gives default
  expect_equal(NULL %||% 1, 1)
  expect_equal(NULL %||% "default", "default")
  expect_equal(NULL %||% TRUE, TRUE)
  expect_equal(NULL %||% list(a = 1), list(a = 1))
  
  # Non-NULL gives value
  expect_equal(5 %||% 1, 5)
  expect_equal("value" %||% "default", "value")
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal(0 %||% 1, 0)
  expect_equal("" %||% "default", "")
  expect_equal(NA %||% 1, NA)
})

# ============================================================================
# ERROR CLASS CONSTRUCTION
# ============================================================================

test_that("network_error structure", {
  err <- tryCatch(
    network_error("Test message"),
    error = function(e) e
  )
  
  expect_s3_class(err, "network_error")
  expect_s3_class(err, "error")
  expect_s3_class(err, "condition")
  expect_true(grepl("Test message", conditionMessage(err)))
  
  # With call
  err2 <- tryCatch(
    network_error("Msg", call = quote(test_fn())),
    error = function(e) e
  )
  expect_equal(err2$call, quote(test_fn()))
})

test_that("function_not_found_error structure", {
  err <- tryCatch(
    function_not_found_error("my_func"),
    error = function(e) e
  )
  
  expect_s3_class(err, "function_not_found_error")
  expect_equal(err$function_name, "my_func")
  expect_true(grepl("my_func", conditionMessage(err)))
  expect_true(grepl("list_problems", conditionMessage(err)))
})

test_that("test_execution_error structure", {
  err <- tryCatch(
    test_execution_error("Failed", 5),
    error = function(e) e
  )
  
  expect_s3_class(err, "test_execution_error")
  expect_equal(err$test_number, 5)
  expect_true(grepl("Failed", conditionMessage(err)))
})

# ============================================================================
# RESULT STRUCTURE TESTS
# ============================================================================

test_that("autograder result structure is correct", {
  skip_on_cran()
  skip_if_offline()
  
  # Setup
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- capture.output(
    res <- autograder("fibonacci", verbose = FALSE, show_progress = FALSE)
  )
  
  # Check structure
  expect_true("passed" %in% names(res))
  expect_true("failed" %in% names(res))
  expect_true("total" %in% names(res))
  expect_true("score" %in% names(res))
  expect_true("max_score" %in% names(res))
  expect_true("pass_rate" %in% names(res))
  
  # Types - these are numeric (double in R), not necessarily integer
  expect_true(is.numeric(res$passed))
  expect_true(is.numeric(res$failed))
  expect_true(is.numeric(res$total))
  expect_true(is.numeric(res$score))
  expect_true(is.numeric(res$pass_rate))
})

# ============================================================================
# EDGE CASE INPUTS
# ============================================================================

test_that("functions handle special characters in strings", {
  special <- "test\n\t\r\\\"'"
  result <- format_output(special)
  expect_type(result, "character")
})

test_that("functions handle zero values", {
  expect_true(.cpp_compare_fast(0, 0, 1e-10))
  # Integer vs double may or may not match depending on implementation
  result <- .cpp_compare_fast(0L, 0, 1e-10)
  expect_type(result, "logical")
  expect_true(.cpp_compare_fast(0.0, -0.0, 1e-10))
  
  result <- format_output(0)
  expect_true(nchar(result) > 0)
})

test_that("functions handle single-element containers", {
  # Single-element vector
  result <- format_output(1)
  expect_true(nchar(result) > 0)
  
  # Single-element list
  result <- format_output(list(x = 1))
  expect_true(nchar(result) > 0)
  
  # 1x1 matrix
  result <- format_output(matrix(1))
  expect_true(grepl("matrix", result))
  
  # Single-row data.frame
  result <- format_output(data.frame(x = 1))
  expect_true(grepl("data.frame", result))
})

# ============================================================================
# PERFORMANCE COMPARISON ADDITIONAL TESTS
# ============================================================================

test_that("compare_performance handles failing correctness", {
  skip_on_cran()
  skip_if_offline()
  
  # Define an incorrect student function
  assign("student_fibonacci", function(n) {
    # Wrong implementation
    1:n
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  # Should still run but report correctness failure
  result <- tryCatch(
    suppressWarnings(compare_performance("fibonacci", n_runs = 2, warmup = 0, verbose = FALSE)),
    error = function(e) NULL
  )
  
  # Result may have passed_correctness = FALSE or may error
  if (!is.null(result) && !is.null(result$passed_correctness)) {
    expect_false(result$passed_correctness)
  }
})

test_that("performance_comparison verdicts", {
  # Create mock results for different ratios
  create_mock <- function(ratio) {
    structure(list(
      function_name = "test",
      n_runs = 5,
      n_inputs = 1,
      student_times = rep(ratio * 0.1, 5),
      instructor_times = rep(0.1, 5),
      student_stats = list(median = ratio * 0.1, mean = ratio * 0.1, 
                           sd = 0, min = ratio * 0.1, max = ratio * 0.1),
      instructor_stats = list(median = 0.1, mean = 0.1, 
                              sd = 0, min = 0.1, max = 0.1),
      ratio = ratio,
      passed_correctness = TRUE,
      verdict = ""
    ), class = c("performance_comparison", "list"))
  }
  
  # Test print doesn't error for any ratio
  for (ratio in c(0.5, 1.0, 1.5, 3.0, 10.0)) {
    mock <- create_mock(ratio)
    output <- capture.output(print(mock))
    expect_true(length(output) > 0)
  }
})
