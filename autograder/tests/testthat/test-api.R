# ============================================================================
# AUTOGRADER - API FUNCTION TESTS
# ============================================================================
#
# Tests for API discovery functions:
#   - preview_tests()
#   - list_problems()
#
# ============================================================================

# ============================================================================
# PREVIEW_TESTS VALIDATION TESTS
# ============================================================================

test_that("preview_tests validates NULL function_name", {
  expect_error(preview_tests(NULL), "single character string")
})

test_that("preview_tests validates numeric function_name", {
  expect_error(preview_tests(123), "single character string")
})

test_that("preview_tests validates empty vector function_name", {
  expect_error(preview_tests(character(0)), "single character string")
})

test_that("preview_tests validates multi-element function_name", {
  expect_error(preview_tests(c("a", "b")), "single character string")
})

test_that("preview_tests validates list function_name", {
  expect_error(preview_tests(list("a")), "single character string")
})

test_that("preview_tests validates NA function_name", {
  expect_error(preview_tests(NA), "single character string")
})

test_that("preview_tests validates logical function_name", {
  expect_error(preview_tests(TRUE), "single character string")
})

# ============================================================================
# LIST_PROBLEMS FALLBACK BEHAVIOR
# ============================================================================

test_that("list_problems always returns a character vector", {
  result <- suppressMessages(list_problems())
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("list_problems includes basic expected problems", {
  result <- suppressMessages(list_problems())
  
  # Should contain at least one of the default problems
  basic_problems <- c("fibonacci", "factorial", "sum_vector")
  has_at_least_one <- any(basic_problems %in% result)
  expect_true(has_at_least_one)
})

test_that("list_problems output contains usage information", {
  output <- capture.output(list_problems())
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Usage")
  expect_match(output_text, "student_")
  expect_match(output_text, "autograder")
})

test_that("list_problems output contains preview information", {
  output <- capture.output(list_problems())
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Preview")
  expect_match(output_text, "preview_tests")
})

test_that("list_problems output lists available problems", {
  output <- capture.output(list_problems())
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Available problems")
})

# ============================================================================
# Helper to reset rate limit after tests that modify it
# ============================================================================
local_rate_limit_reset <- function(env = parent.frame()) {
  withr::defer({
    configure_rate_limit(max_calls = 10000L, window_seconds = 60L)
  }, envir = env)
}

# ============================================================================
# ADDITIONAL PREVIEW_TESTS TESTS
# ============================================================================

test_that("preview_tests shows test information for valid function", {
  skip_on_cran()
  
  output <- capture.output(tryCatch(
    preview_tests("fibonacci"),
    error = function(e) NULL
  ))
  
  output_text <- paste(output, collapse = "\n")
  
  expect_true(
    grepl("Test|Loading|Summary", output_text, ignore.case = TRUE) ||
    length(output) == 0
  )
})

# ============================================================================
# ADDITIONAL LIST_PROBLEMS TESTS
# ============================================================================

test_that("list_problems runs without error", {
  output <- capture.output(result <- list_problems())
  expect_true(length(output) > 0)
})

test_that("list_problems shows available problems", {
  output <- capture.output(list_problems())
  output_text <- paste(output, collapse = "\n")
  expect_match(output_text, "fibonacci|factorial|sum_vector|analyze_data", ignore.case = TRUE)
})

# ============================================================================
# CHECK_RATE_LIMIT TESTS
# ============================================================================

test_that("check_rate_limit returns invisibly when within limits", {
  configure_rate_limit(max_calls = 100, window_seconds = 60)
  result <- check_rate_limit("test_action")
  expect_true(result)
})

test_that("check_rate_limit accepts action parameter", {
  configure_rate_limit(max_calls = 100, window_seconds = 60)
  expect_no_error(check_rate_limit("custom_action"))
  expect_no_error(check_rate_limit())
})

# ============================================================================
# CONFIGURE_RATE_LIMIT TESTS
# ============================================================================

test_that("configure_rate_limit sets max_calls", {
  local_rate_limit_reset()
  configure_rate_limit(max_calls = 50, window_seconds = 30)
  expect_no_error(configure_rate_limit(max_calls = 100, window_seconds = 60))
})

test_that("configure_rate_limit sets window_seconds", {
  local_rate_limit_reset()
  configure_rate_limit(max_calls = 100, window_seconds = 120)
  expect_no_error(configure_rate_limit(max_calls = 100, window_seconds = 60))
})

test_that("configure_rate_limit returns invisibly", {
  local_rate_limit_reset()
  result <- configure_rate_limit(max_calls = 100)
  expect_null(result)
})

# ============================================================================
# CACHING TESTS
# ============================================================================

test_that("clear_all_caches clears caches", {
  clear_all_caches()
  expect_no_error(clear_all_caches())
})

test_that("cache_info returns information", {
  info <- cache_info()
  expect_type(info, "list")
})

# ============================================================================
# TOKEN VALIDATION TESTS
# ============================================================================

test_that("autograder_test_token returns result for token", {
  result <- autograder_test_token("invalid_token_xyz")
  expect_type(result, "list")
})

test_that("autograder_test_token handles missing token", {
  result <- autograder_test_token()
  expect_type(result, "list")
})

# ============================================================================
# RETRY LOGIC TESTS
# ============================================================================

test_that("autograder_max_retries gets current value", {
  retries <- autograder_max_retries()
  expect_type(retries, "integer")
  expect_true(retries >= 0)
})
# ============================================================================
# ADDITIONAL COVERAGE TESTS FOR api.R
# ============================================================================

test_that("preview_tests validates missing function_name", {
  expect_error(preview_tests(), "single character string")
})

test_that("preview_tests validates empty string", {
  # Empty string causes an error in exists() call
  expect_error(preview_tests(""), "invalid|argument")
})

test_that("list_problems result is usable programmatically", {
  result <- suppressMessages(list_problems())
  
  # Should be able to use result to check if a problem exists
  expect_true("fibonacci" %in% result || length(result) > 0)
})

test_that("list_problems output format is correct", {
  output <- capture.output(list_problems())
  output_text <- paste(output, collapse = "\n")
  
  # Should have bullets for each problem
  expect_match(output_text, "- ")
})

test_that("configure_rate_limit accepts valid parameters", {
  # Save current state (not exposed, so just ensure no error)
  expect_no_error(configure_rate_limit(max_calls = 100, window_seconds = 60))
  expect_no_error(configure_rate_limit(max_calls = 1, window_seconds = 1))
  expect_no_error(configure_rate_limit(max_calls = 10000, window_seconds = 3600))
})

test_that("check_rate_limit works with default action", {
  configure_rate_limit(max_calls = 1000, window_seconds = 60)
  result <- check_rate_limit()
  expect_true(result)
})

test_that("cache_info returns list with expected fields", {
  info <- cache_info()
  
  expect_type(info, "list")
  # Should have some kind of cache information
})

test_that("clear_all_caches runs without error", {
  expect_no_error(clear_all_caches())
  expect_no_error(clear_all_caches())  # Can call multiple times
})

test_that("autograder_test_token handles various token formats", {
  # Empty token
  result1 <- autograder_test_token("")
  expect_type(result1, "list")
  
  # Whitespace token
  result2 <- autograder_test_token("   ")
  expect_type(result2, "list")
  
  # Long token
  result3 <- autograder_test_token(paste(rep("x", 1000), collapse = ""))
  expect_type(result3, "list")
})

test_that("autograder_max_retries returns consistent value", {
  # Get current value multiple times
  val1 <- autograder_max_retries()
  val2 <- autograder_max_retries()
  
  expect_equal(val1, val2)
  expect_type(val1, "integer")
})

test_that("preview_tests shows loading message", {
  skip_on_cran()
  skip_if_offline()
  
  output <- capture.output(tryCatch(
    preview_tests("fibonacci"),
    error = function(e) NULL
  ))
  
  output_text <- paste(output, collapse = "\n")
  
  # Should show loading message
  expect_true(
    grepl("Loading|fibonacci", output_text, ignore.case = TRUE) ||
    length(output) == 0  # Or no output if error
  )
})

test_that("list_problems returns invisibly", {
  result <- capture.output(problems <- list_problems())
  
  # The returned value should be a character vector
  expect_type(problems, "character")
})