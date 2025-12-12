# ============================================================================
# AUTOGRADER - FETCH AND EXTRACTION FUNCTION TESTS
# ============================================================================
#
# Tests for code fetching and extraction functions:
#   - get_curl_handle() / reset_curl_pool()
#   - clear_instructor_cache()
#   - extract_instructor_function()
#   - extract_test_cases()
#   - fetch_instructor_code()
#   - validate_test_cases()
#
# ============================================================================

# ============================================================================
# CURL HANDLE POOL TESTS
# ============================================================================

test_that("get_curl_handle returns a curl handle", {
  handle <- get_curl_handle()
  expect_true(inherits(handle, "curl_handle"))
})

test_that("get_curl_handle reuses handles", {
  # Get handle twice
  handle1 <- get_curl_handle()
  handle2 <- get_curl_handle()
  
  # Should be the same handle (reused from pool)
  expect_identical(handle1, handle2)
})

test_that("reset_curl_pool clears the handle", {
  # Get a handle first
  handle1 <- get_curl_handle()
  
  # Reset the pool
  result <- reset_curl_pool()
  expect_null(result)
  
  # Next call should create a new handle
  handle2 <- get_curl_handle()
  expect_true(inherits(handle2, "curl_handle"))
})

# ============================================================================
# INSTRUCTOR CACHE TESTS
# ============================================================================

test_that("clear_instructor_cache returns invisibly", {
  result <- clear_instructor_cache()
  expect_null(result)
})

test_that("clear_instructor_cache clears cache", {
  # Clear and verify no error
  expect_no_error(clear_instructor_cache())
})

# ============================================================================
# EXTRACT_INSTRUCTOR_FUNCTION TESTS
# ============================================================================

test_that("extract_instructor_function finds function in environment", {
  # Create test environment with a function
  env <- new.env()
  env$my_function <- function(x) x + 1
  
  result <- extract_instructor_function(env, "test")
  expect_true(is.function(result))
  expect_equal(result(5), 6)
})

test_that("extract_instructor_function takes first function found", {
  # Create environment with multiple functions
  env <- new.env()
  env$first_fn <- function(x) "first"
  env$second_fn <- function(x) "second"
  
  result <- extract_instructor_function(env, "test")
  expect_true(is.function(result))
  # Note: order depends on ls() which is alphabetical
})

test_that("extract_instructor_function errors when no function found", {
  # Create environment with no functions
  env <- new.env()
  env$data <- 1:10
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

test_that("extract_test_cases retrieves test_cases from environment", {
  # Create environment with valid test_cases
  env <- new.env()
  env$test_cases <- list(
    inputs = list(list(1), list(2), list(3)),
    descriptions = c("test 1", "test 2", "test 3")
  )
  
  result <- extract_test_cases(env, "test")
  expect_type(result, "list")
  expect_true("inputs" %in% names(result))
})

test_that("extract_test_cases errors when test_cases missing", {
  env <- new.env()
  env$some_data <- 1:10
  
  expect_error(
    extract_test_cases(env, "test_function"),
    "No test cases found"
  )
})

test_that("extract_test_cases calls validate_test_cases", {
  # Create environment with invalid test_cases (missing inputs)
  env <- new.env()
  env$test_cases <- list(
    descriptions = c("test 1", "test 2")
    # missing inputs
  )
  
  expect_error(
    extract_test_cases(env, "test"),
    "inputs"
  )
})

# ============================================================================
# FETCH_INSTRUCTOR_CODE ERROR HANDLING TESTS
# ============================================================================

test_that("fetch_instructor_code validates function name", {
  # Invalid characters should be rejected
  expect_error(
    fetch_instructor_code("../../../etc/passwd"),
    "Invalid|not found"
  )
})

test_that("fetch_instructor_code handles non-existent function", {
  skip_if_offline()
  
  expect_error(
    fetch_instructor_code("this_function_definitely_does_not_exist_12345"),
    "not found|Function|does not exist"
  )
})

test_that("fetch_instructor_code uses cache when use_cache=TRUE", {
  # Clear cache first
  clear_instructor_cache()
  
  # The actual function would use cache after first fetch
  # We can't easily test the caching without actual network calls
  # Just verify the parameter is accepted
  expect_error(
    fetch_instructor_code("nonexistent_test_function", use_cache = TRUE),
    "not found|Function"
  )
})

test_that("fetch_instructor_code respects max_retries parameter", {
  # With max_retries = 1, should fail faster on invalid function
  expect_error(
    fetch_instructor_code("nonexistent_xyz", use_cache = FALSE, max_retries = 1),
    "not found|Function"
  )
})

# ============================================================================
# VALIDATE_TEST_CASES TESTS (additional coverage)
# ============================================================================

test_that("validate_test_cases handles minimal valid input", {
  tc <- list(
    inputs = list(list(1), list(2))
  )
  
  result <- validate_test_cases(tc, "test_fn")
  expect_type(result, "list")
  expect_equal(length(result$inputs), 2)
})

test_that("validate_test_cases adds default descriptions", {
  tc <- list(
    inputs = list(list(1), list(2))
  )
  
  result <- validate_test_cases(tc, "test_fn")
  expect_equal(length(result$descriptions), 2)
})

test_that("validate_test_cases adds default hidden flags", {
  tc <- list(
    inputs = list(list(1), list(2), list(3))
  )
  
  result <- validate_test_cases(tc, "test_fn")
  expect_equal(length(result$hidden), 3)
  expect_true(all(result$hidden == FALSE))
})

test_that("validate_test_cases adds default points", {
  tc <- list(
    inputs = list(list(1), list(2))
  )
  
  result <- validate_test_cases(tc, "test_fn")
  expect_equal(length(result$points), 2)
  expect_true(all(result$points == 1))  # default 1 point each
})

test_that("validate_test_cases preserves custom values", {
  tc <- list(
    inputs = list(list(1), list(2)),
    descriptions = c("first test", "second test"),
    hidden = c(FALSE, TRUE),
    points = c(5, 10)
  )
  
  result <- validate_test_cases(tc, "test_fn")
  expect_equal(result$descriptions, c("first test", "second test"))
  expect_equal(result$hidden, c(FALSE, TRUE))
  expect_equal(result$points, c(5, 10))
})

test_that("validate_test_cases errors on empty inputs", {
  tc <- list(
    inputs = list()
  )
  
  expect_error(validate_test_cases(tc, "test_fn"), "at least one")
})

test_that("validate_test_cases errors on mismatched lengths", {
  tc <- list(
    inputs = list(list(1), list(2), list(3)),
    descriptions = c("only one")  # should have 3
  )
  
  expect_error(validate_test_cases(tc, "test_fn"), "length|mismatch|descriptions")
})

# ============================================================================
# HELPER FUNCTION FOR OFFLINE TESTING
# ============================================================================

test_that("curl has_internet helper works", {
  # Just verify the function exists and returns logical
  result <- curl::has_internet()
  expect_type(result, "logical")
})
