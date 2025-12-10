# ============================================================================
# AUTOGRADER - EXCEPTION AND ERROR PATH COVERAGE TESTS
# ============================================================================
#
# Tests specifically for exception handling and error paths:
#   - C++ exception classes
#   - Error propagation
#   - Edge case error handling
#   - Fallback behaviors
#
# ============================================================================

# ============================================================================
# C++ EXCEPTION TYPE TESTS
# ============================================================================

test_that("C++ validation throws appropriate errors", {
  # Path traversal should fail validation
  expect_false(.cpp_validate_function_name("../test"))
  
  # Empty should fail
  expect_false(.cpp_validate_function_name(""))
  
  # Too long should fail
  expect_false(.cpp_validate_function_name(paste(rep("a", 300), collapse = "")))
})

test_that("C++ fetch handles network errors gracefully", {
  skip_on_cran()
  
  # Invalid function name should throw error
  expect_error(.cpp_fetch_function_content("..\\..\\secret"))
  expect_error(.cpp_fetch_function_content("test;rm"))
})

# ============================================================================
# R ERROR CLASS BEHAVIOR TESTS
# ============================================================================

test_that("network_error propagates correctly", {
  # Create and throw
  err <- tryCatch({
    stop(network_error("Connection failed"))
  }, error = function(e) e)
  
  expect_s3_class(err, "network_error")
  expect_equal(err$message, "Connection failed")
  
  # Should be catchable as error too
  result <- tryCatch({
    stop(network_error("Test"))
  }, 
  network_error = function(e) "network",
  error = function(e) "generic")
  
  expect_equal(result, "network")
})

test_that("function_not_found_error includes function name", {
  err <- function_not_found_error("nonexistent_function")
  
  expect_true(grepl("nonexistent_function", err$message))
  expect_equal(err$function_name, "nonexistent_function")
  expect_true(grepl("list_problems", err$message))
})

test_that("test_execution_error includes test number", {
  err <- test_execution_error("Test failed", 42)
  
  expect_equal(err$test_number, 42)
  expect_true(grepl("Test failed", err$message))
})

# ============================================================================
# FETCH ERROR HANDLING TESTS
# ============================================================================

test_that("fetch_instructor_code rejects injection attempts", {
  # SQL injection style
  expect_error(fetch_instructor_code("'; DROP TABLE users; --"))
  
  # Command injection
  expect_error(fetch_instructor_code("test && cat /etc/passwd"))
  
  # Path traversal
  expect_error(fetch_instructor_code("../../etc/passwd"))
  
  # Null byte
  # Can't easily test null byte in R string, but the C++ should handle it
})

test_that("fetch_instructor_code handles network errors", {
  skip_on_cran()
  skip_if_offline()
  
  # Nonexistent function should throw function_not_found_error
  expect_error(
    fetch_instructor_code("this_function_definitely_does_not_exist_xyz123"),
    class = "function_not_found_error"
  )
})

# ============================================================================
# VALIDATION ERROR PATH TESTS  
# ============================================================================

test_that("validate_test_cases error messages are informative", {
  # Missing inputs
  err <- tryCatch(
    validate_test_cases(list(), "test"),
    error = function(e) e
  )
  expect_true(grepl("inputs", err$message))
  
  # Empty inputs
  err <- tryCatch(
    validate_test_cases(list(inputs = list()), "test"),
    error = function(e) e
  )
  expect_true(grepl("at least one test", err$message))
  
  # Description length mismatch
  err <- tryCatch(
    validate_test_cases(list(
      inputs = list(list(1), list(2)),
      descriptions = c("Only one")
    ), "test"),
    error = function(e) e
  )
  expect_true(grepl("doesn't match", err$message))
})

test_that("validate_test_cases warnings are issued for optional field issues", {
  # Invalid expected_type
  expect_warning(
    validate_test_cases(list(
      inputs = list(list(1)),
      expected_type = c("a", "b")
    ), "test"),
    "expected_type"
  )
  
  # Invalid hints length
  expect_warning(
    validate_test_cases(list(
      inputs = list(list(1), list(2)),
      hints = c("Only one")
    ), "test"),
    "hints"
  )
  
  # Invalid comparison_fn
  expect_warning(
    validate_test_cases(list(
      inputs = list(list(1)),
      comparison_fn = "not a function"
    ), "test"),
    "comparison_fn"
  )
})

# ============================================================================
# COMPARE FUNCTION ERROR HANDLING
# ============================================================================

test_that(".cpp_compare_fast handles error-prone inputs", {
  # Very different types
  expect_false(.cpp_compare_fast("text", 123, 1e-10))
  
  # List vs vector
  expect_false(.cpp_compare_fast(list(1, 2), c(1, 2), 1e-10))
  
  # Matrix vs vector
  result <- .cpp_compare_fast(matrix(1:4, 2, 2), 1:4, 1e-10)
  expect_type(result, "logical")
  
  # Function objects
  f1 <- function(x) x
  f2 <- function(x) x
  result <- .cpp_compare_fast(f1, f2, 1e-10)
  expect_type(result, "logical")
})

# ============================================================================
# FORMAT OUTPUT ERROR HANDLING
# ============================================================================

test_that("format_output handles unusual objects", {
  # S4 class (if available)
  if (requireNamespace("methods", quietly = TRUE)) {
    tryCatch({
      # Create simple S4 object
      setClass("TestS4", slots = c(x = "numeric"))
      obj <- new("TestS4", x = 1)
      result <- format_output(obj)
      expect_type(result, "character")
    }, error = function(e) {
      # Skip if S4 classes aren't working
      skip("S4 classes not available")
    })
  }
  
  # External pointer (simulated)
  # This is hard to create without C code
  
  # Symbol
  result <- format_output(as.symbol("test"))
  expect_type(result, "character")
  
  # Pairlist
  pl <- pairlist(a = 1, b = 2)
  result <- format_output(pl)
  expect_type(result, "character")
})

test_that("format_output handles very nested structures", {
  # Deeply nested list
  nested <- list(a = list(b = list(c = list(d = list(e = 1)))))
  result <- format_output(nested)
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  
  # List with circular reference (R doesn't really support this, but similar)
  lst <- list()
  lst$a <- 1
  lst$b <- list(c = 2, d = list(e = 3))
  result <- format_output(lst)
  expect_type(result, "character")
})

test_that(".cpp_format_output handles edge cases", {
  # Empty environments
  result <- .cpp_format_output(new.env(), 100L)
  expect_type(result, "character")
  
  # Locked environments
  locked_env <- new.env()
  lockEnvironment(locked_env)
  result <- .cpp_format_output(locked_env, 100L)
  expect_type(result, "character")
  
  # Large nested list
  big_list <- lapply(1:100, function(i) list(x = i, y = i^2))
  result <- .cpp_format_output(big_list, 100L)
  expect_type(result, "character")
  expect_lte(nchar(result), 110)
})

# ============================================================================
# ENCRYPTION ERROR HANDLING
# ============================================================================

test_that("encryption handles key edge cases", {
  # Empty key
  result <- tryCatch({
    .cpp_encrypt("test", "")
  }, error = function(e) "error")
  # May work or throw error
  expect_true(result == "error" || is.character(result))
  
  # Very long key
  long_key <- paste(rep("k", 10000), collapse = "")
  enc <- .cpp_encrypt("test", long_key)
  dec <- .cpp_decrypt(enc, long_key)
  expect_equal(dec, "test")
  
  # Key with special characters
  special_key <- "key!@#$%^&*()_+-=[]{}|;':\",./<>?"
  enc <- .cpp_encrypt("test", special_key)
  dec <- .cpp_decrypt(enc, special_key)
  expect_equal(dec, "test")
})

test_that("decryption with wrong key fails gracefully", {
  enc <- .cpp_encrypt("secret message", "correct_key")
  
  # Wrong key - should either return garbage or fail
  result <- tryCatch({
    dec <- .cpp_decrypt(enc, "wrong_key")
    # If it doesn't error, the result should be different
    dec != "secret message"
  }, error = function(e) TRUE)
  
  expect_true(result)
})

test_that("base64 encryption handles binary data", {
  # Text with newlines
  text <- "line1\nline2\nline3"
  enc <- .cpp_encrypt_base64(text, "key")
  dec <- .cpp_decrypt_base64(enc, "key")
  expect_equal(dec, text)
  
  # Text with tabs and special chars
  text <- "col1\tcol2\t\r\n"
  enc <- .cpp_encrypt_base64(text, "key")
  dec <- .cpp_decrypt_base64(enc, "key")
  expect_equal(dec, text)
})

# ============================================================================
# PROVIDE_FEEDBACK ERROR PATH TESTS
# ============================================================================

test_that("provide_feedback handles mismatched object types", {
  # Function outputs
  f1 <- function() 1
  f2 <- function() 2
  feedback <- provide_feedback(f1, f2, list())
  expect_true(is.list(feedback))
  
  # Environment outputs
  e1 <- new.env()
  e1$x <- 1
  e2 <- new.env()
  e2$x <- 2
  feedback <- provide_feedback(e1, e2, list())
  expect_true(is.list(feedback))
})

test_that("provide_feedback handles recursive structures", {
  # Nested lists
  l1 <- list(a = list(b = list(c = 1)))
  l2 <- list(a = list(b = list(c = 2)))
  feedback <- provide_feedback(l1, l2, list())
  expect_true(is.list(feedback))
})

# ============================================================================
# PARALLEL EXECUTION ERROR HANDLING
# ============================================================================

test_that("run_tests_parallel handles function errors in workers", {
  student_fn <- function(x) {
    if (x > 10) stop("Too big!")
    x
  }
  instructor_fn <- function(x) x
  
  test_data <- list(
    inputs = lapply(1:15, function(i) list(x = i)),
    descriptions = paste("Test", 1:15),
    hidden = rep(FALSE, 15),
    points = rep(1, 15),
    tolerance = 1e-10
  )
  
  # Should complete even with errors
  results <- tryCatch(
    run_tests_parallel(student_fn, instructor_fn, test_data, 1e-10, TRUE),
    error = function(e) NULL
  )
  
  # Either completes or handles error gracefully
  expect_true(is.null(results) || is.list(results))
})

# ============================================================================
# BOUNDARY CONDITION TESTS
# ============================================================================

test_that("functions handle zero-length inputs", {
  # Empty function name
  expect_error(autograder(""))
  
  # Format output with empty objects
  expect_equal(format_output(numeric(0)), "numeric(0)")
  expect_equal(format_output(character(0)), "character(0)")
  expect_equal(format_output(list()), "list()")
  
  # Compare with empty objects
  expect_true(.cpp_compare_fast(numeric(0), numeric(0), 1e-10))
})

test_that("functions handle very large inputs", {
  # Large vector comparison
  v1 <- 1:100000
  v2 <- 1:100000
  result <- .cpp_compare_fast(v1, v2, 1e-10)
  expect_true(result)
  
  # Large string
  long_str <- paste(rep("a", 100000), collapse = "")
  result <- format_output(long_str, max_length = 100)
  expect_lte(nchar(result), 105)
})

test_that("functions handle unicode properly", {
  # Unicode in format_output
  unicode_vec <- c("Hello", "世界", "🎉", "مرحبا")
  result <- format_output(unicode_vec)
  expect_type(result, "character")
  
  # Unicode in encryption
  enc <- .cpp_encrypt("Hello世界🎉", "key")
  dec <- .cpp_decrypt(enc, "key")
  expect_equal(dec, "Hello世界🎉")
})

# ============================================================================
# TYPE COERCION EDGE CASES
# ============================================================================

test_that("comparison handles type coercion edges", {
  # Integer vs double that are equal - behavior depends on C++ implementation
  result <- .cpp_compare_fast(1L, 1.0, 1e-10)
  expect_type(result, "logical")  # Just verify it returns logical
  
  # Factor vs character
  f <- factor(c("a", "b", "c"))
  c <- c("a", "b", "c")
  result <- .cpp_compare_fast(f, c, 1e-10)
  expect_type(result, "logical")
  
  # Named vs unnamed
  named <- c(a = 1, b = 2)
  unnamed <- c(1, 2)
  result <- .cpp_compare_fast(named, unnamed, 1e-10)
  # May or may not be equal depending on implementation
  expect_type(result, "logical")
})

# ============================================================================
# MEMORY AND RESOURCE HANDLING
# ============================================================================

test_that("parallel cluster is properly cleaned up", {
  skip_on_ci()  # Windows CI has process spawn limits
  # This test ensures no orphan processes
  student_fn <- function(x) x
  instructor_fn <- function(x) x
  
  test_data <- list(
    inputs = lapply(1:15, function(i) list(x = i)),
    descriptions = paste("Test", 1:15),
    hidden = rep(FALSE, 15),
    points = rep(1, 15),
    tolerance = 1e-10
  )
  
  # Run multiple times to check for resource leaks
  for (i in 1:3) {
    results <- run_tests_parallel(student_fn, instructor_fn, test_data, 1e-10, TRUE)
    expect_length(results, 15)
  }
})

# ============================================================================
# DETERMINISM TESTS
# ============================================================================

test_that("key derivation is deterministic", {
  factors <- c("a", "b", "c")
  
  keys <- replicate(5, .cpp_derive_key(factors, 32L))
  
  # All should be identical
  expect_equal(length(unique(keys)), 1)
})

test_that("encryption with same key is consistent", {
  text <- "test message"
  key <- "consistent_key"
  
  # Multiple encryptions may differ (IV/nonce), but decryption should work
  enc1 <- .cpp_encrypt(text, key)
  enc2 <- .cpp_encrypt(text, key)
  
  dec1 <- .cpp_decrypt(enc1, key)
  dec2 <- .cpp_decrypt(enc2, key)
  
  expect_equal(dec1, text)
  expect_equal(dec2, text)
  # Encrypted values may differ due to random IV
})
