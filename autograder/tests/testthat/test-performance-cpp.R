# ============================================================================
# Test Suite for Performance C++ Module
# ============================================================================
# Tests the C++ implementations of performance-critical functions:
# - cpp_compare_case_insensitive: Case-insensitive string comparison
# - cpp_compare_set: Set comparison with tolerance
# - cpp_validate_test_cases: Test case validation
# ============================================================================

test_that("cpp_compare_case_insensitive works correctly", {
  # Skip if C++ functions not available
  skip_if_not(exists(".cpp_compare_case_insensitive", envir = asNamespace("autograder")),
              "C++ performance functions not available")
  
  # Test exact match
  expect_true(.cpp_compare_case_insensitive("hello", "hello"))
  
  # Test case insensitivity
  expect_true(.cpp_compare_case_insensitive("HELLO", "hello"))
  expect_true(.cpp_compare_case_insensitive("Hello", "HELLO"))
  expect_true(.cpp_compare_case_insensitive("HeLLo WoRLd", "hello world"))
  
  # Test with vectors
  expect_true(.cpp_compare_case_insensitive(
    c("Apple", "BANANA", "cherry"),
    c("apple", "banana", "CHERRY")
  ))
  
  # Test mismatch
  expect_false(.cpp_compare_case_insensitive("hello", "world"))
  expect_false(.cpp_compare_case_insensitive(c("a", "b"), c("a", "c")))
  
  # Test length mismatch
  expect_false(.cpp_compare_case_insensitive(c("a", "b"), c("a")))
})

test_that("cpp_compare_set works correctly", {
  # Skip if C++ functions not available
  skip_if_not(exists(".cpp_compare_set", envir = asNamespace("autograder")),
              "C++ performance functions not available")
  
  # Test with integers
  expect_true(.cpp_compare_set(c(1L, 2L, 3L), c(3L, 1L, 2L)))
  expect_false(.cpp_compare_set(c(1L, 2L, 3L), c(1L, 2L, 4L)))
  
  # Test with numeric (tolerance)
  expect_true(.cpp_compare_set(c(1.0, 2.0, 3.0), c(3.0, 1.0, 2.0)))
  expect_true(.cpp_compare_set(
    c(1.0, 2.0, 3.0),
    c(1.0000001, 2.0, 3.0),
    tolerance = 1e-6
  ))
  
  # Test with character
  expect_true(.cpp_compare_set(c("a", "b", "c"), c("c", "a", "b")))
  expect_false(.cpp_compare_set(c("a", "b"), c("a", "c")))
  
  # Test length mismatch
  expect_false(.cpp_compare_set(c(1, 2, 3), c(1, 2)))
  
  # Test empty vectors
  expect_true(.cpp_compare_set(integer(0), integer(0)))
})

# ============================================================================
# TEST CASE VALIDATION (Pure R implementation)
# ============================================================================
# Note: validate_test_cases is now pure R for maintainability.
# These tests verify the R implementation works correctly.

test_that("validate_test_cases works correctly", {
  # Valid test data
  valid_data <- list(
    inputs = list(list(x = 1), list(x = 2)),
    descriptions = c("test 1", "test 2")
  )
  
  result <- validate_test_cases(valid_data, "test_function")
  expect_true(is.list(result))
  expect_equal(length(result$inputs), 2)
  
  # Test with missing inputs - should error
  invalid_data <- list(
    expected = list(1, 2),
    description = c("test 1", "test 2")
  )
  
  expect_error(validate_test_cases(invalid_data, "test_function"), "missing required 'inputs'")
  
  # Test with mismatched lengths - should error for descriptions
  mismatched_data <- list(
    inputs = list(list(x = 1), list(x = 2), list(x = 3)),
    descriptions = c("test 1", "test 2")  # Only 2 descriptions for 3 inputs
  )
  
  expect_error(validate_test_cases(mismatched_data, "test_function"), "doesn't match inputs length")
})

test_that("quick_validate_inputs works correctly", {
  # Valid data with inputs
  valid_data <- list(inputs = list(list(x = 1)))
  expect_true(quick_validate_inputs(valid_data))
  
  # Invalid - missing inputs
  invalid_data <- list(expected = list(1))
  expect_false(quick_validate_inputs(invalid_data))
  
  # Invalid - inputs not a list
  invalid_data2 <- list(inputs = "not a list")
  expect_false(quick_validate_inputs(invalid_data2))
  
  # Invalid - empty inputs
  empty_inputs <- list(inputs = list())
  expect_false(quick_validate_inputs(empty_inputs))
})

test_that("get_test_count works correctly", {
  # Test with valid data
  data <- list(inputs = list(list(x = 1), list(x = 2), list(x = 3)))
  expect_equal(get_test_count(data), 3L)
  
  # Test with empty inputs
  empty_data <- list(inputs = list())
  expect_equal(get_test_count(empty_data), 0L)
  
  # Test with no inputs field (returns -1 to indicate invalid)
  no_inputs <- list(expected = list(1, 2))
  expect_equal(get_test_count(no_inputs), -1L)
})

test_that("validate_field_lengths works correctly", {
  # Valid - all fields match
  valid_data <- list(
    inputs = list(list(x = 1), list(x = 2)),
    descriptions = c("test 1", "test 2"),
    hidden = c(FALSE, FALSE),
    points = c(1, 1)
  )
  result <- validate_field_lengths(valid_data, 2L)
  expect_true(all(result))  # All fields should be valid
  
  # Invalid - descriptions length mismatch
  invalid_data <- list(
    inputs = list(list(x = 1), list(x = 2)),
    descriptions = c("test 1")  # Only 1 description
  )
  result <- validate_field_lengths(invalid_data, 2L)
  expect_false(result["descriptions"])  # descriptions field should fail
})

test_that("validate_test_cases generates defaults correctly", {
  # Minimal data - should generate all defaults
  minimal_data <- list(inputs = list(list(x = 1), list(x = 2)))
  
  result <- validate_test_cases(minimal_data, "test_function")
  
  # Check defaults were generated

  expect_equal(result$descriptions, c("Test 1", "Test 2"))
  expect_equal(result$hidden, c(FALSE, FALSE))
  expect_equal(result$points, c(1, 1))
  expect_equal(result$tolerance, 1e-10)
})

test_that("Performance C++ functions integrate with R functions", {
  # Skip if C++ functions not available
  skip_if_not(exists(".cpp_compare_case_insensitive", envir = asNamespace("autograder")),
              "C++ performance functions not available")
  
  # Test compare_case_insensitive wrapper
  result <- compare_case_insensitive("HELLO", "hello")
  expect_true(result)
  
  # Test compare_set wrapper
  result <- compare_set(c(1, 2, 3), c(3, 1, 2))
  expect_true(result)
})

test_that("Performance C++ handles edge cases", {
  # Skip if C++ functions not available
  skip_if_not(exists(".cpp_compare_case_insensitive", envir = asNamespace("autograder")),
              "C++ performance functions not available")
  
  # Empty strings
  expect_true(.cpp_compare_case_insensitive("", ""))
  
  # Single character
  expect_true(.cpp_compare_case_insensitive("A", "a"))
  
  # Unicode (may not be fully supported in base C++)
  # Skip unicode tests if they fail
  tryCatch({
    result <- .cpp_compare_case_insensitive("café", "CAFÉ")
    # Result may vary based on locale
  }, error = function(e) {
    skip("Unicode case comparison not supported")
  })
})
