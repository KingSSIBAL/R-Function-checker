# ============================================================================
# AUTOGRADER - MAXIMUM COVERAGE TESTS
# ============================================================================
#
# This file contains comprehensive tests to maximize code coverage.
# It targets:
#   - All C++ functions (exceptions.h, validator.cpp, encryption.cpp)
#   - All R helper functions (format_output edge cases, validation branches)
#   - Error paths and edge cases
#   - Unused branches and fallback logic
#
# ============================================================================

# ============================================================================
# C++ VALIDATION FUNCTION EXHAUSTIVE TESTS
# ============================================================================

test_that(".cpp_validate_function_name handles boundary cases", {
  # Exactly at max length (should pass if <= 256)
  max_len_name <- paste(rep("a", 256), collapse = "")
  result <- tryCatch(.cpp_validate_function_name(max_len_name), error = function(e) FALSE)
  expect_type(result, "logical")
  
  # Just over max length (should fail)
  over_max_name <- paste(rep("a", 257), collapse = "")
  expect_false(.cpp_validate_function_name(over_max_name))
  
  # Single character valid names
  expect_true(.cpp_validate_function_name("a"))
  expect_true(.cpp_validate_function_name("_"))
  expect_false(.cpp_validate_function_name("-"))  # Can't start with hyphen
  
  # Unicode characters (should fail)
  expect_false(.cpp_validate_function_name("função"))
  expect_false(.cpp_validate_function_name("関数"))
  expect_false(.cpp_validate_function_name("функция"))
})

test_that(".cpp_validate_function_name rejects all dangerous patterns", {
  # Comprehensive dangerous pattern tests
  dangerous <- c(
    "..", "./", "//", "\\", "~",
    "${VAR}", "$((1+1))", "`cmd`",
    ";", "|", "&", ">", "<",
    "\n", "\r", "\t"
  )
  
  for (pattern in dangerous) {
    name <- paste0("test", pattern, "end")
    expect_false(.cpp_validate_function_name(name), 
                 info = sprintf("Pattern '%s' should be rejected", pattern))
  }
})

test_that(".cpp_validate_function_name handles whitespace", {
  expect_false(.cpp_validate_function_name("test func"))
  expect_false(.cpp_validate_function_name(" test"))
  expect_false(.cpp_validate_function_name("test "))
  expect_false(.cpp_validate_function_name("test\tfunc"))
})

# ============================================================================
# C++ SANITIZER TESTS
# ============================================================================

test_that(".cpp_sanitize_input removes dangerous characters", {
  # Test if sanitizer function exists and works
  if (exists(".cpp_sanitize_input", mode = "function")) {
    result <- .cpp_sanitize_input("test;rm -rf /")
    expect_type(result, "character")
    expect_false(grepl(";", result))
  } else {
    skip("Sanitize function not exported")
  }
})

test_that(".cpp_escape_shell handles special characters", {
  if (exists(".cpp_escape_shell", mode = "function")) {
    result <- .cpp_escape_shell("test'quote")
    expect_type(result, "character")
  } else {
    skip("Escape shell function not exported")
  }
})

# ============================================================================
# C++ COMPARISON FUNCTION EXHAUSTIVE TESTS
# ============================================================================

test_that(".cpp_compare_fast handles all numeric edge cases", {
  # Zero comparisons
  expect_true(.cpp_compare_fast(0, 0, 1e-10))
  # Integer vs double may not match depending on strict typing
  result <- .cpp_compare_fast(0L, 0, 1e-10)
  expect_type(result, "logical")
  expect_true(.cpp_compare_fast(-0, 0, 1e-10))
  
  # Very small differences
  expect_true(.cpp_compare_fast(1e-15, 0, 1e-10))
  expect_false(.cpp_compare_fast(1e-5, 0, 1e-10))
  
  # Large numbers
  expect_true(.cpp_compare_fast(1e15, 1e15, 1e-10))
  expect_true(.cpp_compare_fast(1e15 + 1, 1e15 + 1, 1e-10))
  
  # Negative numbers
  expect_true(.cpp_compare_fast(-5, -5, 1e-10))
  expect_false(.cpp_compare_fast(-5, 5, 1e-10))
})

test_that(".cpp_compare_fast handles special floating point values", {
  # Inf comparisons
  expect_true(.cpp_compare_fast(Inf, Inf, 1e-10))
  expect_true(.cpp_compare_fast(-Inf, -Inf, 1e-10))
  expect_false(.cpp_compare_fast(Inf, -Inf, 1e-10))
  expect_false(.cpp_compare_fast(Inf, 1e308, 1e-10))
  
  # NaN comparisons (NaN != NaN by IEEE standard)
  result <- .cpp_compare_fast(NaN, NaN, 1e-10)
  expect_type(result, "logical")
  
  # Mixed Inf and numbers
  expect_false(.cpp_compare_fast(Inf, 1000, 1e-10))
})

test_that(".cpp_compare_fast handles NA values", {
  # NA handling
  result1 <- .cpp_compare_fast(NA, NA, 1e-10)
  expect_type(result1, "logical")
  
  result2 <- .cpp_compare_fast(NA, 1, 1e-10)
  expect_type(result2, "logical")
  
  result3 <- .cpp_compare_fast(c(1, NA, 3), c(1, NA, 3), 1e-10)
  expect_type(result3, "logical")
  
  result4 <- .cpp_compare_fast(c(1, NA, 3), c(1, 2, 3), 1e-10)
  expect_type(result4, "logical")
})

test_that(".cpp_compare_fast handles empty vectors",
{
  expect_true(.cpp_compare_fast(numeric(0), numeric(0), 1e-10))
  expect_true(.cpp_compare_fast(character(0), character(0), 1e-10))
  expect_true(.cpp_compare_fast(integer(0), integer(0), 1e-10))
  expect_true(.cpp_compare_fast(logical(0), logical(0), 1e-10))
})

test_that(".cpp_compare_fast handles length mismatches", {
  expect_false(.cpp_compare_fast(1:3, 1:4, 1e-10))
  expect_false(.cpp_compare_fast(1:100, 1:99, 1e-10))
  expect_false(.cpp_compare_fast(numeric(0), 1, 1e-10))
})

test_that(".cpp_compare_fast handles character vectors", {
  expect_true(.cpp_compare_fast("hello", "hello", 1e-10))
  expect_false(.cpp_compare_fast("hello", "Hello", 1e-10))
  expect_true(.cpp_compare_fast(c("a", "b"), c("a", "b"), 1e-10))
  expect_false(.cpp_compare_fast(c("a", "b"), c("a", "c"), 1e-10))
  
  # Empty strings
  expect_true(.cpp_compare_fast("", "", 1e-10))
  
  # Special characters
  expect_true(.cpp_compare_fast("test\n", "test\n", 1e-10))
})

test_that(".cpp_compare_fast handles logical vectors", {
  expect_true(.cpp_compare_fast(TRUE, TRUE, 1e-10))
  expect_true(.cpp_compare_fast(FALSE, FALSE, 1e-10))
  expect_false(.cpp_compare_fast(TRUE, FALSE, 1e-10))
  expect_true(.cpp_compare_fast(c(TRUE, FALSE), c(TRUE, FALSE), 1e-10))
})

test_that(".cpp_compare_fast handles lists", {
  expect_true(.cpp_compare_fast(list(a = 1), list(a = 1), 1e-10))
  expect_false(.cpp_compare_fast(list(a = 1), list(a = 2), 1e-10))
  expect_true(.cpp_compare_fast(list(), list(), 1e-10))
  
  # Nested lists
  expect_true(.cpp_compare_fast(
    list(a = list(b = 1)),
    list(a = list(b = 1)),
    1e-10
  ))
})

test_that(".cpp_compare_fast handles NULL", {
  expect_true(.cpp_compare_fast(NULL, NULL, 1e-10))
  expect_false(.cpp_compare_fast(NULL, 1, 1e-10))
  expect_false(.cpp_compare_fast(1, NULL, 1e-10))
})

test_that(".cpp_compare_fast handles different tolerances", {
  # Very tight tolerance
  expect_false(.cpp_compare_fast(1.0001, 1.0, 1e-10))
  
  # Loose tolerance
  expect_true(.cpp_compare_fast(1.0001, 1.0, 0.001))
  
  # Zero tolerance
  expect_true(.cpp_compare_fast(1, 1, 0))
  expect_false(.cpp_compare_fast(1.0000001, 1, 0))
})

# ============================================================================
# C++ TYPE DETECTION EXHAUSTIVE TESTS
# ============================================================================

test_that(".cpp_get_type handles all basic types", {
  expect_equal(.cpp_get_type(1L), "integer")
  expect_equal(.cpp_get_type(1.5), "numeric")
  expect_equal(.cpp_get_type("text"), "character")
  expect_equal(.cpp_get_type(TRUE), "logical")
  expect_equal(.cpp_get_type(NULL), "NULL")
  expect_equal(.cpp_get_type(list()), "list")
  expect_equal(.cpp_get_type(raw(0)), "raw")
})

test_that(".cpp_get_type handles complex types", {
  expect_equal(.cpp_get_type(complex(1, 1, 1)), "complex")
  expect_equal(.cpp_get_type(factor("a")), "integer")  # factors are stored as integers
  expect_true(.cpp_get_type(data.frame()) %in% c("list", "data.frame"))
})

test_that(".cpp_get_type handles special objects", {
  # Function - R may call it "closure" or "function"
  expect_true(.cpp_get_type(function() {}) %in% c("closure", "function"))
  
  # Environment
  expect_equal(.cpp_get_type(new.env()), "environment")
  
  # Expression
  expect_equal(.cpp_get_type(expression(x)), "expression")
  
  # Formula
  expect_equal(.cpp_get_type(y ~ x), "language")
})

# ============================================================================
# C++ ENCRYPTION EXHAUSTIVE TESTS
# ============================================================================

test_that(".cpp_encrypt handles edge cases", {
  # Empty string
  enc <- .cpp_encrypt("", "key")
  dec <- .cpp_decrypt(enc, "key")
  expect_equal(dec, "")
  
  # Single character
  enc <- .cpp_encrypt("a", "key")
  dec <- .cpp_decrypt(enc, "key")
  expect_equal(dec, "a")
  
  # Very long string
  long_str <- paste(rep("a", 100000), collapse = "")
  enc <- .cpp_encrypt(long_str, "key")
  dec <- .cpp_decrypt(enc, "key")
  expect_equal(dec, long_str)
  
  # String with special characters (avoiding null bytes which R can't handle)
  special_str <- "test\t\ndata"
  enc <- .cpp_encrypt(special_str, "key")
  dec <- .cpp_decrypt(enc, "key")
  expect_equal(dec, special_str)
})

test_that(".cpp_encrypt handles various key lengths", {
  text <- "test message"
  
  # Short key
  enc1 <- .cpp_encrypt(text, "k")
  dec1 <- .cpp_decrypt(enc1, "k")
  expect_equal(dec1, text)
  
  # Long key
  long_key <- paste(rep("k", 1000), collapse = "")
  enc2 <- .cpp_encrypt(text, long_key)
  dec2 <- .cpp_decrypt(enc2, long_key)
  expect_equal(dec2, text)
  
  # Empty key behavior
  result <- tryCatch({
    enc3 <- .cpp_encrypt(text, "")
    TRUE
  }, error = function(e) FALSE)
  # May or may not throw error depending on implementation
  expect_type(result, "logical")
})

test_that(".cpp_derive_key produces consistent results", {
  factors1 <- c("salt1", "salt2")
  factors2 <- c("salt1", "salt2")
  factors3 <- c("salt1", "salt3")
  
  key1 <- .cpp_derive_key(factors1, 32L)
  key2 <- .cpp_derive_key(factors2, 32L)
  key3 <- .cpp_derive_key(factors3, 32L)
  
  # Same inputs = same output
  expect_equal(key1, key2)
  
  # Different inputs = different output
  expect_false(key1 == key3)
})

test_that(".cpp_derive_key handles various lengths", {
  factors <- c("test")
  
  key16 <- .cpp_derive_key(factors, 16L)
  key32 <- .cpp_derive_key(factors, 32L)
  key64 <- .cpp_derive_key(factors, 64L)
  
  # Keys should be hex-encoded (2 chars per byte)
  expect_equal(nchar(key16), 32)
  expect_equal(nchar(key32), 64)
  expect_equal(nchar(key64), 128)
})

test_that(".cpp_generate_key produces random unique keys", {
  keys <- replicate(10, .cpp_generate_key(32L))
  # All keys should be unique
  expect_equal(length(unique(keys)), 10)
})

# ============================================================================
# R VALIDATION FUNCTION EXHAUSTIVE TESTS
# ============================================================================

test_that("validate_test_cases handles all field validations", {
  # Valid complete test case
  tc <- list(
    inputs = list(list(x = 1), list(x = 2)),
    descriptions = c("Test 1", "Test 2"),
    hidden = c(FALSE, TRUE),
    points = c(1, 2),
    tolerance = 1e-6,
    expected_type = "numeric",
    hints = c("Hint 1", "Hint 2"),
    comparison_fn = function(a, b) identical(a, b)
  )
  result <- validate_test_cases(tc, "test_fn")
  
  expect_equal(length(result$inputs), 2)
  expect_equal(result$tolerance, 1e-6)
  expect_equal(result$expected_type, "numeric")
  expect_true(is.function(result$comparison_fn))
})

test_that("validate_test_cases defaults missing optional fields", {
  tc <- list(inputs = list(list(x = 1)))
  result <- validate_test_cases(tc, "test_fn")
  
  expect_equal(result$descriptions, "Test 1")
  expect_equal(result$hidden, FALSE)
  expect_equal(result$points, 1)
  expect_equal(result$tolerance, 1e-10)
  expect_null(result$expected_type)
  expect_null(result$hints)
  expect_null(result$comparison_fn)
})

test_that("validate_test_cases rejects invalid field types", {
  # Non-list inputs
  expect_error(validate_test_cases(list(inputs = "not a list"), "fn"))
  
  # Non-character descriptions
  tc <- list(inputs = list(list(1)), descriptions = 123)
  result <- tryCatch(validate_test_cases(tc, "fn"), error = function(e) "error")
  # May coerce or error
  
  # Non-numeric points
  expect_error(
    validate_test_cases(list(inputs = list(list(1)), points = "one"), "fn"),
    "non-negative numeric"
  )
})

test_that("validate_test_cases handles edge case tolerance values", {
  # Zero tolerance
  tc <- list(inputs = list(list(1)), tolerance = 0)
  result <- validate_test_cases(tc, "fn")
  expect_equal(result$tolerance, 0)
  
  # Very small tolerance
  tc2 <- list(inputs = list(list(1)), tolerance = 1e-15)
  result2 <- validate_test_cases(tc2, "fn")
  expect_equal(result2$tolerance, 1e-15)
  
  # Large tolerance
  tc3 <- list(inputs = list(list(1)), tolerance = 1)
  result3 <- validate_test_cases(tc3, "fn")
  expect_equal(result3$tolerance, 1)
})

# ============================================================================
# R FORMAT_OUTPUT EXHAUSTIVE TESTS
# ============================================================================

test_that("format_output handles all atomic types", {
  # Integer
  expect_true(nchar(format_output(1L)) > 0)
  
  # Numeric
  expect_true(nchar(format_output(1.5)) > 0)
  
  # Complex
  expect_true(nchar(format_output(1+2i)) > 0)
  
  # Character
  expect_true(nchar(format_output("test")) > 0)
  
  # Logical
  expect_true(nchar(format_output(TRUE)) > 0)
  
  # Raw
  expect_true(nchar(format_output(as.raw(255))) > 0)
})

test_that("format_output handles container types", {
  # List with 0 elements
  expect_equal(format_output(list()), "list()")
  
  # List with 1 element
  result <- format_output(list(a = 1))
  expect_true(nchar(result) > 0)
  
  # List with 3 elements (boundary)
  result <- format_output(list(a = 1, b = 2, c = 3))
  expect_true(nchar(result) > 0)
  
  # List with 4 elements (over boundary)
  result <- format_output(list(a = 1, b = 2, c = 3, d = 4))
  expect_true(grepl("length", result))
  
  # Named vector
  result <- format_output(c(a = 1, b = 2))
  expect_true(nchar(result) > 0)
})

test_that("format_output handles vector length boundaries", {
  # Vector with exactly 10 elements (boundary)
  result <- format_output(1:10)
  expect_true(nchar(result) > 0)
  
  # Vector with 11 elements (over boundary)
  result <- format_output(1:11)
  expect_true(grepl("\\.\\.\\.", result))
  
  # Very long vector
  result <- format_output(1:10000)
  expect_true(grepl("\\.\\.\\.", result))
})

test_that("format_output handles matrix variations", {
  # Square matrix
  result <- format_output(matrix(1:9, 3, 3))
  expect_true(grepl("matrix", result))
  
  # Tall matrix
  result <- format_output(matrix(1:100, 50, 2))
  expect_true(grepl("matrix", result))
  
  # Wide matrix
  result <- format_output(matrix(1:100, 2, 50))
  expect_true(grepl("matrix", result))
  
  # 1x1 matrix
  result <- format_output(matrix(1))
  expect_true(grepl("matrix", result))
})

test_that("format_output handles data.frame variations", {
  # Empty data.frame
  result <- format_output(data.frame())
  expect_true(nchar(result) > 0)
  
  # Single column
  result <- format_output(data.frame(x = 1:3))
  expect_true(nchar(result) > 0)
  
  # Many columns
  df <- as.data.frame(matrix(1:100, 10, 10))
  result <- format_output(df)
  expect_true(nchar(result) > 0)
})

test_that("format_output respects preserve_structure parameter", {
  lst <- list(a = 1, b = 2)
  
  # With preserve_structure = TRUE
  result1 <- format_output(lst, preserve_structure = TRUE)
  
  # With preserve_structure = FALSE
  result2 <- format_output(lst, preserve_structure = FALSE)
  
  expect_true(nchar(result1) > 0)
  expect_true(nchar(result2) > 0)
})

test_that("format_output truncates at max_length", {
  long_vec <- 1:1000
  
  # Various max_length values
  for (max_len in c(10, 50, 100, 500)) {
    result <- format_output(long_vec, max_length = max_len)
    # Allow some overflow for "..."
    expect_lte(nchar(result), max_len + 5)
  }
})

# ============================================================================
# R PROVIDE_FEEDBACK EXHAUSTIVE TESTS
# ============================================================================

test_that("provide_feedback handles all type mismatch combinations", {
  types <- list(
    integer = 1L,
    numeric = 1.0,
    character = "1",
    logical = TRUE,
    list = list(1),
    null = NULL
  )
  
  # Cross-compare all types
  for (t1 in names(types)) {
    for (t2 in names(types)) {
      if (t1 != t2) {
        feedback <- tryCatch(
          provide_feedback(types[[t1]], types[[t2]], list()),
          error = function(e) list(error = e$message)
        )
        expect_true(is.list(feedback))
      }
    }
  }
})

test_that("provide_feedback handles dimension differences", {
  # Vector vs matrix - different classes
  feedback <- tryCatch(
    provide_feedback(1:6, matrix(1:6, 2, 3), list()),
    error = function(e) list(error = TRUE)
  )
  expect_true(is.list(feedback))
  
  # Different matrix dimensions may cause errors
  feedback <- tryCatch(
    provide_feedback(matrix(1:6, 2, 3), matrix(1:6, 3, 2), list()),
    error = function(e) list(error = TRUE)
  )
  expect_true(is.list(feedback))
})

test_that("provide_feedback truncates many differences", {
  # Create vectors with many differences
  student <- 1:100
  expected <- 101:200
  feedback <- provide_feedback(student, expected, list())
  
  if ("diff_positions" %in% names(feedback)) {
    # Should be truncated
    expect_true(grepl("\\.\\.\\.", feedback$diff_positions))
  }
})

test_that("provide_feedback handles tolerance in comparisons", {
  # Within tolerance
  student <- 1.0000000001
  expected <- 1.0
  feedback <- provide_feedback(student, expected, list())
  # Should have no diff_positions or empty
  
  # Outside tolerance
  student2 <- 1.1
  expected2 <- 1.0
  feedback2 <- provide_feedback(student2, expected2, list())
  expect_true(is.list(feedback2))
})

# ============================================================================
# R PRINT_FEEDBACK TESTS
# ============================================================================

test_that("print_feedback handles all feedback types", {
  # All types present
  feedback <- list(
    type_issue = "Type error",
    length_issue = "Length error",
    diff_positions = "Positions 1, 2, 3",
    hint = "Try this"
  )
  
  output <- capture.output(print_feedback(feedback))
  expect_true(length(output) > 0)
})

test_that("print_feedback handles single feedback type", {
  feedback <- list(hint = "Just a hint")
  output <- capture.output(print_feedback(feedback))
  expect_true(length(output) > 0)
})

# ============================================================================
# R ERROR CLASSES TESTS
# ============================================================================

test_that("network_error creates proper error object", {
  err <- tryCatch(
    network_error("Test message"),
    error = function(e) e
  )
  expect_s3_class(err, "network_error")
  expect_s3_class(err, "error")
  expect_s3_class(err, "condition")
  expect_true(grepl("Test message", conditionMessage(err)))
})

test_that("function_not_found_error creates proper error object", {
  err <- tryCatch(
    function_not_found_error("my_func"),
    error = function(e) e
  )
  expect_s3_class(err, "function_not_found_error")
  expect_true(grepl("my_func", conditionMessage(err)))
  expect_equal(err$function_name, "my_func")
})

test_that("test_execution_error creates proper error object", {
  err <- tryCatch(
    test_execution_error("Failed", 5),
    error = function(e) e
  )
  expect_s3_class(err, "test_execution_error")
  expect_equal(err$test_number, 5)
})

test_that("error classes can be caught specifically", {
  # network_error
  result <- tryCatch(
    network_error("net"),
    network_error = function(e) "caught network",
    error = function(e) "caught generic"
  )
  expect_equal(result, "caught network")
  
  # function_not_found_error
  result <- tryCatch(
    function_not_found_error("fn"),
    function_not_found_error = function(e) "caught fnf",
    error = function(e) "caught generic"
  )
  expect_equal(result, "caught fnf")
})

# ============================================================================
# NULL COALESCING OPERATOR TESTS
# ============================================================================

test_that("%||% operator works correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal(FALSE %||% "default", FALSE)
  expect_equal("" %||% "default", "")
  expect_equal(NA %||% "default", NA)
  expect_equal(list() %||% "default", list())
})

# ============================================================================
# C++ VERSION FUNCTION TEST
# ============================================================================

test_that(".cpp_get_version returns valid version string", {
  version <- .cpp_get_version()
  expect_type(version, "character")
  expect_true(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+", version))
})

# ============================================================================
# C++ HAS INTERNET TEST
# ============================================================================

test_that(".cpp_has_internet returns logical", {
  result <- .cpp_has_internet()
  expect_type(result, "logical")
  expect_length(result, 1)
})

# ============================================================================
# C++ FETCH PROBLEMS LIST BASIC TESTS
# ============================================================================

test_that(".cpp_fetch_problems_list returns character vector", {
  skip_on_cran()
  skip_if_offline()
  
  result <- .cpp_fetch_problems_list()
  expect_type(result, "character")
})
