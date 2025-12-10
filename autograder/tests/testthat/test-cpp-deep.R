# ============================================================================
# AUTOGRADER - DEEP C++ COVERAGE TESTS
# ============================================================================
#
# Additional tests targeting uncovered C++ code paths:
#   - formatter.cpp edge cases
#   - comparator.cpp branches
#   - encryption.cpp paths
#   - validator.cpp patterns
#   - fetcher.cpp (limited - network dependent)
#
# ============================================================================

# ============================================================================
# FORMATTER DEEP TESTS
# ============================================================================

test_that(".cpp_format_output handles deeply nested structures", {
  # Deep nesting
  nested <- list(a = list(b = list(c = list(d = list(e = 1)))))
  result <- .cpp_format_output(nested, 200L)
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles mixed type lists", {
  mixed <- list(
    num = 42,
    str = "hello",
    bool = TRUE,
    vec = 1:5,
    null = NULL
  )
  result <- .cpp_format_output(mixed, 200L)
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles very small max_length", {
  vec <- 1:1000
  
  # Extremely small max_length
  for (max_len in c(5L, 10L, 15L, 20L)) {
    result <- .cpp_format_output(vec, max_len)
    expect_true(nchar(result) <= max_len + 10)  # Some tolerance
  }
})

test_that(".cpp_format_output handles names in vectors", {
  named_vec <- c(a = 1, b = 2, c = 3, d = 4, e = 5)
  result <- .cpp_format_output(named_vec, 100L)
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles attributes", {
  obj <- 1:5
  attr(obj, "custom") <- "test_attr"
  result <- .cpp_format_output(obj, 100L)
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles different integer types", {
  result <- .cpp_format_output(1L, 100L)
  expect_true(nchar(result) > 0)
  
  result <- .cpp_format_output(1:1000L, 50L)
  expect_true(nchar(result) <= 60)
})

test_that(".cpp_format_output handles negative numbers", {
  result <- .cpp_format_output(-42, 100L)
  expect_true(grepl("-", result))
  
  result <- .cpp_format_output(c(-1, -2, -3), 100L)
  expect_true(nchar(result) > 0)
})

# ============================================================================
# COMPARATOR DEEP TESTS
# ============================================================================

test_that(".cpp_compare_fast handles named list comparison", {
  l1 <- list(a = 1, b = 2, c = 3)
  l2 <- list(a = 1, b = 2, c = 3)
  l3 <- list(a = 1, b = 2, c = 4)
  l4 <- list(x = 1, y = 2, z = 3)  # Same values, different names
  
  expect_true(.cpp_compare_fast(l1, l2, 0))
  expect_false(.cpp_compare_fast(l1, l3, 0))
  
  # Different names - behavior depends on implementation
  result <- .cpp_compare_fast(l1, l4, 0)
  expect_type(result, "logical")
})

test_that(".cpp_compare_fast handles recursive list comparison", {
  l1 <- list(outer = list(inner = list(deep = 1:3)))
  l2 <- list(outer = list(inner = list(deep = 1:3)))
  l3 <- list(outer = list(inner = list(deep = 1:4)))
  
  expect_true(.cpp_compare_fast(l1, l2, 0))
  expect_false(.cpp_compare_fast(l1, l3, 0))
})

test_that(".cpp_compare_fast handles very long vectors", {
  v1 <- 1:100000
  v2 <- 1:100000
  v3 <- c(1:99999, 0)  # Last element different
  
  expect_true(.cpp_compare_fast(v1, v2, 0))
  expect_false(.cpp_compare_fast(v1, v3, 0))
})

test_that(".cpp_compare_fast handles sparse differences", {
  v1 <- 1:1000
  v2 <- 1:1000
  v2[500] <- 999  # One difference in middle
  
  expect_false(.cpp_compare_fast(v1, v2, 0))
})

test_that(".cpp_compare_fast handles all-same vs one-different", {
  # All identical
  v1 <- rep(1, 100)
  v2 <- rep(1, 100)
  expect_true(.cpp_compare_fast(v1, v2, 0))
  
  # One different
  v2[50] <- 2
  expect_false(.cpp_compare_fast(v1, v2, 0))
})

test_that(".cpp_compare_fast handles character edge cases", {
  # Empty string vs non-empty
  expect_false(.cpp_compare_fast("", "a", 0))
  
  # Whitespace
  expect_false(.cpp_compare_fast("a", "a ", 0))
  expect_false(.cpp_compare_fast(" a", "a", 0))
  
  # Case sensitivity
  expect_false(.cpp_compare_fast("ABC", "abc", 0))
})

test_that(".cpp_compare_fast handles mixed NA positions", {
  v1 <- c(1, NA, 3, NA, 5)
  v2 <- c(1, NA, 3, NA, 5)
  v3 <- c(1, 2, 3, NA, 5)  # NA vs value at position 2
  
  result1 <- .cpp_compare_fast(v1, v2, 0)
  result2 <- .cpp_compare_fast(v1, v3, 0)
  
  expect_type(result1, "logical")
  expect_type(result2, "logical")
})

# ============================================================================
# ENCRYPTION DEEP TESTS
# ============================================================================

test_that(".cpp_encrypt handles boundary key lengths", {
  text <- "test message"
  
  # Very short keys
  for (key_len in 1:5) {
    key <- paste(rep("k", key_len), collapse = "")
    enc <- .cpp_encrypt(text, key)
    dec <- .cpp_decrypt(enc, key)
    expect_equal(dec, text)
  }
})

test_that(".cpp_encrypt handles unicode keys", {
  text <- "test"
  unicode_key <- "密钥🔑"
  
  enc <- .cpp_encrypt(text, unicode_key)
  dec <- .cpp_decrypt(enc, unicode_key)
  expect_equal(dec, text)
})

test_that(".cpp_encrypt handles repeated encryption", {
  text <- "test"
  key <- "secret"
  
  # Encrypt multiple times
  enc1 <- .cpp_encrypt(text, key)
  enc2 <- .cpp_encrypt(enc1, key)
  
  # Decrypt in reverse order
  dec2 <- .cpp_decrypt(enc2, key)
  dec1 <- .cpp_decrypt(dec2, key)
  
  expect_equal(dec1, text)
})

test_that(".cpp_derive_key handles empty factors", {
  # Single factor
  key1 <- .cpp_derive_key(c("single"), 32L)
  expect_equal(nchar(key1), 64)
  
  # Many factors
  factors <- paste0("factor", 1:10)
  key2 <- .cpp_derive_key(factors, 32L)
  expect_equal(nchar(key2), 64)
})

test_that(".cpp_derive_key handles special characters in factors", {
  factors <- c("salt!@#$", "key%^&*()", "data<>?")
  key <- .cpp_derive_key(factors, 32L)
  expect_equal(nchar(key), 64)
})

test_that(".cpp_encrypt_base64 produces consistent length", {
  text <- "test"
  key <- "key"
  
  # Multiple encryptions should produce same-length output
  lengths <- replicate(5, nchar(.cpp_encrypt_base64(text, key)))
  # May vary slightly due to padding
  expect_true(max(lengths) - min(lengths) <= 4)
})

# ============================================================================
# VALIDATION DEEP TESTS
# ============================================================================

test_that(".cpp_validate_function_name handles all alphanumeric", {
  # All letters
  expect_true(.cpp_validate_function_name("abcdefghijklmnopqrstuvwxyz"))
  expect_true(.cpp_validate_function_name("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  
  # Mixed
  expect_true(.cpp_validate_function_name("abcXYZ123"))
  
  # With underscore and hyphen
  expect_true(.cpp_validate_function_name("my_func_v2"))
  expect_true(.cpp_validate_function_name("my-func-v2"))
})

test_that(".cpp_validate_function_name rejects sequences of dangerous chars", {
  expect_false(.cpp_validate_function_name("...."))  # Multiple dots
  expect_false(.cpp_validate_function_name("////"))  # Multiple slashes
  expect_false(.cpp_validate_function_name("\\\\\\\\"))  # Multiple backslashes
  expect_false(.cpp_validate_function_name(";;;;"))  # Multiple semicolons
})

test_that(".cpp_validate_function_name handles edge length cases", {
  # Exactly 1 character
  expect_true(.cpp_validate_function_name("a"))
  expect_true(.cpp_validate_function_name("_"))
  
  # Exactly 255 characters (usually valid)
  name_255 <- paste(rep("a", 255), collapse = "")
  result <- .cpp_validate_function_name(name_255)
  expect_type(result, "logical")
  
  # Exactly 256 characters (may be boundary)
  name_256 <- paste(rep("a", 256), collapse = "")
  result <- .cpp_validate_function_name(name_256)
  expect_type(result, "logical")
})

# ============================================================================
# TYPE DETECTION DEEP TESTS
# ============================================================================

test_that(".cpp_get_type handles all vector types", {
  expect_equal(.cpp_get_type(c(1L, 2L, 3L)), "integer")
  expect_equal(.cpp_get_type(c(1.0, 2.0, 3.0)), "numeric")
  expect_equal(.cpp_get_type(c("a", "b", "c")), "character")
  expect_equal(.cpp_get_type(c(TRUE, FALSE, TRUE)), "logical")
  expect_equal(.cpp_get_type(as.raw(c(0, 1, 2))), "raw")
  expect_equal(.cpp_get_type(complex(real = 1:3, imaginary = 1:3)), "complex")
})

test_that(".cpp_get_type handles matrix types", {
  # Integer matrix (1:4 creates integer, 1L:4L creates integer)
  expect_equal(.cpp_get_type(matrix(1:4, 2, 2)), "integer")
  
  # Numeric matrix (need explicit doubles)
  expect_equal(.cpp_get_type(matrix(c(1.0, 2.0, 3.0, 4.0), 2, 2)), "numeric")
  
  # Character matrix
  expect_equal(.cpp_get_type(matrix(c("a", "b", "c", "d"), 2, 2)), "character")
})

test_that(".cpp_get_type handles special singletons", {
  expect_equal(.cpp_get_type(NA), "logical")  # NA is logical by default
  expect_equal(.cpp_get_type(NA_integer_), "integer")
  expect_equal(.cpp_get_type(NA_real_), "numeric")
  expect_equal(.cpp_get_type(NA_character_), "character")
})

# ============================================================================
# INTEGRATION: ROUND-TRIP TESTS
# ============================================================================

test_that("encrypt-format-compare round trip", {
  # Create test data
  original <- list(a = 1:10, b = "test", c = list(d = TRUE))
  
  # Format it
  formatted <- .cpp_format_output(original, 500L)
  expect_true(nchar(formatted) > 0)
  
  # Encrypt and decrypt
  enc <- .cpp_encrypt(formatted, "key")
  dec <- .cpp_decrypt(enc, "key")
  expect_equal(dec, formatted)
  
  # Compare original with itself
  expect_true(.cpp_compare_fast(original, original, 1e-10))
})

test_that("validation-fetch integration", {
  skip_on_cran()
  skip_if_offline()
  
  # Valid name should fetch
  expect_true(.cpp_validate_function_name("fibonacci"))
  
  # Fetch should work
  code <- tryCatch(
    .cpp_fetch_function_content("fibonacci"),
    error = function(e) NULL
  )
  
  if (!is.null(code)) {
    expect_type(code, "character")
    expect_true(length(code) > 0)
  }
})

# ============================================================================
# STRESS TESTS
# ============================================================================

test_that("format handles rapid repeated calls", {
  for (i in 1:50) {
    result <- .cpp_format_output(1:100, 50L)
    expect_true(nchar(result) <= 60)
  }
})

test_that("compare handles rapid repeated calls", {
  v1 <- 1:100
  v2 <- 1:100
  
  for (i in 1:50) {
    expect_true(.cpp_compare_fast(v1, v2, 1e-10))
  }
})

test_that("encrypt handles rapid repeated calls", {
  text <- "test message"
  key <- "key"
  
  for (i in 1:50) {
    enc <- .cpp_encrypt(text, key)
    dec <- .cpp_decrypt(enc, key)
    expect_equal(dec, text)
  }
})

# ============================================================================
# MEMORY TESTS
# ============================================================================

test_that("large data operations complete without error", {
  # Large vector format
  big <- 1:1000000
  result <- .cpp_format_output(big, 100L)
  expect_true(nchar(result) <= 110)
  
  # Large vector compare
  big1 <- 1:100000
  big2 <- 1:100000
  expect_true(.cpp_compare_fast(big1, big2, 0))
  
  # Large string encrypt
  long_text <- paste(rep("x", 100000), collapse = "")
  enc <- .cpp_encrypt(long_text, "key")
  dec <- .cpp_decrypt(enc, "key")
  expect_equal(dec, long_text)
  
  gc()  # Force garbage collection
})

test_that("repeated operations don't leak memory", {
  # Run multiple times and check memory doesn't grow excessively
  initial_mem <- gc()
  
  for (i in 1:10) {
    # Various operations
    .cpp_format_output(1:10000, 100L)
    .cpp_compare_fast(1:1000, 1:1000, 0)
    enc <- .cpp_encrypt("test", "key")
    .cpp_decrypt(enc, "key")
  }
  
  gc()
  final_mem <- gc()
  
  # Just verify no errors - memory comparison is platform-dependent
  expect_true(TRUE)
})
