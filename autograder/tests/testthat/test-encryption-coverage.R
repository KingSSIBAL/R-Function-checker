# ============================================================================
# AUTOGRADER - ENCRYPTION R WRAPPER COVERAGE TESTS
# ============================================================================
#
# Tests for encryption.R R wrapper functions to boost coverage:
#   - encrypt_text() / decrypt_text()
#   - generate_key()
#   - derive_key()
#   - Input validation for all functions
#
# ============================================================================

# ============================================================================
# ENCRYPT_TEXT TESTS
# ============================================================================

test_that("encrypt_text with default encoding works", {
  encrypted <- encrypt_text("hello world")
  expect_true(is.character(encrypted))
  expect_true(nchar(encrypted) > 0)
})

test_that("encrypt_text with hex encoding works", {
  encrypted <- encrypt_text("test data", encoding = "hex")
  expect_true(is.character(encrypted))
  # Hex encoding produces hex characters
  expect_true(grepl("^[0-9a-fA-F]+$", encrypted))
})

test_that("encrypt_text with base64 encoding works", {
  encrypted <- encrypt_text("test data", encoding = "base64")
  expect_true(is.character(encrypted))
  expect_true(nchar(encrypted) > 0)
})

test_that("encrypt_text with custom key works", {
  key <- "my-custom-key-123"
  encrypted <- encrypt_text("secret", key = key)
  expect_true(is.character(encrypted))
})

test_that("encrypt_text validates text parameter", {
  expect_error(encrypt_text(123), "single character string")
  expect_error(encrypt_text(c("a", "b")), "single character string")
  expect_error(encrypt_text(NULL), "single character string")
  expect_error(encrypt_text(list()), "single character string")
})

test_that("encrypt_text handles empty string", {
  # Empty string should still work
  encrypted <- encrypt_text("")
  expect_true(is.character(encrypted))
})

test_that("encrypt_text handles special characters", {
  encrypted <- encrypt_text("Hello! @#$%^&*()_+-=[]{}|;':\",./<>?")
  expect_true(is.character(encrypted))
  expect_true(nchar(encrypted) > 0)
})

test_that("encrypt_text handles unicode", {
  encrypted <- encrypt_text("Hello 世界 🌍")
  expect_true(is.character(encrypted))
})

# ============================================================================
# DECRYPT_TEXT TESTS
# ============================================================================

test_that("decrypt_text reverses encrypt_text with hex", {
  original <- "The quick brown fox"
  encrypted <- encrypt_text(original, encoding = "hex")
  decrypted <- decrypt_text(encrypted, encoding = "hex")
  expect_equal(decrypted, original)
})

test_that("decrypt_text reverses encrypt_text with base64", {
  original <- "Jump over the lazy dog"
  encrypted <- encrypt_text(original, encoding = "base64")
  decrypted <- decrypt_text(encrypted, encoding = "base64")
  expect_equal(decrypted, original)
})

test_that("decrypt_text with custom key works", {
  key <- "test-key-456"
  original <- "secret message"
  encrypted <- encrypt_text(original, key = key)
  decrypted <- decrypt_text(encrypted, key = key)
  expect_equal(decrypted, original)
})

test_that("decrypt_text validates ciphertext parameter", {
  expect_error(decrypt_text(123), "single character string")
  expect_error(decrypt_text(c("a", "b")), "single character string")
  expect_error(decrypt_text(NULL), "single character string")
})

test_that("encrypt/decrypt roundtrip preserves data", {
  test_strings <- c(
    "Simple text",
    "",
    "Numbers: 123456789",
    "Special: !@#$%^&*()",
    "Whitespace:\t\n\r",
    "Unicode: 日本語 한국어",
    paste(rep("a", 1000), collapse = "")  # Long string
  )
  
  for (original in test_strings) {
    encrypted <- encrypt_text(original)
    decrypted <- decrypt_text(encrypted)
    expect_equal(decrypted, original)
  }
})

# ============================================================================
# GENERATE_KEY TESTS
# ============================================================================

test_that("generate_key creates key of correct length", {
  # Default length (32 bytes = 64 hex chars)
  key <- generate_key()
  expect_equal(nchar(key), 64)
  
  # Custom length
  key16 <- generate_key(16)
  expect_equal(nchar(key16), 32)
  
  key8 <- generate_key(8)
  expect_equal(nchar(key8), 16)
})

test_that("generate_key produces hex output", {
  key <- generate_key()
  expect_true(grepl("^[0-9a-fA-F]+$", key))
})

test_that("generate_key produces different keys each time", {
  key1 <- generate_key()
  key2 <- generate_key()
  expect_false(key1 == key2)
})

test_that("generate_key validates length parameter", {
  expect_error(generate_key(0), "positive integer")
  expect_error(generate_key(-1), "positive integer")
  expect_error(generate_key(257), "positive integer")
  expect_error(generate_key("abc"), "positive integer")
})

test_that("generate_key handles boundary lengths", {
  # Minimum
  key1 <- generate_key(1)
  expect_equal(nchar(key1), 2)
  
  # Maximum
  key256 <- generate_key(256)
  expect_equal(nchar(key256), 512)
})

# ============================================================================
# DERIVE_KEY TESTS
# ============================================================================

test_that("derive_key creates deterministic keys", {
  key1 <- derive_key("factor1", "factor2")
  key2 <- derive_key("factor1", "factor2")
  expect_equal(key1, key2)
})

test_that("derive_key with different factors produces different keys", {
  key1 <- derive_key("factorA")
  key2 <- derive_key("factorB")
  expect_false(key1 == key2)
})

test_that("derive_key respects length parameter", {
  key16 <- derive_key("test", length = 16)
  expect_equal(nchar(key16), 32)  # 16 bytes = 32 hex chars
  
  key32 <- derive_key("test", length = 32)
  expect_equal(nchar(key32), 64)
})

test_that("derive_key validates factors parameter", {
  expect_error(derive_key(), "At least one character factor")
  expect_error(derive_key(123), "At least one character factor")
})

test_that("derive_key works with multiple factors", {
  key <- derive_key("user", "password", "salt", "pepper")
  expect_true(is.character(key))
  expect_equal(nchar(key), 64)
})

test_that("derive_key can be used for encryption", {
  key <- derive_key("my", "secret", "factors")
  original <- "sensitive data"
  
  encrypted <- encrypt_text(original, key = key)
  decrypted <- decrypt_text(encrypted, key = key)
  
  expect_equal(decrypted, original)
})

# ============================================================================
# C++ WRAPPER FUNCTION TESTS (Internal Functions)
# ============================================================================

test_that(".cpp_compare_fast works correctly", {
  expect_true(.cpp_compare_fast(1, 1, 1e-10))
  expect_true(.cpp_compare_fast(1.0000000001, 1.0, 1e-8))
  expect_false(.cpp_compare_fast(1, 2, 1e-10))
  expect_true(.cpp_compare_fast(1:5, 1:5, 1e-10))
  expect_false(.cpp_compare_fast(1:5, 1:4, 1e-10))
})

test_that(".cpp_compare_identical works correctly", {
  expect_true(.cpp_compare_identical(5, 5))
  expect_true(.cpp_compare_identical("hello", "hello"))
  expect_false(.cpp_compare_identical(5, 6))
  expect_false(.cpp_compare_identical("hello", "world"))
})

test_that(".cpp_compare_detailed returns correct structure", {
  result <- .cpp_compare_detailed(1:5, 1:5, 1e-10)
  expect_true(is.list(result))
  expect_true("equal" %in% names(result) || result[[1]] == TRUE)
  
  result2 <- .cpp_compare_detailed(c(1, 2, 3), c(1, 99, 3), 1e-10)
  expect_true(is.list(result2))
})

test_that(".cpp_find_differences finds correct positions", {
  diffs <- .cpp_find_differences(c(1, 2, 3), c(1, 99, 3), 1e-10, 10L)
  expect_true(2 %in% diffs)  # Position 2 differs
  
  no_diffs <- .cpp_find_differences(1:5, 1:5, 1e-10, 10L)
  expect_equal(length(no_diffs), 0)
})

test_that(".cpp_validate_function_name works correctly", {
  expect_true(.cpp_validate_function_name("fibonacci"))
  expect_true(.cpp_validate_function_name("my_function_123"))
  expect_false(.cpp_validate_function_name(""))
  expect_false(.cpp_validate_function_name("has space"))
})

test_that(".cpp_format_output works correctly", {
  result <- .cpp_format_output(1:5, 200L)
  expect_true(is.character(result))
  expect_true(nchar(result) > 0)
  
  # Long output gets truncated
  result_short <- .cpp_format_output(1:1000, 50L)
  expect_lte(nchar(result_short), 55)
})

test_that(".cpp_get_type returns correct types", {
  expect_equal(.cpp_get_type(1L), "integer")
  expect_equal(.cpp_get_type(1.5), "numeric")  # R reports numeric, not double
  expect_equal(.cpp_get_type("hello"), "character")
  expect_equal(.cpp_get_type(TRUE), "logical")
  expect_equal(.cpp_get_type(1:5), "integer")
})

test_that(".cpp_get_version returns version string", {
  version <- .cpp_get_version()
  expect_true(is.character(version))
  expect_true(grepl("^\\d+\\.\\d+", version))  # Starts with major.minor
})

# ============================================================================
# ENCRYPTION INTERNAL FUNCTIONS
# ============================================================================

test_that(".cpp_encrypt and .cpp_decrypt work correctly", {
  original <- "test string"
  encrypted <- .cpp_encrypt(original, "")
  decrypted <- .cpp_decrypt(encrypted, "")
  expect_equal(decrypted, original)
})

test_that(".cpp_encrypt_base64 and .cpp_decrypt_base64 work correctly", {
  original <- "base64 test"
  encrypted <- .cpp_encrypt_base64(original, "")
  decrypted <- .cpp_decrypt_base64(encrypted, "")
  expect_equal(decrypted, original)
})

test_that(".cpp_derive_key produces consistent output", {
  key1 <- .cpp_derive_key(c("a", "b"), 32L)
  key2 <- .cpp_derive_key(c("a", "b"), 32L)
  expect_equal(key1, key2)
})

test_that(".cpp_generate_key produces random output", {
  key1 <- .cpp_generate_key(32L)
  key2 <- .cpp_generate_key(32L)
  expect_false(key1 == key2)
})

test_that(".cpp_encrypt_url and .cpp_decrypt_url work correctly", {
  url <- "https://example.com/path?query=value"
  encrypted <- .cpp_encrypt_url(url)
  decrypted <- .cpp_decrypt_url(encrypted)
  expect_equal(decrypted, url)
})
