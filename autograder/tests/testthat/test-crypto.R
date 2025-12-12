# ============================================================================
# AUTOGRADER - CRYPTOGRAPHY AND NETWORK TESTS
# ============================================================================
#
# Tests for encryption/decryption and code fetching:
#   - .cpp_encrypt_url() / .cpp_decrypt_url()
#   - .cpp_encrypt() / .cpp_decrypt()
#   - fetch_instructor_code()
#   - extract_instructor_function()
#   - extract_test_cases()
#
# ============================================================================

# ============================================================================
# ENCRYPTION/DECRYPTION TESTS
# ============================================================================

test_that(".cpp_encrypt and .cpp_decrypt are reversible", {
  original <- "Hello, World!"
  encrypted <- .cpp_encrypt(original, "test_key")
  decrypted <- .cpp_decrypt(encrypted, "test_key")
  
  expect_equal(decrypted, original)
})

test_that(".cpp_encrypt_base64 and .cpp_decrypt_base64 are reversible", {
  original <- "https://example.com/test"
  encrypted <- .cpp_encrypt_base64(original, "test_key")
  decrypted <- .cpp_decrypt_base64(encrypted, "test_key")
  
  expect_equal(decrypted, original)
})

test_that(".cpp_encrypt_url produces encrypted output", {
  url <- "https://example.com/test"
  encrypted <- .cpp_encrypt_url(url)
  
  expect_type(encrypted, "character")
  expect_true(nchar(encrypted) > 0)
  # Encrypted should look different from original
  expect_false(encrypted == url)
})

test_that(".cpp_generate_key produces keys of requested length", {
  key32 <- .cpp_generate_key(32L)
  key16 <- .cpp_generate_key(16L)
  
  expect_type(key32, "character")
  expect_type(key16, "character")
  # Keys are hex-encoded, so length is 2x the requested byte length
  expect_equal(nchar(key32), 64)
  expect_equal(nchar(key16), 32)
})

test_that(".cpp_derive_key produces consistent results", {
  factors <- c("salt1", "salt2", "salt3")
  key1 <- .cpp_derive_key(factors, 32L)
  key2 <- .cpp_derive_key(factors, 32L)
  
  expect_equal(key1, key2)
})

# ============================================================================
# FETCH INSTRUCTOR CODE TESTS
# ============================================================================

test_that("fetch_instructor_code returns an environment", {
  skip_on_cran()
  skip_if_offline()
  
  result <- fetch_instructor_code("fibonacci")
  
  expect_type(result, "environment")
})

test_that("fetch_instructor_code environment contains function", {
  skip_on_cran()
  skip_if_offline()
  
  env <- fetch_instructor_code("fibonacci")
  
  # Should contain the instructor function
  funcs <- ls(env)
  expect_true(length(funcs) > 0)
  # At least one should be a function
  has_function <- any(sapply(funcs, function(f) is.function(get(f, envir = env))))
  expect_true(has_function)
})

test_that("fetch_instructor_code handles invalid problem names", {
  skip_on_cran()
  skip_if_offline()
  
  # Nonexistent problem throws function_not_found_error
  expect_error(
    fetch_instructor_code("nonexistent_problem_xyz123"),
    class = "function_not_found_error"
  )
})

test_that("fetch_instructor_code rejects dangerous function names", {
  # Command injection style
  expect_error(fetch_instructor_code("test; rm -rf /"))
  expect_error(fetch_instructor_code("test && cat /etc/passwd"))
  
  # Path traversal style
  expect_error(fetch_instructor_code("../../etc/passwd"))
})

test_that("fetch_instructor_code works for different problems", {
  skip_on_cran()
  skip_if_offline()
  
  # Test multiple problem types
  for (problem in c("fibonacci", "factorial", "sum_vector")) {
    result <- tryCatch(
      fetch_instructor_code(problem),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      expect_type(result, "environment")
    }
  }
})

# ============================================================================
# EXTRACT INSTRUCTOR FUNCTION TESTS
# ============================================================================

test_that("extract_instructor_function extracts function from environment", {
  skip_on_cran()
  skip_if_offline()
  
  env <- fetch_instructor_code("fibonacci")
  func <- extract_instructor_function(env, "fibonacci")
  
  expect_true(is.function(func))
})

test_that("extract_instructor_function returns callable function", {
  skip_on_cran()
  skip_if_offline()
  
  env <- fetch_instructor_code("fibonacci")
  func <- extract_instructor_function(env, "fibonacci")
  
  # Should be able to call it
  result <- func(5)
  expect_equal(result, c(1, 1, 2, 3, 5))
})

# ============================================================================
# EXTRACT TEST CASES TESTS
# ============================================================================

test_that("extract_test_cases returns validated test structure", {
  skip_on_cran()
  skip_if_offline()
  
  env <- fetch_instructor_code("fibonacci")
  tests <- extract_test_cases(env, "fibonacci")
  
  expect_type(tests, "list")
  expect_true(length(tests) > 0)
  
  # Should have the standard test data fields
  expect_true("inputs" %in% names(tests))
  expect_true("descriptions" %in% names(tests))
  expect_true("hidden" %in% names(tests))
  expect_true("points" %in% names(tests))
})

test_that("extract_test_cases handles hidden tests", {
  skip_on_cran()
  skip_if_offline()
  
  env <- fetch_instructor_code("fibonacci")
  tests <- extract_test_cases(env, "fibonacci")
  
  # hidden field should be logical
  expect_type(tests$hidden, "logical")
})

# ============================================================================
# SECURITY TESTS
# ============================================================================

test_that("function name validation rejects injection attempts", {
  # These should all be rejected
  expect_error(fetch_instructor_code("'; DROP TABLE --"))
  expect_error(fetch_instructor_code("test | cat /etc/passwd"))
  expect_error(fetch_instructor_code("test<script>"))
  expect_error(fetch_instructor_code("test$(command)"))
})

test_that("function name validation rejects special characters", {
  expect_error(fetch_instructor_code("test\ncommand"))
  expect_error(fetch_instructor_code("test\rcommand"))
  # Note: null bytes can't be tested directly in R strings
})

# ============================================================================
# ADDITIONAL ENCRYPTION TESTS
# ============================================================================

test_that(".cpp_encrypt handles empty string", {
  encrypted <- .cpp_encrypt("", "test_key")
  expect_type(encrypted, "character")
})

test_that(".cpp_encrypt handles long strings", {
  long_string <- paste(rep("a", 10000), collapse = "")
  encrypted <- .cpp_encrypt(long_string, "test_key")
  decrypted <- .cpp_decrypt(encrypted, "test_key")
  expect_equal(decrypted, long_string)
})

test_that(".cpp_encrypt handles special characters", {
  special <- "Hello\nWorld\tTab\r\n!@#$%^&*()"
  encrypted <- .cpp_encrypt(special, "test_key")
  decrypted <- .cpp_decrypt(encrypted, "test_key")
  expect_equal(decrypted, special)
})

test_that(".cpp_encrypt handles unicode", {
  unicode <- "Hello世界🎉"
  encrypted <- .cpp_encrypt(unicode, "test_key")
  decrypted <- .cpp_decrypt(encrypted, "test_key")
  expect_equal(decrypted, unicode)
})

test_that(".cpp_generate_key produces unique keys", {
  key1 <- .cpp_generate_key(32L)
  key2 <- .cpp_generate_key(32L)
  expect_false(key1 == key2)
})

test_that(".cpp_derive_key with different inputs produces different keys", {
  key1 <- .cpp_derive_key(c("salt1", "salt2"), 32L)
  key2 <- .cpp_derive_key(c("salt1", "salt3"), 32L)
  expect_false(key1 == key2)
})

test_that(".cpp_encrypt_url and .cpp_decrypt_url are reversible", {
  url <- "https://example.com/path/to/resource?query=value"
  encrypted <- .cpp_encrypt_url(url)
  decrypted <- .cpp_decrypt_url(encrypted)
  expect_equal(decrypted, url)
})

test_that(".cpp_get_version returns version string", {
  version <- .cpp_get_version()
  expect_type(version, "character")
  expect_true(grepl("\\.", version))  # Should contain a dot like "0.4.0"
})

# ============================================================================
# ADDITIONAL COVERAGE TESTS
# ============================================================================

test_that(".cpp_encrypt works with default key (empty string)", {
  original <- "Test with default key"
  encrypted <- .cpp_encrypt(original, "")
  decrypted <- .cpp_decrypt(encrypted, "")
  expect_equal(decrypted, original)
})

test_that(".cpp_encrypt_base64 handles binary-like content", {
  # String that might contain various byte values after encryption
  original <- paste(rawToChar(as.raw(1:100)), collapse = "")
  encrypted <- .cpp_encrypt_base64(original, "test_key")
  decrypted <- .cpp_decrypt_base64(encrypted, "test_key")
  expect_equal(decrypted, original)
})

test_that(".cpp_derive_key works with single factor", {
  key <- .cpp_derive_key(c("single_factor"), 32L)
  expect_type(key, "character")
  expect_equal(nchar(key), 64)  # 32 bytes = 64 hex chars
})

test_that(".cpp_derive_key works with many factors", {
  factors <- paste0("factor", 1:20)
  key <- .cpp_derive_key(factors, 32L)
  expect_type(key, "character")
  expect_equal(nchar(key), 64)
})

test_that(".cpp_derive_key produces different output for different lengths", {
  factors <- c("test", "factor")
  key16 <- .cpp_derive_key(factors, 16L)
  key32 <- .cpp_derive_key(factors, 32L)
  
  expect_equal(nchar(key16), 32)  # 16 bytes = 32 hex chars
  expect_equal(nchar(key32), 64)  # 32 bytes = 64 hex chars
  expect_false(key32 == paste0(key16, key16))  # Not just doubled
})

test_that(".cpp_generate_key handles minimum length", {
  key <- .cpp_generate_key(1L)
  expect_equal(nchar(key), 2)  # 1 byte = 2 hex chars
})

test_that(".cpp_generate_key handles large length", {
  key <- .cpp_generate_key(256L)
  expect_equal(nchar(key), 512)  # 256 bytes = 512 hex chars
})

test_that(".cpp_encrypt_url handles URLs with special characters", {
  urls <- c(
    "https://example.com/path?query=value&other=test",
    "https://example.com/path#fragment",
    "https://example.com/path with spaces",
    "https://example.com/path%20encoded",
    "https://user:pass@example.com/path"
  )
  
  for (url in urls) {
    encrypted <- .cpp_encrypt_url(url)
    decrypted <- .cpp_decrypt_url(encrypted)
    expect_equal(decrypted, url)
  }
})

test_that(".cpp_encrypt preserves newlines and tabs", {
  original <- "Line1\nLine2\tTabbed\rCarriage"
  encrypted <- .cpp_encrypt(original, "key")
  decrypted <- .cpp_decrypt(encrypted, "key")
  expect_equal(decrypted, original)
})

test_that(".cpp_encrypt_base64 preserves whitespace", {
  original <- "   leading and trailing spaces   "
  encrypted <- .cpp_encrypt_base64(original, "key")
  decrypted <- .cpp_decrypt_base64(encrypted, "key")
  expect_equal(decrypted, original)
})

test_that("encryption with different keys produces different output", {
  original <- "same plaintext"
  enc1 <- .cpp_encrypt(original, "key1")
  enc2 <- .cpp_encrypt(original, "key2")
  expect_false(enc1 == enc2)
})

test_that("decryption with wrong key produces garbage",
{
  original <- "secret message"
  encrypted <- .cpp_encrypt(original, "correct_key")
  decrypted <- .cpp_decrypt(encrypted, "wrong_key")
  expect_false(decrypted == original)
})

test_that(".cpp_encrypt handles very short strings", {
  for (s in c("a", "ab", "abc")) {
    encrypted <- .cpp_encrypt(s, "key")
    decrypted <- .cpp_decrypt(encrypted, "key")
    expect_equal(decrypted, s)
  }
})

test_that(".cpp_encrypt_base64 output is valid base64", {
  original <- "test data for base64"
  encrypted <- .cpp_encrypt_base64(original, "key")
  
  # Valid base64 characters only (A-Z, a-z, 0-9, +, /, =)
  expect_true(grepl("^[A-Za-z0-9+/=]+$", encrypted))
})

test_that(".cpp_encrypt output is valid hex", {
  original <- "test data for hex"
  encrypted <- .cpp_encrypt(original, "key")
  
  # Valid hex characters only
  expect_true(grepl("^[0-9a-fA-F]+$", encrypted))
})

# ============================================================================
# NETWORK FETCH EDGE CASES
# ============================================================================

test_that("fetch_instructor_code handles empty function name", {
  expect_error(fetch_instructor_code(""), "Invalid|empty|valid")
})

test_that("fetch_instructor_code handles whitespace function name", {
  expect_error(fetch_instructor_code("   "), "Invalid|whitespace|valid")
})

test_that("fetch_instructor_code validates special character inputs", {
  # Various injection attempts
  bad_names <- c(
    "`command`",
    "$(whoami)",
    "${PATH}",
    "name\\path",
    "name/path",
    "name:colon"
  )
  
  for (name in bad_names) {
    expect_error(
      fetch_instructor_code(name),
      regexp = "Invalid|error|valid|found",
      info = paste("Should reject:", name)
    )
  }
})

