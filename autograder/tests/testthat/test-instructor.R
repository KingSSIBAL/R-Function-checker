# ============================================================================
# Test: instructor.R - Instructor Setup Functions
# ============================================================================

# ============================================================================
# Section 1: Build Tools Check
# ============================================================================

test_that("autograder_check_build_tools runs without error", {
  # This just tests the function runs - actual build tools may or may not be present
  output <- capture.output({
    result <- autograder_check_build_tools()
  })
  
  expect_type(result, "logical")
})

test_that("autograder_check_build_tools returns TRUE or FALSE", {
  output <- capture.output({
    result <- autograder_check_build_tools()
  })
  
  expect_true(result %in% c(TRUE, FALSE))
})

# ============================================================================
# Section 2: Token Test Function
# ============================================================================

test_that("autograder_test_token validates missing token", {
  output <- capture.output({
    result <- autograder_test_token(
      url = "https://example.com/repo",
      token = NULL,
      verbose = TRUE
    )
  })
  
  expect_false(result$success)
  expect_match(result$error, "No token")
})

test_that("autograder_test_token validates empty token", {
  output <- capture.output({
    result <- autograder_test_token(
      url = "https://example.com/repo",
      token = "",
      verbose = TRUE
    )
  })
  
  expect_false(result$success)
  expect_match(result$error, "No token")
})

test_that("autograder_test_token validates whitespace token", {
  output <- capture.output({
    result <- autograder_test_token(
      url = "https://example.com/repo",
      token = "   ",
      verbose = TRUE
    )
  })
  
  expect_false(result$success)
})

test_that("autograder_test_token validates missing URL", {
  output <- capture.output({
    result <- autograder_test_token(
      url = NULL,
      token = "test_token_123",
      verbose = TRUE
    )
  })
  
  expect_false(result$success)
  expect_match(result$error, "No URL")
})

test_that("autograder_test_token validates empty URL", {
  output <- capture.output({
    result <- autograder_test_token(
      url = "",
      token = "test_token_123",
      verbose = TRUE
    )
  })
  
  expect_false(result$success)
})

test_that("autograder_test_token returns structured result", {
  output <- capture.output({
    result <- autograder_test_token(
      url = "https://example.com/repo",
      token = NULL,
      verbose = FALSE
    )
  })
  
  expect_type(result, "list")
  expect_true("success" %in% names(result))
  expect_type(result$success, "logical")
})

test_that("autograder_test_token verbose outputs formatted", {
  output <- capture.output({
    result <- autograder_test_token(
      url = "https://example.com/repo",
      token = "test_token_12345678901234",
      verbose = TRUE
    )
  }, type = "message")  # capture cli output
  output_text <- paste(output, collapse = "\n")
  
  # Should show token info or diagnostic output
  # The function may output to message or stdout
  expect_false(result$success)  # We know this will fail (fake URL)
})

# ============================================================================
# Section 3: Internal Encryption Helpers
# ============================================================================

test_that(".sbox_transform transforms input", {
  input <- "test"
  result <- .sbox_transform(input)
  
  expect_type(result, "raw")
  expect_equal(length(result), nchar(input))
})

test_that(".sbox_transform handles empty string", {
  result <- .sbox_transform("")
  expect_type(result, "raw")
  expect_length(result, 0)
})

test_that(".derive_key produces correct length key", {
  factors <- c("factor1", "factor2", "factor3")
  
  key32 <- .derive_key(factors, 32)
  expect_length(key32, 32)
  expect_type(key32, "raw")
  
  key16 <- .derive_key(factors, 16)
  expect_length(key16, 16)
})

test_that(".derive_key is deterministic", {
  # Using simple ASCII factors that won't produce NUL bytes
  factors <- c("test", "key", "factors123")
  
  key1 <- .derive_key(factors, 32)
  key2 <- .derive_key(factors, 32)
  
  expect_equal(key1, key2)
})

test_that(".derive_key produces different keys for different factors", {
  key1 <- .derive_key(c("factor_a"), 32)
  key2 <- .derive_key(c("factor_b"), 32)
  
  expect_false(identical(key1, key2))
})

test_that(".encrypt_string encrypts to raw bytes", {
  key <- .derive_key(c("test_key"), 32)
  result <- .encrypt_string("Hello World", key)
  
  expect_type(result, "raw")
  expect_equal(length(result), nchar("Hello World"))
})

test_that(".encrypt_string produces different output from input", {
  key <- .derive_key(c("test_key"), 32)
  plaintext <- "Hello World"
  encrypted <- .encrypt_string(plaintext, key)
  
  # Encrypted should differ from plaintext bytes
  plain_bytes <- charToRaw(plaintext)
  expect_false(identical(encrypted, plain_bytes))
})

test_that(".bytes_to_cpp generates valid C++ array", {
  bytes <- as.raw(c(0x01, 0x02, 0xff))
  cpp_code <- .bytes_to_cpp(bytes, "TEST_VAR")
  
  expect_type(cpp_code, "character")
  expect_match(cpp_code, "static const uint8_t TEST_VAR")
  expect_match(cpp_code, "0x01")
  expect_match(cpp_code, "0xff")
  expect_match(cpp_code, "TEST_VAR_LEN = 3")
})

test_that(".bytes_to_cpp handles empty bytes", {
  bytes <- raw(0)
  cpp_code <- .bytes_to_cpp(bytes, "EMPTY")
  
  expect_match(cpp_code, "EMPTY_LEN = 0")
})

test_that(".bytes_to_cpp formats rows correctly", {
  # Create enough bytes to span multiple rows
  bytes <- as.raw(1:32)
  cpp_code <- .bytes_to_cpp(bytes, "MULTI_ROW")
  
  expect_type(cpp_code, "character")
  expect_match(cpp_code, "MULTI_ROW_LEN = 32")
})

# ============================================================================
# Section 4: Config Content Generation
# ============================================================================

test_that(".generate_config_content creates valid header", {
  key <- .derive_key(c("test"), 32)
  enc_url <- .encrypt_string("https://example.com", key)
  enc_token <- .encrypt_string("token123", key)
  
  content <- .generate_config_content(
    encrypted_url = enc_url,
    encrypted_token = enc_token,
    key_factors = c("FACTOR1", "FACTOR2"),
    use_auth = TRUE
  )
  
  expect_type(content, "character")
  expect_match(content, "#ifndef AUTOGRADER_ENCRYPTED_CONFIG_HPP")
  expect_match(content, "#endif")
  expect_match(content, "USE_AUTHENTICATION = true")
  expect_match(content, "ENCRYPTED_URL")
  expect_match(content, "ENCRYPTED_TOKEN")
  expect_match(content, "FACTOR1")
  expect_match(content, "KEY_FACTORS_COUNT = 2")
})

test_that(".generate_config_content handles no auth mode", {
  key <- .derive_key(c("test"), 32)
  enc_url <- .encrypt_string("https://example.com", key)
  
  content <- .generate_config_content(
    encrypted_url = enc_url,
    encrypted_token = NULL,
    key_factors = c("FACTOR"),
    use_auth = FALSE
  )
  
  expect_match(content, "USE_AUTHENTICATION = false")
  expect_match(content, "ENCRYPTED_TOKEN_LEN = 0")
})

test_that(".generate_config_content includes timestamp", {
  key <- .derive_key(c("test"), 32)
  enc_url <- .encrypt_string("https://example.com", key)
  
  content <- .generate_config_content(
    encrypted_url = enc_url,
    encrypted_token = NULL,
    key_factors = c("FACTOR"),
    use_auth = FALSE
  )
  
  expect_match(content, "Generated:")
})

# ============================================================================
# Section 5: autograder_configure Input Validation
# ============================================================================

test_that("autograder_configure validates package path", {
  output <- capture.output({
    result <- autograder_configure(
      url = "https://example.com/repo",
      token = "test_token",
      pkg_path = "/nonexistent/path/to/package",
      install = FALSE
    )
  })
  
  expect_false(result$success)
})

# ============================================================================
# Section 6: Auth Mode (from C++)
# ============================================================================

test_that(".cpp_get_auth_mode returns secure", {
  result <- .cpp_get_auth_mode()
  expect_equal(result, "secure")
})

test_that(".cpp_get_auth_info returns structured result", {
  result <- .cpp_get_auth_info()
  expect_type(result, "list")
  expect_true("mode" %in% names(result))
  expect_equal(result$mode, "secure")
})

# ============================================================================
# Section 7: DEFAULT_KEY_FACTORS
# ============================================================================

test_that(".DEFAULT_KEY_FACTORS exists and is character", {
  expect_true(exists(".DEFAULT_KEY_FACTORS"))
  expect_type(.DEFAULT_KEY_FACTORS, "character")
  expect_true(length(.DEFAULT_KEY_FACTORS) > 0)
})

test_that(".AES_SBOX has correct size", {
  expect_true(exists(".AES_SBOX"))
  expect_equal(length(.AES_SBOX), 256)
  expect_true(all(.AES_SBOX >= 0 & .AES_SBOX <= 255))
})

# ============================================================================
# ADDITIONAL INSTRUCTOR FUNCTION TESTS
# ============================================================================

test_that("autograder_check_build_tools is consistent", {
  result1 <- autograder_check_build_tools()
  result2 <- autograder_check_build_tools()
  expect_equal(result1, result2)
})

test_that("autograder_configure is available", {
  expect_true(is.function(autograder_configure))
})

test_that("autograder_setup_wizard is available", {
  expect_true(is.function(autograder_setup_wizard))
})

test_that("autograder_addin_configure is available", {
  expect_true(is.function(autograder_addin_configure))
})

test_that("autograder_log_config accepts valid levels", {
  expect_no_error(autograder_log_config(level = "debug"))
  expect_no_error(autograder_log_config(level = "info"))
  expect_no_error(autograder_log_config(level = "warn"))
  expect_no_error(autograder_log_config(level = "error"))
})

test_that("autograder_log_config rejects invalid level", {
  expect_error(autograder_log_config(level = "invalid"))
})

test_that("autograder_log_history returns log data", {
  history <- autograder_log_history()
  expect_true(is.list(history) || is.null(history) || is.character(history))
})

test_that("autograder_log_clear clears logs", {
  autograder_log_clear()
  expect_no_error(autograder_log_clear())
})
