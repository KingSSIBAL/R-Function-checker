# ============================================================================
# AUTOGRADER - C++ MODULE COVERAGE TESTS
# ============================================================================
#
# Tests specifically targeting C++ modules for maximum coverage:
#   - fetcher.cpp       (network fetching)
#   - validator.cpp     (input validation)
#   - formatter.cpp     (output formatting)
#   - comparator.cpp    (fast comparison)
#   - encryption.cpp    (AES encryption)
#   - types.h           (core types)
#   - exceptions.h      (error handling)
#
# ============================================================================

# ============================================================================
# NETWORK FETCHER TESTS
# ============================================================================

test_that(".cpp_has_internet returns consistent results", {
  result1 <- .cpp_has_internet()
  result2 <- .cpp_has_internet()
  
  expect_type(result1, "logical")
  expect_equal(result1, result2)
})

test_that(".cpp_fetch_function_content validates input", {
  # All these should fail validation before network call
  expect_error(.cpp_fetch_function_content(""))
  expect_error(.cpp_fetch_function_content("../etc/passwd"))
  expect_error(.cpp_fetch_function_content("test;rm -rf"))
  expect_error(.cpp_fetch_function_content("test|cat"))
})

test_that(".cpp_fetch_problems_list returns character vector", {
  skip_on_cran()
  skip_if_offline()
  
  result <- .cpp_fetch_problems_list()
  expect_type(result, "character")
})

# ============================================================================
# VALIDATION MODULE TESTS
# ============================================================================

test_that(".cpp_validate_function_name comprehensive tests", {
  # Valid patterns
  valid_names <- c(
    "fibonacci",
    "my_func",
    "myFunc",
    "MyFunc123",
    "_private",
    "func_with_numbers_123",
    "a",
    "A",
    "_",
    "test-hyphen"
  )
  
  for (name in valid_names) {
    expect_true(.cpp_validate_function_name(name), 
                info = sprintf("'%s' should be valid", name))
  }
  
  # Invalid patterns
  invalid_names <- c(
    "",
    "123start",  # starts with number
    "test name", # space
    "test\ttab",
    "test\nline",
    "../parent",
    "./current",
    "test;cmd",
    "test|pipe",
    "test&bg",
    "test>redirect",
    "test<input",
    "test`backtick`",
    "${var}",
    "~home",
    "test//double",
    "test\\backslash"
  )
  
  for (name in invalid_names) {
    expect_false(.cpp_validate_function_name(name), 
                 info = sprintf("'%s' should be invalid", name))
  }
})

test_that(".cpp_sanitize_for_url handles special characters", {
  if (exists(".cpp_sanitize_for_url", mode = "function")) {
    # Space becomes %20
    result <- .cpp_sanitize_for_url("hello world")
    expect_true(grepl("%20", result) || !grepl(" ", result))
    
    # Alphanumeric unchanged
    result <- .cpp_sanitize_for_url("test123")
    expect_equal(result, "test123")
  } else {
    skip("Function not exported")
  }
})

# ============================================================================
# FORMAT MODULE TESTS
# ============================================================================

test_that(".cpp_format_output handles all R types", {
  # NULL
  expect_true(grepl("NULL", .cpp_format_output(NULL, 100L), ignore.case = TRUE))
  
  # Scalars
  expect_true(nchar(.cpp_format_output(42L, 100L)) > 0)
  expect_true(nchar(.cpp_format_output(3.14, 100L)) > 0)
  expect_true(nchar(.cpp_format_output("text", 100L)) > 0)
  expect_true(nchar(.cpp_format_output(TRUE, 100L)) > 0)
  
  # Vectors
  expect_true(nchar(.cpp_format_output(1:5, 100L)) > 0)
  expect_true(nchar(.cpp_format_output(c("a", "b"), 100L)) > 0)
  expect_true(nchar(.cpp_format_output(c(TRUE, FALSE), 100L)) > 0)
  
  # Long vectors (truncation)
  result <- .cpp_format_output(1:1000, 50L)
  expect_lte(nchar(result), 60)
  
  # Lists
  expect_true(nchar(.cpp_format_output(list(a = 1), 100L)) > 0)
  expect_true(nchar(.cpp_format_output(list(), 100L)) > 0)
  
  # Matrix
  m <- matrix(1:9, 3, 3)
  expect_true(nchar(.cpp_format_output(m, 100L)) > 0)
  
  # Data frame
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  expect_true(nchar(.cpp_format_output(df, 100L)) > 0)
})

test_that(".cpp_format_output handles special values", {
  # NA
  result <- .cpp_format_output(NA, 100L)
  expect_true(nchar(result) > 0)
  
  # Inf - may be formatted differently
  result <- .cpp_format_output(Inf, 100L)
  expect_true(nchar(result) > 0)
  
  # -Inf
  result <- .cpp_format_output(-Inf, 100L)
  expect_true(nchar(result) > 0)
  
  # NaN
  result <- .cpp_format_output(NaN, 100L)
  expect_true(nchar(result) > 0)
  
  # Vector with NA
  result <- .cpp_format_output(c(1, NA, 3), 100L)
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output respects max_length", {
  for (max_len in c(10L, 25L, 50L, 100L, 200L)) {
    result <- .cpp_format_output(1:10000, max_len)
    expect_lte(nchar(result), max_len + 10)  # Allow some tolerance
  }
})

# ============================================================================
# COMPARE MODULE TESTS
# ============================================================================

test_that(".cpp_compare_fast type-specific tests", {
  # Integer equality
  expect_true(.cpp_compare_fast(1L, 1L, 0))
  expect_false(.cpp_compare_fast(1L, 2L, 0))
  
  # Numeric with tolerance
  expect_true(.cpp_compare_fast(1.0, 1.0, 0))
  expect_true(.cpp_compare_fast(1.0, 1.0 + 1e-11, 1e-10))
  expect_false(.cpp_compare_fast(1.0, 1.1, 1e-10))
  
  # Character exact
  expect_true(.cpp_compare_fast("abc", "abc", 0))
  expect_false(.cpp_compare_fast("abc", "ABC", 0))
  
  # Logical
  expect_true(.cpp_compare_fast(TRUE, TRUE, 0))
  expect_false(.cpp_compare_fast(TRUE, FALSE, 0))
  
  # NULL
  expect_true(.cpp_compare_fast(NULL, NULL, 0))
  
  # List
  expect_true(.cpp_compare_fast(list(a=1, b=2), list(a=1, b=2), 0))
  expect_false(.cpp_compare_fast(list(a=1, b=2), list(a=1, b=3), 0))
})

test_that(".cpp_compare_fast handles nested structures", {
  # Nested lists
  l1 <- list(a = list(b = 1, c = 2), d = 3)
  l2 <- list(a = list(b = 1, c = 2), d = 3)
  l3 <- list(a = list(b = 1, c = 99), d = 3)
  
  expect_true(.cpp_compare_fast(l1, l2, 0))
  expect_false(.cpp_compare_fast(l1, l3, 0))
  
  # List with vectors
  l1 <- list(v = 1:5, s = "test")
  l2 <- list(v = 1:5, s = "test")
  expect_true(.cpp_compare_fast(l1, l2, 0))
})

test_that(".cpp_compare_fast handles dimension attributes", {
  # Same data, different dimensions
  m1 <- matrix(1:6, 2, 3)
  m2 <- matrix(1:6, 3, 2)
  
  result <- .cpp_compare_fast(m1, m2, 0)
  expect_type(result, "logical")
  
  # Same dimensions, same data
  m3 <- matrix(1:6, 2, 3)
  expect_true(.cpp_compare_fast(m1, m3, 0))
})

test_that(".cpp_compare_fast handles names", {
  # Named vs unnamed
  named <- c(a = 1, b = 2)
  unnamed <- c(1, 2)
  
  result <- .cpp_compare_fast(named, unnamed, 0)
  expect_type(result, "logical")
  
  # Same names
  named2 <- c(a = 1, b = 2)
  expect_true(.cpp_compare_fast(named, named2, 0))
})

# ============================================================================
# CRYPTO MODULE TESTS
# ============================================================================

test_that(".cpp_encrypt and .cpp_decrypt are inverse operations", {
  test_cases <- list(
    "",
    "a",
    "Hello, World!",
    "Line1\nLine2\nLine3",
    "Tabs\tand\tspaces",
    paste(rep("x", 10000), collapse = ""),
    "Unicode: 你好世界 🎉",
    "Special: !@#$%^&*()_+-=[]{}|;':\",./<>?"
  )
  
  for (text in test_cases) {
    enc <- .cpp_encrypt(text, "test_key")
    dec <- .cpp_decrypt(enc, "test_key")
    expect_equal(dec, text, info = sprintf("Failed for: %s...", substr(text, 1, 20)))
  }
})

test_that(".cpp_encrypt_base64 produces valid base64", {
  text <- "test message"
  result <- .cpp_encrypt_base64(text, "key")
  
  # Base64 should only contain valid characters
  expect_true(grepl("^[A-Za-z0-9+/=]+$", result))
})

test_that(".cpp_derive_key produces correct length keys", {
  for (len in c(16L, 24L, 32L, 64L)) {
    key <- .cpp_derive_key(c("salt"), len)
    # Hex encoding doubles the length
    expect_equal(nchar(key), len * 2)
  }
})

test_that(".cpp_generate_key produces unique values", {
  keys <- character(10)
  for (i in 1:10) {
    keys[i] <- .cpp_generate_key(32L)
  }
  
  expect_equal(length(unique(keys)), 10)
})

test_that(".cpp_encrypt_url and .cpp_decrypt_url round trip", {
  urls <- c(
    "https://example.com",
    "https://example.com/path/to/resource",
    "https://example.com/path?query=value&other=123",
    "http://localhost:8080/test"
  )
  
  for (url in urls) {
    enc <- .cpp_encrypt_url(url)
    dec <- .cpp_decrypt_url(enc)
    expect_equal(dec, url)
  }
})

# ============================================================================
# TYPE HELPER TESTS
# ============================================================================

test_that(".cpp_get_type identifies all basic types", {
  expect_equal(.cpp_get_type(NULL), "NULL")
  expect_equal(.cpp_get_type(TRUE), "logical")
  expect_equal(.cpp_get_type(1L), "integer")
  expect_equal(.cpp_get_type(1.0), "numeric")
  expect_equal(.cpp_get_type("a"), "character")
  expect_equal(.cpp_get_type(raw(1)), "raw")
  expect_equal(.cpp_get_type(list()), "list")
  expect_equal(.cpp_get_type(complex(1)), "complex")
})

test_that(".cpp_get_type handles vectors", {
  expect_equal(.cpp_get_type(1:10), "integer")
  expect_equal(.cpp_get_type(c(1.0, 2.0)), "numeric")
  expect_equal(.cpp_get_type(c("a", "b")), "character")
  expect_equal(.cpp_get_type(c(TRUE, FALSE)), "logical")
})

test_that(".cpp_get_type handles special objects", {
  # Function - may be "closure" or "function" depending on implementation
  expect_true(.cpp_get_type(function() {}) %in% c("closure", "function"))
  expect_equal(.cpp_get_type(new.env()), "environment")
  expect_equal(.cpp_get_type(expression(x)), "expression")
})

# ============================================================================
# VERSION TEST
# ============================================================================

test_that(".cpp_get_version returns valid version", {
  version <- .cpp_get_version()
  expect_type(version, "character")
  
  # Should be x.y.z format
  parts <- strsplit(version, "\\.")[[1]]
  expect_gte(length(parts), 2)
})

# ============================================================================
# INTEGRATION: FULL WORKFLOW SIMULATION
# ============================================================================

test_that("full validation-fetch-compare workflow", {
  skip_on_cran()
  skip_if_offline()
  
  # 1. Validate function name
  expect_true(.cpp_validate_function_name("fibonacci"))
  
  # 2. Check internet
  has_net <- .cpp_has_internet()
  skip_if(!has_net, "No internet")
  
  # 3. Fetch content
  code <- .cpp_fetch_function_content("fibonacci")
  expect_type(code, "character")
  expect_true(length(code) > 0)
  
  # 4. Parse and execute
  code_text <- paste(code, collapse = "\n")
  env <- new.env()
  eval(parse(text = code_text), envir = env)
  
  # 5. Find function
  funcs <- ls(env)
  expect_true(length(funcs) > 0)
  
  # 6. Run and compare
  if (exists("fibonacci", envir = env)) {
    fib <- get("fibonacci", envir = env)
    result <- fib(5)
    expected <- c(1, 1, 2, 3, 5)
    
    expect_true(.cpp_compare_fast(result, expected, 1e-10))
  }
})

# ============================================================================
# STRESS TESTS
# ============================================================================

test_that("functions handle rapid repeated calls", {
  # Validation
  for (i in 1:100) {
    expect_true(.cpp_validate_function_name("test"))
  }
  
  # Comparison
  for (i in 1:100) {
    expect_true(.cpp_compare_fast(1:10, 1:10, 1e-10))
  }
  
  # Formatting
  for (i in 1:100) {
    result <- .cpp_format_output(1:100, 50L)
    expect_type(result, "character")
  }
})

test_that("encryption handles rapid key generation", {
  keys <- character(50)
  for (i in 1:50) {
    keys[i] <- .cpp_generate_key(32L)
  }
  
  # All should be unique
  expect_equal(length(unique(keys)), 50)
})

# ============================================================================
# MEMORY LEAK PREVENTION TESTS
# ============================================================================

test_that("large object formatting doesn't leak", {
  # Create and format many large objects
  for (i in 1:10) {
    big_vec <- 1:100000
    result <- .cpp_format_output(big_vec, 100L)
    rm(big_vec)
    expect_type(result, "character")
  }
  
  # Should complete without memory issues
  gc()
})

test_that("encryption/decryption doesn't leak", {
  long_text <- paste(rep("a", 10000), collapse = "")
  
  for (i in 1:10) {
    enc <- .cpp_encrypt(long_text, "key")
    dec <- .cpp_decrypt(enc, "key")
    expect_equal(dec, long_text)
  }
  
  gc()
})
