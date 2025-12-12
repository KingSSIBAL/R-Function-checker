# ============================================================================
# AUTOGRADER - NETWORK TESTS
# ============================================================================
#
# Tests for network-related functions:
#   - .cpp_fetch_function_content()
#   - .cpp_fetch_problems_list()
#   - .cpp_has_internet()
#   - list_problems()
#   - Error handling for network issues
#
# ============================================================================

# ============================================================================
# INTERNET CONNECTIVITY TESTS
# ============================================================================

test_that(".cpp_has_internet returns boolean", {
  result <- .cpp_has_internet()
  expect_type(result, "logical")
})

# ============================================================================
# FETCH FUNCTION CONTENT TESTS
# ============================================================================

test_that(".cpp_fetch_function_content retrieves code", {
  skip_on_cran()
  skip_if_offline()
  
  code <- .cpp_fetch_function_content("fibonacci")
  
  expect_type(code, "character")
  # It returns a vector of lines, so total content should be substantial
  expect_true(sum(nchar(code)) > 10)
})

test_that(".cpp_fetch_function_content returns R code", {
  skip_on_cran()
  skip_if_offline()
  
  code <- .cpp_fetch_function_content("fibonacci")
  
  # Collapse the vector of lines and check for function definition
  code_text <- paste(code, collapse = "\n")
  expect_true(grepl("function", code_text, ignore.case = TRUE))
})

test_that(".cpp_fetch_function_content handles nonexistent files", {
  skip_on_cran()
  skip_if_offline()
  
  # Should throw an error for nonexistent problem
  expect_error(.cpp_fetch_function_content("nonexistent_xyz123"))
})

# ============================================================================
# FETCH PROBLEMS LIST TESTS
# ============================================================================

test_that(".cpp_fetch_problems_list returns problem list", {
  skip_on_cran()
  skip_if_offline()
  
  problems <- .cpp_fetch_problems_list()
  
  expect_type(problems, "character")
  expect_true(length(problems) > 0)
})

test_that(".cpp_fetch_problems_list includes expected problems", {
  skip_on_cran()
  skip_if_offline()
  
  problems_raw <- .cpp_fetch_problems_list()
  
  # The raw content contains problem names as strings
  problems_text <- paste(problems_raw, collapse = " ")
  
  # At least some expected problems should be mentioned
  expect_true(grepl("fibonacci", problems_text))
  expect_true(grepl("factorial", problems_text))
})

# ============================================================================
# LIST_PROBLEMS FUNCTION TESTS
# ============================================================================

test_that("list_problems returns problem list", {
  skip_on_cran()
  skip_if_offline()
  
  problems <- suppressMessages(list_problems())
  
  expect_type(problems, "character")
  expect_true(length(problems) > 0)
})

test_that("list_problems includes known problems", {
  skip_on_cran()
  skip_if_offline()
  
  problems <- suppressMessages(list_problems())
  
  expect_true("fibonacci" %in% problems)
})

# ============================================================================
# ERROR CLASS TESTS
# ============================================================================

test_that("network_error class is properly defined", {
  err <- tryCatch(
    network_error("Test network error"),
    error = function(e) e
  )
  
  expect_s3_class(err, "network_error")
  expect_s3_class(err, "error")
})

test_that("function_not_found_error class is properly defined", {
  err <- tryCatch(
    function_not_found_error("test_func"),
    error = function(e) e
  )
  
  expect_s3_class(err, "function_not_found_error")
  expect_s3_class(err, "error")
})

test_that("test_execution_error class is properly defined", {
  err <- tryCatch(
    test_execution_error("Test execution failed", 1),
    error = function(e) e
  )
  
  expect_s3_class(err, "test_execution_error")
  expect_s3_class(err, "error")
})

# ============================================================================
# VALIDATION TESTS (NETWORK SECURITY)
# ============================================================================

test_that(".cpp_validate_function_name accepts valid names", {
  # Returns TRUE for valid names
  expect_true(.cpp_validate_function_name("fibonacci"))
  expect_true(.cpp_validate_function_name("my_function"))
  expect_true(.cpp_validate_function_name("testFunc123"))
})

test_that(".cpp_validate_function_name rejects path traversal", {
  # Returns FALSE for invalid names
  expect_false(.cpp_validate_function_name("../../../etc/passwd"))
  expect_false(.cpp_validate_function_name("..\\..\\windows\\system32"))
})

test_that(".cpp_validate_function_name rejects command injection", {
  expect_false(.cpp_validate_function_name("test; rm -rf /"))
  expect_false(.cpp_validate_function_name("test && cat /etc/passwd"))
  expect_false(.cpp_validate_function_name("test | cat secret"))
})

test_that(".cpp_validate_function_name rejects script injection", {
  expect_false(.cpp_validate_function_name("test<script>"))
  expect_false(.cpp_validate_function_name("test$(command)"))
})

# ============================================================================
# CACHING/CONSISTENCY TESTS
# ============================================================================

test_that("repeated fetches return consistent results", {
  skip_on_cran()
  skip_if_offline()
  
  code1 <- .cpp_fetch_function_content("fibonacci")
  code2 <- .cpp_fetch_function_content("fibonacci")
  
  expect_equal(code1, code2)
})
