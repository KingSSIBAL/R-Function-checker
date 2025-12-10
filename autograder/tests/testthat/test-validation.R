# tests/testthat/test-validation.R

test_that("validate_test_cases requires inputs field", {
  expect_error(
    validate_test_cases(list(), "test_fn"),
    "missing required 'inputs' field"
  )
  
  expect_error(
    validate_test_cases(list(inputs = NULL), "test_fn"),
    "must have at least one test"
  )
  
  expect_error(
    validate_test_cases(list(inputs = list()), "test_fn"),
    "must have at least one test"
  )
})

test_that("validate_test_cases handles descriptions field", {
  # Missing descriptions - should auto-generate
  test_case <- list(
    inputs = list(list(x = 1), list(x = 2))
  )
  result <- validate_test_cases(test_case, "test_fn")
  expect_equal(length(result$descriptions), 2)
  expect_equal(result$descriptions[1], "Test 1")
  
  # Wrong length descriptions
  test_case_bad <- list(
    inputs = list(list(x = 1), list(x = 2)),
    descriptions = c("Only one")
  )
  expect_error(
    validate_test_cases(test_case_bad, "test_fn"),
    "descriptions length.*doesn't match"
  )
})

test_that("validate_test_cases handles hidden field", {
  # Missing hidden - should default to FALSE
  test_case <- list(
    inputs = list(list(x = 1))
  )
  result <- validate_test_cases(test_case, "test_fn")
  expect_equal(result$hidden, FALSE)
  
  # Wrong length hidden
  test_case_bad <- list(
    inputs = list(list(x = 1), list(x = 2)),
    hidden = c(TRUE)
  )
  expect_error(
    validate_test_cases(test_case_bad, "test_fn"),
    "hidden.*doesn't match"
  )
  
  # Non-logical hidden
  test_case_bad2 <- list(
    inputs = list(list(x = 1)),
    hidden = c("yes")
  )
  expect_error(
    validate_test_cases(test_case_bad2, "test_fn"),
    "must be logical"
  )
})

test_that("validate_test_cases handles points field", {
  # Missing points - should default to 1
  test_case <- list(
    inputs = list(list(x = 1), list(x = 2))
  )
  result <- validate_test_cases(test_case, "test_fn")
  expect_equal(result$points, c(1, 1))
  
  # Wrong length points
  test_case_bad <- list(
    inputs = list(list(x = 1), list(x = 2)),
    points = c(5)
  )
  expect_error(
    validate_test_cases(test_case_bad, "test_fn"),
    "points.*doesn't match"
  )
  
  # Non-numeric points
  test_case_bad2 <- list(
    inputs = list(list(x = 1)),
    points = c("five")
  )
  expect_error(
    validate_test_cases(test_case_bad2, "test_fn"),
    "must be non-negative numeric"
  )
  
  # Negative points
  test_case_bad3 <- list(
    inputs = list(list(x = 1)),
    points = c(-5)
  )
  expect_error(
    validate_test_cases(test_case_bad3, "test_fn"),
    "must be non-negative numeric"
  )
})

test_that("validate_test_cases handles tolerance field", {
  # Missing tolerance - should default to 1e-10
  test_case <- list(
    inputs = list(list(x = 1))
  )
  result <- validate_test_cases(test_case, "test_fn")
  expect_equal(result$tolerance, 1e-10)
  
  # Non-numeric tolerance
  test_case_bad <- list(
    inputs = list(list(x = 1)),
    tolerance = "small"
  )
  expect_error(
    validate_test_cases(test_case_bad, "test_fn"),
    "must be a single non-negative numeric value"
  )
  
  # Negative tolerance
  test_case_bad2 <- list(
    inputs = list(list(x = 1)),
    tolerance = -0.01
  )
  expect_error(
    validate_test_cases(test_case_bad2, "test_fn"),
    "must be a single non-negative numeric value"
  )
  
  # Vector tolerance
  test_case_bad3 <- list(
    inputs = list(list(x = 1)),
    tolerance = c(0.01, 0.02)
  )
  expect_error(
    validate_test_cases(test_case_bad3, "test_fn"),
    "must be a single non-negative numeric value"
  )
})

test_that("validate_test_cases handles optional fields", {
  # Valid expected_type
  test_case <- list(
    inputs = list(list(x = 1)),
    expected_type = "numeric"
  )
  result <- validate_test_cases(test_case, "test_fn")
  expect_equal(result$expected_type, "numeric")
  
  # Invalid expected_type (warns and ignores)
  test_case_bad <- list(
    inputs = list(list(x = 1)),
    expected_type = c("numeric", "integer")
  )
  expect_warning(
    result <- validate_test_cases(test_case_bad, "test_fn"),
    "expected_type.*single character value"
  )
  expect_null(result$expected_type)
  
  # Valid hints
  test_case_hints <- list(
    inputs = list(list(x = 1)),
    hints = c("Check your logic")
  )
  result <- validate_test_cases(test_case_hints, "test_fn")
  expect_equal(result$hints, c("Check your logic"))
  
  # Invalid hints length (warns and ignores)
  test_case_bad_hints <- list(
    inputs = list(list(x = 1), list(x = 2)),
    hints = c("Only one hint")
  )
  expect_warning(
    result <- validate_test_cases(test_case_bad_hints, "test_fn"),
    "hints.*doesn't match"
  )
  expect_null(result$hints)
  
  # Valid comparison_fn
  test_case_fn <- list(
    inputs = list(list(x = 1)),
    comparison_fn = function(a, b) identical(a, b)
  )
  result <- validate_test_cases(test_case_fn, "test_fn")
  expect_true(is.function(result$comparison_fn))
  
  # Invalid comparison_fn (warns and ignores)
  test_case_bad_fn <- list(
    inputs = list(list(x = 1)),
    comparison_fn = "not a function"
  )
  expect_warning(
    result <- validate_test_cases(test_case_bad_fn, "test_fn"),
    "comparison_fn.*must be a function"
  )
  expect_null(result$comparison_fn)
})

# ============================================================================
# C++ VALIDATION FUNCTION TESTS
# ============================================================================

test_that(".cpp_validate_function_name validates empty names", {
  result <- .cpp_validate_function_name("")
  expect_false(result)
})

test_that(".cpp_validate_function_name validates valid names", {
  expect_true(.cpp_validate_function_name("fibonacci"))
  expect_true(.cpp_validate_function_name("test_function"))
  expect_true(.cpp_validate_function_name("myFunction123"))
  expect_true(.cpp_validate_function_name("_private"))
  expect_true(.cpp_validate_function_name("calc_sum"))
})

test_that(".cpp_validate_function_name rejects invalid first characters", {
  # Names starting with numbers
  expect_false(.cpp_validate_function_name("123func"))
  expect_false(.cpp_validate_function_name("1test"))
})

test_that(".cpp_validate_function_name rejects path traversal", {
  expect_false(.cpp_validate_function_name("../evil"))
  expect_false(.cpp_validate_function_name("path/../attack"))
  expect_false(.cpp_validate_function_name("test\\path"))
  expect_false(.cpp_validate_function_name("./local"))
})

test_that(".cpp_validate_function_name rejects special characters", {
  expect_false(.cpp_validate_function_name("func;rm"))
  expect_false(.cpp_validate_function_name("test|pipe"))
  expect_false(.cpp_validate_function_name("bad>redirect"))
  expect_false(.cpp_validate_function_name("func&back"))
  expect_false(.cpp_validate_function_name("test`cmd`"))
})

test_that(".cpp_validate_function_name rejects shell expansion", {
  expect_false(.cpp_validate_function_name("${VAR}"))
  expect_false(.cpp_validate_function_name("$((1+1))"))
  expect_false(.cpp_validate_function_name("~user"))
})

test_that(".cpp_validate_function_name rejects newlines", {
  expect_false(.cpp_validate_function_name("test\ninjection"))
  expect_false(.cpp_validate_function_name("test\rinjection"))
})

test_that(".cpp_validate_function_name handles hyphenated names", {
  expect_true(.cpp_validate_function_name("my-function"))
  expect_true(.cpp_validate_function_name("test-case-1"))
})

test_that(".cpp_validate_function_name handles long names", {
  # Create a very long name (over 256 chars)
  long_name <- paste(rep("a", 300), collapse = "")
  result <- .cpp_validate_function_name(long_name)
  expect_false(result)
})

# ============================================================================
# C++ TYPE CHECKING TESTS
# ============================================================================

test_that(".cpp_get_type returns correct types", {
  expect_equal(.cpp_get_type(1L), "integer")
  expect_equal(.cpp_get_type(1.5), "numeric")
  expect_equal(.cpp_get_type("text"), "character")
  expect_equal(.cpp_get_type(TRUE), "logical")
  expect_equal(.cpp_get_type(NULL), "NULL")
  expect_equal(.cpp_get_type(list()), "list")
})

test_that(".cpp_get_type handles vectors", {
  expect_equal(.cpp_get_type(1:10), "integer")
  expect_equal(.cpp_get_type(c(1.0, 2.0)), "numeric")
  expect_equal(.cpp_get_type(c("a", "b")), "character")
})

test_that(".cpp_get_type handles complex objects", {
  expect_true(.cpp_get_type(data.frame()) %in% c("list", "data.frame"))
  expect_equal(.cpp_get_type(matrix(1:4, 2, 2)), "integer")
})
