# ============================================================================
# AUTOGRADER - CUSTOM COMPARISON TESTS
# ============================================================================
#
# Tests for custom comparison functions and registry
#
# ============================================================================

# ============================================================================
# COMPARISON REGISTRY TESTS
# ============================================================================

test_that("register_comparison validates inputs", {
  # Invalid name
  expect_error(register_comparison("", function(a, b) TRUE))
  expect_error(register_comparison(123, function(a, b) TRUE))
  
  # Invalid function
  expect_error(register_comparison("test", "not a function"))
  
  # Function with too few arguments
  expect_error(register_comparison("test", function(a) TRUE))
})

test_that("register_comparison and get_comparison work", {
  # Register a test comparison
  register_comparison("test_compare", function(a, b) a == b, "Test comparison")
  
  # Get it back
  fn <- get_comparison("test_compare")
  expect_true(is.function(fn))
  expect_true(fn(5, 5))
  expect_false(fn(5, 6))
  
  # Non-existent returns NULL
  expect_null(get_comparison("nonexistent_comparison_xyz"))
})

test_that("list_comparisons returns data frame", {
  result <- list_comparisons()
  
  expect_true(is.data.frame(result))
  expect_true("name" %in% names(result))
  expect_true("description" %in% names(result))
  expect_true(nrow(result) > 0)  # Built-ins should exist
})

test_that("built-in comparisons are registered", {
  # Check that built-ins exist
  expect_true(!is.null(get_comparison("numeric_tolerance")))
  expect_true(!is.null(get_comparison("exact")))
  expect_true(!is.null(get_comparison("case_insensitive")))
  expect_true(!is.null(get_comparison("set")))
  expect_true(!is.null(get_comparison("dataframe")))
  expect_true(!is.null(get_comparison("length")))
  expect_true(!is.null(get_comparison("class")))
  expect_true(!is.null(get_comparison("relative")))
})

# ============================================================================
# COMPARE_NUMERIC TESTS
# ============================================================================

test_that("compare_numeric handles basic cases", {
  expect_true(compare_numeric(1, 1))
  expect_true(compare_numeric(1:10, 1:10))
  expect_false(compare_numeric(1, 2))
  expect_false(compare_numeric(1:10, 1:9))
})
  
test_that("compare_numeric respects tolerance", {
  expect_true(compare_numeric(1.0, 1.0 + 1e-11))
  expect_false(compare_numeric(1.0, 1.1))
})

test_that("compare_numeric handles NA and NaN", {
  expect_true(compare_numeric(c(1, NA), c(1, NA)))
  expect_true(compare_numeric(c(NaN, 1), c(NaN, 1)))
})

# ============================================================================
# COMPARE_EXACT TESTS
# ============================================================================

test_that("compare_exact uses identical()", {
  expect_true(compare_exact(1L, 1L))
  expect_false(compare_exact(1L, 1.0))  # Different types
  expect_true(compare_exact("hello", "hello"))
  expect_false(compare_exact("hello", "HELLO"))
})

# ============================================================================
# COMPARE_CASE_INSENSITIVE TESTS
# ============================================================================

test_that("compare_case_insensitive ignores case", {
  expect_true(compare_case_insensitive("Hello", "HELLO"))
  expect_true(compare_case_insensitive("abc", "ABC"))
  expect_true(compare_case_insensitive(c("A", "b"), c("a", "B")))
  expect_false(compare_case_insensitive("abc", "abd"))
})

test_that("compare_case_insensitive handles non-character", {
  expect_false(compare_case_insensitive(123, "123"))
  expect_false(compare_case_insensitive("123", 123))
})

# ============================================================================
# COMPARE_SET TESTS
# ============================================================================

test_that("compare_set ignores order", {
  expect_true(compare_set(c(3, 1, 2), c(1, 2, 3)))
  expect_true(compare_set(c("c", "a", "b"), c("a", "b", "c")))
})

test_that("compare_set checks length", {
  expect_false(compare_set(1:3, 1:4))
})

test_that("compare_set handles numeric with tolerance", {
  expect_true(compare_set(c(1.0 + 1e-11, 2), c(2, 1.0)))
})

# ============================================================================
# COMPARE_DATAFRAME TESTS
# ============================================================================

test_that("compare_dataframe handles basic cases", {
  df1 <- data.frame(a = 1:3, b = c("x", "y", "z"))
  df2 <- data.frame(a = 1:3, b = c("x", "y", "z"))
  df3 <- data.frame(a = 1:3, b = c("x", "y", "X"))
  
  expect_true(compare_dataframe(df1, df2))
  expect_false(compare_dataframe(df1, df3))
})

test_that("compare_dataframe checks dimensions", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(a = 1:4)
  
  expect_false(compare_dataframe(df1, df2))
})

test_that("compare_dataframe handles numeric tolerance", {
  df1 <- data.frame(a = c(1.0, 2.0))
  df2 <- data.frame(a = c(1.0 + 1e-11, 2.0))
  
  expect_true(compare_dataframe(df1, df2))
})

test_that("compare_dataframe can ignore row order", {
  df1 <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"))
  df2 <- data.frame(a = c(3, 1, 2), b = c("z", "x", "y"))
  
  expect_false(compare_dataframe(df1, df2, ignore_row_order = FALSE))
  expect_true(compare_dataframe(df1, df2, ignore_row_order = TRUE))
})

# ============================================================================
# COMPARE_LENGTH TESTS
# ============================================================================

test_that("compare_length checks only length", {
  expect_true(compare_length(1:10, 11:20))
  expect_true(compare_length(list(1, 2), c(1, 2)))
  expect_false(compare_length(1:10, 1:5))
})

# ============================================================================
# COMPARE_CLASS TESTS
# ============================================================================

test_that("compare_class checks only class", {
  expect_true(compare_class(data.frame(a = 1), data.frame(b = 2)))
  expect_true(compare_class(matrix(1:4, 2), matrix(5:8, 2)))
  expect_false(compare_class(1, "1"))
})

# ============================================================================
# COMPARE_RELATIVE TESTS
# ============================================================================

test_that("compare_relative uses relative tolerance", {
  # 1% tolerance
  expect_true(compare_relative(1000, 1009, rel_tolerance = 0.01))
  expect_false(compare_relative(1000, 1100, rel_tolerance = 0.01))
})

test_that("compare_relative handles special values", {
  expect_true(compare_relative(c(0, Inf), c(0, Inf)))
  expect_true(compare_relative(c(NA, 1), c(NA, 1)))
  expect_false(compare_relative(Inf, -Inf))
})

test_that("compare_relative validates inputs", {
  expect_false(compare_relative("a", "b"))
  expect_false(compare_relative(1:3, 1:2))
})
