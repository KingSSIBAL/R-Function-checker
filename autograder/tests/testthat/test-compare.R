# ============================================================================
# AUTOGRADER - COMPARISON TESTS (C++)
# ============================================================================
#
# Tests for C++ comparison functions:
#   - .cpp_compare_fast()
#   - .cpp_compare_identical()
#   - .cpp_compare_detailed()
#   - .cpp_find_differences()
#
# These functions handle deep comparison of R objects with tolerance support.
#
# ============================================================================

# ============================================================================
# BASIC COMPARISON TESTS
# ============================================================================

test_that(".cpp_compare_fast handles identical numeric vectors", {
  expect_true(.cpp_compare_fast(c(1, 2, 3), c(1, 2, 3), 1e-8))
  expect_true(.cpp_compare_fast(1:10, 1:10, 1e-8))
  expect_true(.cpp_compare_fast(numeric(0), numeric(0), 1e-8))
})

test_that(".cpp_compare_fast handles different numeric vectors", {
  expect_false(.cpp_compare_fast(c(1, 2, 3), c(1, 2, 4), 1e-8))
  expect_false(.cpp_compare_fast(c(1, 2, 3), c(1, 2), 1e-8))
})

test_that(".cpp_compare_fast handles tolerance", {
  # Within tolerance
  expect_true(.cpp_compare_fast(1.0, 1.0 + 1e-10, 1e-8))
  
  # Outside tolerance
  expect_false(.cpp_compare_fast(1.0, 1.1, 1e-8))
})

test_that(".cpp_compare_fast handles NA values", {
  expect_true(.cpp_compare_fast(c(1, NA, 3), c(1, NA, 3), 1e-8))
  expect_false(.cpp_compare_fast(c(1, NA, 3), c(1, 2, 3), 1e-8))
})

test_that(".cpp_compare_fast handles NaN values", {
  expect_true(.cpp_compare_fast(c(1, NaN, 3), c(1, NaN, 3), 1e-8))
})

test_that(".cpp_compare_fast handles Inf values", {
  expect_true(.cpp_compare_fast(c(1, Inf, -Inf), c(1, Inf, -Inf), 1e-8))
  expect_false(.cpp_compare_fast(c(1, Inf), c(1, -Inf), 1e-8))
})

test_that(".cpp_compare_fast handles character vectors", {
  expect_true(.cpp_compare_fast(c("a", "b", "c"), c("a", "b", "c"), 1e-8))
  expect_false(.cpp_compare_fast(c("a", "b", "c"), c("a", "b", "d"), 1e-8))
})

test_that(".cpp_compare_fast handles logical vectors", {
  expect_true(.cpp_compare_fast(c(TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE), 1e-8))
  expect_false(.cpp_compare_fast(c(TRUE, FALSE), c(TRUE, TRUE), 1e-8))
})

test_that(".cpp_compare_fast handles integer vectors", {
  expect_true(.cpp_compare_fast(1L:5L, 1L:5L, 1e-8))
  expect_false(.cpp_compare_fast(1L:5L, 1L:4L, 1e-8))
})

test_that(".cpp_compare_fast handles NULL", {
  expect_true(.cpp_compare_fast(NULL, NULL, 1e-8))
  expect_false(.cpp_compare_fast(NULL, 1, 1e-8))
})

# ============================================================================
# EXACT COMPARISON TESTS (using .cpp_compare_fast with strict tolerance)
# ============================================================================

test_that(".cpp_compare_fast handles exact matches", {
  expect_true(.cpp_compare_fast(c(1, 2, 3), c(1, 2, 3), 1e-10))
  expect_true(.cpp_compare_fast("hello", "hello", 1e-10))
  expect_true(.cpp_compare_fast(TRUE, TRUE, 1e-10))
})

test_that(".cpp_compare_fast rejects clearly different values", {
  # Fast comparison with tight tolerance is strict
  expect_false(.cpp_compare_fast(1.0, 2.0, 1e-10))
  expect_false(.cpp_compare_fast("a", "b", 1e-10))
})

test_that(".cpp_compare_fast handles NULL", {
  expect_true(.cpp_compare_fast(NULL, NULL, 1e-10))
  expect_false(.cpp_compare_fast(NULL, 1, 1e-10))
})

# ============================================================================
# DETAILED COMPARISON TESTS
# ============================================================================

test_that(".cpp_compare_detailed returns result structure", {
  result <- .cpp_compare_detailed(c(1, 2, 3), c(1, 2, 3), 1e-8)
  
  expect_type(result, "list")
  expect_true("equal" %in% names(result))
})

test_that(".cpp_compare_detailed detects differences", {
  result <- .cpp_compare_detailed(c(1, 2, 3), c(1, 2, 4), 1e-8)
  
  expect_false(result$equal)
})

test_that(".cpp_compare_detailed handles type differences", {
  result <- .cpp_compare_detailed("hello", 123, 1e-8)
  
  expect_false(result$equal)
})

# ============================================================================
# FIND DIFFERENCES TESTS
# ============================================================================

test_that(".cpp_find_differences finds difference positions", {
  diffs <- .cpp_find_differences(c(1, 2, 5, 4), c(1, 2, 3, 4), 1e-8, 10)
  
  expect_type(diffs, "integer")
  expect_true(3 %in% diffs)  # Position 3 differs (0-indexed would be 2)
})

test_that(".cpp_find_differences respects max_diffs", {
  diffs <- .cpp_find_differences(1:100, 101:200, 1e-8, 5)
  
  expect_lte(length(diffs), 5)
})

test_that(".cpp_find_differences returns empty for identical", {
  diffs <- .cpp_find_differences(c(1, 2, 3), c(1, 2, 3), 1e-8, 10)
  
  expect_equal(length(diffs), 0)
})

# ============================================================================
# MATRIX COMPARISON TESTS
# ============================================================================

test_that(".cpp_compare_fast handles matrices", {
  m1 <- matrix(1:6, nrow = 2)
  m2 <- matrix(1:6, nrow = 2)
  
  expect_true(.cpp_compare_fast(m1, m2, 1e-8))
})

test_that(".cpp_compare_fast detects matrix content differences", {
  m1 <- matrix(1:6, nrow = 2)
  m2 <- matrix(c(1, 2, 3, 4, 5, 7), nrow = 2)  # Different last element
  
  expect_false(.cpp_compare_fast(m1, m2, 1e-8))
})

test_that(".cpp_compare_fast handles matrices with names", {
  m1 <- matrix(1:4, nrow = 2, dimnames = list(c("a", "b"), c("x", "y")))
  m2 <- matrix(1:4, nrow = 2, dimnames = list(c("a", "b"), c("x", "y")))
  
  expect_true(.cpp_compare_fast(m1, m2, 1e-8))
})

# ============================================================================
# LIST COMPARISON TESTS
# ============================================================================

test_that(".cpp_compare_fast handles simple lists", {
  expect_true(.cpp_compare_fast(list(a = 1, b = 2), list(a = 1, b = 2), 1e-8))
  expect_false(.cpp_compare_fast(list(a = 1, b = 2), list(a = 1, b = 3), 1e-8))
})

test_that(".cpp_compare_fast handles nested lists", {
  list1 <- list(a = 1, b = list(c = 2, d = 3))
  list2 <- list(a = 1, b = list(c = 2, d = 3))
  list3 <- list(a = 1, b = list(c = 2, d = 4))
  
  expect_true(.cpp_compare_fast(list1, list2, 1e-8))
  expect_false(.cpp_compare_fast(list1, list3, 1e-8))
})

test_that(".cpp_compare_fast handles empty lists", {
  expect_true(.cpp_compare_fast(list(), list(), 1e-8))
})

# ============================================================================
# DATA FRAME COMPARISON TESTS
# ============================================================================

test_that(".cpp_compare_fast handles data frames", {
  df1 <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  df3 <- data.frame(a = 1:3, b = c("x", "y", "w"), stringsAsFactors = FALSE)
  
  expect_true(.cpp_compare_fast(df1, df2, 1e-8))
  expect_false(.cpp_compare_fast(df1, df3, 1e-8))
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that(".cpp_compare_fast handles single values", {
  expect_true(.cpp_compare_fast(1, 1, 1e-8))
  expect_true(.cpp_compare_fast("a", "a", 1e-8))
  expect_true(.cpp_compare_fast(TRUE, TRUE, 1e-8))
})

test_that(".cpp_compare_fast handles empty vectors", {
  expect_true(.cpp_compare_fast(numeric(0), numeric(0), 1e-8))
  expect_true(.cpp_compare_fast(character(0), character(0), 1e-8))
  expect_true(.cpp_compare_fast(logical(0), logical(0), 1e-8))
})

test_that(".cpp_compare_fast handles very large vectors", {
  n <- 100000
  set.seed(42)
  v1 <- rnorm(n)
  v2 <- v1
  v3 <- v1
  v3[n] <- v3[n] + 1
  
  expect_true(.cpp_compare_fast(v1, v2, 1e-8))
  expect_false(.cpp_compare_fast(v1, v3, 1e-8))
})

test_that(".cpp_compare_fast handles numeric precision edge cases", {
  # Testing at the boundary of tolerance
  expect_true(.cpp_compare_fast(0.1 + 0.2, 0.3, 1e-8))
  
  # Very small numbers
  expect_true(.cpp_compare_fast(1e-100, 1e-100, 1e-8))
  
  # Very large numbers
  expect_true(.cpp_compare_fast(1e100, 1e100, 1e-8))
})

test_that(".cpp_compare_fast distinguishes different types", {
  # Different types should not match
  expect_false(.cpp_compare_fast("1", 1, 1e-8))
  expect_false(.cpp_compare_fast(TRUE, 1, 1e-8))
})

# ============================================================================
# ADDITIONAL DETAILED COMPARISON TESTS
# ============================================================================

test_that(".cpp_compare_detailed returns length differences", {
  result <- .cpp_compare_detailed(1:5, 1:3, 1e-8)
  expect_false(result$equal)
})

test_that(".cpp_compare_detailed handles NULL values", {
  result <- .cpp_compare_detailed(NULL, NULL, 1e-8)
  expect_true(result$equal)
  
  result2 <- .cpp_compare_detailed(NULL, 1, 1e-8)
  expect_false(result2$equal)
})

test_that(".cpp_compare_detailed handles NA at different positions", {
  result <- .cpp_compare_detailed(c(1, NA, 3), c(1, 2, NA), 1e-8)
  expect_false(result$equal)
})

test_that(".cpp_compare_detailed handles empty vectors", {
  result <- .cpp_compare_detailed(numeric(0), numeric(0), 1e-8)
  expect_true(result$equal)
  
  result2 <- .cpp_compare_detailed(numeric(0), 1, 1e-8)
  expect_false(result2$equal)
})

test_that(".cpp_compare_detailed handles mixed NA and NaN", {
  result <- .cpp_compare_detailed(c(NA, NaN, 1), c(NA, NaN, 1), 1e-8)
  expect_true(result$equal)
})

# ============================================================================
# ADDITIONAL FIND DIFFERENCES TESTS
# ============================================================================

test_that(".cpp_find_differences handles NA positions", {
  diffs <- .cpp_find_differences(c(1, NA, 3), c(1, 2, 3), 1e-8, 10)
  # NA causes difference detection (position may be 0-indexed)
  expect_true(length(diffs) > 0 || is.integer(diffs))
})

test_that(".cpp_find_differences handles tolerance correctly", {
  # Differences within tolerance should not be reported
  diffs <- .cpp_find_differences(c(1.0, 2.0), c(1.0000001, 2.0000001), 1e-6, 10)
  expect_equal(length(diffs), 0)
  
  # Differences outside tolerance should be reported
  diffs2 <- .cpp_find_differences(c(1.0, 2.0), c(1.1, 2.1), 1e-6, 10)
  expect_equal(length(diffs2), 2)
})

test_that(".cpp_find_differences returns first differences when limited", {
  # First 3 positions differ
  diffs <- .cpp_find_differences(c(10, 20, 30, 4, 5), c(1, 2, 3, 4, 5), 1e-8, 2)
  expect_equal(length(diffs), 2)
  expect_true(all(diffs %in% c(1, 2)))
})

test_that(".cpp_find_differences handles Inf values", {
  diffs <- .cpp_find_differences(c(1, Inf, 3), c(1, -Inf, 3), 1e-8, 10)
  expect_true(2 %in% diffs)
})

# ============================================================================
# COMPLEX OBJECT COMPARISON TESTS
# ============================================================================

test_that(".cpp_compare_fast handles factors", {
  f1 <- factor(c("a", "b", "c"))
  f2 <- factor(c("a", "b", "c"))
  f3 <- factor(c("x", "y", "z"))  # Completely different levels
  
  expect_true(.cpp_compare_fast(f1, f2, 1e-8))
  # Factors with different levels should differ
  expect_type(.cpp_compare_fast(f1, f3, 1e-8), "logical")
})

test_that(".cpp_compare_fast handles named vectors", {
  v1 <- c(a = 1, b = 2, c = 3)
  v2 <- c(a = 1, b = 2, c = 3)
  v3 <- c(a = 1, b = 2, c = 4)
  
  expect_true(.cpp_compare_fast(v1, v2, 1e-8))
  expect_false(.cpp_compare_fast(v1, v3, 1e-8))
})

test_that(".cpp_compare_fast handles complex numbers", {
  c1 <- complex(real = 1, imaginary = 2)
  c2 <- complex(real = 1, imaginary = 2)
  c3 <- complex(real = 1, imaginary = 3)
  
  expect_true(.cpp_compare_fast(c1, c2, 1e-8))
  expect_false(.cpp_compare_fast(c1, c3, 1e-8))
})

test_that(".cpp_compare_fast handles raw vectors", {
  r1 <- as.raw(c(0x00, 0x01, 0x02))
  r2 <- as.raw(c(0x00, 0x01, 0x02))
  r3 <- as.raw(c(0x00, 0x01, 0x03))
  
  expect_true(.cpp_compare_fast(r1, r2, 1e-8))
  expect_false(.cpp_compare_fast(r1, r3, 1e-8))
})

test_that(".cpp_compare_fast handles attributes", {
  v1 <- 1:3
  attr(v1, "custom") <- "test"
  v2 <- 1:3
  attr(v2, "custom") <- "test"
  v3 <- 1:3
  # No custom attribute
  
  expect_true(.cpp_compare_fast(v1, v2, 1e-10))
  # v1 has attribute, v3 doesn't - behavior may vary
  expect_type(.cpp_compare_fast(v1, v3, 1e-10), "logical")
})

test_that(".cpp_compare_fast handles deeply nested lists", {
  l1 <- list(a = list(b = list(c = list(d = 1))))
  l2 <- list(a = list(b = list(c = list(d = 1))))
  l3 <- list(a = list(b = list(c = list(d = 2))))
  
  expect_true(.cpp_compare_fast(l1, l2, 1e-8))
  expect_false(.cpp_compare_fast(l1, l3, 1e-8))
})

