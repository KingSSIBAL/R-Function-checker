# tests/testthat/test-cpp-coverage.R

test_that("cpp_compare_fast handles all NA comparison", {
  expect_true(.cpp_compare_fast(NA_real_, NA_real_, 1e-10)[1])
  expect_true(.cpp_compare_fast(c(NA, NA, NA), c(NA, NA, NA), 1e-10)[1])
})

test_that("cpp_compare_fast handles mixed NA positions", {
  expect_false(.cpp_compare_fast(c(1, NA, 3), c(NA, 2, 3), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(NA, 2, 3), c(1, NA, 3), 1e-10)[1])
})

test_that("cpp_compare_fast handles tolerance correctly", {
  # Well within tolerance should pass
  expect_true(.cpp_compare_fast(1.0 + 0.5e-10, 1.0, 1e-10)[1])
  expect_true(.cpp_compare_fast(1.0 + 1e-12, 1.0, 1e-10)[1])
  
  # Outside tolerance should fail  
  expect_false(.cpp_compare_fast(1.0 + 2e-10, 1.0, 1e-10)[1])
  expect_false(.cpp_compare_fast(1.0 + 1e-8, 1.0, 1e-10)[1])
  
  # Test with different tolerance values
  expect_true(.cpp_compare_fast(1.0 + 0.5e-6, 1.0, 1e-6)[1])
  expect_false(.cpp_compare_fast(1.0 + 2e-6, 1.0, 1e-6)[1])
})

test_that("cpp_compare_fast handles negative numbers", {
  expect_true(.cpp_compare_fast(c(-1, -2, -3), c(-1, -2, -3), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(-1, -2, -3), c(-1, -2, -4), 1e-10)[1])
})

test_that("cpp_compare_fast handles zero", {
  expect_true(.cpp_compare_fast(0, 0, 1e-10)[1])
  expect_true(.cpp_compare_fast(c(0, 0, 0), c(0, 0, 0), 1e-10)[1])
  expect_false(.cpp_compare_fast(0, 1e-9, 1e-10)[1])
})

test_that("cpp_compare_fast handles very small numbers", {
  expect_true(.cpp_compare_fast(1e-15, 1e-15, 1e-10)[1])
  # Both essentially zero within tolerance
  expect_true(.cpp_compare_fast(1e-15, 0, 1e-10)[1])
})

test_that("cpp_compare_fast handles very large numbers", {
  expect_true(.cpp_compare_fast(1e15, 1e15, 1e-10)[1])
  # Large difference relative to tolerance
  expect_false(.cpp_compare_fast(1e15, 1e15 + 1e5, 1e-10)[1])
})

test_that("cpp_compare_fast early terminates on size mismatch", {
  # Should return FALSE immediately without checking values
  result <- .cpp_compare_fast(1:1000, 1:999, 1e-10)
  expect_false(result[1])
})

test_that("cpp_compare_fast early terminates on first difference", {
  # Should stop checking after finding first difference
  vec1 <- c(1, 999, 3, 4, 5)
  vec2 <- c(1, 2, 3, 4, 5)
  
  result <- .cpp_compare_fast(vec1, vec2, 1e-10)
  expect_false(result[1])
})

test_that("cpp_compare_fast handles character edge cases", {
  expect_true(.cpp_compare_fast("", "", 1e-10)[1])
  expect_true(.cpp_compare_fast(c("", ""), c("", ""), 1e-10)[1])
  expect_false(.cpp_compare_fast("", " ", 1e-10)[1])
})

test_that("cpp_compare_fast handles logical NA", {
  expect_true(.cpp_compare_fast(c(TRUE, NA, FALSE), c(TRUE, NA, FALSE), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(TRUE, NA), c(TRUE, FALSE), 1e-10)[1])
})

test_that("cpp_compare_fast handles integer NA", {
  expect_true(.cpp_compare_fast(c(1L, NA_integer_, 3L), c(1L, NA_integer_, 3L), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(1L, NA_integer_), c(1L, 2L), 1e-10)[1])
})

test_that("cpp_compare_fast handles character NA", {
  expect_true(.cpp_compare_fast(c("a", NA_character_, "c"), c("a", NA_character_, "c"), 1e-10)[1])
  expect_false(.cpp_compare_fast(c("a", NA_character_), c("a", "b"), 1e-10)[1])
})

test_that("cpp_compare_fast handles empty vectors", {
  expect_true(.cpp_compare_fast(numeric(0), numeric(0), 1e-10)[1])
  expect_true(.cpp_compare_fast(integer(0), integer(0), 1e-10)[1])
  expect_true(.cpp_compare_fast(character(0), character(0), 1e-10)[1])
})

test_that("cpp_compare_fast handles single element vectors", {
  expect_true(.cpp_compare_fast(5, 5, 1e-10)[1])
  expect_true(.cpp_compare_fast(5L, 5L, 1e-10)[1])
  expect_true(.cpp_compare_fast("a", "a", 1e-10)[1])
  expect_true(.cpp_compare_fast(TRUE, TRUE, 1e-10)[1])
})

test_that("cpp_compare_fast handles Inf and -Inf", {
  expect_true(.cpp_compare_fast(Inf, Inf, 1e-10)[1])
  expect_true(.cpp_compare_fast(-Inf, -Inf, 1e-10)[1])
  expect_false(.cpp_compare_fast(Inf, -Inf, 1e-10)[1])
  expect_false(.cpp_compare_fast(Inf, 1e308, 1e-10)[1])
})

test_that("cpp_compare_fast handles NaN", {
  # Your implementation treats matching NaNs as equal (valid for autograder)
  # Both student and instructor producing NaN should match
  expect_true(.cpp_compare_fast(NaN, NaN, 1e-10)[1])
  expect_true(.cpp_compare_fast(c(1, NaN), c(1, NaN), 1e-10)[1])
  
  # But NaN vs regular number should not match
  expect_false(.cpp_compare_fast(NaN, 1, 1e-10)[1])
  expect_false(.cpp_compare_fast(c(NaN, 2), c(1, 2), 1e-10)[1])
})

test_that("cpp_compare_fast handles type mismatches", {
  # Different types should not match
  expect_false(.cpp_compare_fast(1L, 1.0, 1e-10)[1])
  expect_false(.cpp_compare_fast("1", 1, 1e-10)[1])
  expect_false(.cpp_compare_fast(TRUE, 1, 1e-10)[1])
})

test_that("cpp_compare_fast handles complex vectors", {
  vec1 <- c(1.1, 2.2, 3.3, 4.4, 5.5)
  vec2 <- c(1.1, 2.2, 3.3, 4.4, 5.5)
  vec3 <- c(1.1, 2.2, 3.30001, 4.4, 5.5)
  
  expect_true(.cpp_compare_fast(vec1, vec2, 1e-10)[1])
  expect_false(.cpp_compare_fast(vec1, vec3, 1e-10)[1])
})

test_that("cpp_compare_identical works as expected", {
  expect_true(.cpp_compare_identical(c(1, 2, 3), c(1, 2, 3))[1])
  expect_false(.cpp_compare_identical(c(1, 2, 3), c(1, 2, 4))[1])
  
  # Should use default tolerance
  expect_true(.cpp_compare_identical(1.0 + 1e-12, 1.0)[1])
})
