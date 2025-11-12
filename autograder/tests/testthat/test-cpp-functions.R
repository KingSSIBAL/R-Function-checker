# tests/testthat/test-cpp-functions.R

test_that("cpp_compare_fast handles numeric vectors", {
  expect_true(.cpp_compare_fast(c(1, 2, 3), c(1, 2, 3), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(1, 2, 3), c(1, 2, 4), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(1, 2), c(1, 2, 3), 1e-10)[1])
})

test_that("cpp_compare_fast handles tolerance correctly", {
  expect_true(.cpp_compare_fast(c(1.0000001), c(1.0), 1e-6)[1])
  expect_false(.cpp_compare_fast(c(1.001), c(1.0), 1e-6)[1])
  
  expect_true(.cpp_compare_fast(c(1.0, 2.0000001), c(1.0, 2.0), 1e-6)[1])
})

test_that("cpp_compare_fast handles NA values", {
  expect_true(.cpp_compare_fast(c(1, NA, 3), c(1, NA, 3), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(1, NA, 3), c(1, 2, 3), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(1, 2, 3), c(1, NA, 3), 1e-10)[1])
})

test_that("cpp_compare_fast handles integer vectors", {
  expect_true(.cpp_compare_fast(1L:3L, 1L:3L, 1e-10)[1])
  expect_false(.cpp_compare_fast(1L:3L, 1L:4L, 1e-10)[1])
  expect_false(.cpp_compare_fast(1L:3L, c(1L, 2L, 4L), 1e-10)[1])
})

test_that("cpp_compare_fast handles character vectors", {
  expect_true(.cpp_compare_fast(c("a", "b"), c("a", "b"), 1e-10)[1])
  expect_false(.cpp_compare_fast(c("a", "b"), c("a", "c"), 1e-10)[1])
  expect_false(.cpp_compare_fast(c("a"), c("a", "b"), 1e-10)[1])
})

test_that("cpp_compare_fast handles logical vectors", {
  expect_true(.cpp_compare_fast(c(TRUE, FALSE), c(TRUE, FALSE), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(TRUE, FALSE), c(FALSE, FALSE), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(TRUE), c(TRUE, FALSE), 1e-10)[1])
})

test_that("cpp_compare_fast handles different types", {
  expect_false(.cpp_compare_fast(c(1, 2, 3), c("1", "2", "3"), 1e-10)[1])
  expect_false(.cpp_compare_fast(1L, 1.0, 1e-10)[1])
})

test_that("cpp_compare_fast handles empty vectors", {
  expect_true(.cpp_compare_fast(numeric(0), numeric(0), 1e-10)[1])
  expect_false(.cpp_compare_fast(numeric(0), c(1), 1e-10)[1])
})

test_that("cpp_compare_fast handles large vectors efficiently", {
  large_vec1 <- rnorm(10000)
  large_vec2 <- large_vec1
  large_vec3 <- large_vec1
  large_vec3[5000] <- large_vec3[5000] + 1
  
  expect_true(.cpp_compare_fast(large_vec1, large_vec2, 1e-10)[1])
  expect_false(.cpp_compare_fast(large_vec1, large_vec3, 1e-10)[1])
})

test_that("cpp_compare_identical works as wrapper", {
  expect_true(.cpp_compare_identical(c(1, 2, 3), c(1, 2, 3))[1])
  expect_false(.cpp_compare_identical(c(1, 2, 3), c(1, 2, 4))[1])
})
