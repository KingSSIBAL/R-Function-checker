# tests/testthat/test-format-output-advanced.R

test_that("format_output handles nested lists with structure preservation", {
  obj <- list(
    a = list(x = 1, y = 2),
    b = list(z = 3, w = 4),
    c = list(p = 5, q = 6),
    d = list(r = 7, s = 8)
  )
  
  result <- format_output(obj, preserve_structure = TRUE)
  expect_type(result, "character")
  expect_gt(nchar(result), 10)
})

test_that("format_output handles large nested structures", {
  obj <- list(
    matrix = matrix(1:100, 10, 10),
    df = data.frame(x = 1:50, y = 51:100),
    vector = 1:200
  )
  
  result <- format_output(obj, max_length = 150)
  expect_lte(nchar(result), 200)
})

test_that("format_output handles vectors at length boundary", {
  # Test exactly at the boundary for > 10 condition
  obj_10 <- 1:10
  obj_11 <- 1:11
  
  result_10 <- format_output(obj_10)
  result_11 <- format_output(obj_11)
  
  expect_type(result_10, "character")
  expect_type(result_11, "character")
  
  # The 11-element vector should trigger the "long vector" path
  expect_true(nchar(result_11) > 0)
})

test_that("format_output handles single-row matrices", {
  obj <- matrix(1:5, nrow = 1)
  result <- format_output(obj)
  expect_match(result, "matrix\\[1x5\\]")
})

test_that("format_output handles single-column matrices", {
  obj <- matrix(1:5, ncol = 1)
  result <- format_output(obj)
  expect_match(result, "matrix\\[5x1\\]")
})

test_that("format_output handles large matrices", {
  obj <- matrix(1:1000, nrow = 100)
  result <- format_output(obj)
  expect_match(result, "matrix\\[100x10\\]")
  expect_match(result, "\\.\\.\\.")
})

test_that("format_output handles data.frame with many columns", {
  df <- data.frame(matrix(1:1000, nrow = 10, ncol = 100))
  result <- format_output(df)
  
  expect_type(result, "character")
  expect_gt(nchar(result), 0)
})

test_that("format_output handles special numeric values", {
  obj <- c(1, Inf, -Inf, NaN, NA)
  result <- format_output(obj)
  
  expect_type(result, "character")
  expect_match(result, "(Inf|NaN|NA)")
})

test_that("format_output handles very long character strings", {
  obj <- paste(rep("a", 500), collapse = "")
  result <- format_output(obj, max_length = 100)
  
  expect_lte(nchar(result), 103)  # 100 + "..."
  expect_match(result, "\\.\\.\\.$")
})

test_that("format_output handles mixed-type lists", {
  obj <- list(
    num = 1:5,
    char = letters[1:5],
    log = c(TRUE, FALSE),
    mat = matrix(1:4, 2, 2),
    func = function(x) x^2
  )
  
  result <- format_output(obj, preserve_structure = FALSE)
  expect_type(result, "character")
  expect_gt(nchar(result), 20)
})
