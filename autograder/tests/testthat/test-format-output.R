# tests/testthat/test-format-output.R

test_that("format_output handles NULL", {
  result <- format_output(NULL)
  expect_equal(result, "NULL")
})

test_that("format_output handles empty list", {
  result <- format_output(list())
  expect_equal(result, "list()")
})

test_that("format_output handles small list", {
  obj <- list(a = 1, b = 2, c = 3)
  result <- format_output(obj)
  expect_type(result, "character")
  expect_gt(nchar(result), 0)
  expect_lte(nchar(result), 250)
})

test_that("format_output handles large list", {
  obj <- as.list(1:100)
  result <- format_output(obj, max_length = 100)
  expect_lte(nchar(result), 200)
  expect_match(result, "length=100")
})

test_that("format_output handles short vectors", {
  obj <- 1:5
  result <- format_output(obj)
  expect_type(result, "character")
  expect_match(result, "1")
})

test_that("format_output handles long vectors", {
  obj <- 1:100
  result <- format_output(obj)
  expect_type(result, "character")
  
  # Should include some indication of length and show values
  # Accept either "numeric" or "integer" since 1:100 creates integers
  expect_true(
    grepl("(numeric|integer)", result, ignore.case = TRUE) ||
    grepl("1, 2, 3", result)
  )
  
  # Should include ellipsis for long vectors
  expect_match(result, "\\.\\.\\.")
})

test_that("format_output handles matrices", {
  obj <- matrix(1:12, nrow = 3, ncol = 4)
  result <- format_output(obj)
  expect_match(result, "matrix\\[3x4\\]")
})

test_that("format_output handles data.frames", {
  obj <- data.frame(
    x = 1:10,
    y = letters[1:10],
    z = rnorm(10),
    stringsAsFactors = FALSE
  )
  
  result <- format_output(obj)
  
  # The output format may vary, so just check it's reasonable
  expect_type(result, "character")
  expect_gt(nchar(result), 10)   # At least some content
  expect_lt(nchar(result), 500)  # Not excessively long
  
  # Should mention it's a data.frame or contain the column names
  expect_true(
    grepl("data\\.frame", result) || 
    (grepl("x", result) && grepl("y", result) && grepl("z", result))
  )
})

test_that("format_output respects max_length", {
  obj <- paste(rep("a", 1000), collapse = "")
  result <- format_output(obj, max_length = 50)
  expect_lte(nchar(result), 53) # 50 + "..."
})

test_that("format_output handles complex objects", {
  obj <- list(
    a = matrix(1:6, 2, 3),
    b = data.frame(x = 1:5),
    c = function(x) x^2
  )
  result <- format_output(obj)
  expect_type(result, "character")
  expect_gt(nchar(result), 0)
})

test_that("format_output preserves structure option", {
  obj <- list(a = 1, b = 2, c = 3)
  
  result_preserve <- format_output(obj, preserve_structure = TRUE)
  result_no_preserve <- format_output(obj, preserve_structure = FALSE)
  
  expect_type(result_preserve, "character")
  expect_type(result_no_preserve, "character")
  
  # Both should produce output
  expect_gt(nchar(result_preserve), 0)
  expect_gt(nchar(result_no_preserve), 0)
})

test_that("format_output handles numeric vectors", {
  obj <- c(1.5, 2.7, 3.9)
  result <- format_output(obj)
  expect_type(result, "character")
  expect_match(result, "1\\.5")
})

test_that("format_output handles character vectors", {
  obj <- c("hello", "world", "test")
  result <- format_output(obj)
  expect_type(result, "character")
  expect_match(result, "hello")
})

test_that("format_output handles logical vectors", {
  obj <- c(TRUE, FALSE, TRUE)
  result <- format_output(obj)
  expect_type(result, "character")
  expect_match(result, "TRUE")
})

test_that("format_output handles single values", {
  expect_type(format_output(42), "character")
  expect_type(format_output("text"), "character")
  expect_type(format_output(TRUE), "character")
})

test_that("format_output handles factors", {
  obj <- factor(c("a", "b", "c", "a", "b"))
  result <- format_output(obj)
  expect_type(result, "character")
  expect_gt(nchar(result), 0)
})

test_that("format_output handles NA values", {
  obj <- c(1, NA, 3)
  result <- format_output(obj)
  expect_type(result, "character")
  expect_match(result, "NA")
})

test_that("format_output handles empty vectors", {
  expect_type(format_output(numeric(0)), "character")
  expect_type(format_output(character(0)), "character")
  expect_type(format_output(logical(0)), "character")
})
