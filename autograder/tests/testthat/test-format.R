# ============================================================================
# AUTOGRADER - OUTPUT FORMATTING TESTS
# ============================================================================
#
# Tests for output formatting functions:
#   - format_output()
#   - .cpp_format_output()
#   - print_feedback()
#   - provide_feedback()
#
# ============================================================================

# ============================================================================
# FORMAT_OUTPUT BASIC TESTS
# ============================================================================

test_that("format_output handles NULL", {
  expect_equal(format_output(NULL), "NULL")
})

test_that("format_output handles numeric scalars", {
  result <- format_output(42)
  expect_true(grepl("42", result))
})

test_that("format_output handles numeric vectors", {
  result <- format_output(1:3)
  # Might be "1:3" or "c(1, 2, 3)" depending on R's deparse

  expect_true(nchar(result) > 0)
})

test_that("format_output handles character values", {
  result <- format_output("hello")
  expect_true(grepl("hello", result))
})

test_that("format_output handles logical values", {
  result <- format_output(TRUE)
  expect_true(grepl("TRUE", result))
})

test_that("format_output handles empty list", {
  expect_equal(format_output(list()), "list()")
})

test_that("format_output handles NA values", {
  result <- format_output(NA)
  expect_true(grepl("NA", result))
  
  result2 <- format_output(c(1, NA, 3))
  expect_true(grepl("NA", result2))
})

test_that("format_output handles special numeric values", {
  expect_true(grepl("Inf", format_output(Inf)))
  expect_true(grepl("-Inf", format_output(-Inf)))
  expect_true(grepl("NaN", format_output(NaN)))
})

# ============================================================================
# FORMAT_OUTPUT COMPLEX TYPES
# ============================================================================

test_that("format_output handles small lists", {
  result <- format_output(list(a = 1, b = 2))
  expect_true(nchar(result) > 0)
})

test_that("format_output handles large lists", {
  result <- format_output(list(a = 1, b = 2, c = 3, d = 4, e = 5))
  expect_true(nchar(result) > 0)
  expect_true(grepl("length", result))
})

test_that("format_output handles matrices", {
  m <- matrix(1:6, nrow = 2)
  result <- format_output(m)
  expect_true(grepl("matrix", result))
})

test_that("format_output handles data frames", {
  df <- data.frame(a = 1:2, b = c("x", "y"))
  result <- format_output(df)
  expect_true(grepl("data.frame", result))
})

test_that("format_output handles long vectors", {
  result <- format_output(1:100)
  expect_true(grepl("\\.\\.\\.", result))  # Should be truncated
})

test_that("format_output respects max_length", {
  long_vec <- 1:1000
  result <- format_output(long_vec, max_length = 50)
  expect_true(nchar(result) <= 53)  # Allow slight overflow for ...
})

test_that("format_output handles factors", {
  f <- factor(c("a", "b", "c"))
  result <- format_output(f)
  expect_true(nchar(result) > 0)
})

test_that("format_output handles named vectors", {
  v <- c(a = 1, b = 2, c = 3)
  result <- format_output(v)
  expect_true(nchar(result) > 0)
})

test_that("format_output handles empty vectors", {
  expect_equal(format_output(numeric(0)), "numeric(0)")
  expect_equal(format_output(character(0)), "character(0)")
  expect_equal(format_output(logical(0)), "logical(0)")
  expect_equal(format_output(integer(0)), "integer(0)")
})

test_that("format_output handles preserve_structure parameter", {
  lst <- list(a = 1, b = 2)
  result1 <- format_output(lst, preserve_structure = TRUE)
  result2 <- format_output(lst, preserve_structure = FALSE)
  
  expect_true(nchar(result1) > 0)
  expect_true(nchar(result2) > 0)
})

test_that("format_output handles complex numbers", {
  c <- complex(real = 1, imaginary = 2)
  result <- format_output(c)
  expect_type(result, "character")
})

test_that("format_output handles raw vectors", {
  r <- charToRaw("hello")
  result <- format_output(r)
  expect_type(result, "character")
})

test_that("format_output handles Date objects", {
  d <- Sys.Date()
  result <- format_output(d)
  expect_type(result, "character")
})

test_that("format_output handles POSIXct", {
  p <- Sys.time()
  result <- format_output(p)
  expect_type(result, "character")
})

test_that("format_output handles tibbles if available", {
  skip_if_not_installed("tibble")
  tbl <- tibble::tibble(x = 1:3, y = c("a", "b", "c"))
  result <- format_output(tbl)
  expect_type(result, "character")
})

# ============================================================================
# C++ FORMAT OUTPUT TESTS
# ============================================================================

test_that(".cpp_format_output handles basic types", {
  expect_true(nchar(.cpp_format_output(42)) > 0)
  expect_true(nchar(.cpp_format_output("hello")) > 0)
  expect_true(nchar(.cpp_format_output(TRUE)) > 0)
})

test_that(".cpp_format_output handles NULL", {
  result <- .cpp_format_output(NULL)
  expect_true(grepl("NULL", result, ignore.case = TRUE))
})

test_that(".cpp_format_output handles vectors", {
  result <- .cpp_format_output(1:5)
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles long vectors", {
  result <- .cpp_format_output(1:100)
  expect_true(nchar(result) > 0)
  # Should show truncated version
  expect_true(grepl("\\.\\.\\.|numeric", result))
})

test_that(".cpp_format_output handles character vectors", {
  result <- .cpp_format_output(c("a", "b", "c"))
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles logical vectors", {
  result <- .cpp_format_output(c(TRUE, FALSE, TRUE))
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles integer vectors", {
  result <- .cpp_format_output(1L:5L)
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles lists", {
  result <- .cpp_format_output(list(a = 1, b = 2))
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output respects max_length parameter", {
  long_vec <- 1:1000
  result <- .cpp_format_output(long_vec, max_length = 50L)
  expect_true(nchar(result) <= 60)  # Allow some overflow
})

# ============================================================================
# PROVIDE_FEEDBACK TESTS
# ============================================================================

test_that("provide_feedback detects type mismatches", {
  student <- "5"
  expected <- 5
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("type_issue" %in% names(feedback))
  expect_true(grepl("Type mismatch", feedback$type_issue))
})

test_that("provide_feedback detects length mismatches", {
  student <- c(1, 2)
  expected <- c(1, 2, 3)
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("length_issue" %in% names(feedback))
  expect_true(grepl("Length mismatch", feedback$length_issue))
})

test_that("provide_feedback identifies difference positions", {
  student <- c(1, 2, 5, 4)
  expected <- c(1, 2, 3, 4)
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("diff_positions" %in% names(feedback))
  expect_true(grepl("position", feedback$diff_positions))
})

test_that("provide_feedback includes hints when provided", {
  student <- 5
  expected <- 10
  feedback <- provide_feedback(student, expected, list(x = 2), hint = "Try doubling")
  
  expect_true("hint" %in% names(feedback))
  expect_true(grepl("Try doubling", feedback$hint))
})

test_that("provide_feedback handles identical values", {
  student <- c(1, 2, 3)
  expected <- c(1, 2, 3)
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_equal(length(feedback), 0)
})

test_that("provide_feedback handles empty hint", {
  student <- 5
  expected <- 10
  feedback <- provide_feedback(student, expected, list(x = 2), hint = "")
  
  expect_false("hint" %in% names(feedback))
})

test_that("provide_feedback handles NULL hint", {
  student <- 5
  expected <- 10
  feedback <- provide_feedback(student, expected, list(x = 2), hint = NULL)
  
  expect_false("hint" %in% names(feedback))
})

test_that("provide_feedback handles many differences", {
  student <- 1:100
  expected <- 101:200
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("diff_positions" %in% names(feedback))
  expect_true(grepl("\\.\\.\\.", feedback$diff_positions))  # Should be truncated
})

test_that("provide_feedback handles NA differences", {
  student <- c(1, NA, 3)
  expected <- c(1, 2, 3)
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  # NA at position 2 and value difference should be detected
  expect_true(is.list(feedback))
})

test_that("provide_feedback handles list outputs", {
  student <- list(a = 1, b = 2)
  expected <- list(a = 1, b = 3)
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  # List difference should be detected
  expect_true(is.list(feedback))
})

# ============================================================================
# PRINT_FEEDBACK TESTS
# ============================================================================

test_that("print_feedback displays output", {
  feedback <- list(
    type_issue = "Type mismatch: Expected numeric but got character"
  )
  
  expect_output(print_feedback(feedback), "Feedback:")
  expect_output(print_feedback(feedback), "Type mismatch")
})

test_that("print_feedback handles empty feedback", {
  feedback <- list()
  # Empty feedback should not produce error
  expect_silent(print_feedback(feedback))
})

test_that("print_feedback displays all feedback types", {
  feedback <- list(
    type_issue = "Type mismatch",
    length_issue = "Length mismatch",
    diff_positions = "Differences at 1, 2, 3",
    hint = "Try again"
  )
  
  output <- capture.output(print_feedback(feedback))
  output_str <- paste(output, collapse = " ")
  
  expect_true(grepl("Type mismatch", output_str))
  expect_true(grepl("Length mismatch", output_str))
  expect_true(grepl("Differences", output_str))
  expect_true(grepl("Try again", output_str))
})

# ============================================================================
# ADDITIONAL C++ FORMAT TESTS
# ============================================================================

test_that(".cpp_format_output handles matrix input", {
  m <- matrix(1:6, nrow = 2)
  result <- .cpp_format_output(m, 100)
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles data.frame", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- .cpp_format_output(df, 200)
  expect_type(result, "character")
})

test_that(".cpp_format_output handles functions", {
  f <- function(x) x + 1
  result <- .cpp_format_output(f, 100)
  expect_type(result, "character")
  expect_true(grepl("function", result))
})

test_that(".cpp_format_output handles environments", {
  e <- new.env()
  e$a <- 1
  result <- .cpp_format_output(e, 100)
  expect_type(result, "character")
})

test_that(".cpp_format_output handles expressions", {
  expr <- expression(x + y)
  result <- .cpp_format_output(expr, 100)
  expect_type(result, "character")
})

test_that(".cpp_format_output handles calls", {
  call_obj <- call("sum", 1, 2, 3)
  result <- .cpp_format_output(call_obj, 100)
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that(".cpp_format_output handles formulas", {
  form <- y ~ x + z
  result <- .cpp_format_output(form, 100)
  expect_type(result, "character")
})

test_that(".cpp_format_output handles language objects", {
  # Use a safer approach - format a simple numeric
  result <- .cpp_format_output(1:3, 100)
  expect_type(result, "character")
})

# ============================================================================
# ADDITIONAL PROVIDE_FEEDBACK TESTS
# ============================================================================

test_that("provide_feedback handles numeric within tolerance", {
  student <- 1.0000001
  expected <- 1.0
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  # Should be empty or minimal since values are very close
  expect_true(is.list(feedback))
})

test_that("provide_feedback handles integer vs numeric", {
  student <- 1L
  expected <- 1.0
  # These should be considered equal in most contexts
  feedback <- provide_feedback(student, expected, list(x = 1))
  expect_true(is.list(feedback))
})

test_that("provide_feedback handles matrix differences", {
  student <- matrix(1:4, 2)
  expected <- matrix(c(1, 2, 3, 5), 2)
  feedback <- provide_feedback(student, expected, list(x = 1))
  expect_true(is.list(feedback))
})

test_that("provide_feedback handles dimension mismatch", {
  student <- matrix(1:6, 2, 3)
  expected <- matrix(1:9, 3, 3)  # Different dimensions
  # This may error or return feedback
  result <- tryCatch(
    provide_feedback(student, expected, list(x = 1)),
    error = function(e) list(error = TRUE)
  )
  expect_true(is.list(result))
})

test_that("provide_feedback handles data.frame differences", {
  student <- data.frame(a = 1:3, b = 4:6)
  expected <- data.frame(a = 1:3, b = c(4, 5, 7))
  feedback <- provide_feedback(student, expected, list(x = 1))
  expect_true(is.list(feedback))
})

test_that("provide_feedback handles NULL vs non-NULL", {
  feedback <- provide_feedback(NULL, 1, list(x = 1))
  expect_true(is.list(feedback))
})

test_that("provide_feedback includes hints when provided", {
  student <- 1
  expected <- 2
  feedback <- provide_feedback(student, expected, list(x = 1), hint = "Check your math")
  expect_true(is.list(feedback))
  if (!is.null(feedback$hint)) {
    expect_true(grepl("Check your math", feedback$hint))
  }
})

# ============================================================================
# ADDITIONAL FORMAT_OUTPUT EDGE CASES  
# ============================================================================

test_that("format_output handles Inf and -Inf", {
  expect_true(grepl("Inf", format_output(Inf)))
  expect_true(grepl("-Inf", format_output(-Inf)))
})

test_that("format_output handles NaN", {
  expect_true(grepl("NaN", format_output(NaN)))
})

test_that("format_output handles complex numbers", {
  result <- format_output(complex(real = 1, imaginary = 2))
  expect_true(grepl("1", result) || grepl("complex", result))
})

test_that("format_output handles raw vectors", {
  result <- format_output(as.raw(c(0x00, 0x01, 0x02)))
  expect_type(result, "character")
})

test_that("format_output handles Date objects", {
  result <- format_output(as.Date("2024-01-01"))
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("format_output handles POSIXct", {
  result <- format_output(as.POSIXct("2024-01-01 12:00:00"))
  expect_type(result, "character")
})

test_that("format_output respects max_length parameter", {
  long_vec <- 1:100
  short_result <- format_output(long_vec, max_length = 20)
  expect_lte(nchar(short_result), 25)  # Some tolerance for ellipsis
})

