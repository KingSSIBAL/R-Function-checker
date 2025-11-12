test_that("format_output handles different types correctly", {
  expect_type(format_output(NULL), "character")
  expect_match(format_output(NULL), "NULL")
  
  expect_type(format_output(1:3), "character")
  expect_type(format_output(list(a = 1, b = 2)), "character")
  
  long_vec <- 1:100
  out <- format_output(long_vec, max_length = 50)
  expect_lte(nchar(out), 200)  # Should be truncated smartly
})

test_that("validate_test_cases catches errors", {
  expect_error(
    validate_test_cases(list(), "test_fn"),
    "missing required 'inputs'"
  )
  
  expect_error(
    validate_test_cases(list(inputs = list()), "test_fn"),
    "must have at least one test"
  )
  
  # Valid test case
  valid_test <- list(
    inputs = list(list(x = 1), list(x = 2)),
    points = c(1, 1)
  )
  result <- validate_test_cases(valid_test, "test_fn")
  expect_equal(length(result$inputs), 2)
})

test_that("validate_test_cases stops on critical mismatches", {
  test_case <- list(
    inputs = list(list(x = 1), list(x = 2)),
    points = c(1)  # Wrong length!
  )
  
  expect_error(
    validate_test_cases(test_case, "test_fn"),
    "points.*doesn't match"
  )
})

test_that("provide_feedback detects type mismatches", {
  student <- "5"
  expected <- 5
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("type_issue" %in% names(feedback))
  expect_match(feedback$type_issue, "Type mismatch")
})

test_that("provide_feedback detects length mismatches", {
  student <- c(1, 2)
  expected <- c(1, 2, 3)
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("length_issue" %in% names(feedback))
  expect_match(feedback$length_issue, "Length mismatch")
})

test_that("provide_feedback includes hints", {
  student <- 5
  expected <- 10
  feedback <- provide_feedback(student, expected, list(x = 2), hint = "Try doubling")
  
  expect_true("hint" %in% names(feedback))
  expect_match(feedback$hint, "Try doubling")
})

test_that("custom error functions work", {
  err <- network_error("test message")
  expect_s3_class(err, "network_error")
  expect_match(err$message, "test message")
  
  err2 <- function_not_found_error("my_func")
  expect_s3_class(err2, "function_not_found_error")
  expect_match(err2$message, "my_func")
  expect_equal(err2$function_name, "my_func")
})

test_that("cpp_compare_fast works correctly", {
  # Numeric comparison
  expect_true(.cpp_compare_fast(c(1, 2, 3), c(1, 2, 3), 1e-10)[1])
  expect_false(.cpp_compare_fast(c(1, 2, 3), c(1, 2, 4), 1e-10)[1])
  
  # Integer comparison  
  expect_true(.cpp_compare_fast(1L:3L, 1L:3L, 1e-10)[1])
  expect_false(.cpp_compare_fast(1L:3L, 1L:4L, 1e-10)[1])
  
  # Character comparison
  expect_true(.cpp_compare_fast(c("a", "b"), c("a", "b"), 1e-10)[1])
  expect_false(.cpp_compare_fast(c("a", "b"), c("a", "c"), 1e-10)[1])
  
  # Tolerance test
  expect_true(.cpp_compare_fast(c(1.0000001, 2), c(1, 2), 1e-6)[1])
  expect_false(.cpp_compare_fast(c(1.001, 2), c(1, 2), 1e-6)[1])
})
