# tests/testthat/test-feedback.R

test_that("provide_feedback detects type mismatches", {
  student <- "5"
  expected <- 5
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("type_issue" %in% names(feedback))
  expect_match(feedback$type_issue, "Type mismatch")
  expect_match(feedback$type_issue, "character")
  expect_match(feedback$type_issue, "numeric")
})

test_that("provide_feedback detects length mismatches", {
  student <- c(1, 2)
  expected <- c(1, 2, 3)
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("length_issue" %in% names(feedback))
  expect_match(feedback$length_issue, "Length mismatch")
  expect_match(feedback$length_issue, "Expected length 3")
  expect_match(feedback$length_issue, "got 2")
})

test_that("provide_feedback identifies difference positions", {
  student <- c(1, 2, 5, 4)
  expected <- c(1, 2, 3, 4)
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("diff_positions" %in% names(feedback))
  expect_match(feedback$diff_positions, "position")
  expect_match(feedback$diff_positions, "3")
})

test_that("provide_feedback includes hints when provided", {
  student <- 5
  expected <- 10
  feedback <- provide_feedback(student, expected, list(x = 2), hint = "Try doubling")
  
  expect_true("hint" %in% names(feedback))
  expect_match(feedback$hint, "Try doubling")
})

test_that("provide_feedback handles no differences", {
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
  
  # Should truncate to first 5 positions
  expect_true("diff_positions" %in% names(feedback))
  expect_match(feedback$diff_positions, "\\.\\.\\.")
})

test_that("print_feedback displays correctly", {
  feedback <- list(
    type_issue = "Type mismatch: Expected numeric but got character",
    hint = "Hint: Convert to numeric first"
  )
  
  expect_output(
    print_feedback(feedback),
    "Feedback:"
  )
  
  expect_output(
    print_feedback(feedback),
    "Type mismatch"
  )
  
  expect_output(
    print_feedback(feedback),
    "Convert to numeric"
  )
})

test_that("print_feedback handles empty feedback", {
  expect_silent(print_feedback(list()))
})
