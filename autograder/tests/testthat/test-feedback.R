# ============================================================================
# Test: feedback.R - Student Feedback System
# ============================================================================

# ============================================================================
# Section 1: provide_feedback - Type Checking
# ============================================================================

test_that("provide_feedback detects type mismatch", {
  result <- provide_feedback(
    student_out = "5",  # character
    expected_out = 5,   # numeric
    input_args = list(x = 5)
  )
  
  expect_true("type_issue" %in% names(result))
  expect_match(result$type_issue, "Type mismatch")
  expect_match(result$type_issue, "character")
  expect_match(result$type_issue, "numeric")
})

test_that("provide_feedback detects class mismatch", {
  result <- provide_feedback(
    student_out = list(a = 1),
    expected_out = c(a = 1),
    input_args = list()
  )
  
  expect_true("type_issue" %in% names(result))
})

test_that("provide_feedback no type issue when types match", {
  result <- provide_feedback(
    student_out = 5,
    expected_out = 10,
    input_args = list()
  )
  
  expect_false("type_issue" %in% names(result))
})

# ============================================================================
# Section 2: provide_feedback - Length Checking
# ============================================================================

test_that("provide_feedback detects length mismatch", {
  result <- provide_feedback(
    student_out = 1:5,
    expected_out = 1:10,
    input_args = list(n = 10)
  )
  
  expect_true("length_issue" %in% names(result))
  expect_match(result$length_issue, "Length mismatch")
  expect_match(result$length_issue, "10")
  expect_match(result$length_issue, "5")
})

test_that("provide_feedback detects off-by-one length errors", {
  result <- provide_feedback(
    student_out = 1:9,
    expected_out = 1:10,
    input_args = list(n = 10)
  )
  
  expect_true("length_issue" %in% names(result))
})

test_that("provide_feedback no length issue when lengths match", {
  result <- provide_feedback(
    student_out = c(1, 5, 3),
    expected_out = c(1, 2, 3),
    input_args = list()
  )
  
  expect_false("length_issue" %in% names(result))
})

# ============================================================================
# Section 3: provide_feedback - Value Differences
# ============================================================================

test_that("provide_feedback detects value differences", {
  result <- provide_feedback(
    student_out = c(1, 5, 3),
    expected_out = c(1, 2, 3),
    input_args = list()
  )
  
  expect_true("diff_positions" %in% names(result))
  expect_match(result$diff_positions, "2")
})

test_that("provide_feedback detects multiple value differences", {
  result <- provide_feedback(
    student_out = c(1, 5, 7, 4, 9),
    expected_out = c(1, 2, 3, 4, 5),
    input_args = list()
  )
  
  expect_true("diff_positions" %in% names(result))
  # Should mention positions 2, 3, 5
  expect_match(result$diff_positions, "2")
})

test_that("provide_feedback no diff when values match", {
  result <- provide_feedback(
    student_out = c(1, 2, 3),
    expected_out = c(1, 2, 3),
    input_args = list()
  )
  
  expect_false("diff_positions" %in% names(result))
})

test_that("provide_feedback respects tolerance", {
  result <- provide_feedback(
    student_out = c(1.0001, 2.0002, 3.0003),
    expected_out = c(1, 2, 3),
    input_args = list()
  )
  
  # With default tolerance, small differences should not trigger diff
  # This depends on autograder_tolerance()
})

# ============================================================================
# Section 4: provide_feedback - Hints
# ============================================================================

test_that("provide_feedback includes hint when provided", {
  result <- provide_feedback(
    student_out = 5,
    expected_out = 10,
    input_args = list(),
    hint = "Try multiplying by 2"
  )
  
  expect_true("hint" %in% names(result))
  expect_match(result$hint, "Hint:")
  expect_match(result$hint, "multiplying")
})

test_that("provide_feedback excludes hint when NULL", {
  result <- provide_feedback(
    student_out = 5,
    expected_out = 10,
    input_args = list(),
    hint = NULL
  )
  
  expect_false("hint" %in% names(result))
})

test_that("provide_feedback excludes empty hint", {
  result <- provide_feedback(
    student_out = 5,
    expected_out = 10,
    input_args = list(),
    hint = ""
  )
  
  expect_false("hint" %in% names(result))
})

# ============================================================================
# Section 5: provide_feedback - Empty/Edge Cases
# ============================================================================

test_that("provide_feedback returns empty list when all matches", {
  result <- provide_feedback(
    student_out = 5,
    expected_out = 5,
    input_args = list()
  )
  
  expect_length(result, 0)
})

test_that("provide_feedback handles empty vectors", {
  result <- provide_feedback(
    student_out = numeric(0),
    expected_out = numeric(0),
    input_args = list()
  )
  
  expect_length(result, 0)
})

test_that("provide_feedback handles NULL inputs", {
  result <- provide_feedback(
    student_out = NULL,
    expected_out = NULL,
    input_args = list()
  )
  
  expect_type(result, "list")
})

# ============================================================================
# Section 6: print_feedback
# ============================================================================

test_that("print_feedback outputs nothing for empty feedback", {
  output <- capture.output(print_feedback(list()))
  # Empty feedback should produce minimal/no meaningful output
  output_text <- paste(output, collapse = "")
  expect_true(nchar(trimws(output_text)) == 0 || length(output) <= 1)
})

test_that("print_feedback formats single feedback item", {
  feedback <- list(type_issue = "Type mismatch: Expected numeric got character")
  
  output <- capture.output(print_feedback(feedback))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Feedback")
  expect_match(output_text, "Type mismatch")
  expect_match(output_text, "\\*")  # bullet point
})

test_that("print_feedback formats multiple feedback items", {
  feedback <- list(
    type_issue = "Type mismatch",
    length_issue = "Length mismatch",
    hint = "Hint: Check your code"
  )
  
  output <- capture.output(print_feedback(feedback))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Type mismatch")
  expect_match(output_text, "Length mismatch")
  expect_match(output_text, "Hint")
})

# ============================================================================
# Section 7: show_diff
# ============================================================================

test_that("show_diff handles type mismatch", {
  output <- capture.output(show_diff(5, "5", use_waldo = FALSE))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "type|Type", ignore.case = TRUE)
})

test_that("show_diff handles numeric length mismatch", {
  output <- capture.output(show_diff(1:10, 1:5, use_waldo = FALSE))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "length|Length", ignore.case = TRUE)
})

test_that("show_diff handles numeric value differences", {
  output <- capture.output(show_diff(c(1, 2, 3), c(1, 5, 3), use_waldo = FALSE))
  output_text <- paste(output, collapse = "\n")
  
  # Should show some kind of diff
  expect_true(nchar(output_text) > 0)
})

test_that("show_diff handles character differences", {
  output <- capture.output(show_diff(c("a", "b"), c("a", "c"), use_waldo = FALSE))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(nchar(output_text) > 0)
})

test_that("show_diff handles data frame differences", {
  df1 <- data.frame(a = 1:3, b = c("x", "y", "z"))
  df2 <- data.frame(a = 1:3, b = c("x", "w", "z"))
  
  output <- capture.output(show_diff(df1, df2, use_waldo = FALSE))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(nchar(output_text) > 0)
})

test_that("show_diff respects max_show parameter", {
  output <- capture.output(show_diff(1:100, 101:200, max_show = 3, use_waldo = FALSE))
  output_text <- paste(output, collapse = "\n")
  
  # Should limit output
  expect_true(nchar(output_text) > 0)
})

test_that("show_diff returns invisible NULL", {
  result <- capture.output(ret <- show_diff(1:3, 1:3, use_waldo = FALSE))
  expect_null(ret)
})

# ============================================================================
# Section 8: show_waldo_diff (when waldo available)
# ============================================================================

test_that("show_waldo_diff works when waldo available", {
  skip_if_not_installed("waldo")
  
  output <- capture.output(show_diff(c(1, 2, 3), c(1, 5, 3), use_waldo = TRUE))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(nchar(output_text) > 0)
})

test_that("show_diff falls back when waldo disabled", {
  output <- capture.output(show_diff(c(1, 2, 3), c(1, 5, 3), use_waldo = FALSE))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(nchar(output_text) > 0)
})

# ============================================================================
# Section 9: Specific Diff Functions
# ============================================================================

test_that("show_numeric_diff handles vectors", {
  output <- capture.output(show_numeric_diff(
    expected = c(1, 2, 3, 4, 5),
    actual = c(1, 2, 10, 4, 5),
    max_show = 5,
    tolerance = 1e-6
  ))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(nchar(output_text) > 0)
})

test_that("show_character_diff handles strings", {
  output <- capture.output(show_character_diff(
    expected = c("apple", "banana", "cherry"),
    actual = c("apple", "orange", "cherry"),
    max_show = 5
  ))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(nchar(output_text) > 0)
})
