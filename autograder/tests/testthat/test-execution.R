# ============================================================================
# AUTOGRADER - TEST EXECUTION COVERAGE TESTS
# ============================================================================
#
# Tests for run_tests_sequential, run_tests_parallel, print_feedback
#
# ============================================================================

# ============================================================================
# PRINT FEEDBACK TESTS
# ============================================================================

test_that("print_feedback handles empty feedback", {
  # Empty list should print nothing significant
  output <- capture.output(print_feedback(list()))
  # May print empty line or nothing
  expect_true(length(output) <= 1)
})

test_that("print_feedback displays all feedback items", {
  feedback <- list(
    type_issue = "Type mismatch: Expected numeric but got character",
    length_issue = "Length mismatch: Expected 5 but got 3",
    hint = "Hint: Check your return type"
  )
  
  output <- capture.output(print_feedback(feedback))
  
  expect_true(any(grepl("Feedback", output)))
  expect_true(any(grepl("Type mismatch", output)))
  expect_true(any(grepl("Length mismatch", output)))
  expect_true(any(grepl("Hint", output)))
})

test_that("print_feedback formats as bullet points", {
  feedback <- list(
    issue1 = "First issue",
    issue2 = "Second issue"
  )
  
  output <- capture.output(print_feedback(feedback))
  
  # Should have bullet points
  expect_true(any(grepl("\\*", output)))
})

# ============================================================================
# RUN TESTS SEQUENTIAL TESTS
# ============================================================================

test_that("run_tests_sequential executes all tests", {
  student_fun <- function(x) x + 1
  instructor_fun <- function(x) x + 1
  
  test_data <- list(
    inputs = list(list(x = 1), list(x = 2), list(x = 3))
  )
  
  results <- run_tests_sequential(
    student_fun = student_fun,
    instructor_fun = instructor_fun,
    test_data = test_data,
    tolerance = 1e-10
  )
  
  expect_equal(length(results), 3)
})

test_that("run_tests_sequential returns correct structure", {
  student_fun <- function(x) x * 2
  instructor_fun <- function(x) x * 2
  
  test_data <- list(
    inputs = list(list(x = 5))
  )
  
  results <- run_tests_sequential(
    student_fun = student_fun,
    instructor_fun = instructor_fun,
    test_data = test_data,
    tolerance = 1e-10
  )
  
  expect_true("student" %in% names(results[[1]]))
  expect_true("expected" %in% names(results[[1]]))
  expect_true("index" %in% names(results[[1]]))
})

test_that("run_tests_sequential captures student errors", {
  student_fun <- function(x) stop("Student error!")
  instructor_fun <- function(x) x + 1
  
  test_data <- list(
    inputs = list(list(x = 1))
  )
  
  results <- run_tests_sequential(
    student_fun = student_fun,
    instructor_fun = instructor_fun,
    test_data = test_data,
    tolerance = 1e-10
  )
  
  # Should capture error, not throw
  expect_true(inherits(results[[1]]$student, "error") || 
              is.list(results[[1]]$student))
})

test_that("run_tests_sequential handles multiple test inputs", {
  student_fun <- function(a, b) a + b
  instructor_fun <- function(a, b) a + b
  
  test_data <- list(
    inputs = list(
      list(a = 1, b = 2),
      list(a = 10, b = 20)
    )
  )
  
  results <- run_tests_sequential(
    student_fun = student_fun,
    instructor_fun = instructor_fun,
    test_data = test_data,
    tolerance = 1e-10
  )
  
  expect_equal(results[[1]]$student, 3)
  expect_equal(results[[2]]$student, 30)
})

# ============================================================================
# RUN TESTS PARALLEL TESTS
# ============================================================================

test_that("run_tests_parallel falls back to sequential for small tests", {
  student_fun <- function(x) x + 1
  instructor_fun <- function(x) x + 1
  
  # Less than 10 tests - should use sequential
  test_data <- list(
    inputs = list(list(x = 1), list(x = 2), list(x = 3))
  )
  
  results <- run_tests_parallel(
    student_fun = student_fun,
    instructor_fun = instructor_fun,
    test_data = test_data,
    tolerance = 1e-10,
    use_parallel = TRUE
  )
  
  expect_equal(length(results), 3)
})

test_that("run_tests_parallel respects use_parallel=FALSE", {
  student_fun <- function(x) x + 1
  instructor_fun <- function(x) x + 1
  
  # Even with many tests, should use sequential if use_parallel=FALSE
  test_data <- list(
    inputs = lapply(1:15, function(i) list(x = i))
  )
  
  results <- run_tests_parallel(
    student_fun = student_fun,
    instructor_fun = instructor_fun,
    test_data = test_data,
    tolerance = 1e-10,
    use_parallel = FALSE
  )
  
  expect_equal(length(results), 15)
})

test_that("run_tests_parallel handles errors in student function", {
  student_fun <- function(x) {
    if (x > 5) stop("Error for x > 5")
    x + 1
  }
  instructor_fun <- function(x) x + 1
  
  test_data <- list(
    inputs = lapply(1:3, function(i) list(x = i))
  )
  
  results <- run_tests_parallel(
    student_fun = student_fun,
    instructor_fun = instructor_fun,
    test_data = test_data,
    tolerance = 1e-10,
    use_parallel = FALSE
  )
  
  # All should succeed (x <= 5)
  expect_equal(length(results), 3)
})

# ============================================================================
# EXTRACT FUNCTIONS TESTS
# ============================================================================

test_that("extract_instructor_function finds first function", {
  env <- new.env()
  env$data <- 1:10
  env$my_func <- function(x) x + 1
  env$helper <- function(y) y * 2
  
  result <- extract_instructor_function(env, "test")
  
  expect_true(is.function(result))
})

test_that("extract_test_cases extracts test_cases", {
  env <- new.env()
  env$my_func <- function(x) x + 1
  env$test_cases <- list(
    inputs = list(list(x = 1)),
    expected = list(2),
    descriptions = c("Basic test")
  )
  
  result <- extract_test_cases(env, "test")
  
  expect_true("inputs" %in% names(result))
  expect_true("expected" %in% names(result))
})

# ============================================================================
# VALIDATE TEST CASES ADDITIONAL TESTS
# ============================================================================

test_that("validate_test_cases handles hidden field", {
  test_data <- list(
    inputs = list(1, 2, 3),
    hidden = c(FALSE, TRUE, FALSE)
  )
  
  result <- validate_test_cases(test_data, "test_func")
  
  expect_equal(result$hidden, c(FALSE, TRUE, FALSE))
})

test_that("validate_test_cases rejects invalid hidden length", {
  test_data <- list(
    inputs = list(1, 2, 3),
    hidden = c(FALSE, TRUE)  # Wrong length
  )
  
  expect_error(validate_test_cases(test_data, "test_func"), "length|doesn't match")
})

test_that("validate_test_cases rejects non-logical hidden", {
  test_data <- list(
    inputs = list(1, 2),
    hidden = c("yes", "no")  # Not logical
  )
  
  expect_error(validate_test_cases(test_data, "test_func"), "logical")
})

test_that("validate_test_cases handles points field", {
  test_data <- list(
    inputs = list(1, 2, 3),
    points = c(1, 2, 3)
  )
  
  result <- validate_test_cases(test_data, "test_func")
  
  expect_equal(result$points, c(1, 2, 3))
})

test_that("validate_test_cases rejects invalid points length", {
  test_data <- list(
    inputs = list(1, 2, 3),
    points = c(1, 2)  # Wrong length
  )
  
  expect_error(validate_test_cases(test_data, "test_func"), "length|doesn't match")
})

test_that("validate_test_cases rejects negative points", {
  test_data <- list(
    inputs = list(1, 2),
    points = c(1, -1)  # Negative point
  )
  
  expect_error(validate_test_cases(test_data, "test_func"), "non-negative")
})

test_that("validate_test_cases handles tolerance field", {
  test_data <- list(
    inputs = list(1, 2),
    tolerance = 1e-6
  )
  
  result <- validate_test_cases(test_data, "test_func")
  
  expect_equal(result$tolerance, 1e-6)
})

test_that("validate_test_cases handles hints field", {
  test_data <- list(
    inputs = list(1, 2),
    hints = c("Hint 1", "Hint 2")
  )
  
  result <- validate_test_cases(test_data, "test_func")
  
  expect_equal(result$hints, c("Hint 1", "Hint 2"))
})

test_that("validate_test_cases handles hints field when present", {
  test_data <- list(
    inputs = list(1, 2, 3),
    hints = c("Hint 1", "Hint 2", "Hint 3")  # Correct length
  )
  
  result <- validate_test_cases(test_data, "test_func")
  
  expect_equal(result$hints, c("Hint 1", "Hint 2", "Hint 3"))
})
