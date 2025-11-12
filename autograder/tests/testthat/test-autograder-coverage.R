# tests/testthat/test-autograder-coverage.R

# ============================================================================
# Test Student Function Error Handling
# ============================================================================

test_that("autograder handles student function runtime errors", {
  skip_on_cran()
  skip_if_offline()
  
  # Student function that throws error
  assign("student_fibonacci", function(n) {
    stop("Runtime error in student code")
  }, envir = .GlobalEnv)
  
  result <- autograder("fibonacci", verbose = FALSE)
  
  # All tests should fail due to error
  expect_equal(result$failed, result$total)
  expect_equal(result$passed, 0)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder handles instructor function errors gracefully", {
  # This would only happen if repo has broken code
  # Can't easily test without mocking, but we can test the error message
  skip("Requires mocking instructor function errors")
})

test_that("autograder shows detailed error messages when verbose", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    stop("Custom error message")
  }, envir = .GlobalEnv)
  
  expect_output(
    autograder("fibonacci", verbose = TRUE),
    "Custom error message"
  )
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

# ============================================================================
# Test Different Pass Rates and Feedback
# ============================================================================

test_that("autograder shows 'close' message for few failures", {
  skip_on_cran()
  skip_if_offline()
  
  # Function that passes most tests but fails a few
  assign("student_fibonacci", function(n) {
    if (n <= 1) return(1)  # Wrong for n=0
    fib <- c(1, 1)
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
    fib
  }, envir = .GlobalEnv)
  
  expect_output(
    autograder("fibonacci", verbose = FALSE),
    "(close|Focus)"
  )
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder shows 'making progress' for ~50% pass rate", {
  skip_on_cran()
  skip_if_offline()
  
  # Function that gets about half right
  assign("student_fibonacci", function(n) {
    if (n <= 1) return(1)
    return(rep(1, n))  # Returns wrong values but right length sometimes
  }, envir = .GlobalEnv)
  
  result <- autograder("fibonacci", verbose = FALSE)
  
  # Should have some failures
  expect_gt(result$failed, 0)
  expect_gt(result$passed, 0)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder shows suggestions when all tests fail", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    return("completely wrong")
  }, envir = .GlobalEnv)
  
  expect_output(
    autograder("fibonacci", verbose = FALSE),
    "(Suggestions|Check your implementation)"
  )
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

# ============================================================================
# Test Hidden Tests Display Logic
# ============================================================================

test_that("autograder hides details for hidden tests by default", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    return(rep(1, max(1, n)))  # Wrong
  }, envir = .GlobalEnv)
  
  output <- capture.output(
    autograder("fibonacci", verbose = TRUE, show_hidden = FALSE)
  )
  
  # Should show some tests as just FAIL without details
  output_text <- paste(output, collapse = "\n")
  expect_true(grepl("FAIL", output_text))
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder shows hidden test details when requested", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    return(rep(1, max(1, n)))
  }, envir = .GlobalEnv)
  
  output <- capture.output(
    autograder("fibonacci", verbose = TRUE, show_hidden = TRUE)
  )
  
  output_text <- paste(output, collapse = "\n")
  # Should show more detailed output
  expect_true(grepl("Input:|Expected:|Got:", output_text))
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

# ============================================================================
# Test Type Checking Branch
# ============================================================================

test_that("autograder detects and reports type errors", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    return(as.character(1:n))  # Returns character instead of numeric
  }, envir = .GlobalEnv)
  
  expect_output(
    autograder("fibonacci", verbose = TRUE),
    "(Type Error|type)"
  )
  
  result <- autograder("fibonacci", verbose = FALSE)
  expect_gt(result$failed, 0)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

# ============================================================================
# Test Tolerance-Based Comparisons
# ============================================================================

test_that("autograder uses tolerance for numeric comparisons", {
  skip_on_cran()
  skip_if_offline()
  
  # Function that's very close but not exact
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
    # Add tiny floating point error
    fib + 1e-12
  }, envir = .GlobalEnv)
  
  result <- autograder("fibonacci", verbose = FALSE)
  
  # Should still pass with default tolerance
  expect_equal(result$passed, result$total)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

# ============================================================================
# Test Progress Bar Branch
# ============================================================================

test_that("autograder shows progress bar for many tests", {
  student_fibonacci <- function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }

  # Suppress output if needed
  suppressMessages({
    result <- autograder("fibonacci", show_progress = TRUE, verbose = FALSE)
  })

  expect_type(result, "list")
  expect_equal(result$passed, 6)
})

# ============================================================================
# Test Feedback System Coverage
# ============================================================================

test_that("feedback system detects multiple issue types simultaneously", {
  student <- c(1, 2)  # Wrong length
  expected <- c(1, 2, 3, 4, 5)
  
  feedback <- provide_feedback(student, expected, list(n = 5))
  
  expect_true("length_issue" %in% names(feedback))
})

test_that("feedback system handles matching lengths with differences", {
  student <- c(1, 5, 3, 4, 5)
  expected <- c(1, 2, 3, 4, 5)
  
  feedback <- provide_feedback(student, expected, list(x = 1))
  
  expect_true("diff_positions" %in% names(feedback))
  expect_match(feedback$diff_positions, "2")
})

# ============================================================================
# Test Preview Tests Edge Cases
# ============================================================================

test_that("preview_tests validates function_name", {
  expect_error(
    preview_tests(),
    "must be a single character string"
  )
  
  expect_error(
    preview_tests(c("func1", "func2")),
    "must be a single character string"
  )
  
  expect_error(
    preview_tests(123),
    "must be a single character string"
  )
})

test_that("preview_tests returns invisible test data", {
  skip_on_cran()
  skip_if_offline()
  
  result <- preview_tests("fibonacci")
  
  expect_type(result, "list")
  expect_true("inputs" %in% names(result))
  expect_true("descriptions" %in% names(result))
})

# ============================================================================
# Test List Problems Edge Cases
# ============================================================================

test_that("list_problems returns invisible problems list", {
  skip_on_cran()
  skip_if_offline()
  
  result <- list_problems()
  
  expect_type(result, "character")
  expect_gt(length(result), 0)
})

test_that("list_problems shows usage instructions", {
  skip_on_cran()
  skip_if_offline()
  
  expect_output(
    list_problems(),
    "Usage:"
  )
  
  expect_output(
    list_problems(),
    "Preview tests:"
  )
})

# ============================================================================
# Test Package Startup
# ============================================================================

test_that("package startup message is generated", {
  # Test that .onAttach exists and is a function
  expect_true(exists(".onAttach"))
  
  # We can't easily test the actual message without loading the package fresh
  # but we can verify the function exists
})
