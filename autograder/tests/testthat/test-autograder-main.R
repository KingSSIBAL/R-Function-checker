# tests/testthat/test-autograder-main.R

test_that("autograder shows progress bar for many tests", {
  skip_on_cran()
  skip_if_offline()
  
  # Create a function that will have progress bar
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
    fib
  }, envir = .GlobalEnv)
  
  # This should show progress (captured in output)
  expect_output(
    autograder("fibonacci", show_progress = TRUE, verbose = FALSE),
    # Progress bar or test output
    "Running Tests"
  )
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder handles type checking", {
  skip_on_cran()
  skip_if_offline()
  
  # Create function that returns wrong type
  assign("student_fibonacci", function(n) {
    return("wrong type")  # Should be numeric
  }, envir = .GlobalEnv)
  
  result <- autograder("fibonacci", verbose = FALSE)
  
  # Should fail some tests due to type mismatch
  expect_gt(result$failed, 0)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder returns proper result structure", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
    fib
  }, envir = .GlobalEnv)
  
  result <- autograder("fibonacci", verbose = FALSE)
  
  # Verify all required fields
  expect_true(all(c("passed", "failed", "total", "score", "max_score", "pass_rate") %in% names(result)))
  
  # Verify logical relationships
  expect_equal(result$total, result$passed + result$failed)
  expect_lte(result$score, result$max_score)
  expect_gte(result$pass_rate, 0)
  expect_lte(result$pass_rate, 100)
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder shows hints when enabled", {
  skip_on_cran()
  skip_if_offline()
  
  # Create wrong function
  assign("student_fibonacci", function(n) {
    return(1:n)  # Wrong implementation
  }, envir = .GlobalEnv)
  
  # With hints
  expect_output(
    autograder("fibonacci", show_hints = TRUE, verbose = TRUE),
    "(Feedback|Type|Length|position)"  # Some feedback keyword
  )
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("autograder summary messages vary by performance", {
  skip_on_cran()
  skip_if_offline()
  
  # Perfect score
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
    fib
  }, envir = .GlobalEnv)
  
  expect_output(
    autograder("fibonacci", verbose = FALSE),
    "ALL TESTS PASSED"
  )
  
  # Failed tests
  assign("student_fibonacci", function(n) {
    return(rep(1, max(1, n)))  # Wrong
  }, envir = .GlobalEnv)
  
  expect_output(
    autograder("fibonacci", verbose = FALSE),
    "failed"
  )
  
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("list_problems handles network errors gracefully", {
  # Should have fallback problems list
  problems <- list_problems()
  
  expect_type(problems, "character")
  expect_gt(length(problems), 0)
  expect_true("fibonacci" %in% problems)
})

test_that("preview_tests shows correct summary", {
  skip_on_cran()
  skip_if_offline()
  
  expect_output(
    result <- preview_tests("fibonacci"),
    "Total tests:"
  )
  
  expect_output(
    preview_tests("fibonacci"),
    "Visible tests:"
  )
  
  expect_output(
    preview_tests("fibonacci"),
    "Hidden tests:"
  )
})
