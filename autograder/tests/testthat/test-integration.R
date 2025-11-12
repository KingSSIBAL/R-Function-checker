# tests/testthat/test-integration.R

test_that("full autograder workflow works", {
  skip_on_cran()
  skip_if_offline()
  
  # Define student function
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
    fib
  }, envir = .GlobalEnv)
  
  # Run autograder
  result <- autograder("fibonacci", verbose = FALSE, show_progress = FALSE)
  
  # Check result structure
  expect_type(result, "list")
  expect_true("passed" %in% names(result))
  expect_true("failed" %in% names(result))
  expect_true("total" %in% names(result))
  expect_true("score" %in% names(result))
  expect_true("max_score" %in% names(result))
  expect_true("pass_rate" %in% names(result))
  
  # Check values
  expect_equal(result$total, result$passed + result$failed)
  expect_lte(result$score, result$max_score)
  expect_gte(result$pass_rate, 0)
  expect_lte(result$pass_rate, 100)
  
  # Cleanup
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("preview_tests works correctly", {
  skip_on_cran()
  skip_if_offline()
  
  expect_output(
    preview_tests("fibonacci"),
    "Test Cases Preview"
  )
  
  expect_output(
    preview_tests("fibonacci"),
    "HIDDEN TEST"
  )
  
  result <- preview_tests("fibonacci")
  expect_type(result, "list")
  expect_true("inputs" %in% names(result))
})

test_that("list_problems returns problems", {
  skip_on_cran()
  skip_if_offline()
  
  expect_output(
    problems <- list_problems(),
    "fibonacci"
  )
  
  expect_type(problems, "character")
  expect_true("fibonacci" %in% problems)
})

test_that("autograder handles wrong answers correctly", {
  skip_on_cran()
  skip_if_offline()
  
  # Define incorrect function
  assign("student_fibonacci", function(n) {
    return(rep(1, n))  # Wrong implementation
  }, envir = .GlobalEnv)
  
  result <- autograder("fibonacci", verbose = FALSE)
  
  expect_gt(result$failed, 0)
  expect_lt(result$pass_rate, 100)
  
  # Cleanup
  rm(student_fibonacci, envir = .GlobalEnv)
})
