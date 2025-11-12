# tests/testthat/test-edge-cases.R

test_that("autograder handles missing student function", {
  # Ensure student function doesn't exist
  if (exists("student_fibonacci", envir = .GlobalEnv)) {
    rm(student_fibonacci, envir = .GlobalEnv)
  }
  
  # Should error when student function is missing
  expect_error(
    autograder("fibonacci"),
    "not found in your environment"
  )
})

test_that("autograder validates function_name parameter", {
  expect_error(
    autograder(),
    "Missing required argument"
  )
  
  expect_error(
    autograder(c("func1", "func2")),
    "must be a single character string"
  )
  
  expect_error(
    autograder(123),
    "must be a single character string"
  )
})

test_that("autograder validates logical parameters", {
  skip_on_cran()
  skip_if_offline()
  
  # Create a valid test function first
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
    fib
  }, envir = .GlobalEnv)
  
  # Test invalid verbose parameter
  expect_error(
    autograder("fibonacci", verbose = "yes"),
    "must be TRUE or FALSE"
  )
  
  # Test invalid show_hidden parameter
  expect_error(
    autograder("fibonacci", show_hidden = 1),
    "must be TRUE or FALSE"
  )
  
  # Test invalid use_parallel parameter  
  expect_error(
    autograder("fibonacci", use_parallel = NULL),
    "must be TRUE or FALSE"
  )
  
  # Cleanup
  rm(student_fibonacci, envir = .GlobalEnv)
})

test_that("custom error classes work correctly", {
  err1 <- network_error("Connection failed")
  expect_s3_class(err1, "network_error")
  expect_s3_class(err1, "error")
  expect_match(err1$message, "Connection failed")
  
  err2 <- function_not_found_error("my_function")
  expect_s3_class(err2, "function_not_found_error")
  expect_equal(err2$function_name, "my_function")
  expect_match(err2$message, "my_function")
  expect_match(err2$message, "list_problems")
  
  err3 <- test_execution_error("Test failed", 5)
  expect_s3_class(err3, "test_execution_error")
  expect_equal(err3$test_number, 5)
})

test_that("NULL coalescing operator works", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal(FALSE %||% "default", FALSE)
  expect_equal(NA %||% "default", NA)
})

test_that("fetch_instructor_code handles invalid input", {
  expect_error(
    fetch_instructor_code("../invalid/path"),
    "Invalid function name format"
  )
  
  expect_error(
    fetch_instructor_code("path/with/slash"),
    "Invalid function name format"
  )
  
  expect_error(
    fetch_instructor_code("path\\with\\backslash"),
    "Invalid function name format"
  )
})

test_that("fetch_instructor_code handles nonexistent functions", {
  skip_on_cran()
  skip_if_offline()
  
  # This will try to fetch from GitHub and fail with 404
  expect_error(
    fetch_instructor_code("nonexistent_xyz_12345"),
    class = "function_not_found_error"
  )
})

test_that("extract_instructor_function handles missing function", {
  env <- new.env()
  env$not_a_function <- "just a string"
  env$another_var <- 123
  
  expect_error(
    extract_instructor_function(env, "test"),
    "No function implementation found"
  )
})

test_that("extract_instructor_function finds function", {
  env <- new.env()
  env$my_function <- function(x) x * 2
  env$other_var <- "not a function"
  
  result <- extract_instructor_function(env, "test")
  expect_true(is.function(result))
  expect_equal(result(5), 10)
})

test_that("extract_test_cases handles missing test_cases", {
  env <- new.env()
  env$some_function <- function(x) x
  
  expect_error(
    extract_test_cases(env, "test"),
    "No test cases found"
  )
})

test_that("extract_test_cases validates and returns test data", {
  env <- new.env()
  env$test_cases <- list(
    inputs = list(list(x = 1), list(x = 2))
  )
  
  result <- extract_test_cases(env, "test")
  expect_type(result, "list")
  expect_true("inputs" %in% names(result))
  expect_equal(length(result$inputs), 2)
})
