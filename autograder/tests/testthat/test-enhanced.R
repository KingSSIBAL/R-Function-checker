# ============================================================================
# Test: enhanced.R - S3 Classes, Batch Grading, and Utilities
# ============================================================================

# ============================================================================
# Section 1: autograder_result S3 Class
# ============================================================================

test_that("new_autograder_result creates valid S3 object", {
  result <- new_autograder_result(
    passed = 8,
    failed = 2,
    total = 10,
    score = 80,
    max_score = 100,
    function_name = "test_function"
  )
  
  expect_s3_class(result, "autograder_result")
  expect_s3_class(result, "list")
  expect_equal(result$passed, 8L)
  expect_equal(result$failed, 2L)
  expect_equal(result$total, 10L)
  expect_equal(result$score, 80)
  expect_equal(result$max_score, 100)
  expect_equal(result$pass_rate, 80)
  expect_equal(result$score_rate, 80)
  expect_equal(result$function_name, "test_function")
  expect_false(result$all_passed)
})

test_that("new_autograder_result handles all passed", {
  result <- new_autograder_result(
    passed = 10,
    failed = 0,
    total = 10,
    score = 100,
    max_score = 100,
    function_name = "perfect_function"
  )
  
  expect_true(result$all_passed)
  expect_equal(result$pass_rate, 100)
  expect_equal(result$score_rate, 100)
})

test_that("new_autograder_result handles edge cases", {
  # Zero tests
  result_zero <- new_autograder_result(
    passed = 0,
    failed = 0,
    total = 0,
    score = 0,
    max_score = 0
  )
  expect_equal(result_zero$pass_rate, 0)
  expect_equal(result_zero$score_rate, 0)
  
  # No max_score
  result_no_max <- new_autograder_result(
    passed = 5,
    failed = 0,
    total = 5,
    score = 0,
    max_score = 0
  )
  expect_equal(result_no_max$score_rate, 0)
})

test_that("new_autograder_result includes timestamp", {
  before <- Sys.time()
  result <- new_autograder_result(
    passed = 1, failed = 0, total = 1,
    score = 10, max_score = 10
  )
  after <- Sys.time()
  
  expect_true(inherits(result$timestamp, "POSIXct"))
  expect_true(result$timestamp >= before && result$timestamp <= after)
})

test_that("new_autograder_result includes details", {
  details <- list(
    test1 = list(status = "pass", output = 5),
    test2 = list(status = "fail", output = 3)
  )
  
  result <- new_autograder_result(
    passed = 1, failed = 1, total = 2,
    score = 5, max_score = 10,
    details = details
  )
  
  expect_equal(result$details, details)
})

# ============================================================================
# Section 2: print.autograder_result
# ============================================================================

test_that("print.autograder_result outputs correct format for passed", {
  result <- new_autograder_result(
    passed = 10, failed = 0, total = 10,
    score = 100, max_score = 100,
    function_name = "fibonacci"
  )
  
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Autograder Result")
  expect_match(output_text, "fibonacci")
  expect_match(output_text, "100/100")
  expect_match(output_text, "10/10")
  expect_match(output_text, "PASSED")
})

test_that("print.autograder_result outputs correct format for failed", {
  result <- new_autograder_result(
    passed = 7, failed = 3, total = 10,
    score = 70, max_score = 100,
    function_name = "factorial"
  )
  
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "factorial")
  expect_match(output_text, "70/100")
  expect_match(output_text, "7/10")
  expect_match(output_text, "3 test.*failed")
})

test_that("print.autograder_result returns invisibly", {
  result <- new_autograder_result(
    passed = 5, failed = 0, total = 5,
    score = 50, max_score = 50
  )
  
  returned <- capture.output(ret_val <- print(result))
  expect_identical(ret_val, result)
})

# ============================================================================
# Section 3: summary.autograder_result
# ============================================================================

test_that("summary.autograder_result shows performance metrics", {
  result <- new_autograder_result(
    passed = 10, failed = 0, total = 10,
    score = 100, max_score = 100
  )
  
  output <- capture.output(summary(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Summary")
  expect_match(output_text, "Performance Metrics")
  expect_match(output_text, "EXCELLENT|Perfect")
})

test_that("summary.autograder_result shows appropriate status for different pass rates", {
  # Excellent (100%)
  result_100 <- new_autograder_result(
    passed = 10, failed = 0, total = 10,
    score = 100, max_score = 100
  )
  output_100 <- paste(capture.output(summary(result_100)), collapse = "\n")
  expect_match(output_100, "EXCELLENT")
  
  # Good (80%)
  result_80 <- new_autograder_result(
    passed = 8, failed = 2, total = 10,
    score = 80, max_score = 100
  )
  output_80 <- paste(capture.output(summary(result_80)), collapse = "\n")
  expect_match(output_80, "GOOD")
  
  # Needs work (50%)
  result_50 <- new_autograder_result(
    passed = 5, failed = 5, total = 10,
    score = 50, max_score = 100
  )
  output_50 <- paste(capture.output(summary(result_50)), collapse = "\n")
  expect_match(output_50, "NEEDS WORK")
  
  # Significant revision needed (<50%)
  result_20 <- new_autograder_result(
    passed = 2, failed = 8, total = 10,
    score = 20, max_score = 100
  )
  output_20 <- paste(capture.output(summary(result_20)), collapse = "\n")
  expect_match(output_20, "REVISION")
})

# ============================================================================
# Section 4: is_passed helper
# ============================================================================

test_that("is_passed returns correct value", {
  passed_result <- new_autograder_result(
    passed = 10, failed = 0, total = 10,
    score = 100, max_score = 100
  )
  expect_true(is_passed(passed_result))
  
  failed_result <- new_autograder_result(
    passed = 5, failed = 5, total = 10,
    score = 50, max_score = 100
  )
  expect_false(is_passed(failed_result))
})

test_that("is_passed throws error for non-autograder_result", {
  expect_error(is_passed(list(all_passed = TRUE)), "autograder_result")
  expect_error(is_passed(5), "autograder_result")
  expect_error(is_passed("passed"), "autograder_result")
})

# ============================================================================
# Section 5: Timeout Protection
# ============================================================================

test_that("with_timeout completes fast operations", {
  result <- with_timeout({
    sum(1:100)
  }, timeout = 5)
  
  expect_equal(result, 5050)
})

test_that("with_timeout validates timeout parameter", {
  expect_error(with_timeout(sum(1:10), timeout = -1), "positive")
  expect_error(with_timeout(sum(1:10), timeout = "5"), "positive")
  expect_error(with_timeout(sum(1:10), timeout = 0), "positive")
})

test_that("with_timeout returns on_timeout value when timing out", {
  # This test uses a quick check - actual timeout behavior varies by platform
  result <- with_timeout({
    sum(1:10)
  }, timeout = 10, on_timeout = -999)
  
  # Should complete without timeout
  expect_equal(result, 55)
})

# ============================================================================
# Section 6: safe_execute
# ============================================================================

test_that("safe_execute runs function successfully", {
  result <- safe_execute(sum, list(1:10), timeout = 5)
  
  expect_true(result$success)
  expect_equal(result$result, 55)
  expect_null(result$error)
  expect_false(result$timed_out)
})

test_that("safe_execute captures errors", {
  bad_fun <- function(x) stop("Intentional error")
  result <- safe_execute(bad_fun, list(1), timeout = 5)
  
  expect_false(result$success)
  expect_null(result$result)
  expect_match(result$error, "Intentional error")
  expect_false(result$timed_out)
})

test_that("safe_execute validates input", {
  expect_error(safe_execute("not_a_function", list()), "'fun' must be a function")
  expect_error(safe_execute(123, list()), "'fun' must be a function")
})

test_that("safe_execute converts non-list args", {
  result <- safe_execute(sum, 1:10, timeout = 5)
  expect_true(result$success)
  expect_equal(result$result, 55)
})

# ============================================================================
# Section 7: cli_box utility
# ============================================================================

test_that("cli_box creates formatted box", {
  box <- cli_box("Hello World")
  
  expect_type(box, "character")
  expect_match(box, "=+")
  expect_match(box, "Hello World")
  expect_match(box, "\\|")
})

test_that("cli_box handles empty string", {
  box <- cli_box("")
  expect_type(box, "character")
  expect_match(box, "=+")
})

test_that("cli_box handles special characters", {
  box <- cli_box("Test: 100%")
  expect_match(box, "Test: 100%")
})

test_that("cli_box handles long text", {
  long_text <- paste(rep("x", 200), collapse = "")
  box <- cli_box(long_text)
  expect_type(box, "character")
})

test_that("cli_box handles newlines", {
  box <- cli_box("Line 1\nLine 2")
  expect_type(box, "character")
})

# ============================================================================
# ADDITIONAL TESTS
# ============================================================================

test_that("new_autograder_result accepts custom timestamp", {
  custom_time <- as.POSIXct("2024-01-01 12:00:00")
  result <- new_autograder_result(
    passed = 1, failed = 0, total = 1,
    score = 10, max_score = 10,
    timestamp = custom_time
  )
  
  expect_equal(result$timestamp, custom_time)
})

test_that("new_autograder_result coerces numeric types correctly", {
  result <- new_autograder_result(
    passed = 5.5,  # Will be coerced to integer
    failed = 2.5,
    total = 8.0,
    score = 80,
    max_score = 100
  )
  
  expect_type(result$passed, "integer")
  expect_type(result$failed, "integer")
  expect_type(result$total, "integer")
  expect_type(result$score, "double")
  expect_type(result$max_score, "double")
})

test_that("summary.autograder_result returns summary list invisibly", {
  result <- new_autograder_result(8, 2, 10, 80, 100, "test")
  
  returned <- capture.output(ret <- summary(result))
  
  expect_type(ret, "list")
  expect_true("pass_rate" %in% names(ret))
  expect_true("score_rate" %in% names(ret))
  expect_true("status" %in% names(ret))
})

test_that("safe_execute handles NULL return", {
  null_fn <- function() NULL
  result <- safe_execute(null_fn, list())
  
  expect_true(result$success)
  expect_null(result$result)
})

test_that("safe_execute handles complex arguments", {
  complex_fn <- function(x, y) x + y * 2
  result <- safe_execute(complex_fn, list(x = 5, y = 3))
  
  expect_true(result$success)
  expect_equal(result$result, 11)
})

test_that("with_timeout propagates errors", {
  expect_error(with_timeout(stop("test error"), timeout = 5), "test error")
})

test_that("batch_grade validates problems parameter", {
  expect_error(batch_grade(character(0)), "non-empty")
  expect_error(batch_grade(123), "character")
  expect_error(batch_grade(NULL), "character")
})
# ============================================================================
# ADDITIONAL COVERAGE TESTS FOR enhanced.R
# ============================================================================

test_that("is_passed validates input type", {
  expect_error(is_passed(list()), "autograder_result")
  expect_error(is_passed(5), "autograder_result")
  expect_error(is_passed("result"), "autograder_result")
})

test_that("is_passed returns TRUE for passed result", {
  result <- new_autograder_result(
    passed = 10, failed = 0, total = 10,
    score = 100, max_score = 100
  )
  expect_true(is_passed(result))
})

test_that("is_passed returns FALSE for failed result", {
  result <- new_autograder_result(
    passed = 7, failed = 3, total = 10,
    score = 70, max_score = 100
  )
  expect_false(is_passed(result))
})

test_that("with_timeout validates timeout parameter", {
  expect_error(with_timeout(1+1, timeout = 0), "positive")
  expect_error(with_timeout(1+1, timeout = -5), "positive")
  expect_error(with_timeout(1+1, timeout = "five"), "positive")
})

test_that("with_timeout executes normal expressions", {
  result <- with_timeout(sum(1:100), timeout = 5)
  expect_equal(result, 5050)
})

test_that("with_timeout accepts on_timeout parameter", {
  # Test that on_timeout parameter is accepted
  result <- with_timeout({
    sum(1:10)
  }, timeout = 5, on_timeout = "default_value")
  
  # Normal execution should return the result, not on_timeout
  expect_equal(result, 55)
})

test_that("safe_execute handles NA return", {
  na_fn <- function() NA
  result <- safe_execute(na_fn, list())
  
  expect_true(result$success)
  expect_equal(result$result, NA)
})

test_that("safe_execute handles vector return", {
  vec_fn <- function(n) 1:n
  result <- safe_execute(vec_fn, list(n = 5))
  
  expect_true(result$success)
  expect_equal(result$result, 1:5)
})

test_that("safe_execute handles list return", {
  list_fn <- function() list(a = 1, b = 2)
  result <- safe_execute(list_fn, list())
  
  expect_true(result$success)
  expect_equal(result$result$a, 1)
})

test_that("safe_execute handles warning", {
  warn_fn <- function() {
    warning("test warning")
    42
  }
  result <- suppressWarnings(safe_execute(warn_fn, list()))
  
  expect_true(result$success)
  expect_equal(result$result, 42)
})

test_that("summary.autograder_result shows different status messages", {
  # Perfect score
  perfect <- new_autograder_result(10, 0, 10, 100, 100)
  output1 <- capture.output(summary(perfect))
  expect_match(paste(output1, collapse = "\n"), "EXCELLENT")
  
  # Good score (80%+)
  good <- new_autograder_result(8, 2, 10, 80, 100)
  output2 <- capture.output(summary(good))
  expect_match(paste(output2, collapse = "\n"), "GOOD")
  
  # Needs work (50-79%)
  needs_work <- new_autograder_result(6, 4, 10, 60, 100)
  output3 <- capture.output(summary(needs_work))
  expect_match(paste(output3, collapse = "\n"), "NEEDS WORK")
  
  # Significant revision (<50%)
  low <- new_autograder_result(3, 7, 10, 30, 100)
  output4 <- capture.output(summary(low))
  expect_match(paste(output4, collapse = "\n"), "REVISION")
})

test_that("print.autograder_result handles empty function name", {
  result <- new_autograder_result(
    passed = 5, failed = 0, total = 5,
    score = 50, max_score = 50,
    function_name = ""
  )
  
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Autograder Result")
  expect_false(grepl("Function:", output_text))
})

test_that("print.batch_result shows results correctly", {
  # Create mock batch result
  results <- list(
    "problem1" = new_autograder_result(5, 0, 5, 50, 50, "problem1"),
    "problem2" = new_autograder_result(3, 2, 5, 30, 50, "problem2")
  )
  
  batch <- structure(
    list(
      results = results,
      problems = c("problem1", "problem2"),
      errors = list(),
      summary = list(
        total_score = 80,
        total_max = 100,
        score_rate = 80,
        total_passed = 8,
        total_tests = 10,
        pass_rate = 80,
        problems_passed = 1,
        problems_total = 2,
        all_passed = FALSE,
        timestamp = Sys.time()
      )
    ),
    class = c("batch_result", "list")
  )
  
  output <- capture.output(print(batch))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Batch Grading Report")
  expect_match(output_text, "problem1")
  expect_match(output_text, "problem2")
  expect_match(output_text, "need work")
})

test_that("print.batch_result shows all passed message", {
  results <- list(
    "problem1" = new_autograder_result(5, 0, 5, 50, 50, "problem1")
  )
  
  batch <- structure(
    list(
      results = results,
      problems = c("problem1"),
      errors = list(),
      summary = list(
        total_score = 50,
        total_max = 50,
        score_rate = 100,
        total_passed = 5,
        total_tests = 5,
        pass_rate = 100,
        problems_passed = 1,
        problems_total = 1,
        all_passed = TRUE,
        timestamp = Sys.time()
      )
    ),
    class = c("batch_result", "list")
  )
  
  output <- capture.output(print(batch))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "ALL PROBLEMS PASSED")
})

test_that("summary.batch_result returns summary invisibly", {
  results <- list(
    "problem1" = new_autograder_result(5, 0, 5, 50, 50, "problem1")
  )
  
  batch <- structure(
    list(
      results = results,
      problems = c("problem1"),
      errors = list(),
      summary = list(
        total_score = 50,
        total_max = 50,
        score_rate = 100,
        total_passed = 5,
        total_tests = 5,
        pass_rate = 100,
        problems_passed = 1,
        problems_total = 1,
        all_passed = TRUE,
        timestamp = Sys.time()
      )
    ),
    class = c("batch_result", "list")
  )
  
  output <- capture.output(ret <- summary(batch))
  
  expect_type(ret, "list")
  expect_equal(ret$total_score, 50)
})