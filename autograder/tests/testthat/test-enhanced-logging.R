# ============================================================================
# Test: enhanced.R - Logging System and Additional Features
# ============================================================================

# ============================================================================
# Section 1: Logging Configuration
# ============================================================================

test_that("autograder_log_config enables logging", {
  # Clear any existing state
  autograder_log_clear()
  
  old <- autograder_log_config(enabled = TRUE, level = "DEBUG", console = FALSE)
  
  expect_type(old, "list")
  expect_true("enabled" %in% names(old))
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
})

test_that("autograder_log_config disables logging", {
  autograder_log_config(enabled = TRUE, console = FALSE)
  autograder_log_config(enabled = FALSE)
  
  # Logging should be disabled now
  expect_true(TRUE)  # If we get here, no error
})

test_that("autograder_log_config returns previous settings", {
  autograder_log_config(enabled = FALSE)
  old <- autograder_log_config(enabled = TRUE, level = "WARN", console = FALSE)
  
  expect_equal(old$enabled, FALSE)
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
})

test_that("autograder_log_config accepts valid log levels", {
  levels <- c("DEBUG", "INFO", "WARN", "ERROR")
  
  for (level in levels) {
    expect_no_error(
      autograder_log_config(enabled = TRUE, level = level, console = FALSE)
    )
  }
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
})

test_that("autograder_log_config handles case-insensitive levels", {
  expect_no_error(
    autograder_log_config(enabled = TRUE, level = "debug", console = FALSE)
  )
  expect_no_error(
    autograder_log_config(enabled = TRUE, level = "Info", console = FALSE)
  )
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
})

# ============================================================================
# Section 2: Log History
# ============================================================================

test_that("autograder_log_history returns character vector", {
  autograder_log_clear()
  
  result <- autograder_log_history()
  expect_type(result, "character")
})

test_that("autograder_log_history respects n parameter", {
  autograder_log_clear()
  autograder_log_config(enabled = TRUE, level = "DEBUG", console = FALSE)
  
  # Generate some log entries by triggering internal logging
  .log_message("INFO", "Test message 1")
  .log_message("INFO", "Test message 2")
  .log_message("INFO", "Test message 3")
  
  history_all <- autograder_log_history()
  history_2 <- autograder_log_history(n = 2)
  
  expect_true(length(history_2) <= 2)
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
  autograder_log_clear()
})

test_that("autograder_log_history filters by level", {
  autograder_log_clear()
  autograder_log_config(enabled = TRUE, level = "DEBUG", console = FALSE)
  
  .log_message("DEBUG", "Debug message")
  .log_message("INFO", "Info message")
  .log_message("WARN", "Warn message")
  .log_message("ERROR", "Error message")
  
  info_only <- autograder_log_history(level = "INFO")
  
  # All INFO entries should contain [INFO]
  expect_true(all(grepl("\\[INFO\\]", info_only)))
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
  autograder_log_clear()
})

# ============================================================================
# Section 3: Log Clear
# ============================================================================

test_that("autograder_log_clear clears history", {
  autograder_log_config(enabled = TRUE, level = "DEBUG", console = FALSE)
  .log_message("INFO", "Test message")
  
  autograder_log_clear()
  
  history <- autograder_log_history()
  expect_length(history, 0)
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
})

test_that("autograder_log_clear returns TRUE", {
  result <- autograder_log_clear()
  expect_true(result)
})

# ============================================================================
# Section 4: Internal Log Message
# ============================================================================

test_that(".log_message respects enabled flag", {
  autograder_log_clear()
  autograder_log_config(enabled = FALSE)
  
  result <- .log_message("INFO", "This should not be logged")
  
  history <- autograder_log_history()
  expect_length(history, 0)
})

test_that(".log_message respects log level", {
  autograder_log_clear()
  autograder_log_config(enabled = TRUE, level = "WARN", console = FALSE)
  
  .log_message("DEBUG", "Debug - should be filtered")
  .log_message("INFO", "Info - should be filtered")
  .log_message("WARN", "Warn - should be logged")
  .log_message("ERROR", "Error - should be logged")
  
  history <- autograder_log_history()
  
  # DEBUG and INFO should be filtered out (level is WARN)
  expect_false(any(grepl("\\[DEBUG\\]", history)))
  expect_false(any(grepl("\\[INFO\\]", history)))
  expect_true(any(grepl("\\[WARN\\]", history)))
  expect_true(any(grepl("\\[ERROR\\]", history)))
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
  autograder_log_clear()
})

test_that(".log_message formats with sprintf", {
  autograder_log_clear()
  autograder_log_config(enabled = TRUE, level = "DEBUG", console = FALSE)
  
  .log_message("INFO", "Count: %d, Name: %s", 42, "test")
  
  history <- autograder_log_history()
  expect_true(any(grepl("Count: 42, Name: test", history)))
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
  autograder_log_clear()
})

test_that(".log_message includes timestamp", {
  autograder_log_clear()
  autograder_log_config(enabled = TRUE, level = "DEBUG", console = FALSE)
  
  .log_message("INFO", "Timestamped message")
  
  history <- autograder_log_history()
  
  # Should contain date-like pattern
  expect_true(any(grepl("\\d{4}-\\d{2}-\\d{2}", history)))
  
  # Cleanup
  autograder_log_config(enabled = FALSE)
  autograder_log_clear()
})

# ============================================================================
# Section 5: Batch Result S3 Class
# ============================================================================

test_that("batch_result print method works", {
  # Create a mock batch result
  mock_batch <- structure(
    list(
      results = list(
        problem1 = new_autograder_result(
          passed = 3, failed = 0, total = 3,
          score = 10, max_score = 10
        ),
        problem2 = new_autograder_result(
          passed = 2, failed = 1, total = 3,
          score = 7, max_score = 10
        )
      ),
      problems = c("problem1", "problem2"),
      errors = list(),
      summary = list(
        total_score = 17,
        total_max = 20,
        score_rate = 85,
        total_passed = 5,
        total_tests = 6,
        pass_rate = 83.33,
        problems_passed = 1,
        problems_total = 2,
        all_passed = FALSE,
        timestamp = Sys.time()
      )
    ),
    class = c("batch_result", "list")
  )
  
  output <- capture.output(print(mock_batch))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Batch")
  expect_match(output_text, "17/20")
  expect_match(output_text, "problem1")
  expect_match(output_text, "problem2")
})

test_that("batch_result summary method works", {
  mock_batch <- structure(
    list(
      results = list(),
      problems = c("test"),
      errors = list(),
      summary = list(
        total_score = 10,
        total_max = 10,
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
  
  output <- capture.output(result <- summary(mock_batch))
  
  expect_equal(result, mock_batch$summary)
})

# ============================================================================
# Section 6: get_student_function (if it exists)
# ============================================================================

test_that("get_student_function returns NULL for missing function", {
  # This tests the function retrieval with validation
  # Make sure no student_test_xyz exists
  if (exists("student_test_xyz", envir = globalenv())) {
    rm("student_test_xyz", envir = globalenv())
  }
  
  # The function should handle missing functions gracefully
  result <- tryCatch(
    get_student_function("test_xyz"),
    error = function(e) NULL
  )
  
  expect_null(result)
})

# ============================================================================
# Cleanup
# ============================================================================

test_that("cleanup: ensure logging is disabled", {
  autograder_log_config(enabled = FALSE)
  autograder_log_clear()
  expect_true(TRUE)
})
