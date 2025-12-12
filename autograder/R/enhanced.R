# ============================================================================
# AUTOGRADER PACKAGE - ENHANCED FEATURES
# ============================================================================
#
# File: enhanced.R
# Purpose: Advanced features for improved UX and functionality
#
# This module provides:
#   - S3 classes for grading results
#   - Batch grading functionality
#   - Timeout protection
#   - Enhanced parallel execution
#   - Logging system
#   - Helper utilities
#
# Author: Reijel Agub (rcagub@up.edu.ph)
# Version: 0.4.0
# License: MIT
#
# ============================================================================

# ============================================================================
# SECTION 1: S3 CLASSES FOR RESULTS
# ============================================================================

#' Create an autograder result object
#' 
#' @description
#' Creates a structured S3 object representing grading results.
#' Provides nice printing and summary methods.
#' 
#' @param passed Integer. Number of tests passed.
#' @param failed Integer. Number of tests failed.
#' @param total Integer. Total number of tests.
#' @param score Numeric. Points earned.
#' @param max_score Numeric. Maximum possible points.
#' @param function_name Character. Name of the graded function.
#' @param details List. Detailed results for each test (optional).
#' @param timestamp POSIXct. When the grading occurred.
#' 
#' @return An S3 object of class "autograder_result".
#' 
#' @examples
#' \dontrun{
#' result <- autograder("fibonacci")
#' print(result)
#' summary(result)
#' }
#' 
#' @keywords internal
new_autograder_result <- function(passed, failed, total, score, max_score, 
                                   function_name = "", details = list(),
                                   timestamp = Sys.time()) {
  structure(
    list(
      passed = as.integer(passed),
      failed = as.integer(failed),
      total = as.integer(total),
      score = as.numeric(score),
      max_score = as.numeric(max_score),
      pass_rate = if (total > 0) (passed / total) * 100 else 0,
      score_rate = if (max_score > 0) (score / max_score) * 100 else 0,
      function_name = as.character(function_name),
      details = details,
      timestamp = timestamp,
      all_passed = (passed == total)
    ),
    class = c("autograder_result", "list")
  )
}

#' Print method for autograder_result
#' 
#' @param x An autograder_result object.
#' @param ... Additional arguments (ignored).
#' 
#' @return Invisibly returns x.
#' 
#' @export
print.autograder_result <- function(x, ...) {
  cat("\n")
  cat("=== Autograder Result ===\n")
  if (nchar(x$function_name) > 0) {
    cat(sprintf("Function: %s\n", x$function_name))
  }
  
  # Handle decimal scores from benchmark bonus
  if (x$score == floor(x$score) && x$max_score == floor(x$max_score)) {
    cat(sprintf("Score: %d/%d points (%.1f%%)\n", 
                as.integer(x$score), as.integer(x$max_score), x$score_rate))
  } else {
    cat(sprintf("Score: %.1f/%.1f points (%.1f%%)\n", 
                x$score, x$max_score, x$score_rate))
  }
  
  cat(sprintf("Tests: %d/%d passed (%.1f%%)\n", x$passed, x$total, x$pass_rate))
  cat(sprintf("Time: %s\n", format(x$timestamp, "%Y-%m-%d %H:%M:%S")))
  
  # Show benchmark info if available
  if (!is.null(x$details$benchmark)) {
    bm <- x$details$benchmark
    cat(sprintf("\nBenchmark: %.2fx %s\n", bm$ratio, bm$verdict))
    if (!is.null(x$details$benchmark_bonus) && x$details$benchmark_bonus > 0) {
      cat(sprintf("Bonus: +%.1f points\n", x$details$benchmark_bonus))
    }
  }
  
  if (x$all_passed) {
    cat("\nStatus: PASSED - All tests successful!\n")
  } else {
    cat(sprintf("\nStatus: %d test(s) failed\n", x$failed))
  }
  cat("\n")
  
  invisible(x)
}

#' Summary method for autograder_result
#' 
#' @param object An autograder_result object.
#' @param ... Additional arguments (ignored).
#' 
#' @return A summary list.
#' 
#' @export
summary.autograder_result <- function(object, ...) {
  cat("\n=== Autograder Result Summary ===\n\n")
  
  # Basic stats
  cat("Performance Metrics:\n")
  cat(sprintf("  - Tests passed: %d/%d (%.1f%%)\n", object$passed, object$total, object$pass_rate))
  
  # Handle decimal scores from benchmark bonus
  if (object$score == floor(object$score) && object$max_score == floor(object$max_score)) {
    cat(sprintf("  - Points earned: %d/%d (%.1f%%)\n", 
                as.integer(object$score), as.integer(object$max_score), object$score_rate))
  } else {
    cat(sprintf("  - Points earned: %.1f/%.1f (%.1f%%)\n", 
                object$score, object$max_score, object$score_rate))
  }
  
  # Show benchmark details if available
  if (!is.null(object$details$benchmark)) {
    bm <- object$details$benchmark
    cat("\nBenchmark Results:\n")
    cat(sprintf("  - Your time: %.6f sec (median over %d runs)\n", 
                bm$student_median, bm$n_runs))
    cat(sprintf("  - Reference time: %.6f sec\n", bm$instructor_median))
    cat(sprintf("  - Performance ratio: %.2fx (%s)\n", bm$ratio, bm$verdict))
    
    if (!is.null(object$details$benchmark_bonus) && object$details$benchmark_bonus > 0) {
      cat(sprintf("  - Bonus earned: +%.1f points\n", object$details$benchmark_bonus))
    }
  }
  
  # Status indicator
  cat("\nStatus: ")
  if (object$all_passed) {
    cat("EXCELLENT - Perfect score!\n")
  } else if (object$pass_rate >= 80) {
    cat("GOOD - Minor issues to fix\n")
  } else if (object$pass_rate >= 50) {
    cat("NEEDS WORK - Review your implementation\n")
  } else {
    cat("NEEDS SIGNIFICANT REVISION\n")
  }
  
  # Return summary data invisibly
  invisible(list(
    pass_rate = object$pass_rate,
    score_rate = object$score_rate,
    status = if (object$all_passed) "passed" else "failed",
    benchmark = object$details$benchmark
  ))
}

#' Check if result passed all tests
#' 
#' @param result An autograder_result object.
#' 
#' @return Logical. TRUE if all tests passed.
#' 
#' @examples
#' \dontrun{
#' result <- autograder("fibonacci")
#' if (is_passed(result)) {
#'   message("Great job!")
#' }
#' }
#' 
#' @keywords internal
is_passed <- function(result) {
  if (!inherits(result, "autograder_result")) {
    stop("Expected an autograder_result object", call. = FALSE)
  }
  result$all_passed
}

# ============================================================================
# SECTION 2: BATCH GRADING
# ============================================================================

#' Grade multiple problems at once
#' 
#' @description
#' Runs the autograder on multiple problems and returns a combined report.
#' Useful for grading an entire assignment or problem set.
#' 
#' @param problems Character vector. Names of problems to grade.
#' @param verbose Logical. Show detailed output? Default: FALSE.
#' @param stop_on_error Logical. Stop if a problem errors? Default: FALSE.
#' @param parallel Logical. Run problems in parallel? Default: FALSE.
#' 
#' @return An S3 object of class "batch_result" containing:
#'   - results: List of individual autograder_result objects
#'   - summary: Overall statistics
#'   - problems: Names of graded problems
#' 
#' @examples
#' \dontrun{
#' # Grade multiple problems
#' batch_result <- batch_grade(c("fibonacci", "factorial", "sum_vector"))
#' print(batch_result)
#' 
#' # Check overall status
#' summary(batch_result)
#' }
#' 
#' @keywords internal
batch_grade <- function(problems, verbose = FALSE, stop_on_error = FALSE, 
                        parallel = FALSE) {
  if (!is.character(problems) || length(problems) == 0) {
    stop("'problems' must be a non-empty character vector", call. = FALSE)
  }
  
  cat(sprintf("\n=== Batch Grading %d Problems ===\n\n", length(problems)))
  
  results <- list()
  errors <- list()
  
  for (i in seq_along(problems)) {
    problem <- problems[i]
    cat(sprintf("[%d/%d] Grading: %s\n", i, length(problems), problem))
    
    result <- tryCatch({
      # Suppress output unless verbose
      if (verbose) {
        autograder(problem, verbose = TRUE)
      } else {
        suppressMessages(
          utils::capture.output(
            res <- autograder(problem, verbose = FALSE),
            type = "output"
          )
        )
        res
      }
    }, error = function(e) {
      if (stop_on_error) {
        stop(sprintf("Error grading '%s': %s", problem, e$message), call. = FALSE)
      }
      errors[[problem]] <<- e$message
      # Return a failed result
      new_autograder_result(
        passed = 0, failed = 1, total = 1,
        score = 0, max_score = 0,
        function_name = problem,
        details = list(error = e$message)
      )
    })
    
    # Wrap in autograder_result if needed
    if (!inherits(result, "autograder_result")) {
      result <- new_autograder_result(
        passed = result$passed,
        failed = result$failed,
        total = result$total,
        score = result$score,
        max_score = result$max_score,
        function_name = problem
      )
    }
    
    results[[problem]] <- result
    
    # Show quick status
    status_icon <- if (result$all_passed) "\u2714" else "\u2718"
    cat(sprintf("   %s Score: %d/%d (%.0f%%)\n", 
                status_icon, result$score, result$max_score, result$score_rate))
  }
  
  # Calculate overall summary - vectorized single-pass extraction
  stats <- vapply(results, function(r) {
    c(r$score, r$max_score, r$passed, r$total, as.numeric(r$all_passed))
  }, numeric(5))
  
  total_score <- sum(stats[1, ])
  total_max <- sum(stats[2, ])
  total_passed <- sum(stats[3, ])
  total_tests <- sum(stats[4, ])
  problems_passed <- sum(stats[5, ])
  
  batch <- structure(
    list(
      results = results,
      problems = problems,
      errors = errors,
      summary = list(
        total_score = total_score,
        total_max = total_max,
        score_rate = if (total_max > 0) (total_score / total_max) * 100 else 0,
        total_passed = total_passed,
        total_tests = total_tests,
        pass_rate = if (total_tests > 0) (total_passed / total_tests) * 100 else 0,
        problems_passed = problems_passed,
        problems_total = length(problems),
        all_passed = (problems_passed == length(problems)),
        timestamp = Sys.time()
      )
    ),
    class = c("batch_result", "list")
  )
  
  # Print summary
  cat("\n=== Batch Summary ===\n")
  cat(sprintf("Problems: %d/%d passed\n", problems_passed, length(problems)))
  cat(sprintf("Overall Score: %d/%d (%.1f%%)\n", total_score, total_max, batch$summary$score_rate))
  cat(sprintf("Tests: %d/%d passed (%.1f%%)\n", total_passed, total_tests, batch$summary$pass_rate))
  
  if (length(errors) > 0) {
    cat(sprintf("\nErrors: %d problem(s) had errors\n", length(errors)))
  }
  
  invisible(batch)
}

#' Print method for batch_result
#' 
#' @param x A batch_result object.
#' @param ... Additional arguments (ignored).
#' 
#' @return Invisibly returns x.
#' 
#' @keywords internal
print.batch_result <- function(x, ...) {
  cat("\n=== Batch Grading Report ===\n\n")
  cat(sprintf("Problems Graded: %d\n", length(x$problems)))
  cat(sprintf("Overall Score: %d/%d (%.1f%%)\n", 
              x$summary$total_score, x$summary$total_max, x$summary$score_rate))
  cat(sprintf("Tests Passed: %d/%d (%.1f%%)\n",
              x$summary$total_passed, x$summary$total_tests, x$summary$pass_rate))
  
  cat("\nPer-Problem Results:\n")
  for (name in names(x$results)) {
    r <- x$results[[name]]
    icon <- if (r$all_passed) "\u2714" else "\u2718"
    cat(sprintf("  %s %-20s %d/%d pts (%.0f%%)\n",
                icon, name, r$score, r$max_score, r$score_rate))
  }
  
  if (x$summary$all_passed) {
    cat("\nStatus: ALL PROBLEMS PASSED!\n")
  } else {
    cat(sprintf("\nStatus: %d problem(s) need work\n", 
                x$summary$problems_total - x$summary$problems_passed))
  }
  
  invisible(x)
}

#' Summary method for batch_result
#' 
#' @param object A batch_result object.
#' @param ... Additional arguments (ignored).
#' 
#' @return Summary statistics.
#' 
#' @keywords internal
summary.batch_result <- function(object, ...) {
  print(object)
  invisible(object$summary)
}

# ============================================================================
# SECTION 3: TIMEOUT PROTECTION
# ============================================================================

#' Run function with timeout protection
#' 
#' @description
#' Executes a function with a timeout limit to prevent infinite loops
#' from hanging the R session.
#' 
#' @param expr Expression to evaluate.
#' @param timeout Numeric. Timeout in seconds. Default: 30.
#' @param on_timeout Value to return on timeout. Default: NULL.
#' 
#' @return The result of expr, or on_timeout if timeout occurs.
#' 
#' @details
#' Uses R's setTimeLimit for timeout enforcement.
#' Note: This works on Unix systems. On Windows, timeout may not be
#' perfectly enforced for all operations.
#' 
#' @examples
#' \dontrun{
#' # This will timeout
#' with_timeout({
#'   while(TRUE) {}
#' }, timeout = 2)
#' 
#' # This will complete
#' with_timeout({
#'   sum(1:1000)
#' }, timeout = 5)
#' }
#' 
#' @keywords internal
with_timeout <- function(expr, timeout = 30, on_timeout = NULL) {
  if (!is.numeric(timeout) || timeout <= 0) {
    stop("'timeout' must be a positive number", call. = FALSE)
  }
  
  # Set time limit
  setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
  
  tryCatch({
    expr
  }, error = function(e) {
    if (grepl("time limit|reached elapsed|reached CPU", e$message, ignore.case = TRUE)) {
      warning(sprintf("Operation timed out after %d seconds", timeout), call. = FALSE)
      return(on_timeout)
    }
    stop(e)
  })
}

#' Safe function execution with timeout
#' 
#' @description
#' Wrapper to safely execute a student function with timeout protection.
#' 
#' @param fun Function to execute.
#' @param args List of arguments to pass to fun.
#' @param timeout Numeric. Timeout in seconds. Default: 30.
#' 
#' @return A list with:
#'   - result: The function result (or NULL on timeout/error)
#'   - success: Logical. TRUE if completed successfully.
#'   - error: Error message if any.
#'   - timed_out: Logical. TRUE if timeout occurred.
#' 
#' @examples
#' \dontrun{
#' result <- safe_execute(sum, list(1:10), timeout = 5)
#' if (result$success) {
#'   print(result$result)
#' }
#' }
#' 
#' @keywords internal
safe_execute <- function(fun, args, timeout = 30) {
  if (!is.function(fun)) {
    stop("'fun' must be a function", call. = FALSE)
  }
  if (!is.list(args)) {
    args <- list(args)
  }
  
  result <- list(
    result = NULL,
    success = FALSE,
    error = NULL,
    timed_out = FALSE,
    elapsed_time = NA
  )
  
  start_time <- Sys.time()
  
  setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
  
  tryCatch({
    result$result <- do.call(fun, args)
    result$success <- TRUE
  }, error = function(e) {
    if (grepl("time limit|reached elapsed|reached CPU", e$message, ignore.case = TRUE)) {
      result$timed_out <<- TRUE
      result$error <<- sprintf("Timeout: exceeded %d seconds", timeout)
    } else {
      result$error <<- e$message
    }
  })
  
  result$elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  result
}

# ============================================================================
# SECTION 4: LOGGING SYSTEM
# ============================================================================

# Private environment for logging
.log_env <- new.env(parent = emptyenv())
.log_env$enabled <- FALSE
.log_env$level <- "INFO"
.log_env$file <- NULL
.log_env$history <- character(0)

#' Configure autograder logging
#' 
#' @description
#' Enables or configures the logging system for debugging and diagnostics.
#' 
#' @param enabled Logical. Enable logging? Default: TRUE.
#' @param level Character. Log level: "DEBUG", "INFO", "WARN", "ERROR".
#' @param file Character. Path to log file (optional).
#' @param console Logical. Also print to console? Default: TRUE.
#' 
#' @return Invisibly returns the previous settings.
#' 
#' @examples
#' \dontrun{
#' # Enable debug logging
#' autograder_log_config(enabled = TRUE, level = "DEBUG")
#' 
#' # Log to file
#' autograder_log_config(file = "autograder.log")
#' 
#' # Disable logging
#' autograder_log_config(enabled = FALSE)
#' }
#' 
#' @keywords internal
autograder_log_config <- function(enabled = TRUE, level = "INFO", 
                                   file = NULL, console = TRUE) {
  old_settings <- list(
    enabled = .log_env$enabled,
    level = .log_env$level,
    file = .log_env$file,
    console = .log_env$console
  )
  
  .log_env$enabled <- enabled
  .log_env$level <- match.arg(toupper(level), c("DEBUG", "INFO", "WARN", "ERROR"))
  .log_env$file <- file
  .log_env$console <- console
  
  if (enabled) {
    .log_message("INFO", "Logging enabled at level: %s", level)
  }
  
  invisible(old_settings)
}

#' Internal log message function
#' @keywords internal
.log_message <- function(level, message, ...) {
  if (!.log_env$enabled) return(invisible(NULL))
  
  levels <- c("DEBUG" = 1, "INFO" = 2, "WARN" = 3, "ERROR" = 4)
  if (levels[level] < levels[.log_env$level]) return(invisible(NULL))
  
  # Format message
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted <- sprintf("[%s] [%s] %s", timestamp, level, sprintf(message, ...))
  
  # Store in history
  .log_env$history <- c(.log_env$history, formatted)
  
  # Print to console if enabled
  if (isTRUE(.log_env$console)) {
    cat(formatted, "\n")
  }
  
  # Write to file if configured
  if (!is.null(.log_env$file)) {
    tryCatch({
      cat(formatted, "\n", file = .log_env$file, append = TRUE)
    }, error = function(e) {
      warning("Failed to write to log file: ", e$message)
    })
  }
  
  invisible(formatted)
}

#' Get log history
#' 
#' @description
#' Retrieves the log message history from the current session.
#' 
#' @param n Integer. Number of recent messages to return. Default: all.
#' @param level Character. Filter by minimum level (optional).
#' 
#' @return Character vector of log messages.
#' 
#' @examples
#' \dontrun{
#' # Get all log messages
#' autograder_log_history()
#' 
#' # Get last 10 messages
#' autograder_log_history(n = 10)
#' }
#' 
#' @keywords internal
autograder_log_history <- function(n = NULL, level = NULL) {
  history <- .log_env$history
  
  if (!is.null(level)) {
    level <- match.arg(toupper(level), c("DEBUG", "INFO", "WARN", "ERROR"))
    history <- history[grepl(sprintf("\\[%s\\]", level), history)]
  }
  
  if (!is.null(n) && is.numeric(n) && n > 0) {
    history <- tail(history, n)
  }
  
  history
}

#' Clear log history
#' 
#' @description
#' Clears the in-memory log history.
#' 
#' @return Invisibly returns TRUE.
#' 
#' @keywords internal
autograder_log_clear <- function() {
  .log_env$history <- character(0)
  invisible(TRUE)
}

# ============================================================================
# SECTION 5: HELPER FUNCTIONS
# ============================================================================

#' Get student function safely
#' 
#' @description
#' Retrieves a student function from the global environment with validation.
#' 
#' @param problem_name Character. The problem name (e.g., "fibonacci").
#' 
#' @return The student's function, or NULL if not found.
#' 
#' @examples
#' \dontrun{
#' fn <- get_student_function("fibonacci")
#' if (!is.null(fn)) {
#'   fn(10)  # Test it
#' }
#' }
#' 
#' @keywords internal
get_student_function <- function(problem_name) {
  if (!is.character(problem_name) || length(problem_name) != 1) {
    stop("'problem_name' must be a single character string", call. = FALSE)
  }
  
  fn_name <- paste0("student_", problem_name)
  
  if (!exists(fn_name, envir = .GlobalEnv, mode = "function")) {
    return(NULL)
  }
  
  get(fn_name, envir = .GlobalEnv)
}

#' Create function template
#' 
#' @description
#' Generates a template function definition for a given problem.
#' 
#' @param problem_name Character. The problem name.
#' @param args Character vector. Argument names. Default: "n".
#' 
#' @return Character string with function template.
#' 
#' @examples
#' \dontrun{
#' # Get template
#' cat(function_template("fibonacci"))
#' 
#' # With custom arguments
#' cat(function_template("analyze", args = c("data", "method")))
#' }
#' 
#' @keywords internal
function_template <- function(problem_name, args = "n") {
  if (!is.character(problem_name) || length(problem_name) != 1) {
    stop("'problem_name' must be a single character string", call. = FALSE)
  }
  
  fn_name <- paste0("student_", problem_name)
  args_str <- paste(args, collapse = ", ")
  
  template <- sprintf(
    '%s <- function(%s) {\n  # Your implementation here\n  \n  # Return your result\n  \n}',
    fn_name, args_str
  )
  
  template
}

#' Test function locally
#' 
#' @description
#' Tests a function with given inputs without running full autograder.
#' Useful for quick manual testing.
#' 
#' @param problem_name Character. The problem name.
#' @param ... Arguments to pass to the function.
#' 
#' @return The function result, or error information.
#' 
#' @examples
#' \dontrun{
#' student_fibonacci <- function(n) { if (n <= 1) return(1); ... }
#' 
#' # Quick test
#' test_locally("fibonacci", 5)
#' test_locally("fibonacci", 10)
#' }
#' 
#' @keywords internal
test_locally <- function(problem_name, ...) {
  fn <- get_student_function(problem_name)
  
  if (is.null(fn)) {
    stop(sprintf("Function 'student_%s' not found. Define it first.", problem_name), 
         call. = FALSE)
  }
  
  args <- list(...)
  cat(sprintf("Testing student_%s with inputs: %s\n", 
              problem_name, 
              paste(sapply(args, format_output), collapse = ", ")))
  
  result <- safe_execute(fn, args, timeout = 30)
  
  if (result$success) {
    cat(sprintf("Result: %s\n", format_output(result$result)))
    cat(sprintf("Time: %.4f seconds\n", result$elapsed_time))
    invisible(result$result)
  } else if (result$timed_out) {
    cat("TIMEOUT: Function took too long\n")
    invisible(NULL)
  } else {
    cat(sprintf("ERROR: %s\n", result$error))
    invisible(NULL)
  }
}

#' Compare two results
#' 
#' @description
#' Compares two values and reports differences.
#' Useful for debugging test failures.
#' 
#' @param expected The expected value.
#' @param actual The actual value.
#' @param tolerance Numeric tolerance for comparisons. Default: 1e-6.
#' 
#' @return Logical. TRUE if values match.
#' 
#' @examples
#' \dontrun{
#' expected <- c(1, 1, 2, 3, 5)
#' actual <- student_fibonacci(5)
#' compare_results(expected, actual)
#' }
#' 
#' @keywords internal
compare_results <- function(expected, actual, tolerance = 1e-6) {
  cat("\n=== Result Comparison ===\n")
  cat(sprintf("Expected: %s\n", format_output(expected)))
  cat(sprintf("Actual:   %s\n", format_output(actual)))
  
  # Type check
  if (typeof(expected) != typeof(actual)) {
    cat(sprintf("\nType mismatch: expected '%s', got '%s'\n", 
                typeof(expected), typeof(actual)))
    return(invisible(FALSE))
  }
  
  # Length check
  if (length(expected) != length(actual)) {
    cat(sprintf("\nLength mismatch: expected %d, got %d\n",
                length(expected), length(actual)))
    return(invisible(FALSE))
  }
  
  # Value comparison
  if (is.numeric(expected) && is.numeric(actual)) {
    diffs <- which(abs(expected - actual) > tolerance)
    if (length(diffs) > 0) {
      cat(sprintf("\nDifferences at positions: %s\n", 
                  paste(head(diffs, 5), collapse = ", ")))
      cat("\nFirst few differences:\n")
      for (i in head(diffs, 3)) {
        cat(sprintf("  Position %d: expected %.6f, got %.6f (diff: %.6f)\n",
                    i, expected[i], actual[i], abs(expected[i] - actual[i])))
      }
      return(invisible(FALSE))
    }
  } else {
    if (!identical(expected, actual)) {
      cat("\nValues do not match exactly\n")
      return(invisible(FALSE))
    }
  }
  
  cat("\nResult: MATCH\n")
  invisible(TRUE)
}

# ============================================================================
# SECTION 6: ENHANCED PARALLEL EXECUTION
# ============================================================================

#' Get parallel configuration
#' 
#' @description
#' Returns information about parallel execution capabilities.
#' 
#' @return A list with parallel configuration details.
#' 
#' @examples
#' \dontrun{
#' parallel_info()
#' }
#' 
#' @keywords internal
parallel_info <- function() {
  cores <- parallel::detectCores()
  
  list(
    available_cores = cores,
    usable_cores = min(cores - 1, 4),
    parallel_threshold = 10,  # Tests needed to trigger parallel
    parallel_enabled = cores > 1,
    os = .Platform$OS.type,
    cluster_type = if (.Platform$OS.type == "windows") "PSOCK" else "FORK"
  )
}

#' Set parallel options
#' 
#' @description
#' Configure parallel execution options for the autograder.
#' 
#' @param max_cores Integer. Maximum cores to use. Default: 4.
#' @param threshold Integer. Minimum tests to trigger parallel. Default: 10.
#' @param enabled Logical. Enable parallel execution? Default: TRUE.
#' 
#' @return Invisibly returns previous settings.
#' 
#' @examples
#' \dontrun{
#' # Use up to 8 cores
#' set_parallel_options(max_cores = 8)
#' 
#' # Disable parallel
#' set_parallel_options(enabled = FALSE)
#' }
#' 
#' @keywords internal
set_parallel_options <- function(max_cores = 4, threshold = 10, enabled = TRUE) {
  old_settings <- list(
    max_cores = getOption("autograder.max_cores", 4),
    threshold = getOption("autograder.parallel_threshold", 10),
    enabled = getOption("autograder.parallel_enabled", TRUE)
  )
  
  options(
    autograder.max_cores = max_cores,
    autograder.parallel_threshold = threshold,
    autograder.parallel_enabled = enabled
  )
  
  invisible(old_settings)
}

# ============================================================================
# SECTION 7: ERROR RECOVERY
# ============================================================================

#' Retry operation with backoff
#' 
#' @description
#' Retries an operation with exponential backoff on failure.
#' Useful for network operations that may temporarily fail.
#' 
#' @param expr Expression to evaluate.
#' @param max_attempts Integer. Maximum retry attempts. Default: 3.
#' @param initial_delay Numeric. Initial delay in seconds. Default: 1.
#' @param backoff_factor Numeric. Multiplier for delay. Default: 2.
#' @param silent Logical. Suppress retry messages? Default: FALSE.
#' 
#' @return The result of expr on success.
#' 
#' @examples
#' \dontrun{
#' # Retry network operation
#' result <- with_retry({
#'   fetch_data_from_server()
#' }, max_attempts = 3)
#' }
#' 
#' @keywords internal
with_retry <- function(expr, max_attempts = 3, initial_delay = 1, 
                       backoff_factor = 2, silent = FALSE) {
  delay <- initial_delay
  last_error <- NULL
  
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      list(value = expr, success = TRUE)
    }, error = function(e) {
      list(error = e, success = FALSE)
    })
    
    if (result$success) {
      return(result$value)
    }
    
    last_error <- result$error
    
    if (attempt < max_attempts) {
      if (!silent) {
        message(sprintf("Attempt %d failed, retrying in %.1f seconds...", 
                        attempt, delay))
      }
      Sys.sleep(delay)
      delay <- delay * backoff_factor
    }
  }
  
  stop(sprintf("Operation failed after %d attempts. Last error: %s",
               max_attempts, last_error$message), call. = FALSE)
}

#' Graceful error recovery wrapper
#' 
#' @description
#' Wraps an expression with graceful error handling and recovery.
#' 
#' @param expr Expression to evaluate.
#' @param on_error Value or function to call on error.
#' @param finally Expression to always execute (cleanup).
#' 
#' @return The result of expr, or result of on_error on failure.
#' 
#' @examples
#' \dontrun{
#' result <- graceful({
#'   risky_operation()
#' }, on_error = NA)
#' }
#' 
#' @keywords internal
graceful <- function(expr, on_error = NULL, finally = NULL) {
  tryCatch({
    result <- expr
    if (!is.null(finally)) eval(finally)
    result
  }, error = function(e) {
    if (!is.null(finally)) eval(finally)
    if (is.function(on_error)) {
      on_error(e)
    } else {
      on_error
    }
  })
}
