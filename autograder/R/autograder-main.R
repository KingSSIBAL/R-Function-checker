# ============================================================================
# AUTOGRADER PACKAGE - MAIN AUTOGRADER FUNCTION
# ============================================================================
#
# File: autograder-main.R
# Purpose: Primary user-facing autograder function
#
# This is the main function students use to test their implementations.
# It orchestrates the entire grading workflow.
#
# Related Files:
#   - errors.R: Custom error classes
#   - utils.R: Helper functions (format_output, %||%)
#   - validation.R: Test case validation
#   - fetch.R: Instructor code fetching
#   - feedback.R: Student feedback generation
#   - execution.R: Test execution (parallel/sequential)
#   - api.R: Discovery functions (list_problems, preview_tests)
#   - performance.R: Performance comparison
#   - zzz.R: Package startup
#
# Design Philosophy:
#   - User-friendly: Clear error messages, helpful feedback
#   - Robust: Handles all error cases gracefully
#   - Performant: Uses parallel execution for large test sets
#   - Informative: Provides detailed feedback to guide learning
#
# Workflow Summary:
#   1. Validate all input parameters
#   2. Check prerequisites (internet, student function)
#   3. Fetch instructor code and test cases
#   4. Run tests (parallel or sequential)
#   5. Compare outputs and generate feedback
#   6. Display results and summary
#   7. Return structured results object
#
# ============================================================================

#' Run autograder by function name
#'
#' @description
#' Tests student implementation against reference outputs with support for
#' points, descriptions, hidden tests, type checking, tolerance, parallel
#' execution, and detailed feedback.
#' 
#' This is the main function students use to test their work.
#'
#' @param function_name Character. Name of the function to test.
#'        Must match a function in the repository (see list_problems()).
#'        Example: "fibonacci", "factorial", "sum_vector"
#'        
#' @param verbose Logical. Show detailed output for each test? 
#'        TRUE (default): Shows inputs, expected, actual output for failures
#'        FALSE: Shows only test result (PASS/FAIL) and summary
#'        
#' @param show_hidden Logical. Show details for hidden tests?
#'        FALSE (default): Hidden tests show only PASS/FAIL
#'        TRUE: Hidden tests show full details (for instructor review)
#'        
#' @param show_progress Logical. Show progress bar for many tests?
#'        FALSE (default): No progress bar
#'        TRUE: Shows progress bar if ≥6 tests
#'        
#' @param use_parallel Logical. Use parallel processing?
#'        TRUE (default): Automatic parallel for ≥10 tests
#'        FALSE: Always use sequential execution
#'        
#' @param show_hints Logical. Show hints for failed tests?
#'        TRUE (default): Display instructor hints when available
#'        FALSE: Hide hints (for exams/assessments)
#'        
#' @param benchmark Logical. Run performance benchmark after tests?
#'        FALSE (default): Skip performance comparison
#'        TRUE: Compare your implementation's speed vs instructor's
#'        
#' @param benchmark_runs Integer. Number of benchmark iterations (default: 50).
#'        Higher values give more accurate timing but take longer.
#'        Only used when benchmark = TRUE.
#'        
#' @param benchmark_bonus Numeric. Maximum bonus percentage for performance (default: 0).
#'        Set to e.g. 10 to award up to 10% bonus points for fast implementations.
#'        Only awarded if all tests pass and implementation is faster than reference.
#'
#' @return Invisibly returns a list with test results:
#' \itemize{
#'   \item \code{passed}: Integer. Number of tests passed
#'   \item \code{failed}: Integer. Number of tests failed
#'   \item \code{total}: Integer. Total number of tests
#'   \item \code{score}: Numeric. Points earned (includes benchmark bonus if any)
#'   \item \code{max_score}: Numeric. Maximum possible points (includes max bonus)
#'   \item \code{pass_rate}: Numeric. Percentage of tests passed (0-100)
#'   \item \code{benchmark}: List with performance data (if benchmark = TRUE)
#' }
#'
#' @section Usage Workflow:
#' 1. See available problems: `list_problems()`
#' 2. Preview test cases: `preview_tests("function_name")`
#' 3. Define your function: `student_function_name <- function(...) { ... }`
#' 4. Run autograder: `autograder("function_name")`
#' 5. Review feedback and improve your code
#' 6. Repeat until all tests pass
#'
#' @section Performance:
#' - Sequential mode: ~100-500ms per test (depends on function complexity)
#' - Parallel mode: ~50-200ms per test on 4-core system
#' - Network fetch: ~1-3 seconds (cached by OS after first call)
#'
#' @section Error Messages:
#' The function provides specific, actionable error messages:
#' - "Function not found" → Use list_problems()
#' - "Type mismatch" → Check your return type
#' - "Length mismatch" → Check loop bounds or edge cases
#' - "Network error" → Check internet connection
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Define your solution
#' student_fibonacci <- function(n) {
#'   if (n <= 0) return(numeric(0))
#'   if (n == 1) return(1)
#'   fib <- c(1, 1)
#'   for (i in 3:n) {
#'     fib[i] <- fib[i-1] + fib[i-2]
#'   }
#'   fib
#' }
#' 
#' # 2. Run autograder
#' result <- autograder("fibonacci")
#' 
#' # 3. Check your score
#' print(result$pass_rate)  # 100 if all passed
#' 
#' # 4. Run with different options
#' autograder("fibonacci", verbose = FALSE)           # Minimal output
#' autograder("fibonacci", show_hints = TRUE)         # Show hints
#' autograder("fibonacci", use_parallel = FALSE)      # Force sequential
#' autograder("fibonacci", benchmark = TRUE)          # Include performance benchmark
#' autograder("fibonacci", benchmark = TRUE, benchmark_bonus = 10)  # With bonus points
#' }
#'
autograder <- function(function_name, verbose = TRUE, show_hidden = FALSE, 
                       show_progress = FALSE, use_parallel = TRUE, show_hints = TRUE,
                       benchmark = FALSE, benchmark_runs = 50L, benchmark_bonus = 0) {
  
  # ==========================================================================
  # SECTION 1: INPUT VALIDATION
  # ==========================================================================
  # Comprehensive validation using checkmate for robust type checking.
  # Better to fail fast with clear message than proceed with bad input.
  
  # ===== Check function_name is provided =====
  if (missing(function_name)) {
    stop(
      "Missing required argument 'function_name'.\n",
      "Usage: autograder('fibonacci')\n",
      "See list_problems() for available functions.",
      call. = FALSE
    )
  }
  
  # ===== Validate function_name using checkmate =====
  checkmate::assert_string(function_name, min.chars = 1, 
                           .var.name = "function_name")
  
  # ===== Validate all logical parameters using checkmate =====
  checkmate::assert_flag(verbose, .var.name = "verbose")
  checkmate::assert_flag(show_hidden, .var.name = "show_hidden")
  checkmate::assert_flag(show_progress, .var.name = "show_progress")
  checkmate::assert_flag(use_parallel, .var.name = "use_parallel")
  checkmate::assert_flag(show_hints, .var.name = "show_hints")
  checkmate::assert_flag(benchmark, .var.name = "benchmark")
  
  # ===== Validate benchmark parameters =====
  checkmate::assert_integerish(benchmark_runs, lower = 1, len = 1, 
                                .var.name = "benchmark_runs")
  benchmark_runs <- as.integer(benchmark_runs)
  
  checkmate::assert_number(benchmark_bonus, lower = 0, upper = 100,
                           .var.name = "benchmark_bonus")

  # ===== Check internet connection =====
  # Better to check now than let download fail with cryptic error
  if (!curl::has_internet()) {
    stop(network_error(
      "No internet connection detected.\nThe autograder requires internet to fetch test cases."
    ))
  }
  
  # User feedback: show we're starting
  cat(sprintf("Loading %s...\n", function_name))
  
  # ==========================================================================
  # SECTION 2: FETCH AND LOAD INSTRUCTOR CODE
  # ==========================================================================
  # Download test cases from repository. All errors handled in helper function.
  
  instructor_env <- fetch_instructor_code(function_name)
  instructor_fun <- extract_instructor_function(instructor_env, function_name)
  test_data <- extract_test_cases(instructor_env, function_name)
  
  # ==========================================================================
  # SECTION 3: VERIFY STUDENT FUNCTION EXISTS
  # ==========================================================================
  # Student must define student_<function_name> in their global environment
  
  student_fun_name <- paste0("student_", function_name)
  
  # Check if function exists and is actually a function
  if (!exists(student_fun_name, envir = .GlobalEnv, mode = "function")) {
    stop(student_function_error(function_name))
  }
  
  # Get the student's function
  student_fun <- get(student_fun_name, envir = .GlobalEnv)
  
  # ==========================================================================
  # SECTION 4: EXTRACT TEST CONFIGURATION
  # ==========================================================================
  # Pull out all test parameters for easy access
  
  n_tests <- length(test_data$inputs)
  descriptions <- test_data$descriptions
  hidden <- test_data$hidden
  points <- test_data$points
  tolerance <- test_data$tolerance
  expected_type <- test_data$expected_type
  hints <- test_data$hints %||% rep("", n_tests)  # Default to empty hints
  comparison_fn <- test_data$comparison_fn
  
  # ==========================================================================
  # SECTION 5: RUN TESTS
  # ==========================================================================
  
  cat("\n=== Running Tests ===\n")
  
  # ===== Optional: Progress Bar =====
  # Show progress bar for longer test runs (>5 tests)
  # Provides feedback that something is happening
  if (show_progress && n_tests > 5) {
    pb <- txtProgressBar(min = 0, max = n_tests, style = 3)
  }
  
  # ===== Execute Tests (Parallel or Sequential) =====
  # Decision logic:
  #   - use_parallel=TRUE AND n_tests≥10 → Parallel
  #   - Otherwise → Sequential
  test_results <- if (use_parallel && n_tests >= 10) {
    cat(sprintf("Using parallel processing (%d cores)...\n", 
                min(parallel::detectCores() - 1, 4)))
    run_tests_parallel(student_fun, instructor_fun, test_data, tolerance, TRUE)
  } else {
    run_tests_sequential(student_fun, instructor_fun, test_data, tolerance)
  }
  
  # ==========================================================================
  # SECTION 6: PROCESS RESULTS AND PROVIDE FEEDBACK
  # ==========================================================================
  # Loop through results, compare outputs, generate feedback
  
  passed <- 0
  failed <- 0
  passed_indices <- integer(0)  # Track which tests passed (for scoring)
  
  # Process each test result
  for (result in test_results) {
    i <- result$index
    student_out <- result$student
    expected_out <- result$expected
    
    # Extract test metadata
    input_args <- test_data$inputs[[i]]
    is_hidden <- hidden[i]
    desc <- descriptions[i]
    test_points <- points[i]
    hint <- hints[i]
    
    # ===== CASE 1: Student Function Threw Error =====
    if (inherits(student_out, "error")) {
      # Display based on hidden status and verbosity
      if (is_hidden && !show_hidden) {
        # Hidden test: minimal output
        cat(sprintf("[Test %d] (%d pt): FAIL\n", i, test_points))
      } else {
        # Visible test or show_hidden=TRUE: detailed output
        cat(sprintf("\n[Test %d] %s (%d pt): FAIL (Error)\n", i, desc, test_points))
        
        if (verbose) {
          cat(sprintf("  Input: %s\n", format_output(input_args)))
          cat(sprintf("  Error: %s\n", student_out$error))
          
          # Show hint if available and enabled
          if (show_hints && nchar(hint) > 0) {
            cat(sprintf("  Hint: %s\n", hint))
          }
        }
      }
      
      failed <- failed + 1
      
      # Update progress bar if shown
      if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
      next  # Skip to next test
    }
    
    # ===== CASE 2: Instructor Function Threw Error =====
    # This shouldn't happen (instructor code should be tested)
    # But handle it gracefully and tell user to contact instructor
    if (inherits(expected_out, "error")) {
      cat(sprintf("\n[Test %d] %s: ERROR (Contact instructor - reference implementation failed)\n", 
                  i, desc))
      failed <- failed + 1
      
      if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
      next
    }
    
    # ===== CASE 3: Type Checking (if specified) =====
    # Verify output type matches expected type
    # Helps catch "returning wrong type" errors early
    if (!is.null(expected_type)) {
      actual_type <- class(student_out)[1]
      
      if (actual_type != expected_type) {
        # Type mismatch found
        if (is_hidden && !show_hidden) {
          cat(sprintf("[Test %d] (%d pt): FAIL\n", i, test_points))
        } else {
          cat(sprintf("\n[Test %d] %s (%d pt): FAIL (Type Error)\n", i, desc, test_points))
          
          if (verbose) {
            cat(sprintf("  Expected type: %s\n", expected_type))
            cat(sprintf("  Got type: %s\n", actual_type))
            
            if (show_hints && nchar(hint) > 0) {
              cat(sprintf("  Hint: %s\n", hint))
            }
          }
        }
        
        failed <- failed + 1
        if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
        next
      }
    }
    
    # ===== CASE 4: Value Comparison =====
    # Compare actual vs expected output
    # Use custom comparison function if provided, otherwise use C++ comparison
    is_identical <- if (!is.null(comparison_fn)) {
      # Custom comparison (for special cases like plots, complex objects)
      comparison_fn(student_out, expected_out)
    } else {
      # Default: fast C++ comparison with tolerance
      .cpp_compare_fast(student_out, expected_out, tolerance)
    }
    
    # ===== DISPLAY RESULT =====
    if (is_identical[1]) {
      # ===== TEST PASSED =====
      if (is_hidden && !show_hidden) {
        cat(sprintf("[Test %d] (%d pt): PASS\n", i, test_points))
      } else {
        cat(sprintf("[Test %d] %s (%d pt): PASS\n", i, desc, test_points))
      }
      
      passed <- passed + 1
      passed_indices <- c(passed_indices, i)
      
    } else {
      # ===== TEST FAILED =====
      if (is_hidden && !show_hidden) {
        cat(sprintf("[Test %d] (%d pt): FAIL\n", i, test_points))
      } else {
        cat(sprintf("\n[Test %d] %s (%d pt): FAIL\n", i, desc, test_points))
        
        if (verbose) {
          # Show detailed comparison
          cat(sprintf("  Input:    %s\n", format_output(input_args)))
          cat(sprintf("  Expected: %s\n", format_output(expected_out)))
          cat(sprintf("  Got:      %s\n", format_output(student_out)))
          
          # Generate and display feedback if hints enabled
          if (show_hints) {
            feedback <- provide_feedback(student_out, expected_out, input_args, hint)
            print_feedback(feedback)
          }
        }
      }
      
      failed <- failed + 1
    }
    
    # Update progress bar
    if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
  }
  
  # Close progress bar if it was shown
  if (show_progress && n_tests > 5) {
    close(pb)
    cat("\n")
  }
  
  # ==========================================================================
  # SECTION 7: DISPLAY SUMMARY AND CALCULATE FINAL SCORE
  # ==========================================================================
  
  cat("\n=== Summary ===\n")
  
  # Calculate metrics
  total_points <- sum(points)
  earned_points <- sum(points[passed_indices])
  pass_rate <- (passed / n_tests) * 100
  
  # Display score (points and percentage)
  cat(sprintf("Score: %d/%d points (%.1f%%)\n", 
              earned_points, total_points, 
              (earned_points/total_points) * 100))
  
  # Display pass rate
  cat(sprintf("Tests: %d/%d passed (%.1f%%)\n", passed, n_tests, pass_rate))
  
  # ===== Contextual Feedback Based on Performance =====
  # Different messages for different performance levels
  # Encourages students and provides guidance
  
  if (passed == n_tests) {
    # ===== PERFECT SCORE =====
    cat("\n\u2713 ALL TESTS PASSED! Excellent work!\n")
    
  } else if (passed > 0) {
    # ===== PARTIAL SUCCESS =====
    cat(sprintf("\n\u2717 %d/%d tests failed. Review the output above.\n", failed, n_tests))
    
    # Tailored encouragement based on how close they are
    if (failed <= 3) {
      # Very close - just a few issues
      cat("  You're close! Focus on the failing test cases.\n")
    } else if (pass_rate >= 50) {
      # Halfway there - making progress
      cat("  You're making progress! Review your logic for edge cases.\n")
    } else {
      # Less than half - needs more work
      cat("  Review the expected behavior and test your function with simple inputs first.\n")
    }
    
  } else {
    # ===== NO TESTS PASSED =====
    cat("\n\u2717 All tests failed. Check your implementation carefully.\n")
    cat("  Suggestions:\n")
    cat("    * Test your function manually with simple inputs\n")
    cat("    * Review the function requirements\n")
    cat("    * Check for syntax errors or typos\n")
  }
  
  # ==========================================================================
  # SECTION 8: PERFORMANCE BENCHMARK (Optional)
  # ==========================================================================
  # Run performance comparison if requested and all tests passed
  
  benchmark_result <- NULL
  benchmark_bonus_earned <- 0
  
  if (benchmark) {
    cat("\n=== Performance Benchmark ===\n")
    
    if (passed != n_tests) {
      # Can't benchmark if tests failed
      cat("Skipped: All tests must pass before benchmarking.\n")
      cat("Fix the failing tests first, then run again with benchmark = TRUE.\n")
    } else {
      # Run benchmark
      cat(sprintf("Running %d benchmark iterations...\n", benchmark_runs))
      
      # Get test inputs for benchmarking (use visible tests only)
      visible_indices <- which(!hidden)
      bench_inputs <- if (length(visible_indices) > 0) {
        test_data$inputs[visible_indices]
      } else {
        test_data$inputs
      }
      
      # Warmup phase
      for (w in seq_len(5)) {
        for (input in bench_inputs) {
          suppressWarnings(do.call(student_fun, input))
          suppressWarnings(do.call(instructor_fun, input))
        }
      }
      
      # Wrap functions for quiet benchmarking
      student_fn_quiet <- function(...) suppressWarnings(student_fun(...))
      instructor_fn_quiet <- function(...) suppressWarnings(instructor_fun(...))
      
      # Run benchmark using C++ high-precision timing
      student_times <- .cpp_benchmark(student_fn_quiet, bench_inputs, benchmark_runs)
      instructor_times <- .cpp_benchmark(instructor_fn_quiet, bench_inputs, benchmark_runs)
      
      # Compute statistics
      student_median <- median(student_times)
      instructor_median <- median(instructor_times)
      ratio <- student_median / instructor_median
      
      # Determine verdict
      verdict <- if (ratio < 0.9) {
        "FASTER"
      } else if (ratio <= 1.1) {
        "COMPARABLE"
      } else if (ratio <= 2.0) {
        "SLOWER"
      } else if (ratio <= 5.0) {
        "MUCH SLOWER"
      } else {
        "VERY SLOW"
      }
      
      # Store benchmark results
      benchmark_result <- list(
        student_median = student_median,
        instructor_median = instructor_median,
        ratio = ratio,
        verdict = verdict,
        n_runs = benchmark_runs,
        student_times = student_times,
        instructor_times = instructor_times
      )
      
      # Display benchmark results
      cat(sprintf("\nYour time:       %.6f seconds (median)\n", student_median))
      cat(sprintf("Reference time:  %.6f seconds (median)\n", instructor_median))
      cat(sprintf("Performance:     %.2fx (%s)\n", ratio, verdict))
      
      # Calculate bonus points if applicable
      if (benchmark_bonus > 0 && ratio < 1.0) {
        # Bonus scales with how much faster the student is
        # ratio=0.5 (2x faster) -> 100% of bonus
        # ratio=0.9 (10% faster) -> ~10% of bonus
        bonus_fraction <- min(1.0, (1.0 - ratio) / 0.5)
        benchmark_bonus_earned <- round(total_points * (benchmark_bonus / 100) * bonus_fraction, 1)
        
        cat(sprintf("\n\342\230\205 PERFORMANCE BONUS: +%.1f points (%.1f%% of max %.1f%%)\n", 
                    benchmark_bonus_earned, 
                    bonus_fraction * benchmark_bonus,
                    benchmark_bonus))
      } else if (benchmark_bonus > 0) {
        cat(sprintf("\nNo bonus earned. Implement a faster solution to earn up to %.1f%% bonus.\n",
                    benchmark_bonus))
      }
    }
  }
  
  # ==========================================================================
  # SECTION 9: RETURN STRUCTURED RESULTS
  # ==========================================================================
  # Return an S3 object for nice printing and further processing
  
  # Calculate final score including bonus
  final_score <- earned_points + benchmark_bonus_earned
  final_max <- total_points + (if (benchmark_bonus > 0) total_points * benchmark_bonus / 100 else 0)
  
  # Display final score if bonus was applied
  if (benchmark_bonus_earned > 0) {
    cat(sprintf("\n=== Final Score: %.1f/%.1f points (%.1f%%) ===\n",
                final_score, final_max, (final_score / final_max) * 100))
  }
  
  invisible(new_autograder_result(
    passed = passed,
    failed = failed,
    total = n_tests,
    score = final_score,
    max_score = final_max,
    function_name = function_name,
    details = list(
      base_score = earned_points,
      base_max = total_points,
      benchmark_bonus = benchmark_bonus_earned,
      benchmark = benchmark_result
    )
  ))
}
