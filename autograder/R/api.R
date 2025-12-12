# ============================================================================
# AUTOGRADER PACKAGE - PUBLIC API FUNCTIONS
# ============================================================================
#
# File: api.R
# Purpose: User-facing discovery and preview functions
#
# Functions:
#   - preview_tests(): Preview test cases before implementing
#   - list_problems(): Show available problems
#
# These are "entry point" functions students use to explore assignments.
#
# ============================================================================

# ============================================================================
# PREVIEW TESTS FUNCTION
# ============================================================================
#
# Purpose: Allow students to see test cases before attempting implementation
#
# Benefits:
#   - Understand requirements before coding
#   - Plan implementation strategy
#   - Identify edge cases to handle
#   - No surprises during testing
#
# Privacy:
#   - Hidden tests show only description and points
#   - Hidden test inputs and outputs are not revealed
#   - Maintains assessment integrity
#
# ============================================================================

#' Preview test cases without running them
#'
#' @description
#' Shows test case inputs, descriptions, and point values without executing
#' any code. Hidden tests show only summary information to maintain
#' assessment integrity.
#' 
#' Use Case:
#'   Students can review test cases to understand requirements before
#'   implementing their solution.
#'
#' @param function_name Character. Name of the function to preview.
#' 
#' @return Invisibly returns the test_data structure (for programmatic access)
#'
#' @section Display Format:
#' ```
#' === Test Cases Preview ===
#' 
#' [Test 1] Base case: n = 1 (1 pt)
#'   Input: list(1)
#' 
#' [Test 2] Small input: n = 5 (2 pt)
#'   Input: list(5)
#' 
#' [Test 3] [HIDDEN TEST] (2 pt)
#' 
#' === Summary ===
#' Total tests: 3
#' Visible tests: 2
#' Hidden tests: 1
#' Total points: 5
#' ```
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # See what tests exist for fibonacci
#' preview_tests("fibonacci")
#' 
#' # Capture test data for analysis
#' test_data <- preview_tests("fibonacci")
#' print(test_data$points)  # See point distribution
#' }
#'
preview_tests <- function(function_name) {
  
  # ===== VALIDATE INPUT =====
  if (missing(function_name) || !is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string", call. = FALSE)
  }
  
  cat(sprintf("Loading %s...\n", function_name))
  
  # ===== FETCH TEST CASES =====
  # Use shared fetch function (same as autograder)
  instructor_env <- fetch_instructor_code(function_name)
  test_data <- extract_test_cases(instructor_env, function_name)
  
  # ===== EXTRACT TEST METADATA =====
  n_tests <- length(test_data$inputs)
  descriptions <- test_data$descriptions
  hidden <- test_data$hidden
  points <- test_data$points
  
  cat("\n=== Test Cases Preview ===\n\n")
  
  # ===== DISPLAY TESTS (Vectorized approach) =====
  # Pre-calculate counts using vectorized operations
  visible_count <- sum(!hidden)
  hidden_count <- sum(hidden)
  
  # Process visible tests
  visible_idx <- which(!hidden)
  invisible(lapply(visible_idx, function(i) {
    cat(sprintf("[Test %d] %s (%d pt)\n", i, descriptions[i], points[i]))
    cat(sprintf("  Input: %s\n\n", format_output(test_data$inputs[[i]])))
  }))
  
  # Process hidden tests
  hidden_idx <- which(hidden)
  if (length(hidden_idx) > 0) {
    cat(sprintf("[Test %d] [HIDDEN TEST] (%d pt)\n\n", hidden_idx, points[hidden_idx]), sep = "")
  }
  
  # ===== DISPLAY SUMMARY =====
  cat("=== Summary ===\n")
  cat(sprintf("Total tests: %d\n", n_tests))
  cat(sprintf("Visible tests: %d\n", visible_count))
  cat(sprintf("Hidden tests: %d\n", hidden_count))
  cat(sprintf("Total points: %d\n", sum(points)))
  
  # Explain why hidden tests are hidden
  cat("\nNote: Hidden test details are not shown to maintain assessment integrity.\n")
  
  # Return test data invisibly for programmatic access
  invisible(test_data)
}

# ============================================================================
# LIST PROBLEMS FUNCTION
# ============================================================================
#
# Purpose: Discovery - show students what functions are available to test
#
# Implementation:
#   1. Try to fetch from repository (_problems.R file)
#   2. If fetch fails, use hardcoded fallback list
#   3. Display in user-friendly format
#   4. Show usage instructions
#
# Graceful Degradation:
#   Even if repository is unavailable, students can still see basic problems
#
# ============================================================================

#' List available problems/functions to test
#'
#' @description
#' Displays all functions available for testing. Shows usage instructions
#' and how to preview test cases.
#' 
#' Problem Discovery:
#'   1. Attempts to fetch from repository (_problems.R)
#'   2. Falls back to default list if fetch fails
#'   3. Returns problems invisibly for programmatic access
#'
#' @return Invisibly returns character vector of problem names
#'
#' @section Display Format:
#' ```
#' Available problems:
#' 
#'   - fibonacci
#'   - factorial
#'   - sum_vector
#' 
#' Usage:
#'   student_<function_name> <- function(...) { ... }
#'   autograder('<function_name>')
#' 
#' Preview tests:
#'   preview_tests('<function_name>')
#' ```
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # See available functions
#' list_problems()
#' 
#' # Capture list programmatically
#' problems <- list_problems()
#' print(problems)  # c("fibonacci", "factorial", ...)
#' }
#'
list_problems <- function() {
  cat("Available problems:\n\n")
  
  # ===== TRY TO FETCH PROBLEMS FROM REPOSITORY =====
  # Use memoised version for better performance
  # If this fails, we have a fallback list
  problems <- tryCatch({
    # Get memoised fetcher (falls back to direct call if not available)
    fetch_fn <- .get_memoised_fetch_problems()
    code <- fetch_fn()
    
    if (length(code) > 0) {
      # Parse and evaluate _problems.R
      env <- new.env()
      eval(parse(text = code), envir = env)
      
      # Extract problems vector
      get("problems", envir = env)
    } else {
      # Empty return = fetch failed, use fallback
      c("fibonacci", "factorial", "sum_vector")
    }
  }, error = function(e) {
    # Any error: use fallback list
    # This ensures function always returns something useful
    c("fibonacci", "factorial", "sum_vector")
  })
  
  # ===== DISPLAY PROBLEMS =====
  for (prob in problems) {
    cat(sprintf("  - %s\n", prob))
  }
  
  # ===== DISPLAY USAGE INSTRUCTIONS =====
  cat("\nUsage:\n")
  cat("  student_<function_name> <- function(...) { ... }\n")
  cat("  autograder('<function_name>')\n")
  
  cat("\nPreview tests:\n")
  cat("  preview_tests('<function_name>')\n")
  
  # Return invisibly for programmatic use
  invisible(problems)
}
