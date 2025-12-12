# ============================================================================
# AUTOGRADER PACKAGE - TEST CASE VALIDATION
# ============================================================================
#
# File: validation.R
# Purpose: Validate and normalize test case structures
#
# Note: Pure R implementation for maintainability. List operations are 
# R's native strength, so there's no performance benefit from C++ here.
#
# ============================================================================

#' Validate and normalize test case structure
#' 
#' @description
#' Performs comprehensive validation of test case data structure with
#' strict error checking. Ensures all test components are properly 
#' formatted and consistent.
#' 
#' Required Fields:
#'   - **inputs**: list of input argument lists (REQUIRED)
#' 
#' Optional Fields (with defaults):
#'   - **descriptions**: character vector of test descriptions
#'   - **hidden**: logical vector indicating hidden tests
#'   - **points**: numeric vector of point values per test
#'   - **tolerance**: single numeric for floating-point comparison
#'   - **expected_type**: single character specifying expected output type
#'   - **hints**: character vector of hints for failed tests
#'   - **comparison_fn**: custom comparison function
#' 
#' @param test_data List containing test case configuration
#' @param function_name Name of function (for error messages)
#' 
#' @return Validated and normalized test_data with all fields present
#' 
#' @details
#' Validation Strategy:
#' 
#' **Critical Fields (STOP on error):**
#'   - Missing inputs: Hard error (can't proceed)
#'   - Mismatched lengths: Hard error (data corruption)
#'   - Invalid types: Hard error (wrong configuration)
#' 
#' **Optional Fields (WARN and fix):**
#'   - Missing: Fill with defaults
#'   - Wrong format: Warn and ignore
#'   - Can proceed: Yes
#' 
#' Error Messages:
#'   - Clear indication of what's wrong
#'   - Tells user to contact instructor (config issue)
#'   - Never blames the student for instructor errors
#' 
#' @examples
#' \dontrun{
#' # Valid test case
#' test_data <- list(
#'   inputs = list(list(1), list(2)),
#'   descriptions = c("Test 1", "Test 2"),
#'   points = c(1, 2)
#' )
#' validated <- validate_test_cases(test_data, "my_function")
#' 
#' # Missing optional fields - filled with defaults
#' test_data_minimal <- list(
#'   inputs = list(list(1))
#' )
#' validated <- validate_test_cases(test_data_minimal, "my_function")
#' # Returns: descriptions = "Test 1", hidden = FALSE, points = 1, tolerance = 1e-10
#' }
#' 
#' @keywords internal
validate_test_cases <- function(test_data, function_name) {
  
  # ===== REQUIRED FIELD: inputs =====
  # NULL is treated as missing, but empty list is a separate error
  if (is.null(test_data$inputs) && !"inputs" %in% names(test_data)) {
    stop(
      sprintf("CRITICAL: Test cases for '%s' are missing required 'inputs' field.\nContact instructor to fix test configuration.", function_name),
      call. = FALSE
    )
  }
  
  if (is.null(test_data$inputs) || !is.list(test_data$inputs) || length(test_data$inputs) == 0L) {
    stop(
      sprintf("CRITICAL: Test cases for '%s' must have at least one test.\nContact instructor to fix test configuration.", function_name),
      call. = FALSE
    )
  }
  
  n_tests <- length(test_data$inputs)
  
  # ===== OPTIONAL FIELD: descriptions =====
  if (!is.null(test_data$descriptions)) {
    if (is.character(test_data$descriptions)) {
      if (length(test_data$descriptions) != n_tests) {
        stop(
          sprintf("CRITICAL: Test case descriptions length (%d) doesn't match inputs length (%d).\nContact instructor to fix test configuration.",
                  length(test_data$descriptions), n_tests),
          call. = FALSE
        )
      }
    }
  } else {
    # Generate default descriptions
    test_data$descriptions <- paste("Test", seq_len(n_tests))
  }
  
  # ===== OPTIONAL FIELD: hidden =====
  if (!is.null(test_data$hidden)) {
    if (is.logical(test_data$hidden)) {
      if (length(test_data$hidden) != n_tests) {
        stop(
          sprintf("CRITICAL: 'hidden' field length (%d) doesn't match inputs length (%d).\nContact instructor.",
                  length(test_data$hidden), n_tests),
          call. = FALSE
        )
      }
    } else {
      stop("CRITICAL: 'hidden' field must be logical (TRUE/FALSE). Contact instructor.", call. = FALSE)
    }
  } else {
    # Default: all tests visible
    test_data$hidden <- rep(FALSE, n_tests)
  }
  
  # ===== OPTIONAL FIELD: points =====
  if (!is.null(test_data$points)) {
    if (!is.numeric(test_data$points)) {
      stop("CRITICAL: 'points' must be non-negative numeric values. Contact instructor.", call. = FALSE)
    }
    if (length(test_data$points) != n_tests) {
      stop(
        sprintf("CRITICAL: 'points' field length (%d) doesn't match inputs length (%d).\nContact instructor.",
                length(test_data$points), n_tests),
        call. = FALSE
      )
    }
    if (any(test_data$points < 0)) {
      stop("CRITICAL: 'points' must be non-negative numeric values. Contact instructor.", call. = FALSE)
    }
  } else {
    # Default: all tests worth 1 point
    test_data$points <- rep(1, n_tests)
  }
  
  # ===== OPTIONAL FIELD: tolerance =====
  if (!is.null(test_data$tolerance)) {
    if (!is.numeric(test_data$tolerance) || length(test_data$tolerance) != 1L || test_data$tolerance < 0) {
      stop("CRITICAL: 'tolerance' must be a single non-negative numeric value. Contact instructor.", call. = FALSE)
    }
  } else {
    # Default tolerance
    test_data$tolerance <- 1e-10
  }
  
  # ===== OPTIONAL FIELD: hints =====
  if (!is.null(test_data$hints)) {
    if (is.character(test_data$hints) && length(test_data$hints) != n_tests) {
      # Length mismatch - remove hints with warning
      warning(sprintf("'hints' length (%d) doesn't match number of tests (%d). Ignoring hints.",
                      length(test_data$hints), n_tests))
      test_data$hints <- NULL
    }
  }
  
  # ===== OPTIONAL FIELD: expected_type =====
  if (!is.null(test_data$expected_type)) {
    # Must be single character string
    if (!is.character(test_data$expected_type) || length(test_data$expected_type) != 1L) {
      warning("Test case 'expected_type' must be a single character value. Ignoring.")
      test_data$expected_type <- NULL
    }
  }
  
  # ===== OPTIONAL FIELD: comparison_fn =====
  # Custom comparison for special cases (e.g., comparing plots, custom objects)
  if (!is.null(test_data$comparison_fn)) {
    if (!is.function(test_data$comparison_fn)) {
      warning("'comparison_fn' must be a function. Using default comparison.")
      test_data$comparison_fn <- NULL
    }
  }
  
  # Return validated and normalized test data
  test_data
}

#' Quick validation check for test case inputs (pure R)
#' 
#' @param test_data List containing test case configuration
#' @return TRUE if basic structure is valid
#' @keywords internal
quick_validate_inputs <- function(test_data) {
  !is.null(test_data$inputs) && 
    is.list(test_data$inputs) && 
    length(test_data$inputs) > 0L
}

#' Get test count from test data (pure R)
#' 
#' @param test_data List containing test case configuration
#' @return Number of tests, or -1L if invalid
#' @keywords internal
get_test_count <- function(test_data) {
  if (is.null(test_data$inputs) || !is.list(test_data$inputs)) {
    return(-1L)
  }
  length(test_data$inputs)
}

#' Validate field lengths match test count (pure R)
#' 
#' @param test_data List containing test case configuration
#' @param n_tests Expected number of tests
#' @return Named logical vector with validation results for each field
#' @keywords internal
validate_field_lengths <- function(test_data, n_tests) {
  c(
    descriptions = is.null(test_data$descriptions) || length(test_data$descriptions) == n_tests,
    hidden = is.null(test_data$hidden) || length(test_data$hidden) == n_tests,
    points = is.null(test_data$points) || length(test_data$points) == n_tests,
    hints = is.null(test_data$hints) || length(test_data$hints) == n_tests
  )
}
