# ============================================================================
# AUTOGRADER PACKAGE - TEST CASE VALIDATION
# ============================================================================
#
# File: validation.R
# Purpose: Validate and normalize test case structures
#
# ============================================================================

#' Validate and normalize test case structure
#' 
#' @description
#' Performs comprehensive validation of test case data structure with
#' strict error checking. Ensures all test components are properly formatted
#' and consistent.
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
  # Without inputs, we can't run any tests - CRITICAL ERROR
  if (!("inputs" %in% names(test_data))) {
    stop(sprintf(
      "CRITICAL: Test cases for '%s' are missing required 'inputs' field.\nContact instructor to fix test configuration.",
      function_name
    ), call. = FALSE)
  }
  
  # Validate inputs is a non-empty list
  if (!is.list(test_data$inputs) || length(test_data$inputs) == 0) {
    stop(sprintf(
      "CRITICAL: Test cases for '%s' must have at least one test.\nContact instructor to fix test configuration.",
      function_name
    ), call. = FALSE)
  }
  
  # Number of tests determines expected length of all other fields
  n_tests <- length(test_data$inputs)
  
  # ===== OPTIONAL FIELD: descriptions =====
  # Descriptions help students understand what each test checks
  if (!is.null(test_data$descriptions)) {
    # If provided, length must match inputs
    if (length(test_data$descriptions) != n_tests) {
      stop(sprintf(
        "CRITICAL: Test case descriptions length (%d) doesn't match inputs length (%d).\nContact instructor to fix test configuration.",
        length(test_data$descriptions), n_tests
      ), call. = FALSE)
    }
  } else {
    # Default: "Test 1", "Test 2", etc.
    test_data$descriptions <- paste0("Test ", seq_len(n_tests))
  }
  
  # ===== OPTIONAL FIELD: hidden =====
  # Hidden tests prevent students from reverse-engineering all cases
  if (!is.null(test_data$hidden)) {
    # Validate length
    if (length(test_data$hidden) != n_tests) {
      stop(sprintf(
        "CRITICAL: 'hidden' field length (%d) doesn't match inputs length (%d).\nContact instructor.",
        length(test_data$hidden), n_tests
      ), call. = FALSE)
    }
    # Validate type
    if (!is.logical(test_data$hidden)) {
      stop("CRITICAL: 'hidden' field must be logical (TRUE/FALSE). Contact instructor.", 
           call. = FALSE)
    }
  } else {
    # Default: all tests visible
    test_data$hidden <- rep(FALSE, n_tests)
  }
  
  # ===== OPTIONAL FIELD: points =====
  # Points allow weighted grading (some tests worth more than others)
  if (!is.null(test_data$points)) {
    # Validate length
    if (length(test_data$points) != n_tests) {
      stop(sprintf(
        "CRITICAL: 'points' field length (%d) doesn't match inputs length (%d).\nContact instructor.",
        length(test_data$points), n_tests
      ), call. = FALSE)
    }
    # Validate type and values
    if (!is.numeric(test_data$points) || any(test_data$points < 0)) {
      stop("CRITICAL: 'points' must be non-negative numeric values. Contact instructor.", 
           call. = FALSE)
    }
  } else {
    # Default: all tests worth 1 point
    test_data$points <- rep(1, n_tests)
  }
  
  # ===== OPTIONAL FIELD: tolerance =====
  # Tolerance for floating-point comparison (handles precision issues)
  if (!is.null(test_data$tolerance)) {
    # Must be single non-negative number
    if (!is.numeric(test_data$tolerance) || length(test_data$tolerance) != 1 || 
        test_data$tolerance < 0) {
      stop("CRITICAL: 'tolerance' must be a single non-negative numeric value. Contact instructor.", 
           call. = FALSE)
    }
  } else {
    # Default: 1e-10 (suitable for most numerical work)
    test_data$tolerance <- 1e-10
  }
  
  # ===== OPTIONAL FIELD: expected_type =====
  # Type checking helps catch common student errors early
  if (!is.null(test_data$expected_type)) {
    # Must be single character string
    if (!is.character(test_data$expected_type) || length(test_data$expected_type) != 1) {
      warning("Test case 'expected_type' must be a single character value. Ignoring.")
      test_data$expected_type <- NULL
    }
  }
  
  # ===== OPTIONAL FIELD: hints =====
  # Hints provide guidance without giving away the answer
  if (!is.null(test_data$hints)) {
    if (length(test_data$hints) != n_tests) {
      warning("Test case 'hints' length doesn't match. Ignoring hints.")
      test_data$hints <- NULL
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
