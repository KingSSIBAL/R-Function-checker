# ============================================================================
# AUTOGRADER PACKAGE - MAIN R MODULE
# ============================================================================
#
# File: autograder.R
# Purpose: Automated grading system for R programming assignments
#
# This module provides the R-side interface for:
#   1. Function testing and grading
#   2. Test case validation
#   3. Student feedback generation
#   4. Progress reporting
#   5. Parallel test execution
#
# Architecture Overview:
#   ┌─────────────┐
#   │   Student   │
#   └──────┬──────┘
#          │ autograder("fibonacci")
#          ▼
#   ┌─────────────────────────────┐
#   │  Input Validation (R)       │ ← Check parameters, existence
#   └──────────┬──────────────────┘
#              │
#              ▼
#   ┌─────────────────────────────┐
#   │  Fetch Tests (C++)          │ ← Secure download, sanitization
#   └──────────┬──────────────────┘
#              │
#              ▼
#   ┌─────────────────────────────┐
#   │  Run Tests (R/C++)          │ ← Sequential or parallel
#   └──────────┬──────────────────┘
#              │
#              ▼
#   ┌─────────────────────────────┐
#   │  Compare (C++)              │ ← Fast comparison, tolerance
#   └──────────┬──────────────────┘
#              │
#              ▼
#   ┌─────────────────────────────┐
#   │  Feedback (R)               │ ← Analyze, format, display
#   └──────────┬──────────────────┘
#              │
#              ▼
#   ┌─────────────────────────────┐
#   │  Results                     │ ← Score, pass rate, details
#   └─────────────────────────────┘
#
# Design Principles:
#   - Fail gracefully with actionable error messages
#   - Never expose internal implementation details to students
#   - Performance: optimize for common cases (numeric vectors)
#   - Usability: provide helpful feedback to guide learning
#   - Security: validate all inputs, sanitize all operations
#
# Author: Reijel Agub (rcagub@up.edu.ph)
# Version: 0.3.0
# License: MIT
# Repository: https://github.com/KingSSIBAL/R-Function-checker
#
# ============================================================================

#' @useDynLib autograder, .registration=TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster
#' @importFrom utils head tail txtProgressBar setTxtProgressBar
NULL


# ============================================================================
# CUSTOM ERROR CLASSES
# ============================================================================
#
# Custom error classes provide:
#   1. Better error messages (context-specific)
#   2. Type-safe error handling (catch specific errors)
#   3. Structured error information (fields for debugging)
#   4. Clearer user feedback (vs generic errors)
#
# Error Hierarchy:
#   condition (base R)
#     └── error
#           ├── network_error
#           ├── function_not_found_error
#           └── test_execution_error
#
# Usage Pattern:
#   tryCatch(
#     risky_operation(),
#     network_error = function(e) { handle_network_issue(e) },
#     error = function(e) { handle_generic_error(e) }
#   )
#
# ============================================================================

#' Custom error for network-related issues
#' 
#' @description
#' Creates a structured error object for network failures. This allows
#' calling code to handle network issues specifically, separate from
#' other types of errors.
#' 
#' @param message Human-readable error message describing the network issue
#' @param call The call that generated the error (usually NULL or sys.call())
#' 
#' @return An S3 object of class c("network_error", "error", "condition")
#' 
#' @details
#' Common network issues this error represents:
#'   - No internet connection
#'   - Server unreachable
#'   - Timeout (>30 seconds)
#'   - DNS resolution failure
#'   - Firewall blocking connection
#' 
#' @examples
#' \dontrun{
#' if (!curl::has_internet()) {
#'   stop(network_error("No internet connection detected"))
#' }
#' }
#' 
#' @keywords internal
network_error <- function(message, call = NULL) {
  structure(
    list(message = message, call = call),
    class = c("network_error", "error", "condition")
  )
}

#' Custom error for missing functions
#' 
#' @description
#' Creates a structured error when a requested function doesn't exist in
#' the repository. Provides helpful guidance on what to do next.
#' 
#' @param function_name Name of the function that wasn't found
#' @param call The call that generated the error (usually NULL)
#' 
#' @return An S3 object of class c("function_not_found_error", "error", "condition")
#' 
#' @details
#' This error includes:
#'   - Function name that was requested
#'   - Suggestion to use list_problems()
#'   - Common troubleshooting steps
#' 
#' User Experience:
#'   Instead of: "Error: HTTP 404"
#'   Students see: "Function 'fibonaci' not found. Use list_problems() to see available functions."
#' 
#' @keywords internal
function_not_found_error <- function(function_name, call = NULL) {
  message <- sprintf(
    "Function '%s' not found.\nUse list_problems() to see available functions.\n\nCommon issues:\n  * Check spelling\n  * Ensure the function exists in the repository\n  * Verify your internet connection",
    function_name
  )
  structure(
    list(message = message, call = call, function_name = function_name),
    class = c("function_not_found_error", "error", "condition")
  )
}

#' Custom error for test execution failures
#' 
#' @description
#' Creates a structured error when a specific test case fails to execute.
#' Includes test number for precise debugging.
#' 
#' @param message Description of what went wrong
#' @param test_number Which test failed (1-based index)
#' @param call The call that generated the error
#' 
#' @return An S3 object of class c("test_execution_error", "error", "condition")
#' 
#' @details
#' Used when:
#'   - Test setup fails
#'   - Input arguments are invalid
#'   - Unexpected runtime error in test harness
#' 
#' @keywords internal
test_execution_error <- function(message, test_number, call = NULL) {
  structure(
    list(message = message, call = call, test_number = test_number),
    class = c("test_execution_error", "error", "condition")
  )
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================
#
# These utility functions reduce code duplication and improve readability.
# Each function has a single, well-defined purpose.
#
# Benefits:
#   - DRY principle (Don't Repeat Yourself)
#   - Easier testing (test helpers independently)
#   - Better maintainability (change in one place)
#   - Clearer intent (named functions vs inline logic)
#
# ============================================================================

#' NULL coalescing operator
#' 
#' @description
#' Returns the first non-NULL value. Similar to `??` in other languages.
#' Useful for providing default values.
#' 
#' @param x Primary value to use
#' @param y Fallback value if x is NULL
#' 
#' @return x if x is not NULL, otherwise y
#' 
#' @examples
#' NULL %||% "default"    # Returns: "default"
#' "value" %||% "default" # Returns: "value"
#' 0 %||% "default"       # Returns: 0 (not NULL)
#' FALSE %||% "default"   # Returns: FALSE (not NULL)
#' 
#' @usage x %||% y
#' 
#' @noRd
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Intelligent output formatting with smart truncation
#' 
#' @description
#' Formats R objects for display with type-aware strategies to prevent
#' overwhelming output while preserving essential information.
#' 
#' Design Goals:
#'   - Show enough information to understand the object
#'   - Prevent screen overflow for large objects
#'   - Preserve structure for small objects
#'   - Handle all common R types gracefully
#' 
#' @param obj The R object to format (any type)
#' @param max_length Maximum characters in output (default: 200)
#' @param preserve_structure Keep full structure for lists? (default: TRUE)
#' 
#' @return Character string representation, never exceeds max_length + 3
#' 
#' @details
#' Formatting Strategy by Type:
#' 
#' **NULL:**
#'   Returns: "NULL"
#' 
#' **Lists (small, ≤3 elements):**
#'   Shows: Full structure
#'   Example: list(a = 1, b = 2, c = 3)
#' 
#' **Lists (large, >3 elements):**
#'   Shows: list(length=100, first 3: ..., ...)
#' 
#' **Vectors (≤10 elements):**
#'   Shows: Full deparsed representation
#'   Example: c(1, 2, 3, 4, 5)
#' 
#' **Vectors (>10 elements):**
#'   Shows: type\[1:n\] = first, second, third ... type\[n\] = last
#'   Example: integer\[1:100\] = 1, 2, 3 ... integer\[100\] = 100
#' 
#' **Matrices:**
#'   Shows: matrix\[rows x cols\]: first_values ...
#'   Example: matrix\[3x4\]: 1 2 3 ...
#' 
#' **Data Frames:**
#'   Shows: data.frame\[rows x cols\] columns: col1, col2, col3
#'   Example: data.frame\[10 x 3\] columns: x, y, z
#' 
#' @section Performance:
#'   Time: O(1) for most types (uses head/tail)
#'   Space: O(k) where k = min(object_size, max_length)
#' 
#' @examples
#' \dontrun{
#' # Small vector - full output
#' format_output(1:5)
#' # "1:5"
#' 
#' # Large vector - truncated
#' format_output(1:1000)
#' # "integer[1:1000] = 1, 2, 3 ... integer[1000] = 1000"
#' 
#' # Data frame - summary
#' format_output(mtcars)
#' # "data.frame[32 x 11] columns: mpg, cyl, disp, hp, ..."
#' 
#' # Matrix - dimensions
#' format_output(matrix(1:100, 10, 10))
#' # "matrix[10x10]: 1 2 3 ..."
#' }
#' 
#' @keywords internal
format_output <- function(obj, max_length = 200, preserve_structure = TRUE) {
  # Handle NULL specially
  if (is.null(obj)) return("NULL")
  
  # ===== LISTS: Context-dependent formatting =====
  if (is.list(obj) && preserve_structure) {
    if (length(obj) == 0) {
      return("list()")  # Empty list
    }
    
    if (length(obj) <= 3) {
      # Small lists: show full structure
      output <- deparse(obj, width.cutoff = max_length)
      output <- paste(output, collapse = " ")
    } else {
      # Large lists: show summary with first 3 elements
      output <- sprintf("list(length=%d, first 3: %s, ...)", 
                       length(obj),
                       paste(deparse(obj[1:3]), collapse = " "))
    }
    
  # ===== VECTORS: Length-dependent formatting =====
  } else if (is.vector(obj) && length(obj) > 10) {
    # Long vectors: show head ... tail pattern
    # This gives context without overwhelming output
    output <- sprintf("%s[1:%d] = %s ... %s[%d] = %s",
                     class(obj)[1],          # Type (integer, numeric, character)
                     length(obj),            # Total length
                     paste(head(obj, 3), collapse = ", "),  # First 3 elements
                     class(obj)[1],
                     length(obj),
                     tail(obj, 1))           # Last element
    
  # ===== MATRICES: Show dimensions and sample =====
  } else if (is.matrix(obj)) {
    # Extract first few elements from first column
    sample_size <- min(3, nrow(obj))
    sample <- as.vector(obj[1:sample_size, 1])
    
    output <- sprintf("matrix[%dx%d]: %s ...",
                     nrow(obj),
                     ncol(obj),
                     paste(sample, collapse = ", "))
    
  # ===== DATA FRAMES: Show structure summary =====
  } else if (is.data.frame(obj)) {
    # Data frames are common in data analysis courses
    # Show dimensions and column names (most useful info)
    output <- sprintf("data.frame[%d x %d] columns: %s",
                     nrow(obj),
                     ncol(obj),
                     paste(names(obj), collapse = ", "))
    
  # ===== DEFAULT: Use deparse =====
  } else {
    # For other types (factors, dates, small vectors, etc.)
    # Use R's deparse which handles most types well
    output <- paste(deparse(obj, width.cutoff = max_length), collapse = " ")
  }
  
  # ===== FINAL TRUNCATION: Ensure max_length is respected =====
  # Safety check: if output is still too long, truncate
  # This can happen with complex nested structures
  if (nchar(output) > max_length) {
    output <- paste0(substr(output, 1, max_length - 3), "...")
  }
  
  output
}

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

# ============================================================================
# SHARED FETCH FUNCTION (Reduces code duplication)
# ============================================================================
#
# This function is used by both autograder() and preview_tests() to fetch
# instructor code. Centralizing this logic:
#   - Reduces duplication (DRY principle)
#   - Ensures consistent error handling
#   - Makes security updates easier
#   - Simplifies testing
#
# ============================================================================

#' Securely fetch and load instructor code from repository
#' 
#' @description
#' Downloads instructor's reference implementation and test cases from
#' GitHub repository with comprehensive error handling.
#' 
#' Security Measures:
#'   1. Input validation via C++ (prevents path traversal)
#'   2. HTTPS transport (encrypted in transit)
#'   3. Error message sanitization (no path exposure)
#'   4. Timeout handling (30 seconds)
#' 
#' @param function_name Name of function to fetch (validated in C++)
#' 
#' @return Environment containing:
#'   - Instructor's function implementation
#'   - test_cases list with all test data
#' 
#' @details
#' Workflow:
#'   1. Call C++ function to download code
#'   2. Create new isolated environment
#'   3. Parse and evaluate code in that environment
#'   4. Return environment for extraction
#' 
#' Error Handling:
#'   - Invalid name → InvalidInputError
#'   - Network issue → network_error
#'   - 404 error → function_not_found_error
#'   - Other → generic error
#' 
#' @section Side Effects:
#'   Creates temporary file (automatically cleaned up by R)
#' 
#' @keywords internal
fetch_instructor_code <- function(function_name) {
  tryCatch({
    # Call C++ function for secure download
    # C++ handles: validation, URL building, download, content verification
    code <- .cpp_fetch_function_content(function_name)
    
    # Create isolated environment for instructor code
    # This prevents pollution of global environment
    env <- new.env()
    
    # Parse and evaluate code in isolated environment
    # This loads the instructor function and test_cases
    eval(parse(text = code), envir = env)
    
    # Return environment containing loaded code
    env
    
  }, error = function(e) {
    # ===== ERROR CLASSIFICATION AND MESSAGING =====
    
    # Path traversal or invalid characters
    if (grepl("Invalid function name", e$message)) {
      stop("Invalid function name format. Use only letters, numbers, underscores, and hyphens.",
           call. = FALSE)
    } 
    # Network/connection issues
    else if (grepl("Network error", e$message)) {
      stop(network_error(
        "Unable to connect to the test server. Please check your internet connection and try again."
      ))
    } 
    # Function doesn't exist (404)
    else {
      stop(function_not_found_error(function_name))
    }
  })
}

#' Extract instructor function from loaded environment
#' 
#' @description
#' Searches environment for the first function object. Instructor code
#' typically defines one function, which this extracts.
#' 
#' Search Strategy:
#'   - Iterate through all objects in environment
#'   - Return first object that is.function() == TRUE
#'   - Error if no function found
#' 
#' @param instructor_env Environment loaded from fetch_instructor_code()
#' @param function_name Function name (for error messages)
#' 
#' @return The instructor's function object
#' 
#' @details
#' Why search instead of direct access?
#'   - Function name might differ from file name
#'   - Allows flexibility in instructor code structure
#'   - Handles helper functions gracefully
#' 
#' @keywords internal
extract_instructor_function <- function(instructor_env, function_name) {
  instructor_fun <- NULL
  
  # Search through all objects in environment
  for (name in ls(instructor_env)) {
    obj <- get(name, envir = instructor_env)
    
    # Take the first function found
    if (is.function(obj)) {
      instructor_fun <- obj
      break  # Found it, stop searching
    }
  }
  
  # Validate we found a function
  if (is.null(instructor_fun)) {
    stop(sprintf("No function implementation found for '%s'. Contact instructor.", 
                function_name),
         call. = FALSE)
  }
  
  instructor_fun
}

#' Extract and validate test cases from environment
#' 
#' @description
#' Retrieves test_cases object from instructor environment and validates
#' its structure using validate_test_cases().
#' 
#' Expected Structure:
#'   test_cases <- list(
#'     inputs = list(...),         # Required
#'     descriptions = c(...),      # Optional
#'     hidden = c(...),            # Optional
#'     points = c(...),            # Optional
#'     tolerance = 1e-10,          # Optional
#'     expected_type = "numeric",  # Optional
#'     hints = c(...),             # Optional
#'     comparison_fn = function()  # Optional
#'   )
#' 
#' @param instructor_env Environment loaded from fetch_instructor_code()
#' @param function_name Function name (for error messages)
#' 
#' @return Validated test_data list with all fields normalized
#' 
#' @keywords internal
extract_test_cases <- function(instructor_env, function_name) {
  # Check if test_cases exists
  if (!exists("test_cases", envir = instructor_env)) {
    stop(sprintf("No test cases found for '%s'. Contact instructor.", function_name),
         call. = FALSE)
  }
  
  # Extract and validate
  test_data <- get("test_cases", envir = instructor_env)
  validate_test_cases(test_data, function_name)
}

# ============================================================================
# STUDENT FEEDBACK SYSTEM
# ============================================================================
#
# Philosophy: Help students learn, don't just mark wrong
#
# Feedback Strategy:
#   - Identify specific error types (type, length, values)
#   - Show where differences occur (positions)
#   - Provide hints when available (guidance without answers)
#   - Use clear, actionable language
#
# Feedback Levels:
#   1. Type mismatch: "Expected numeric but got character"
#   2. Length mismatch: "Expected length 10 but got 5"
#   3. Value differences: "Differences at positions: 3, 5, 7"
#   4. Hints: "Hint: Remember to handle negative numbers"
#
# ============================================================================

#' Analyze differences and generate helpful feedback
#' 
#' @description
#' Compares student output with expected output and generates specific,
#' actionable feedback to help students understand their mistakes.
#' 
#' Analysis Categories:
#' 
#' 1. **Type Analysis:**
#'    Checks if output type matches expectation
#'    Common student errors:
#'      - Returning character "5" instead of numeric 5
#'      - Returning list instead of vector
#'      - Returning data.frame instead of matrix
#' 
#' 2. **Dimension Analysis:**
#'    For vectors: checks length
#'    Common student errors:
#'      - Off-by-one errors (length 9 instead of 10)
#'      - Not handling edge cases (empty input)
#'      - Starting from wrong index
#' 
#' 3. **Value Analysis:**
#'    For numeric vectors: identifies which positions differ
#'    Common student errors:
#'      - Logic errors in loop
#'      - Wrong formula
#'      - Incorrect initial values
#' 
#' 4. **Hint Integration:**
#'    Adds instructor-provided hints when available
#' 
#' @param student_out Student's function output
#' @param expected_out Expected (correct) output
#' @param input_args Input arguments used for this test
#' @param hint Optional instructor hint (NULL or character string)
#' 
#' @return Named list of feedback messages, may be empty if outputs match
#' 
#' @details
#' Return Structure:
#'   list(
#'     type_issue = "Type mismatch: ...",      # If types differ
#'     length_issue = "Length mismatch: ...",  # If lengths differ
#'     diff_positions = "Differences at...",   # If values differ
#'     hint = "Hint: ..."                      # If hint provided
#'   )
#' 
#' @section Performance:
#'   Time: O(n) for vectors, O(1) for type/length checks
#'   Space: O(k) where k = number of feedback messages (typically 1-3)
#' 
#' @examples
#' \dontrun{
#' # Type mismatch
#' provide_feedback("5", 5, list(x = 5))
#' # Returns: list(type_issue = "Type mismatch: Expected numeric but got character")
#' 
#' # Length mismatch
#' provide_feedback(1:5, 1:10, list(n = 10))
#' # Returns: list(length_issue = "Length mismatch: Expected length 10 but got 5")
#' 
#' # Value differences
#' provide_feedback(c(1, 5, 3), c(1, 2, 3), list())
#' # Returns: list(diff_positions = "Differences at positions: 2")
#' 
#' # With hint
#' provide_feedback(5, 10, list(x = 2), "Try multiplying by 2")
#' # Returns: list(hint = "Hint: Try multiplying by 2")
#' }
#' 
#' @keywords internal
provide_feedback <- function(student_out, expected_out, input_args, hint = NULL) {
  feedback <- list()
  
  # ===== CHECK 1: Type Mismatch =====
  # Type errors are usually fundamental misunderstandings
  # Example: student returns string when number expected
  if (class(student_out)[1] != class(expected_out)[1]) {
    feedback$type_issue <- sprintf(
      "Type mismatch: Expected %s but got %s",
      class(expected_out)[1],
      class(student_out)[1]
    )
  }
  
  # ===== CHECK 2: Length Mismatch (for vectors) =====
  # Length errors often indicate:
  #   - Off-by-one errors in loops
  #   - Not handling edge cases (n=0, n=1)
  #   - Wrong loop bounds
  if (is.vector(student_out) && is.vector(expected_out)) {
    if (length(student_out) != length(expected_out)) {
      feedback$length_issue <- sprintf(
        "Length mismatch: Expected length %d but got %d",
        length(expected_out),
        length(student_out)
      )
    }
  }
  
  # ===== CHECK 3: Value Differences (for numeric vectors) =====
  # Only check if types and lengths match
  # Shows exactly where calculations went wrong
  if (is.numeric(student_out) && is.numeric(expected_out) && 
      length(student_out) == length(expected_out)) {
    
    # Find positions where values differ (beyond tolerance)
    diff_indices <- which(abs(student_out - expected_out) > 1e-10)
    
    if (length(diff_indices) > 0) {
      # Show first 5 positions (avoid overwhelming output)
      positions_to_show <- head(diff_indices, 5)
      
      feedback$diff_positions <- sprintf(
        "Differences at positions: %s",
        paste(positions_to_show, collapse = ", ")
      )
      
      # Indicate if there are more differences
      if (length(diff_indices) > 5) {
        feedback$diff_positions <- paste(feedback$diff_positions, "...")
      }
    }
  }
  
  # ===== CHECK 4: Include Hint =====
  # Instructor-provided hint (optional guidance)
  if (!is.null(hint) && nchar(hint) > 0) {
    feedback$hint <- paste("Hint:", hint)
  }
  
  feedback
}

#' Display feedback messages in formatted style
#' 
#' @description
#' Prints feedback list in a clean, readable format with bullet points.
#' 
#' @param feedback Named list of feedback messages from provide_feedback()
#' 
#' @return NULL (invisible), called for side effect of printing
#' 
#' @details
#' Output Format:
#'   Feedback:
#'     * Type mismatch: Expected numeric but got character
#'     * Hint: Check your return type
#' 
#' Handles empty feedback gracefully (prints nothing).
#' 
#' @keywords internal
print_feedback <- function(feedback) {
  # Empty feedback = nothing to print
  if (length(feedback) == 0) return()
  
  cat("\n  Feedback:\n")
  
  # Print each feedback item as a bullet point
  for (name in names(feedback)) {
    cat(sprintf("    * %s\n", feedback[[name]]))
  }
}

# ============================================================================
# PARALLEL TEST EXECUTION
# ============================================================================
#
# Performance Optimization for Large Test Sets
#
# Decision Tree:
#   - < 10 tests: Use sequential (overhead not worth it)
#   - ≥ 10 tests AND use_parallel=TRUE: Use parallel
#   - use_parallel=FALSE: Always use sequential
#
# Parallel Benefits:
#   - Tests run independently (embarassingly parallel)
#   - Speedup: ~2-4x on multi-core systems
#   - No shared state = no synchronization overhead
#
# Parallel Overhead:
#   - Cluster setup: ~100-200ms
#   - Data serialization: ~10ms per test
#   - Worth it for: 10+ tests
#   - Not worth it for: <10 tests
#
# Implementation Notes:
#   - Uses snow-style cluster (portable across OS)
#   - Limits cores to min(available-1, 4) to keep system responsive
#   - Exports necessary objects explicitly
#   - Proper cleanup with on.exit()
#
# ============================================================================

#' Run tests in parallel using multiple CPU cores
#' 
#' @description
#' Executes test cases in parallel for improved performance on multi-core
#' systems. Automatically falls back to sequential for small test sets.
#' 
#' Parallel Execution Strategy:
#'   - Create cluster with n cores (leave 1 free for system)
#'   - Export student_fun, instructor_fun, tolerance to workers
#'   - Distribute test indices across workers
#'   - Each worker: runs test, catches errors, returns result
#'   - Main process: collects results, cleans up cluster
#' 
#' @param student_fun Student's function implementation
#' @param instructor_fun Instructor's reference implementation
#' @param test_data Validated test case data
#' @param tolerance Numeric tolerance for comparisons
#' @param use_parallel Whether to actually use parallel (TRUE) or fall back (FALSE)
#' 
#' @return List of test results, each containing:
#'   - student: student output or error object
#'   - expected: expected output or error object
#'   - index: test number (1-based)
#' 
#' @details
#' Performance Characteristics:
#'   - Setup cost: ~100-200ms (cluster creation)
#'   - Per-test overhead: ~10ms (serialization)
#'   - Speedup: typically 2-4x on 4-core system
#'   - Worth it for: ≥10 tests
#' 
#' Core Allocation Strategy:
#'   - Detect available cores
#'   - Use cores - 1 (keep system responsive)
#'   - Cap at 4 cores (diminishing returns beyond this)
#'   - Example: 8-core system → use 4 cores
#' 
#' @section Resource Management:
#'   Cluster is always stopped via on.exit(), even if errors occur.
#'   This prevents orphaned R processes.
#' 
#' @keywords internal
run_tests_parallel <- function(student_fun, instructor_fun, test_data, tolerance, use_parallel = TRUE) {
  
  n_tests <- length(test_data$inputs)
  
  # ===== DECISION: Use parallel or not? =====
  # Parallel has overhead; only worth it for many tests
  if (!use_parallel || n_tests < 10) {
    return(run_tests_sequential(student_fun, instructor_fun, test_data, tolerance))
  }
  
  # ===== SETUP PARALLEL CLUSTER =====
  # Determine optimal number of cores
  # Strategy: use available-1, capped at 4
  n_cores <- min(
    parallel::detectCores() - 1,  # Leave one core for system
    4                              # Cap at 4 (diminishing returns)
  )
  
  # Create cluster (starts background R processes)
  cl <- parallel::makeCluster(n_cores)
  
  # Ensure cluster is stopped even if error occurs
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  # Export necessary objects to worker processes
  # Workers are independent R processes with empty environments
  # Must explicitly export anything they need
  parallel::clusterExport(
    cl, 
    c("student_fun", "instructor_fun", "tolerance"),
    envir = environment()
  )
  
  # ===== RUN TESTS IN PARALLEL =====
  # parLapply distributes indices across workers
  # Each worker processes a subset of test indices
  results <- parallel::parLapply(cl, seq_along(test_data$inputs), function(i) {
    input_args <- test_data$inputs[[i]]
    
    # Run student function with error catching
    # Errors are captured as error objects, not thrown
    student_out <- tryCatch(
      do.call(student_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    # Run instructor function with error catching
    # If instructor function errors, it's a config problem
    expected_out <- tryCatch(
      do.call(instructor_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    # Return structured result
    list(
      student = student_out,
      expected = expected_out,
      index = i
    )
  })
  
  # Cluster is automatically stopped by on.exit()
  results
}

#' Run tests sequentially (one after another)
#' 
#' @description
#' Executes test cases one at a time in order. Used for:
#'   - Small test sets (<10 tests)
#'   - When use_parallel = FALSE
#'   - Debugging (easier to trace)
#' 
#' Advantages over Parallel:
#'   - No setup overhead
#'   - Easier debugging (linear execution)
#'   - More predictable
#'   - Better for small test sets
#' 
#' @param student_fun Student's function
#' @param instructor_fun Instructor's reference function
#' @param test_data Validated test case data
#' @param tolerance Numeric tolerance
#' 
#' @return List of test results (same structure as parallel version)
#' 
#' @details
#' Execution Flow:
#'   For each test:
#'     1. Extract input arguments
#'     2. Call student function (catch errors)
#'     3. Call instructor function (catch errors)
#'     4. Package results
#'     5. Move to next test
#' 
#' Error Handling:
#'   Errors are captured, not thrown. This allows:
#'   - All tests to run (one failure doesn't stop others)
#'   - Detailed error reporting for each test
#'   - Graceful handling of student code errors
#' 
#' @keywords internal
run_tests_sequential <- function(student_fun, instructor_fun, test_data, tolerance) {
  
  # Simple lapply - process tests one by one
  results <- lapply(seq_along(test_data$inputs), function(i) {
    input_args <- test_data$inputs[[i]]
    
    # ===== RUN STUDENT FUNCTION =====
    # Wrap in tryCatch to capture errors without stopping
    student_out <- tryCatch(
      do.call(student_fun, input_args),
      error = function(e) {
        # Convert error to structured object
        # This allows checking for errors later with inherits()
        structure(list(error = e$message), class = "error")
      }
    )
    
    # ===== RUN INSTRUCTOR FUNCTION =====
    # Should never error (instructor code should be tested)
    # But catch anyway for robustness
    expected_out <- tryCatch(
      do.call(instructor_fun, input_args),
      error = function(e) {
        structure(list(error = e$message), class = "error")
      }
    )
    
    # ===== PACKAGE RESULTS =====
    # Return list with outputs and index
    # Index is needed for matching with test metadata later
    list(
      student = student_out,
      expected = expected_out,
      index = i
    )
  })
  
  results
}

# ============================================================================
# MAIN AUTOGRADER FUNCTION
# ============================================================================
#
# This is the primary user-facing function. It orchestrates the entire
# grading workflow from input validation to final score reporting.
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
#' @return Invisibly returns a list with test results:
#' \itemize{
#'   \item \code{passed}: Integer. Number of tests passed
#'   \item \code{failed}: Integer. Number of tests failed
#'   \item \code{total}: Integer. Total number of tests
#'   \item \code{score}: Numeric. Points earned
#'   \item \code{max_score}: Numeric. Maximum possible points
#'   \item \code{pass_rate}: Numeric. Percentage of tests passed (0-100)
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
#' }
#'
autograder <- function(function_name, verbose = TRUE, show_hidden = FALSE, 
                       show_progress = FALSE, use_parallel = TRUE, show_hints = TRUE) {
  
  # ==========================================================================
  # SECTION 1: INPUT VALIDATION
  # ==========================================================================
  # Comprehensive validation prevents confusing errors later.
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
  
  # ===== Validate function_name type and length =====
  if (!is.character(function_name) || length(function_name) != 1) {
    stop(
      "function_name must be a single character string.\n",
      sprintf("Got: %s (type: %s)", deparse(function_name), class(function_name)),
      call. = FALSE
    )
  }
  
  # ===== Validate all logical parameters =====
  # Students often pass "yes"/"no" or 1/0 instead of TRUE/FALSE
  # Catch these early with clear error messages
  for (param in list(verbose, show_hidden, show_progress, use_parallel, show_hints)) {
    if (!is.logical(param) || length(param) != 1) {
      param_name <- deparse(substitute(param))
      stop(sprintf("%s must be TRUE or FALSE", param_name), call. = FALSE)
    }
  }
  
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
    stop(sprintf(
      "Function '%s' not found in your environment.\n\nPlease define:\n  %s <- function(...) { ... }\n\nThen run autograder('%s') again.",
      student_fun_name, student_fun_name, function_name
    ), call. = FALSE)
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
  # SECTION 8: RETURN STRUCTURED RESULTS
  # ==========================================================================
  # Return results invisibly so they can be captured but don't print by default
  
  invisible(list(
    passed = passed,
    failed = failed,
    total = n_tests,
    score = earned_points,
    max_score = total_points,
    pass_rate = pass_rate
  ))
}

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
  
  # ===== DISPLAY EACH TEST =====
  visible_count <- 0
  hidden_count <- 0
  
  for (i in seq_along(test_data$inputs)) {
    desc <- descriptions[i]
    pts <- points[i]
    is_hidden <- hidden[i]
    
    if (!is_hidden) {
      # ===== VISIBLE TEST: Show Details =====
      cat(sprintf("[Test %d] %s (%d pt)\n", i, desc, pts))
      cat(sprintf("  Input: %s\n\n", format_output(test_data$inputs[[i]])))
      visible_count <- visible_count + 1
      
    } else {
      # ===== HIDDEN TEST: Show Only Existence =====
      # Don't reveal inputs or expected outputs
      # This maintains assessment integrity
      cat(sprintf("[Test %d] [HIDDEN TEST] (%d pt)\n\n", i, pts))
      hidden_count <- hidden_count + 1
    }
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
  # If this fails, we have a fallback list
  problems <- tryCatch({
    # Call C++ function to download _problems.R
    code <- .cpp_fetch_problems_list()
    
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

# ============================================================================
# PACKAGE STARTUP MESSAGE
# ============================================================================
#
# Displayed when library(autograder) is called.
#
# Purpose:
#   - Welcome users
#   - Show version
#   - Quick usage reminder
#   - Highlight new features
#
# Design:
#   - Brief (not overwhelming)
#   - Actionable (shows how to get started)
#   - Versioned (mentions new features)
#
# ============================================================================

#' Package startup message
#' 
#' @description
#' Displays welcome message when package is loaded via library() or require().
#' 
#' @param libname Library name (passed by R)
#' @param pkgname Package name (passed by R)
#' 
#' @details
#' Message includes:
#'   - Package version
#'   - Quick start instructions (list_problems, preview_tests, autograder)
#'   - New features in this version
#' 
#' Uses packageStartupMessage() which can be suppressed with:
#'   suppressPackageStartupMessages(library(autograder))
#' 
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Get current package version
  version <- utils::packageVersion("autograder")
  
  # Display friendly startup message
  packageStartupMessage(
    sprintf("Autograder v%s loaded.", version),
    "\n\nUse list_problems() to see available assignments.",
    "\nUse preview_tests('<function_name>') to preview test cases.",
    "\nUse autograder('<function_name>') to grade your work.",
    "\n\nNew features in this version:",
    "\n  * Faster test execution with parallel processing",
    "\n  * Better error messages and hints",
    "\n  * Improved feedback for failed tests"
  )
}

# ============================================================================
# END OF FILE
# ============================================================================
#
# Summary Statistics:
#   - Total functions: 11 (3 exported, 8 internal)
#   - Lines of code: ~650
#   - Test coverage: 60%+
#   - Documentation: Comprehensive inline + roxygen2
#
# Maintenance Notes:
#   - Update .onAttach() message when adding major features
#   - Keep fallback problems list in sync with repository
#   - Test all error paths when modifying fetch functions
#   - Update version in derive_key() if encryption scheme changes
#
# ============================================================================
