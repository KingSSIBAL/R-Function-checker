# ============================================================================
# AUTOGRADER PACKAGE - CUSTOM ERROR CLASSES
# ============================================================================
#
# File: errors.R
# Purpose: Custom error classes for better error handling using rlang
#
# Custom error classes provide:
#   1. Better error messages (context-specific)
#   2. Type-safe error handling (catch specific errors)
#   3. Structured error information (fields for debugging)
#   4. Clearer user feedback (vs generic errors)
#   5. Rich formatting with rlang and glue
#
# Error Hierarchy:
#   condition (base R)
#     └── error
#           ├── autograder_error (base class)
#           │     ├── network_error
#           │     ├── function_not_found_error
#           │     ├── test_execution_error
#           │     ├── type_mismatch_error
#           │     ├── length_mismatch_error
#           │     ├── student_function_error
#           │     └── timeout_error
#
# Usage Pattern:
#   tryCatch(
#     risky_operation(),
#     autograder_network_error = function(e) { handle_network_issue(e) },
#     error = function(e) { handle_generic_error(e) }
#   )
#
# ============================================================================

#' @importFrom rlang abort caller_env
#' @importFrom glue glue glue_collapse
NULL

#' Custom error for network-related issues
#' 
#' @description
#' Creates a structured error object for network failures using rlang.
#' This allows calling code to handle network issues specifically.
#' 
#' @param message Human-readable error message describing the network issue
#' @param call The call that generated the error (usually NULL or sys.call())
#' 
#' @return An S3 object of class c("autograder_network_error", "autograder_error", "error", "condition")
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
  rlang::abort(
    message,
    class = c("autograder_network_error", "autograder_error", "network_error"),
    call = call,
    body = c(
      "i" = "Check your internet connection",
      "i" = "Verify the server is reachable",
      ">" = "Try again in a few moments"
    )
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
#' @return An S3 object of class c("autograder_function_not_found_error", "autograder_error", "error", "condition")
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
  rlang::abort(
    glue::glue("Function '{function_name}' not found"),
    class = c("autograder_function_not_found_error", "autograder_error", "function_not_found_error"),
    call = call,
    function_name = function_name,
    body = c(
      "i" = "Use list_problems() to see available functions",
      "x" = glue::glue("'{function_name}' does not exist in the repository"),
      ">" = "Check spelling and try again"
    ),
    footer = c("*" = "Common issues: typos, case sensitivity, missing underscore")
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
#' @return An S3 object of class c("autograder_test_execution_error", "autograder_error", "error", "condition")
#' 
#' @details
#' Used when:
#'   - Test setup fails
#'   - Input arguments are invalid
#'   - Unexpected runtime error in test harness
#' 
#' @keywords internal
test_execution_error <- function(message, test_number, call = NULL) {
  rlang::abort(
    glue::glue("Test {test_number} failed: {message}"),
    class = c("autograder_test_execution_error", "autograder_error", "test_execution_error"),
    call = call,
    test_number = test_number,
    body = c(
      "i" = glue::glue("Error occurred in test case #{test_number}"),
      ">" = "Review the error message and fix your function"
    )
  )
}

#' Custom error for type mismatch
#' 
#' @description
#' Creates a structured error when the student's output type doesn't match
#' the expected type.
#' 
#' @param expected_type The type that was expected
#' @param actual_type The type that was returned
#' @param call The call that generated the error
#' 
#' @return An S3 object of class c("autograder_type_mismatch_error", "autograder_error", "error", "condition")
#' 
#' @keywords internal
type_mismatch_error <- function(expected_type, actual_type, call = NULL) {
  rlang::abort(
    "Type mismatch in function output",
    class = c("autograder_type_mismatch_error", "autograder_error", "type_mismatch_error"),
    call = call,
    expected_type = expected_type,
    actual_type = actual_type,
    body = c(
      "x" = glue::glue("Expected type: {expected_type}"),
      "x" = glue::glue("Your output:   {actual_type}"),
      "i" = "Check the problem description for expected return type",
      "i" = "Use typeof(your_result) to check your output type",
      ">" = glue::glue("Convert using: as.{expected_type}() if appropriate")
    ),
    footer = c("*" = glue::glue("Example: result <- as.{expected_type}(your_value)"))
  )
}

#' Custom error for length mismatch
#' 
#' @description
#' Creates a structured error when the student's output length doesn't match
#' the expected length.
#' 
#' @param expected_length The length that was expected
#' @param actual_length The length that was returned
#' @param call The call that generated the error
#' 
#' @return An S3 object of class c("autograder_length_mismatch_error", "autograder_error", "error", "condition")
#' 
#' @keywords internal
length_mismatch_error <- function(expected_length, actual_length, call = NULL) {
  plural <- if (actual_length == 1) "" else "s"
  rlang::abort(
    "Length mismatch in function output",
    class = c("autograder_length_mismatch_error", "autograder_error", "length_mismatch_error"),
    call = call,
    expected_length = expected_length,
    actual_length = actual_length,
    body = c(
      "x" = glue::glue("Expected length: {expected_length}"),
      "x" = glue::glue("Your output: {actual_length} element{plural}"),
      "i" = "Check your loop termination conditions",
      "i" = "Verify handling of edge cases (n=0, n=1)",
      "i" = "Ensure you're returning all required elements",
      ">" = "Use length(your_result) to verify before returning"
    ),
    footer = c("*" = "Debug tip: Run your function manually and check: length(your_result)")
  )
}

#' Custom error for student function not defined
#' 
#' @description
#' Creates a structured error when the student's function is not found
#' in the global environment.
#' 
#' @param problem_name The problem name (e.g., "fibonacci")
#' @param call The call that generated the error
#' 
#' @return An S3 object of class c("autograder_student_function_error", "autograder_error", "error", "condition")
#' 
#' @keywords internal
student_function_error <- function(problem_name, call = NULL) {
  fn_name <- paste0("student_", problem_name)
  rlang::abort(
    glue::glue("Function '{fn_name}' not found in your environment"),
    class = c("autograder_student_function_error", "autograder_error", "student_function_error"),
    call = call,
    problem_name = problem_name,
    function_name = fn_name,
    body = c(
      "x" = glue::glue("Your function '{fn_name}' was not found in the current R session"),
      "i" = glue::glue("Define your function with the exact name: {fn_name}"),
      "i" = "Run the function definition in R",
      ">" = glue::glue("Then call autograder('{problem_name}')")
    ),
    footer = c(
      "*" = glue::glue("Template: {fn_name} <- function(...) {{ your code here }}"),
      "*" = glue::glue("Quick check: exists('{fn_name}') should return TRUE")
    )
  )
}

#' Custom error for timeout
#' 
#' @description
#' Creates a structured error when a function execution exceeds the time limit.
#' 
#' @param timeout_seconds The timeout limit that was exceeded
#' @param call The call that generated the error
#' 
#' @return An S3 object of class c("autograder_timeout_error", "autograder_error", "error", "condition")
#' 
#' @keywords internal
timeout_error <- function(timeout_seconds, call = NULL) {
  rlang::abort(
    glue::glue("Execution exceeded {timeout_seconds} seconds"),
    class = c("autograder_timeout_error", "autograder_error", "timeout_error"),
    call = call,
    timeout_seconds = timeout_seconds,
    body = c(
      "x" = "Your function took too long to run",
      "i" = "Check loop termination conditions (infinite loop?)",
      "i" = "Consider algorithm efficiency (exponential complexity?)",
      "i" = "Use memoization for recursive functions",
      ">" = "Test with small inputs first"
    ),
    footer = c("*" = "Debug tip: Test with small values: your_function(3), your_function(5)")
  )
}

# ============================================================================
# ERROR HANDLER WITH AUTOMATIC SOLUTION SUGGESTIONS
# ============================================================================

#' Handle error with solution suggestions
#' 
#' @description
#' Wraps an error with automatic solution suggestions based on error patterns.
#' Uses rlang for structured error handling.
#' 
#' @param e An error condition
#' @param context Optional context string for better suggestions
#' 
#' @return Never returns; always throws an error with enhanced message
#' 
#' @keywords internal
handle_error_with_solutions <- function(e, context = NULL) {
  original_message <- conditionMessage(e)
  
  # Get solution if help.R is loaded
  solution <- tryCatch({
    get_error_solution(original_message, verbose = FALSE)
  }, error = function(e2) NULL)
  
  if (!is.null(solution)) {
    rlang::abort(
      original_message,
      class = "autograder_error_with_solution",
      body = c(
        "i" = "--- Suggested Solutions ---",
        stats::setNames(solution$solutions, rep("*", length(solution$solutions)))
      ),
      footer = if (!is.null(solution$code)) c(">" = glue::glue("Helpful code: {solution$code}")) else NULL
    )
  } else {
    rlang::abort(original_message, parent = e)
  }
}

# ============================================================================
# VALIDATION ERROR HELPERS (using rlang)
# ============================================================================

#' Abort with validation error
#' 
#' @description
#' Creates a validation error with structured messaging using rlang and glue.
#' 
#' @param arg_name Name of the argument that failed validation
#' @param expected What was expected
#' @param actual What was received
#' @param call The calling environment
#' 
#' @keywords internal
abort_validation <- function(arg_name, expected, actual, call = rlang::caller_env()) {
  rlang::abort(
    glue::glue("Invalid argument: {arg_name}"),
    class = c("autograder_validation_error", "autograder_error"),
    arg_name = arg_name,
    expected = expected,
    actual = actual,
    call = call,
    body = c(
      "x" = glue::glue("Expected: {expected}"),
      "x" = glue::glue("Got: {format_output(actual)}"),
      "i" = glue::glue("Check the type and value of '{arg_name}'")
    )
  )
}

#' Abort with input error
#' 
#' @description
#' Creates an input error for user-facing validation failures.
#' 
#' @param message Error message
#' @param suggestions Character vector of suggestions
#' @param call The calling environment
#' 
#' @keywords internal
abort_input <- function(message, suggestions = NULL, call = rlang::caller_env()) {
  body <- if (!is.null(suggestions)) {
    stats::setNames(suggestions, rep("i", length(suggestions)))
  } else {
    NULL
  }
  
  rlang::abort(
    message,
    class = c("autograder_input_error", "autograder_error"),
    call = call,
    body = body
  )
}
