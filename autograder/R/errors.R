# ============================================================================
# AUTOGRADER PACKAGE - CUSTOM ERROR CLASSES
# ============================================================================
#
# File: errors.R
# Purpose: Custom error classes for better error handling
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
