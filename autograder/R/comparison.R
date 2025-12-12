# ============================================================================
# AUTOGRADER PACKAGE - CUSTOM COMPARISON FUNCTIONS
# ============================================================================
#
# File: comparison.R
# Purpose: Registry and utilities for custom comparison functions
#
# This module provides:
#   - A registry of reusable comparison functions
#   - Built-in comparators for common data types
#   - Tools for instructors to create custom comparators
#
# Author: Reijel Agub (rcagub@up.edu.ph)
# Version: 0.4.0
# License: MIT
#
# ============================================================================

# ============================================================================
# COMPARISON FUNCTION REGISTRY
# ============================================================================

# Private environment to store registered comparison functions
.comparison_registry <- new.env(parent = emptyenv())

#' Register a custom comparison function
#' 
#' @description
#' Registers a named comparison function that can be used in test cases.
#' Comparison functions should take two arguments (student output, expected output)
#' and return TRUE if they match, FALSE otherwise.
#' 
#' @param name Character. Unique name for the comparison function.
#' @param fn Function. Comparison function taking (actual, expected) and returning logical.
#' @param description Character. Optional description of what this comparator does.
#' 
#' @return Invisibly returns TRUE on success.
#' 
#' @examples
#' \dontrun{
#' # Register a case-insensitive string comparison
#' register_comparison("case_insensitive", function(actual, expected) {
#'   tolower(actual) == tolower(expected)
#' })
#' 
#' # Use in test cases
#' test_cases <- list(
#'   inputs = list(list("Hello")),
#'   comparison_fn = get_comparison("case_insensitive")
#' )
#' }
#' 
#' @keywords internal
register_comparison <- function(name, fn, description = NULL) {
  # Validate inputs

  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    stop("'name' must be a non-empty character string", call. = FALSE)
  }
  
  if (!is.function(fn)) {
    stop("'fn' must be a function", call. = FALSE)
  }
  
  # Check function signature (should accept at least 2 arguments)
  fn_args <- names(formals(fn))
  if (length(fn_args) < 2) {
    stop("Comparison function must accept at least 2 arguments (actual, expected)", 
         call. = FALSE)
  }
  
  # Store in registry
  .comparison_registry[[name]] <- list(
    fn = fn,
    description = description %||% ""
  )
  
  invisible(TRUE)
}

#' Get a registered comparison function
#' 
#' @description
#' Retrieves a comparison function from the registry by name.
#' 
#' @param name Character. Name of the registered comparison function.
#' 
#' @return The comparison function, or NULL if not found.
#' 
#' @examples
#' \dontrun{
#' # Get the built-in numeric tolerance comparison
#' compare_fn <- get_comparison("numeric_tolerance")
#' compare_fn(1.0000001, 1.0)  # TRUE (within default tolerance)
#' }
#' 
#' @keywords internal
get_comparison <- function(name) {
  if (!exists(name, envir = .comparison_registry)) {
    return(NULL)
  }
  .comparison_registry[[name]]$fn
}

#' List all registered comparison functions
#' 
#' @description
#' Returns a data frame of all registered comparison functions with their
#' names and descriptions.
#' 
#' @return Data frame with columns: name, description
#' 
#' @examples
#' \dontrun{
#' list_comparisons()
#' }
#' 
#' @keywords internal
list_comparisons <- function() {
  names <- ls(envir = .comparison_registry)
  
  if (length(names) == 0) {
    return(data.frame(name = character(0), description = character(0)))
  }
  
  descriptions <- vapply(names, function(n) {
    .comparison_registry[[n]]$description %||% ""
  }, character(1))
  
  data.frame(
    name = names,
    description = descriptions,
    stringsAsFactors = FALSE
  )
}

#' Clear all registered comparison functions
#' 
#' @description
#' Removes all registered comparison functions from the registry.
#' Built-in comparisons are re-registered after clearing.
#' 
#' @param keep_builtin Logical. If TRUE (default), re-register built-in comparisons.
#' 
#' @return Invisibly returns TRUE.
#' 
#' @keywords internal
clear_comparisons <- function(keep_builtin = TRUE) {
  rm(list = ls(envir = .comparison_registry), envir = .comparison_registry)
  
  if (keep_builtin) {
    .register_builtin_comparisons()
  }
  
  invisible(TRUE)
}

# ============================================================================
# BUILT-IN COMPARISON FUNCTIONS
# ============================================================================

#' Numeric comparison with tolerance
#' 
#' @description
#' Compares numeric values with a specified tolerance. Handles NA, NaN, Inf correctly.
#' 
#' @param actual Numeric value or vector from student
#' @param expected Numeric value or vector expected
#' @param tolerance Numeric. Maximum allowed difference (default: package tolerance)
#' 
#' @return Logical indicating if values are equal within tolerance.
#' 
#' @examples
#' \dontrun{
#' compare_numeric(1.0000001, 1.0)  # TRUE
#' compare_numeric(c(1, 2, 3), c(1, 2, 3))  # TRUE
#' compare_numeric(c(1, NA), c(1, NA))  # TRUE (NAs match)
#' }
#' 
#' @keywords internal
compare_numeric <- function(actual, expected, tolerance = autograder_tolerance()) {
  .cpp_compare_fast(actual, expected, tolerance)
}

#' Exact comparison (strict equality)
#' 
#' @description
#' Compares values using R's identical() function. No tolerance for numeric values.
#' 
#' @param actual Value from student
#' @param expected Expected value
#' 
#' @return Logical indicating if values are identical.
#' 
#' @examples
#' \dontrun{
#' compare_exact(1L, 1L)  # TRUE
#' compare_exact(1L, 1.0)  # FALSE (different types)
#' }
#' 
#' @keywords internal
compare_exact <- function(actual, expected) {
  identical(actual, expected)
}

#' Case-insensitive string comparison
#' 
#' @description
#' Compares character values ignoring case differences.
#' Uses C++ implementation for better performance on large vectors.
#' 
#' @param actual Character value from student
#' @param expected Expected character value
#' 
#' @return Logical indicating if strings match (case-insensitive).
#' 
#' @examples
#' \dontrun{
#' compare_case_insensitive("Hello", "HELLO")  # TRUE
#' compare_case_insensitive("abc", "ABC")  # TRUE
#' }
#' 
#' @keywords internal
compare_case_insensitive <- function(actual, expected) {
  if (!is.character(actual) || !is.character(expected)) {
    return(FALSE)
  }
  .cpp_compare_case_insensitive(actual, expected)
}

#' Unordered set comparison
#' 
#' @description
#' Compares two vectors as unordered sets (ignoring element order).
#' Uses C++ implementation for better performance with sorting and comparison.
#' Useful when the order of results doesn't matter.
#' 
#' @param actual Vector from student
#' @param expected Expected vector
#' @param tolerance Numeric tolerance for numeric comparisons
#' 
#' @return Logical indicating if sets contain same elements.
#' 
#' @examples
#' \dontrun{
#' compare_set(c(3, 1, 2), c(1, 2, 3))  # TRUE
#' compare_set(c("b", "a"), c("a", "b"))  # TRUE
#' }
#' 
#' @keywords internal
compare_set <- function(actual, expected, tolerance = autograder_tolerance()) {
  if (length(actual) != length(expected)) {
    return(FALSE)
  }
  
  # Use C++ implementation for sorting and comparison
  .cpp_compare_set(actual, expected, tolerance)
}

#' Data frame comparison (ignoring row order)
#' 
#' @description
#' Compares data frames with tolerance for numeric columns.
#' Can optionally ignore row order.
#' 
#' @param actual Data frame from student
#' @param expected Expected data frame
#' @param tolerance Numeric tolerance for numeric columns
#' @param ignore_row_order Logical. If TRUE, sort rows before comparison.
#' 
#' @return Logical indicating if data frames are equivalent.
#' 
#' @examples
#' \dontrun{
#' df1 <- data.frame(a = 1:3, b = c("x", "y", "z"))
#' df2 <- data.frame(a = 1:3, b = c("x", "y", "z"))
#' compare_dataframe(df1, df2)  # TRUE
#' }
#' 
#' @keywords internal
compare_dataframe <- function(actual, expected, tolerance = autograder_tolerance(),
                               ignore_row_order = FALSE) {
  # Check types
  if (!is.data.frame(actual) || !is.data.frame(expected)) {
    return(FALSE)
  }
  
  # Check column names (required before C++ call)
  if (!identical(names(actual), names(expected))) {
    return(FALSE)
  }
  
  # Use optimized C++ implementation
  .cpp_compare_dataframe(actual, expected, tolerance, ignore_row_order)
}

#' Length-only comparison
#' 
#' @description
#' Compares only the length/size of objects, not their content.
#' Useful for testing that functions return the correct number of elements.
#' 
#' @param actual Object from student
#' @param expected Expected object
#' 
#' @return Logical indicating if lengths match.
#' 
#' @examples
#' \dontrun{
#' compare_length(1:10, 11:20)  # TRUE (both length 10)
#' compare_length(list(1, 2), c(1, 2))  # TRUE (both length 2)
#' }
#' 
#' @keywords internal
compare_length <- function(actual, expected) {
  length(actual) == length(expected)
}

#' Class-only comparison
#' 
#' @description
#' Compares only the class of objects, not their content.
#' Useful for testing that functions return the correct type.
#' 
#' @param actual Object from student
#' @param expected Expected object
#' 
#' @return Logical indicating if classes match.
#' 
#' @examples
#' \dontrun{
#' compare_class(data.frame(a = 1), data.frame(b = 2))  # TRUE
#' compare_class(matrix(1), 1)  # FALSE
#' }
#' 
#' @keywords internal
compare_class <- function(actual, expected) {
  identical(class(actual), class(expected))
}

#' Approximate numeric comparison (relative tolerance)
#' 
#' @description
#' Compares numeric values using relative tolerance. Better for values
#' spanning many orders of magnitude.
#' 
#' @param actual Numeric value from student
#' @param expected Expected numeric value
#' @param rel_tolerance Relative tolerance (e.g., 0.01 for 1% difference)
#' 
#' @return Logical indicating if values are within relative tolerance.
#' 
#' @examples
#' \dontrun{
#' compare_relative(1000, 1001, rel_tolerance = 0.01)  # TRUE (within 1%)
#' compare_relative(1000, 1100, rel_tolerance = 0.01)  # FALSE (10% difference)
#' }
#' 
#' @keywords internal
compare_relative <- function(actual, expected, rel_tolerance = 0.01) {
  if (!is.numeric(actual) || !is.numeric(expected)) {
    return(FALSE)
  }
  
  if (length(actual) != length(expected)) {
    return(FALSE)
  }
  
  # Use optimized C++ implementation with early termination
  .cpp_compare_relative(actual, expected, rel_tolerance)
}

# ============================================================================
# INTERNAL REGISTRATION
# ============================================================================

#' Register built-in comparison functions
#' @keywords internal
.register_builtin_comparisons <- function() {
  register_comparison(
    "numeric_tolerance", 
    compare_numeric,
    "Compare numeric values with absolute tolerance"
  )
  
  register_comparison(
    "exact",
    compare_exact,
    "Strict identical() comparison"
  )
  
  register_comparison(
    "case_insensitive",
    compare_case_insensitive,
    "Case-insensitive string comparison"
  )
  
  register_comparison(
    "set",
    compare_set,
    "Unordered set comparison (ignores element order)"
  )
  
  register_comparison(
    "dataframe",
    compare_dataframe,
    "Data frame comparison with numeric tolerance"
  )
  
  register_comparison(
    "length",
    compare_length,
    "Compare only the length of objects"
  )
  
  register_comparison(
    "class",
    compare_class,
    "Compare only the class of objects"
  )
  
  register_comparison(
    "relative",
    compare_relative,
    "Relative tolerance comparison for numerics"
  )
}
