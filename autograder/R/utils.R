# ============================================================================
# AUTOGRADER PACKAGE - UTILITY FUNCTIONS
# ============================================================================
#
# File: utils.R
# Purpose: Helper functions and output formatting
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
#'   Shows: `type[1:n] = first, second, third ... type[n] = last`
#'   Example: `integer[1:100] = 1, 2, 3 ... integer[100] = 100`
#' 
#' **Matrices:**
#'   Shows: `matrix[rows x cols]: first_values ...`
#'   Example: `matrix[3x4]: 1 2 3 ...`
#' 
#' **Data Frames:**
#'   Shows: `data.frame[rows x cols] columns: col1, col2, col3`
#'   Example: `data.frame[10 x 3] columns: x, y, z`
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
