# ============================================================================
# AUTOGRADER PACKAGE - STUDENT FEEDBACK SYSTEM
# ============================================================================
#
# File: feedback.R
# Purpose: Generate and display helpful feedback for students
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
    
    # Use C++ function for faster difference finding with early termination
    diff_indices <- .cpp_find_differences(student_out, expected_out, 
                                           autograder_tolerance(), 5L)
    
    if (length(diff_indices) > 0) {
      feedback$diff_positions <- sprintf(
        "Differences at positions: %s",
        paste(diff_indices, collapse = ", ")
      )
      
      # Note: .cpp_find_differences returns max 5 positions
      # If all 5 are returned, there may be more
      if (length(diff_indices) >= 5) {
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
# DIFF VIEWER FOR FAILED TESTS
# ============================================================================

#' Show side-by-side comparison of expected vs actual values
#' 
#' @description
#' Provides a detailed diff view for failed tests, showing exactly where
#' values differ. Useful for debugging complex outputs.
#' 
#' @param expected Expected (correct) output
#' @param actual Student's output
#' @param max_show Maximum number of differences to show (default: 10)
#' @param tolerance Numeric tolerance for comparison (default: package tolerance)
#' @param use_waldo Logical. If TRUE and waldo package is available, use waldo
#'   for rich diff output. Set to FALSE to use built-in diff. Default uses
#'   the option `autograder.use_waldo` (TRUE by default).
#' 
#' @return Invisible NULL, called for side effect of printing
#' 
#' @details
#' For numeric vectors, shows:
#'   - Indices where values differ
#'   - Expected vs actual values at those indices
#'   - Absolute and relative differences
#' 
#' For character vectors, shows:
#'   - String differences
#' 
#' For data frames, shows:
#'   - Column-wise differences
#'
#' When waldo is available and `use_waldo = TRUE`, provides colorful,
#' context-aware diffs that are easier to read.
#' 
#' @keywords internal
#' 
#' @examples
#' \dontrun{
#' show_diff(c(1, 2, 3), c(1, 5, 3))
#' # Shows: Index 2: Expected 2, Got 5 (diff: 3)
#' 
#' # Disable waldo for simpler output
#' show_diff(c(1, 2), c(1, 3), use_waldo = FALSE)
#' }
show_diff <- function(expected, actual, max_show = 10, 
                      tolerance = autograder_tolerance(),
                      use_waldo = getOption("autograder.use_waldo", TRUE)) {
  cat(cli::col_cyan("\n=== Diff View ===\n\n"))
  
  # Try waldo first if available and enabled
  if (use_waldo && requireNamespace("waldo", quietly = TRUE)) {
    waldo_diff <- show_waldo_diff(expected, actual, tolerance = tolerance, max_diffs = max_show)
    if (!is.null(waldo_diff)) {
      return(invisible(NULL))
    }
    # Fall through to custom diff if waldo didn't produce output
  }
  
  # Type comparison
  if (!identical(class(expected), class(actual))) {
    cat(cli::col_yellow("Type mismatch:\n"))
    cat(glue::glue("  Expected type: {paste(class(expected), collapse = ', ')}\n"))
    cat(glue::glue("  Actual type:   {paste(class(actual), collapse = ', ')}\n"))
    return(invisible(NULL))
  }
  
  # Numeric comparison
  if (is.numeric(expected) && is.numeric(actual)) {
    show_numeric_diff(expected, actual, max_show, tolerance)
    return(invisible(NULL))
  }
  
  # Character comparison
  if (is.character(expected) && is.character(actual)) {
    show_character_diff(expected, actual, max_show)
    return(invisible(NULL))
  }
  
  # Data frame comparison
  if (is.data.frame(expected) && is.data.frame(actual)) {
    show_dataframe_diff(expected, actual, max_show, tolerance)
    return(invisible(NULL))
  }
  
  # List comparison
  if (is.list(expected) && is.list(actual)) {
    show_list_diff(expected, actual, max_show, tolerance)
    return(invisible(NULL))
  }
  
  # Fallback for other types
  cat("Cannot show detailed diff for this type.\n")
  cat(sprintf("Expected: %s\n", format_output(expected)))
  cat(sprintf("Actual:   %s\n", format_output(actual)))
  
  invisible(NULL)
}

#' Show numeric vector differences
#' @keywords internal
show_numeric_diff <- function(expected, actual, max_show, tolerance) {
  # Length check
  if (length(expected) != length(actual)) {
    cat(cli::col_yellow("Length mismatch:\n"))
    cat(sprintf("  Expected length: %d\n", length(expected)))
    cat(sprintf("  Actual length:   %d\n", length(actual)))
    
    # Show first few elements of each
    n_show <- min(5, min(length(expected), length(actual)))
    if (n_show > 0) {
      cat("\n  First elements comparison:\n")
      idx <- seq_len(n_show)
      exp_vals <- ifelse(idx <= length(expected), sprintf("%.6g", expected[idx]), "N/A")
      act_vals <- ifelse(idx <= length(actual), sprintf("%.6g", actual[idx]), "N/A")
      cat(sprintf("    [%d] Expected: %s, Actual: %s\n", idx, exp_vals, act_vals), sep = "")
    }
    return(invisible(NULL))
  }
  
  # Find differences
  diff_indices <- .cpp_find_differences(expected, actual, tolerance, as.integer(max_show))
  
  if (length(diff_indices) == 0) {
    cat(cli::col_green("No differences found within tolerance.\n"))
    return(invisible(NULL))
  }
  
  cat(sprintf("Found %d difference(s):\n\n", length(diff_indices)))
  cat(sprintf("%-8s %-15s %-15s %-12s %-12s\n", 
              "Index", "Expected", "Actual", "Abs Diff", "Rel Diff"))
  cat(paste(rep("-", 65), collapse = ""), "\n")
  
  # Vectorized difference calculations
  exp_vals <- expected[diff_indices]
  act_vals <- actual[diff_indices]
  abs_diffs <- abs(exp_vals - act_vals)
  rel_diffs <- ifelse(abs(exp_vals) > 1e-10, abs_diffs / abs(exp_vals) * 100, NA)
  rel_strs <- ifelse(is.na(rel_diffs), "N/A", sprintf("%.2f%%", rel_diffs))
  
  cat(sprintf("%-8d %-15.6g %-15.6g %-12.6g %-12s\n", 
              diff_indices, exp_vals, act_vals, abs_diffs, rel_strs), sep = "")
  
  if (length(diff_indices) >= max_show) {
    cat(cli::col_grey(sprintf("\n... (showing first %d differences)\n", max_show)))
  }
  
  invisible(NULL)
}

#' Show character vector differences
#' @keywords internal
show_character_diff <- function(expected, actual, max_show) {
  # Length check
  if (length(expected) != length(actual)) {
    cat(cli::col_yellow("Length mismatch:\n"))
    cat(sprintf("  Expected length: %d\n", length(expected)))
    cat(sprintf("  Actual length:   %d\n", length(actual)))
    return(invisible(NULL))
  }
  
  # Find differences
  diffs <- which(expected != actual)
  
  if (length(diffs) == 0) {
    cat(cli::col_green("No differences found.\n"))
    return(invisible(NULL))
  }
  
  n_show <- min(length(diffs), max_show)
  cat(sprintf("Found %d difference(s):\n\n", length(diffs)))
  
  # Vectorized diff display
  show_idx <- diffs[seq_len(n_show)]
  cat(sprintf("[%d] Expected: \"%s\"\n     Actual:   \"%s\"\n\n", 
              show_idx, expected[show_idx], actual[show_idx]), sep = "")
  
  if (length(diffs) > max_show) {
    cat(cli::col_grey(sprintf("... (showing first %d differences)\n", max_show)))
  }
  
  invisible(NULL)
}

#' Show data frame differences
#' @keywords internal
show_dataframe_diff <- function(expected, actual, max_show, tolerance) {
  # Dimension check
  if (nrow(expected) != nrow(actual) || ncol(expected) != ncol(actual)) {
    cat(cli::col_yellow("Dimension mismatch:\n"))
    cat(sprintf("  Expected: %d rows x %d cols\n", nrow(expected), ncol(expected)))
    cat(sprintf("  Actual:   %d rows x %d cols\n", nrow(actual), ncol(actual)))
    return(invisible(NULL))
  }
  
  # Column name check
  if (!identical(names(expected), names(actual))) {
    cat(cli::col_yellow("Column name mismatch:\n"))
    cat(sprintf("  Expected: %s\n", paste(names(expected), collapse = ", ")))
    cat(sprintf("  Actual:   %s\n", paste(names(actual), collapse = ", ")))
    return(invisible(NULL))
  }
  
  # Find cell differences
  diff_count <- 0
  cat("Column-wise differences:\n\n")
  
  for (col in names(expected)) {
    exp_col <- expected[[col]]
    act_col <- actual[[col]]
    
    if (is.numeric(exp_col) && is.numeric(act_col)) {
      diffs <- which(abs(exp_col - act_col) > tolerance)
    } else {
      diffs <- which(exp_col != act_col)
    }
    
    if (length(diffs) > 0 && diff_count < max_show) {
      cat(sprintf("Column '%s': %d difference(s)\n", col, length(diffs)))
      for (idx in head(diffs, 3)) {
        cat(sprintf("  Row %d: Expected %s, Got %s\n", 
                    idx, format(exp_col[idx]), format(act_col[idx])))
      }
      diff_count <- diff_count + 1
    }
  }
  
  invisible(NULL)
}

#' Show list differences
#' @keywords internal
show_list_diff <- function(expected, actual, max_show, tolerance) {
  if (length(expected) != length(actual)) {
    cat(cli::col_yellow("Length mismatch:\n"))
    cat(sprintf("  Expected length: %d\n", length(expected)))
    cat(sprintf("  Actual length:   %d\n", length(actual)))
    return(invisible(NULL))
  }
  
  # Check each element
  diff_count <- 0
  for (i in seq_along(expected)) {
    if (!.cpp_compare_fast(expected[[i]], actual[[i]], tolerance)) {
      diff_count <- diff_count + 1
      if (diff_count <= max_show) {
        name <- if (!is.null(names(expected))) names(expected)[i] else i
        cat(sprintf("Element [[%s]]:\n", name))
        cat(sprintf("  Expected: %s\n", format_output(expected[[i]])))
        cat(sprintf("  Actual:   %s\n\n", format_output(actual[[i]])))
      }
    }
  }
  
  if (diff_count > max_show) {
    cat(cli::col_grey(sprintf("... (%d more differences)\n", diff_count - max_show)))
  }
  
  invisible(NULL)
}

# ============================================================================
# WALDO-BASED DIFF (Enhanced diff when waldo package is available)
# ============================================================================

#' Show differences using waldo package
#' 
#' @description
#' Uses the waldo package for rich, colorful diff output when available.
#' Falls back gracefully if waldo is not installed.
#' 
#' @param expected The expected value
#' @param actual The actual value from student code
#' @param tolerance Numeric tolerance for comparisons
#' @param max_diffs Maximum number of differences to show
#' 
#' @return TRUE if diff was shown, NULL if waldo produced no output
#' 
#' @keywords internal
show_waldo_diff <- function(expected, actual, tolerance = 1e-10, max_diffs = 10) {
  if (!requireNamespace("waldo", quietly = TRUE)) {
    return(NULL)
  }
  
  # Use waldo::compare for rich diff output
  diff_output <- tryCatch({
    waldo::compare(
      expected, actual,
      x_arg = "expected",
      y_arg = "your_output",
      tolerance = tolerance,
      max_diffs = max_diffs
    )
  }, error = function(e) NULL)
  
  if (is.null(diff_output) || length(diff_output) == 0) {
    return(NULL)
  }
  
  # Print waldo output with styling
  cat(cli::col_cyan("Differences (via waldo):\n\n"))
  for (line in diff_output) {
    cat(line, "\n")
  }
  cat("\n")
  
  invisible(TRUE)
}

#' Compare objects with waldo
#' 
#' @description
#' Uses waldo::compare to check if two objects are equal, with rich diff output.
#' 
#' @param expected The expected value
#' @param actual The actual value
#' @param tolerance Numeric tolerance
#' 
#' @return Logical indicating if objects are equal
#' 
#' @keywords internal
compare_with_waldo <- function(expected, actual, tolerance = 1e-10) {
  if (!requireNamespace("waldo", quietly = TRUE)) {
    # Fall back to internal comparison
    return(.cpp_compare_fast(expected, actual, tolerance))
  }
  
  diff <- waldo::compare(expected, actual, tolerance = tolerance)
  length(diff) == 0
}
