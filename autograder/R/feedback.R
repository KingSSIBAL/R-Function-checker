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
