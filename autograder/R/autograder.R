#' @useDynLib autograder, .registration=TRUE
#' @importFrom Rcpp sourceCpp
NULL

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' NULL coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Format output for display (truncate if too long)
#' @keywords internal
format_output <- function(obj, max_length = 100) {
  output <- paste(deparse(obj), collapse = " ")
  if (nchar(output) > max_length) {
    output <- paste0(substr(output, 1, max_length), "...")
  }
  output
}

#' Validate test case structure
#' @keywords internal
validate_test_cases <- function(test_data, function_name) {
  
  # Required field: inputs
  if (!("inputs" %in% names(test_data))) {
    stop(sprintf(
      "Test cases for '%s' are missing required 'inputs' field.",
      function_name
    ))
  }
  
  if (!is.list(test_data$inputs) || length(test_data$inputs) == 0) {
    stop(sprintf(
      "Test cases for '%s' must have at least one test.",
      function_name
    ))
  }
  
  n_tests <- length(test_data$inputs)
  
  # Validate optional fields have correct length
  if (!is.null(test_data$descriptions)) {
    if (length(test_data$descriptions) != n_tests) {
      warning(sprintf(
        "Test case descriptions length (%d) doesn't match inputs length (%d). Using defaults.",
        length(test_data$descriptions), n_tests
      ))
      test_data$descriptions <- NULL
    }
  }
  
  if (!is.null(test_data$hidden)) {
    if (length(test_data$hidden) != n_tests) {
      warning("Test case 'hidden' length doesn't match inputs. Using defaults.")
      test_data$hidden <- NULL
    }
    if (!is.logical(test_data$hidden)) {
      warning("Test case 'hidden' must be logical. Using defaults.")
      test_data$hidden <- NULL
    }
  }
  
  if (!is.null(test_data$points)) {
    if (length(test_data$points) != n_tests) {
      warning("Test case 'points' length doesn't match inputs. Using defaults.")
      test_data$points <- NULL
    }
    if (!is.numeric(test_data$points)) {
      warning("Test case 'points' must be numeric. Using defaults.")
      test_data$points <- NULL
    }
  }
  
  if (!is.null(test_data$tolerance)) {
    if (!is.numeric(test_data$tolerance) || length(test_data$tolerance) != 1) {
      warning("Test case 'tolerance' must be a single numeric value. Using default.")
      test_data$tolerance <- 1e-10
    }
  }
  
  if (!is.null(test_data$expected_type)) {
    if (!is.character(test_data$expected_type) || length(test_data$expected_type) != 1) {
      warning("Test case 'expected_type' must be a single character value. Ignoring.")
      test_data$expected_type <- NULL
    }
  }
  
  test_data
}

# ============================================================================
# MAIN AUTOGRADER FUNCTION
# ============================================================================

#' Run autograder by function name
#'
#' Tests student implementation against reference outputs with support for
#' points, descriptions, hidden tests, type checking, and tolerance.
#'
#' @param function_name Character. Name of the function to test.
#' @param verbose Logical. Show detailed output for each test? Default TRUE.
#' @param show_hidden Logical. Show details for hidden tests? Default FALSE.
#' @param show_progress Logical. Show progress bar for many tests? Default FALSE.
#'
#' @return Invisibly returns a list with:
#' \itemize{
#'   \item \code{passed}: Number of tests passed
#'   \item \code{failed}: Number of tests failed
#'   \item \code{total}: Total number of tests
#'   \item \code{score}: Points earned (if points specified)
#'   \item \code{max_score}: Maximum points (if points specified)
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' student_fibonacci <- function(n) {
#'   if (n <= 0) return(numeric(0))
#'   if (n == 1) return(1)
#'   fib <- c(1, 1)
#'   for (i in 3:n) {
#'     fib[i] <- fib[i-1] + fib[i-2]
#'   }
#'   fib
#' }
#' autograder("fibonacci")
#' }
#'
autograder <- function(function_name, verbose = TRUE, show_hidden = FALSE, show_progress = FALSE) {
  
  # ===== INPUT VALIDATION =====
  
  if (missing(function_name)) {
    stop(
      "Missing required argument 'function_name'.\n",
      "Usage: autograder('fibonacci')\n",
      "See list_problems() for available functions."
    )
  }
  
  if (!is.character(function_name) || length(function_name) != 1) {
    stop(
      "function_name must be a single character string.\n",
      sprintf("Got: %s (type: %s)", deparse(function_name), class(function_name))
    )
  }
  
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("verbose must be TRUE or FALSE")
  }
  
  if (!is.logical(show_hidden) || length(show_hidden) != 1) {
    stop("show_hidden must be TRUE or FALSE")
  }
  
  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("show_progress must be TRUE or FALSE")
  }
  
  # Check internet connection
  if (!curl::has_internet()) {
    stop(
      "No internet connection detected.\n",
      "The autograder requires internet to fetch test cases."
    )
  }
  
  cat(sprintf("Loading %s...\n", function_name))
  
  # ===== FETCH AND LOAD INSTRUCTOR FILE (SECURE) =====
  
  instructor_env <- tryCatch({
    # Secure fetch - URL never exposed to R
    code <- .cpp_fetch_function_content(function_name)
    
    # Evaluate in isolated environment
    env <- new.env()
    eval(parse(text = code), envir = env)
    
    env
  }, error = function(e) {
    # Sanitized error - no URL exposure
    stop(sprintf(
      "Function '%s' not found. Please check the function name.\n",
      function_name
    ))
  })
  
  # ===== EXTRACT INSTRUCTOR FUNCTION =====
  
  instructor_fun <- NULL
  for (name in ls(instructor_env)) {
    obj <- get(name, envir = instructor_env)
    if (is.function(obj)) {
      instructor_fun <- obj
      break
    }
  }
  
  if (is.null(instructor_fun)) {
    stop(sprintf("No function found for '%s'", function_name))
  }
  
  # ===== EXTRACT TEST CASES =====
  
  if (!exists("test_cases", envir = instructor_env)) {
    stop(sprintf("No test cases found for '%s'", function_name))
  }
  
  test_data <- get("test_cases", envir = instructor_env)
  
  # Validate test case structure
  test_data <- validate_test_cases(test_data, function_name)
  
  # ===== CHECK STUDENT FUNCTION EXISTS =====
  
  student_fun_name <- paste0("student_", function_name)
  
  if (!exists(student_fun_name, envir = .GlobalEnv, mode = "function")) {
    stop(sprintf(
      "Function '%s' not found in your environment.\n\nPlease define:\n  %s <- function(...) { ... }",
      student_fun_name, student_fun_name
    ))
  }
  
  student_fun <- get(student_fun_name, envir = .GlobalEnv)
  
  # ===== EXTRACT TEST CONFIGURATION =====
  
  n_tests <- length(test_data$inputs)
  descriptions <- test_data$descriptions %||% rep("", n_tests)
  hidden <- test_data$hidden %||% rep(FALSE, n_tests)
  points <- test_data$points %||% rep(1, n_tests)
  tolerance <- test_data$tolerance %||% 1e-10
  expected_type <- test_data$expected_type %||% NULL
  
  # ===== RUN TESTS =====
  
  cat("\n=== Running Tests ===\n")
  
  # Progress bar for many tests
  if (show_progress && n_tests > 5) {
    pb <- txtProgressBar(min = 0, max = n_tests, style = 3)
  }
  
  passed <- 0
  failed <- 0
  passed_indices <- integer(0)
  
  for (i in seq_along(test_data$inputs)) {
    input_args <- test_data$inputs[[i]]
    is_hidden <- hidden[i]
    desc <- descriptions[i]
    test_points <- points[i]
    
    # Run student function
    student_out <- tryCatch(
      do.call(student_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    # Run instructor function
    expected_out <- tryCatch(
      do.call(instructor_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    # Check for errors
    if (inherits(student_out, "error")) {
      if (is_hidden && !show_hidden) {
        cat(sprintf("[Test %d] (%d pt): FAIL\n", i, test_points))
      } else {
        cat(sprintf("\n[Test %d] %s (%d pt): FAIL (Error)\n", i, desc, test_points))
        if (verbose) {
          cat(sprintf("  Input: %s\n", format_output(input_args)))
          cat(sprintf("  Error: %s\n", student_out$error))
        }
      }
      failed <- failed + 1
      
      if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
      next
    }
    
    if (inherits(expected_out, "error")) {
      cat(sprintf("\n[Test %d] %s: ERROR (Contact instructor)\n", i, desc))
      failed <- failed + 1
      
      if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
      next
    }
    
    # Type checking
    if (!is.null(expected_type)) {
      actual_type <- class(student_out)[1]
      if (actual_type != expected_type) {
        if (is_hidden && !show_hidden) {
          cat(sprintf("[Test %d] (%d pt): FAIL\n", i, test_points))
        } else {
          cat(sprintf("\n[Test %d] %s (%d pt): FAIL (Type Error)\n", i, desc, test_points))
          if (verbose) {
            cat(sprintf("  Expected type: %s\n", expected_type))
            cat(sprintf("  Got type: %s\n", actual_type))
          }
        }
        failed <- failed + 1
        
        if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
        next
      }
    }
    
    # Comparison
    is_identical <- .cpp_compare_identical(student_out, expected_out)
    
    # If not identical, try tolerance comparison for numerics
    if (!is_identical[1] && is.numeric(student_out) && is.numeric(expected_out)) {
      is_identical[1] <- isTRUE(all.equal(student_out, expected_out, tolerance = tolerance))
    }
    
    if (is_identical[1]) {
      if (is_hidden && !show_hidden) {
        cat(sprintf("[Test %d] (%d pt): PASS\n", i, test_points))
      } else {
        cat(sprintf("[Test %d] %s (%d pt): PASS\n", i, desc, test_points))
      }
      passed <- passed + 1
      passed_indices <- c(passed_indices, i)
    } else {
      if (is_hidden && !show_hidden) {
        cat(sprintf("[Test %d] (%d pt): FAIL\n", i, test_points))
      } else {
        cat(sprintf("\n[Test %d] %s (%d pt): FAIL\n", i, desc, test_points))
        if (verbose) {
          cat(sprintf("  Input:    %s\n", format_output(input_args)))
          cat(sprintf("  Expected: %s\n", format_output(expected_out)))
          cat(sprintf("  Got:      %s\n", format_output(student_out)))
        }
      }
      failed <- failed + 1
    }
    
    if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
  }
  
  if (show_progress && n_tests > 5) {
    close(pb)
    cat("\n")
  }
  
  # ===== SUMMARY =====
  
  cat("\n=== Summary ===\n")
  
  # Calculate score
  total_points <- sum(points)
  earned_points <- sum(points[passed_indices])
  
  cat(sprintf("Score: %d/%d points (%.1f%%)\n", 
              earned_points, total_points, 
              (earned_points/total_points) * 100))
  cat(sprintf("Tests: %d/%d passed\n", passed, n_tests))
  
  if (passed == n_tests) {
    cat("\n✓ ALL TESTS PASSED! Excellent work!\n")
  } else if (passed > 0) {
    cat(sprintf("\n✗ %d/%d tests failed. Review the output above.\n", failed, n_tests))
  } else {
    cat("\n✗ All tests failed. Check your implementation.\n")
  }
  
  invisible(list(
    passed = passed,
    failed = failed,
    total = n_tests,
    score = earned_points,
    max_score = total_points
  ))
}

# ============================================================================
# PREVIEW TESTS (SECURE - Only shows non-hidden)
# ============================================================================

#' Preview test cases without running
#'
#' Shows test case inputs and descriptions for non-hidden tests only.
#' Hidden tests are not revealed to maintain assessment integrity.
#'
#' @param function_name Character. Name of the function to preview.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' preview_tests("fibonacci")
#' }
#'
preview_tests <- function(function_name) {
  
  if (missing(function_name) || !is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string")
  }
  
  cat(sprintf("Loading %s...\n", function_name))
  
  # Fetch function securely
  instructor_env <- tryCatch({
    code <- .cpp_fetch_function_content(function_name)
    env <- new.env()
    eval(parse(text = code), envir = env)
    env
  }, error = function(e) {
    stop(sprintf("Function '%s' not found.", function_name))
  })
  
  # Get test cases
  if (!exists("test_cases", envir = instructor_env)) {
    stop(sprintf("No test cases found for '%s'", function_name))
  }
  
  test_data <- get("test_cases", envir = instructor_env)
  test_data <- validate_test_cases(test_data, function_name)
  
  n_tests <- length(test_data$inputs)
  descriptions <- test_data$descriptions %||% rep("", n_tests)
  hidden <- test_data$hidden %||% rep(FALSE, n_tests)
  points <- test_data$points %||% rep(1, n_tests)
  
  cat("\n=== Test Cases Preview ===\n\n")
  
  visible_count <- 0
  hidden_count <- 0
  
  for (i in seq_along(test_data$inputs)) {
    desc <- descriptions[i]
    pts <- points[i]
    is_hidden <- hidden[i]
    
    if (!is_hidden) {
      # Show details for visible tests
      cat(sprintf("[Test %d] %s (%d pt)\n", i, desc, pts))
      cat(sprintf("  Input: %s\n\n", format_output(test_data$inputs[[i]])))
      visible_count <- visible_count + 1
    } else {
      # Don't show details for hidden tests
      cat(sprintf("[Test %d] [HIDDEN TEST] (%d pt)\n\n", i, pts))
      hidden_count <- hidden_count + 1
    }
  }
  
  cat("=== Summary ===\n")
  cat(sprintf("Total tests: %d\n", n_tests))
  cat(sprintf("Visible tests: %d\n", visible_count))
  cat(sprintf("Hidden tests: %d\n", hidden_count))
  cat(sprintf("Total points: %d\n", sum(points)))
  
  cat("\nNote: Hidden test details are not shown to maintain assessment integrity.\n")
  
  invisible(test_data)
}

# ============================================================================
# LIST PROBLEMS
# ============================================================================

#' List available problems
#'
#' Shows all available functions that can be tested with autograder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_problems()
#' }
#'
list_problems <- function() {
  cat("Available problems:\n\n")
  
  problems <- tryCatch({
    # Secure fetch - URL never exposed
    code <- .cpp_fetch_problems_list()
    
    if (length(code) > 0) {
      env <- new.env()
      eval(parse(text = code), envir = env)
      get("problems", envir = env)
    } else {
      # Fallback
      c("fibonacci", "factorial", "sum_vector")
    }
  }, error = function(e) {
    # Fallback to common problems
    c("fibonacci", "factorial", "sum_vector")
  })
  
  for (prob in problems) {
    cat(sprintf("  - %s\n", prob))
  }
  
  cat("\nUsage:\n")
  cat("  student_<function_name> <- function(...) { ... }\n")
  cat("  autograder('<function_name>')\n")
  cat("\nPreview tests:\n")
  cat("  preview_tests('<function_name>')\n")
  
  invisible(problems)
}

# ============================================================================
# PACKAGE STARTUP
# ============================================================================

.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion("autograder")
  packageStartupMessage(
    sprintf("Autograder v%s loaded.", version),
    "\n\nUse list_problems() to see available assignments.",
    "\nUse preview_tests('<function_name>') to preview test cases.",
    "\nUse autograder('<function_name>') to grade your work."
  )
}
