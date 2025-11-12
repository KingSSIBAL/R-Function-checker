#' @useDynLib autograder, .registration=TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom parallel detectCores makeCluster clusterExport parLapply stopCluster
#' @importFrom utils head tail txtProgressBar setTxtProgressBar
NULL


# ============================================================================
# CUSTOM ERROR CLASSES
# ============================================================================

#' Custom error for network issues
#' @keywords internal
network_error <- function(message, call = NULL) {
  structure(
    list(message = message, call = call),
    class = c("network_error", "error", "condition")
  )
}

#' Custom error for missing functions
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

#' Custom error for test execution
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

#' NULL coalescing operator
#' @noRd
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Improved format output with smart truncation
#' @keywords internal
format_output <- function(obj, max_length = 200, preserve_structure = TRUE) {
  if (is.null(obj)) return("NULL")
  
  # Handle different types intelligently
  if (is.list(obj) && preserve_structure) {
    if (length(obj) == 0) return("list()")
    if (length(obj) <= 3) {
      output <- deparse(obj, width.cutoff = max_length)
      output <- paste(output, collapse = " ")
    } else {
      output <- sprintf("list(length=%d, first 3: %s, ...)", 
                       length(obj),
                       paste(deparse(obj[1:3]), collapse = " "))
    }
  } else if (is.vector(obj) && length(obj) > 10) {
    output <- sprintf("%s[1:%d] = %s ... %s[%d] = %s",
                     class(obj)[1], length(obj),
                     paste(head(obj, 3), collapse = ", "),
                     class(obj)[1], length(obj),
                     tail(obj, 1))
  } else if (is.matrix(obj)) {
    output <- sprintf("matrix[%dx%d]: %s ...",
                     nrow(obj), ncol(obj),
                     paste(as.vector(obj[1:min(3, nrow(obj)), 1]), collapse = ", "))
  } else if (is.data.frame(obj)) {
    output <- sprintf("data.frame[%d x %d] columns: %s",
                     nrow(obj), ncol(obj),
                     paste(names(obj), collapse = ", "))
  } else {
    output <- paste(deparse(obj, width.cutoff = max_length), collapse = " ")
  }
  
  # Truncate if still too long
  if (nchar(output) > max_length) {
    output <- paste0(substr(output, 1, max_length - 3), "...")
  }
  
  output
}

#' Improved validate test cases with stricter checks
#' @keywords internal
validate_test_cases <- function(test_data, function_name) {
  
  # Required field: inputs
  if (!("inputs" %in% names(test_data))) {
    stop(sprintf(
      "CRITICAL: Test cases for '%s' are missing required 'inputs' field.\nContact instructor to fix test configuration.",
      function_name
    ), call. = FALSE)
  }
  
  if (!is.list(test_data$inputs) || length(test_data$inputs) == 0) {
    stop(sprintf(
      "CRITICAL: Test cases for '%s' must have at least one test.\nContact instructor to fix test configuration.",
      function_name
    ), call. = FALSE)
  }
  
  n_tests <- length(test_data$inputs)
  
  # Validate optional fields - STOP on critical mismatches
  if (!is.null(test_data$descriptions)) {
    if (length(test_data$descriptions) != n_tests) {
      stop(sprintf(
        "CRITICAL: Test case descriptions length (%d) doesn't match inputs length (%d).\nContact instructor to fix test configuration.",
        length(test_data$descriptions), n_tests
      ), call. = FALSE)
    }
  } else {
    test_data$descriptions <- paste0("Test ", seq_len(n_tests))
  }
  
  if (!is.null(test_data$hidden)) {
    if (length(test_data$hidden) != n_tests) {
      stop(sprintf(
        "CRITICAL: 'hidden' field length (%d) doesn't match inputs length (%d).\nContact instructor.",
        length(test_data$hidden), n_tests
      ), call. = FALSE)
    }
    if (!is.logical(test_data$hidden)) {
      stop("CRITICAL: 'hidden' field must be logical (TRUE/FALSE). Contact instructor.", 
           call. = FALSE)
    }
  } else {
    test_data$hidden <- rep(FALSE, n_tests)
  }
  
  if (!is.null(test_data$points)) {
    if (length(test_data$points) != n_tests) {
      stop(sprintf(
        "CRITICAL: 'points' field length (%d) doesn't match inputs length (%d).\nContact instructor.",
        length(test_data$points), n_tests
      ), call. = FALSE)
    }
    if (!is.numeric(test_data$points) || any(test_data$points < 0)) {
      stop("CRITICAL: 'points' must be non-negative numeric values. Contact instructor.", 
           call. = FALSE)
    }
  } else {
    test_data$points <- rep(1, n_tests)
  }
  
  if (!is.null(test_data$tolerance)) {
    if (!is.numeric(test_data$tolerance) || length(test_data$tolerance) != 1 || 
        test_data$tolerance < 0) {
      stop("CRITICAL: 'tolerance' must be a single non-negative numeric value. Contact instructor.", 
           call. = FALSE)
    }
  } else {
    test_data$tolerance <- 1e-10
  }
  
  if (!is.null(test_data$expected_type)) {
    if (!is.character(test_data$expected_type) || length(test_data$expected_type) != 1) {
      warning("Test case 'expected_type' must be a single character value. Ignoring.")
      test_data$expected_type <- NULL
    }
  }
  
  # Add hints field support
  if (!is.null(test_data$hints)) {
    if (length(test_data$hints) != n_tests) {
      warning("Test case 'hints' length doesn't match. Ignoring hints.")
      test_data$hints <- NULL
    }
  }
  
  # Add custom comparison functions support
  if (!is.null(test_data$comparison_fn)) {
    if (!is.function(test_data$comparison_fn)) {
      warning("'comparison_fn' must be a function. Using default comparison.")
      test_data$comparison_fn <- NULL
    }
  }
  
  test_data
}

# ============================================================================
# SHARED FETCH FUNCTION (Reduces duplication)
# ============================================================================

#' Securely fetch and load instructor code
#' @keywords internal
fetch_instructor_code <- function(function_name) {
  tryCatch({
    code <- .cpp_fetch_function_content(function_name)
    env <- new.env()
    eval(parse(text = code), envir = env)
    env
  }, error = function(e) {
    if (grepl("Invalid function name", e$message)) {
      stop("Invalid function name format. Use only letters, numbers, underscores, and hyphens.",
           call. = FALSE)
    } else if (grepl("Network error", e$message)) {
      stop(network_error(
        "Unable to connect to the test server. Please check your internet connection and try again."
      ))
    } else {
      stop(function_not_found_error(function_name))
    }
  })
}

#' Extract instructor function from environment
#' @keywords internal
extract_instructor_function <- function(instructor_env, function_name) {
  instructor_fun <- NULL
  for (name in ls(instructor_env)) {
    obj <- get(name, envir = instructor_env)
    if (is.function(obj)) {
      instructor_fun <- obj
      break
    }
  }
  
  if (is.null(instructor_fun)) {
    stop(sprintf("No function implementation found for '%s'. Contact instructor.", 
                function_name),
         call. = FALSE)
  }
  
  instructor_fun
}

#' Extract and validate test cases
#' @keywords internal
extract_test_cases <- function(instructor_env, function_name) {
  if (!exists("test_cases", envir = instructor_env)) {
    stop(sprintf("No test cases found for '%s'. Contact instructor.", function_name),
         call. = FALSE)
  }
  
  test_data <- get("test_cases", envir = instructor_env)
  validate_test_cases(test_data, function_name)
}

# ============================================================================
# BETTER STUDENT FEEDBACK
# ============================================================================

#' Analyze error and provide helpful hints
#' @keywords internal
provide_feedback <- function(student_out, expected_out, input_args, hint = NULL) {
  feedback <- list()
  
  # Type mismatch
  if (class(student_out)[1] != class(expected_out)[1]) {
    feedback$type_issue <- sprintf(
      "Type mismatch: Expected %s but got %s",
      class(expected_out)[1], class(student_out)[1]
    )
  }
  
  # Length mismatch for vectors
  if (is.vector(student_out) && is.vector(expected_out)) {
    if (length(student_out) != length(expected_out)) {
      feedback$length_issue <- sprintf(
        "Length mismatch: Expected length %d but got %d",
        length(expected_out), length(student_out)
      )
    }
  }
  
  # Numeric comparison details
  if (is.numeric(student_out) && is.numeric(expected_out) && 
      length(student_out) == length(expected_out)) {
    diff_indices <- which(abs(student_out - expected_out) > 1e-10)
    if (length(diff_indices) > 0) {
      feedback$diff_positions <- sprintf(
        "Differences at positions: %s",
        paste(head(diff_indices, 5), collapse = ", ")
      )
      if (length(diff_indices) > 5) {
        feedback$diff_positions <- paste(feedback$diff_positions, "...")
      }
    }
  }
  
  # Include hint if provided
  if (!is.null(hint) && nchar(hint) > 0) {
    feedback$hint <- paste("Hint:", hint)
  }
  
  feedback
}

#' Print detailed feedback
#' @keywords internal
print_feedback <- function(feedback) {
  if (length(feedback) == 0) return()
  
  cat("\n  Feedback:\n")
  for (name in names(feedback)) {
    cat(sprintf("    * %s\n", feedback[[name]]))
  }
}

# ============================================================================
# PARALLEL TEST EXECUTION
# ============================================================================

#' Run tests in parallel
#' @keywords internal
run_tests_parallel <- function(student_fun, instructor_fun, test_data, tolerance, use_parallel = TRUE) {
  
  n_tests <- length(test_data$inputs)
  
  # Only use parallel for large test sets
  if (!use_parallel || n_tests < 10) {
    return(run_tests_sequential(student_fun, instructor_fun, test_data, tolerance))
  }
  
  # Setup parallel cluster
  n_cores <- min(parallel::detectCores() - 1, 4) # Leave one core free, max 4
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  # Export necessary objects
  parallel::clusterExport(cl, c("student_fun", "instructor_fun", "tolerance"),
                         envir = environment())
  
  # Run tests in parallel
  results <- parallel::parLapply(cl, seq_along(test_data$inputs), function(i) {
    input_args <- test_data$inputs[[i]]
    
    student_out <- tryCatch(
      do.call(student_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    expected_out <- tryCatch(
      do.call(instructor_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    list(
      student = student_out,
      expected = expected_out,
      index = i
    )
  })
  
  results
}

#' Run tests sequentially
#' @keywords internal
run_tests_sequential <- function(student_fun, instructor_fun, test_data, tolerance) {
  
  results <- lapply(seq_along(test_data$inputs), function(i) {
    input_args <- test_data$inputs[[i]]
    
    student_out <- tryCatch(
      do.call(student_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    expected_out <- tryCatch(
      do.call(instructor_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    list(
      student = student_out,
      expected = expected_out,
      index = i
    )
  })
  
  results
}

# ============================================================================
# MAIN AUTOGRADER FUNCTION (Improved)
# ============================================================================

#' Run autograder by function name
#'
#' Tests student implementation against reference outputs with support for
#' points, descriptions, hidden tests, type checking, tolerance, and parallel execution.
#'
#' @param function_name Character. Name of the function to test.
#' @param verbose Logical. Show detailed output for each test? Default TRUE.
#' @param show_hidden Logical. Show details for hidden tests? Default FALSE.
#' @param show_progress Logical. Show progress bar for many tests? Default FALSE.
#' @param use_parallel Logical. Use parallel processing for tests? Default TRUE for 10+ tests.
#' @param show_hints Logical. Show hints for failed tests? Default TRUE.
#'
#' @return Invisibly returns a list with:
#' \itemize{
#'   \item \code{passed}: Number of tests passed
#'   \item \code{failed}: Number of tests failed
#'   \item \code{total}: Total number of tests
#'   \item \code{score}: Points earned (if points specified)
#'   \item \code{max_score}: Maximum points (if points specified)
#'   \item \code{pass_rate}: Percentage of tests passed
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
autograder <- function(function_name, verbose = TRUE, show_hidden = FALSE, 
                       show_progress = FALSE, use_parallel = TRUE, show_hints = TRUE) {
  
  # ===== INPUT VALIDATION =====
  
  if (missing(function_name)) {
    stop(
      "Missing required argument 'function_name'.\n",
      "Usage: autograder('fibonacci')\n",
      "See list_problems() for available functions.",
      call. = FALSE
    )
  }
  
  if (!is.character(function_name) || length(function_name) != 1) {
    stop(
      "function_name must be a single character string.\n",
      sprintf("Got: %s (type: %s)", deparse(function_name), class(function_name)),
      call. = FALSE
    )
  }
  
  # Validate logical parameters
  for (param in list(verbose, show_hidden, show_progress, use_parallel, show_hints)) {
    if (!is.logical(param) || length(param) != 1) {
      param_name <- deparse(substitute(param))
      stop(sprintf("%s must be TRUE or FALSE", param_name), call. = FALSE)
    }
  }
  
  # Check internet connection
  if (!curl::has_internet()) {
    stop(network_error(
      "No internet connection detected.\nThe autograder requires internet to fetch test cases."
    ))
  }
  
  cat(sprintf("Loading %s...\n", function_name))
  
  # ===== FETCH AND LOAD INSTRUCTOR CODE (Using shared function) =====
  
  instructor_env <- fetch_instructor_code(function_name)
  instructor_fun <- extract_instructor_function(instructor_env, function_name)
  test_data <- extract_test_cases(instructor_env, function_name)
  
  # ===== CHECK STUDENT FUNCTION EXISTS =====
  
  student_fun_name <- paste0("student_", function_name)
  
  if (!exists(student_fun_name, envir = .GlobalEnv, mode = "function")) {
    stop(sprintf(
      "Function '%s' not found in your environment.\n\nPlease define:\n  %s <- function(...) { ... }\n\nThen run autograder('%s') again.",
      student_fun_name, student_fun_name, function_name
    ), call. = FALSE)
  }
  
  student_fun <- get(student_fun_name, envir = .GlobalEnv)
  
  # ===== EXTRACT TEST CONFIGURATION =====
  
  n_tests <- length(test_data$inputs)
  descriptions <- test_data$descriptions
  hidden <- test_data$hidden
  points <- test_data$points
  tolerance <- test_data$tolerance
  expected_type <- test_data$expected_type
  hints <- test_data$hints %||% rep("", n_tests)
  comparison_fn <- test_data$comparison_fn
  
  # ===== RUN TESTS =====
  
  cat("\n=== Running Tests ===\n")
  
  # Progress bar for many tests
  if (show_progress && n_tests > 5) {
    pb <- txtProgressBar(min = 0, max = n_tests, style = 3)
  }
  
  # Run tests (parallel or sequential)
  test_results <- if (use_parallel && n_tests >= 10) {
    cat(sprintf("Using parallel processing (%d cores)...\n", 
                min(parallel::detectCores() - 1, 4)))
    run_tests_parallel(student_fun, instructor_fun, test_data, tolerance, TRUE)
  } else {
    run_tests_sequential(student_fun, instructor_fun, test_data, tolerance)
  }
  
  # Process results
  passed <- 0
  failed <- 0
  passed_indices <- integer(0)
  
  for (result in test_results) {
    i <- result$index
    student_out <- result$student
    expected_out <- result$expected
    
    input_args <- test_data$inputs[[i]]
    is_hidden <- hidden[i]
    desc <- descriptions[i]
    test_points <- points[i]
    hint <- hints[i]
    
    # Check for errors
    if (inherits(student_out, "error")) {
      if (is_hidden && !show_hidden) {
        cat(sprintf("[Test %d] (%d pt): FAIL\n", i, test_points))
      } else {
        cat(sprintf("\n[Test %d] %s (%d pt): FAIL (Error)\n", i, desc, test_points))
        if (verbose) {
          cat(sprintf("  Input: %s\n", format_output(input_args)))
          cat(sprintf("  Error: %s\n", student_out$error))
          if (show_hints && nchar(hint) > 0) {
            cat(sprintf("  Hint: %s\n", hint))
          }
        }
      }
      failed <- failed + 1
      
      if (show_progress && n_tests > 5) setTxtProgressBar(pb, i)
      next
    }
    
    if (inherits(expected_out, "error")) {
      cat(sprintf("\n[Test %d] %s: ERROR (Contact instructor - reference implementation failed)\n", 
                  i, desc))
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
    
    # Comparison (using optimized C++ comparison)
    is_identical <- if (!is.null(comparison_fn)) {
      comparison_fn(student_out, expected_out)
    } else {
      .cpp_compare_fast(student_out, expected_out, tolerance)
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
          
          if (show_hints) {
            feedback <- provide_feedback(student_out, expected_out, input_args, hint)
            print_feedback(feedback)
          }
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
  pass_rate <- (passed / n_tests) * 100
  
  cat(sprintf("Score: %d/%d points (%.1f%%)\n", 
              earned_points, total_points, 
              (earned_points/total_points) * 100))
  cat(sprintf("Tests: %d/%d passed (%.1f%%)\n", passed, n_tests, pass_rate))
  
  if (passed == n_tests) {
    cat("\n\u2713 ALL TESTS PASSED! Excellent work!\n")
  } else if (passed > 0) {
    cat(sprintf("\n\u2717 %d/%d tests failed. Review the output above.\n", failed, n_tests))
    if (failed <= 3) {
      cat("  You're close! Focus on the failing test cases.\n")
    } else if (pass_rate >= 50) {
      cat("  You're making progress! Review your logic for edge cases.\n")
    } else {
      cat("  Review the expected behavior and test your function with simple inputs first.\n")
    }
  } else {
    cat("\n\u2717 All tests failed. Check your implementation carefully.\n")
    cat("  Suggestions:\n")
    cat("    * Test your function manually with simple inputs\n")
    cat("    * Review the function requirements\n")
    cat("    * Check for syntax errors or typos\n")
  }
  
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
# PREVIEW TESTS (Using shared functions)
# ============================================================================

#' Preview test cases without running
#'
#' Shows test case inputs and descriptions for non-hidden tests only.
#'
#' @param function_name Character. Name of the function to preview.
#'
#' @export
#'
preview_tests <- function(function_name) {
  
  if (missing(function_name) || !is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string", call. = FALSE)
  }
  
  cat(sprintf("Loading %s...\n", function_name))
  
  # Use shared fetch function
  instructor_env <- fetch_instructor_code(function_name)
  test_data <- extract_test_cases(instructor_env, function_name)
  
  n_tests <- length(test_data$inputs)
  descriptions <- test_data$descriptions
  hidden <- test_data$hidden
  points <- test_data$points
  
  cat("\n=== Test Cases Preview ===\n\n")
  
  visible_count <- 0
  hidden_count <- 0
  
  for (i in seq_along(test_data$inputs)) {
    desc <- descriptions[i]
    pts <- points[i]
    is_hidden <- hidden[i]
    
    if (!is_hidden) {
      cat(sprintf("[Test %d] %s (%d pt)\n", i, desc, pts))
      cat(sprintf("  Input: %s\n\n", format_output(test_data$inputs[[i]])))
      visible_count <- visible_count + 1
    } else {
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
#' @export
#'
list_problems <- function() {
  cat("Available problems:\n\n")
  
  problems <- tryCatch({
    code <- .cpp_fetch_problems_list()
    
    if (length(code) > 0) {
      env <- new.env()
      eval(parse(text = code), envir = env)
      get("problems", envir = env)
    } else {
      c("fibonacci", "factorial", "sum_vector")
    }
  }, error = function(e) {
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
    "\nUse autograder('<function_name>') to grade your work.",
    "\n\nNew features in this version:",
    "\n  * Faster test execution with parallel processing",
    "\n  * Better error messages and hints",
    "\n  * Improved feedback for failed tests"
  )
}
