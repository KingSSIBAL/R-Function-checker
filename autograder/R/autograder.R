#' @useDynLib autograder, .registration=TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Run autograder by function name
#'
#' Tests student implementation against reference outputs with support for
#' points, descriptions, hidden tests, type checking, and tolerance.
#'
#' @param function_name Character. Name of the function to test.
#' @param verbose Logical. Show detailed output for each test? Default TRUE.
#' @param show_hidden Logical. Show details for hidden tests? Default FALSE.
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
autograder <- function(function_name, verbose = TRUE, show_hidden = FALSE) {
  
  # ===== INPUT VALIDATION =====
  
  if (!is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string")
  }
  
  # ===== GET URL (Hidden in compiled binary) =====
  
  url <- .cpp_get_function_url(function_name)
  
  cat(sprintf("Loading %s...\n", function_name))
  
  # ===== FETCH AND LOAD INSTRUCTOR FILE =====
  
  instructor_env <- tryCatch({
    temp_file <- tempfile()
    download.file(url, temp_file, mode = "w", quiet = TRUE)
    
    # Read and evaluate R code
    code <- readLines(temp_file)
    env <- new.env()
    eval(parse(text = code), envir = env)
    
    env
  }, error = function(e) {
    stop(sprintf("Function '%s' not found. Please check the function name.", function_name))
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
  
  if (!("inputs" %in% names(test_data))) {
    stop("Test cases must have 'inputs' field")
  }
  
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
          cat(sprintf("  Input: %s\n", paste(deparse(input_args), collapse = " ")))
          cat(sprintf("  Error: %s\n", student_out$error))
        }
      }
      failed <- failed + 1
      next
    }
    
    if (inherits(expected_out, "error")) {
      cat(sprintf("\n[Test %d] %s: ERROR (Instructor function error)\n", i, desc))
      failed <- failed + 1
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
          cat(sprintf("  Input:    %s\n", paste(deparse(input_args), collapse = " ")))
          cat(sprintf("  Expected: %s\n", paste(deparse(expected_out), collapse = " ")))
          cat(sprintf("  Got:      %s\n", paste(deparse(student_out), collapse = " ")))
        }
      }
      failed <- failed + 1
    }
  }
  
  # ===== SUMMARY =====
  
  cat("\n=== Summary ===\n")
  
  # Calculate score if points are used
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
    # Try to fetch problems list
    url <- .cpp_get_function_url("_problems")
    temp_file <- tempfile()
    download.file(url, temp_file, mode = "w", quiet = TRUE)
    readLines(temp_file)
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
  
  invisible(problems)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
