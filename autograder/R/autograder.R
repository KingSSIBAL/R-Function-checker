#' @useDynLib autograder, .registration=TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Run autograder by function name
#'
#' Fetches instructor function and test cases from GitHub,
#' then compares student implementation against reference outputs.
#'
#' @param function_name Character. Name of the function to test.
#'
#' @return Invisibly returns a list with passed, failed, and total counts.
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
autograder <- function(function_name) {
  
  # ===== INPUT VALIDATION =====
  
  if (!is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string")
  }
  
  # ===== GET GITHUB URL (Hidden in compiled binary) =====
  
  GITHUB_URL <- .cpp_get_function_url(function_name)
  
  cat(sprintf("Fetching %s from GitHub...\n", function_name))
  
  # ===== FETCH AND LOAD INSTRUCTOR FILE =====
  
  instructor_env <- tryCatch({
    temp_file <- tempfile()
    download.file(GITHUB_URL, temp_file, mode = "w", quiet = TRUE)
    
    # Read and evaluate R code
    code <- readLines(temp_file)
    env <- new.env()
    eval(parse(text = code), envir = env)
    
    env
  }, error = function(e) {
    stop(sprintf(
      "Function '%s' not found on GitHub.\nError: %s",
      function_name, e$message
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
    stop(sprintf("No function found in '%s.R'", function_name))
  }
  
  # ===== EXTRACT TEST CASES =====
  
  if (!exists("test_cases", envir = instructor_env)) {
    stop(sprintf("No test_cases found in '%s.R'", function_name))
  }
  
  test_data <- get("test_cases", envir = instructor_env)
  
  if (!("inputs" %in% names(test_data))) {
    stop(sprintf("test_cases must have 'inputs' field in '%s.R'", function_name))
  }
  
  # ===== CHECK STUDENT FUNCTION EXISTS =====
  
  student_fun_name <- paste0("student_", function_name)
  
  if (!exists(student_fun_name, envir = .GlobalEnv, mode = "function")) {
    stop(sprintf(
      "Function '%s' not found.\nDefine: %s <- function(...) { ... }",
      student_fun_name, student_fun_name
    ))
  }
  
  student_fun <- get(student_fun_name, envir = .GlobalEnv)
  
  # ===== RUN TESTS =====
  
  cat("\n=== Running Tests ===\n")
  passed <- 0
  failed <- 0
  
  for (i in seq_along(test_data$inputs)) {
    input_args <- test_data$inputs[[i]]
    
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
    
    # Compare outputs
    if (inherits(student_out, "error")) {
      cat(sprintf("\n[Test %d]: FAIL (Student Error)\n", i))
      cat(sprintf("  Input: %s\n", paste(deparse(input_args), collapse = " ")))
      cat(sprintf("  Error: %s\n", student_out$error))
      failed <- failed + 1
      
    } else if (inherits(expected_out, "error")) {
      cat(sprintf("\n[Test %d]: ERROR (Instructor Function Error)\n", i))
      cat(sprintf("  Input: %s\n", paste(deparse(input_args), collapse = " ")))
      cat(sprintf("  Error: %s\n", expected_out$error))
      failed <- failed + 1
      
    } else {
      # Use compiled C++ comparison (hidden in binary)
      is_identical <- .cpp_compare_identical(student_out, expected_out)
      
      if (is_identical[1]) {
        cat(sprintf("[Test %d]: PASS\n", i))
        passed <- passed + 1
      } else {
        cat(sprintf("\n[Test %d]: FAIL\n", i))
        cat(sprintf("  Input:    %s\n", paste(deparse(input_args), collapse = " ")))
        cat(sprintf("  Expected: %s\n", paste(deparse(expected_out), collapse = " ")))
        cat(sprintf("  Got:      %s\n", paste(deparse(student_out), collapse = " ")))
        failed <- failed + 1
      }
    }
  }
  
  # ===== SUMMARY =====
  
  cat("\n=== Summary ===\n")
  cat(sprintf("Passed: %d/%d\n", passed, length(test_data$inputs)))
  cat(sprintf("Failed: %d/%d\n", failed, length(test_data$inputs)))
  
  if (passed == length(test_data$inputs)) {
    cat("\n✓ ALL TESTS PASSED!\n")
  } else {
    cat("\n✗ Some tests failed. Review output above.\n")
  }
  
  invisible(list(
    passed = passed,
    failed = failed,
    total = length(test_data$inputs)
  ))
}
