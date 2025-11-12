#' Run autograder by function name
#'
#' Fetches instructor function and test cases from GitHub,
#' then compares your implementation against the reference.
#'
#' @param function_name Character. Name of the function to test.
#'   Examples: "fibonacci", "sort_asc", "merge_sorted"
#'
#' @details
#' Your function should be named: student_{function_name}
#'
#' For example, if testing "fibonacci", define:
#' \code{student_fibonacci <- function(n) { ... }}
#'
#' @return
#' Invisibly returns a list with components:
#' \itemize{
#'   \item \code{passed}: Number of tests passed
#'   \item \code{failed}: Number of tests failed
#'   \item \code{total}: Total number of tests
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define your implementation
#' student_fibonacci <- function(n) {
#'   if (n <= 0) return(numeric(0))
#'   if (n == 1) return(1)
#'   fib <- c(1, 1)
#'   for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
#'   fib
#' }
#'
#' # Run autograder
#' autograder("fibonacci")
#' }
#'
autograder <- function(function_name) {
  
  # ===== INPUT VALIDATION =====
  
  if (!is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string")
  }
  
  # ===== GET GITHUB URLS (Hidden in compiled binary) =====
  
  # These functions are compiled in C++ - URLs are hidden!
  GITHUB_FUNCTION_URL <- .cpp_get_function_url(function_name)
  GITHUB_TESTDATA_URL <- .cpp_get_testdata_url()
  
  cat(sprintf("Fetching %s from GitHub...\n", function_name))
  
  # ===== FETCH FROM GITHUB =====
  
  # Fetch instructor function code (URL is hidden in C++)
  instructor_code_raw <- tryCatch(
    .cpp_fetch_from_github(GITHUB_FUNCTION_URL),
    error = function(e) {
      stop(sprintf(
        "Function '%s' not found on GitHub.\nError: %s",
        function_name, e$message
      ))
    }
  )
  instructor_code <- rawToChar(instructor_code_raw)
  
  # Fetch master test data (URL is hidden in C++)
  testdata_raw <- tryCatch(
    .cpp_fetch_from_github(GITHUB_TESTDATA_URL),
    error = function(e) {
      stop(sprintf(
        "Test data not found on GitHub.\nError: %s",
        e$message
      ))
    }
  )
  
  # ===== LOAD INSTRUCTOR FUNCTION =====
  
  # Create isolated environment for instructor function
  instructor_env <- new.env()
  
  tryCatch(
    source(textConnection(instructor_code), local = instructor_env),
    error = function(e) {
      stop(sprintf("Failed to load instructor function: %s", e$message))
    }
  )
  
  # Extract the function (first function found in the file)
  instructor_fun <- NA
  for (name in ls(instructor_env)) {
    obj <- get(name, envir = instructor_env)
    if (is.function(obj)) {
      instructor_fun <- obj
      break
    }
  }
  
  if (is.na(instructor_fun)) {
    stop(sprintf("No function found in instructor code for '%s'", function_name))
  }
  
  # ===== LOAD AND EXTRACT TEST DATA =====
  
  # Deserialize master test data
  all_test_data <- tryCatch(
    unserialize(testdata_raw),
    error = function(e) {
      stop(sprintf("Failed to deserialize test data: %s", e$message))
    }
  )
  
  # Extract test cases for this specific function
  if (!(function_name %in% names(all_test_data))) {
    available <- paste(names(all_test_data), collapse = ", ")
    stop(sprintf(
      "Test cases for '%s' not found.\nAvailable functions: %s",
      function_name, available
    ))
  }
  
  test_data <- all_test_data[[function_name]]
  
  # Validate test data structure
  if (!("inputs" %in% names(test_data))) {
    stop(sprintf("Test data for '%s' must have 'inputs' field", function_name))
  }
  
  # ===== CHECK STUDENT FUNCTION =====
  
  student_fun_name <- paste0("student_", function_name)
  
  if (!exists(student_fun_name, envir = .GlobalEnv, mode = "function")) {
    stop(sprintf(
      "Function '%s' not found in your environment.\nPlease define: %s <- function(...) { ... }",
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
      # Use compiled C++ comparison function (hidden in binary)
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
