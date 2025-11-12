#' Run autograder by function name
#'
#' @param function_name Character. Name of the function to test.
#' @export
#'
autograder <- function(function_name) {
  
  if (!is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string")
  }
  
  # Get URLs (hidden in C++)
  GITHUB_FUNCTION_URL <- .cpp_get_function_url(function_name)
  GITHUB_TESTDATA_URL <- .cpp_get_testdata_url()
  
  cat(sprintf("Fetching %s from GitHub...\n", function_name))
  
  # Fetch instructor function
  instructor_code <- tryCatch({
    temp_file <- tempfile()
    download.file(GITHUB_FUNCTION_URL, temp_file, mode = "w", quiet = TRUE)
    readLines(temp_file)
  }, error = function(e) {
    stop(sprintf("Function '%s' not found on GitHub.\nError: %s", function_name, e$message))
  })
  
  # Load instructor function
  instructor_env <- new.env()
  eval(parse(text = instructor_code), envir = instructor_env)
  
  instructor_fun <- NULL
  for (name in ls(instructor_env)) {
    obj <- get(name, envir = instructor_env)
    if (is.function(obj)) {
      instructor_fun <- obj
      break
    }
  }
  
  if (is.null(instructor_fun)) {
    stop(sprintf("No function found in '%s'", function_name))
  }
  
  # Fetch test data
  testdata_raw <- tryCatch({
    temp_file <- tempfile()
    download.file(GITHUB_TESTDATA_URL, temp_file, mode = "wb", quiet = TRUE)
    readBin(temp_file, "raw", file.size(temp_file))
  }, error = function(e) {
    stop(sprintf("Test data not found.\nError: %s", e$message))
  })
  
  all_test_data <- unserialize(testdata_raw)
  
  if (!(function_name %in% names(all_test_data))) {
    available <- paste(names(all_test_data), collapse = ", ")
    stop(sprintf("Test cases for '%s' not found.\nAvailable: %s", function_name, available))
  }
  
  test_data <- all_test_data[[function_name]]
  
  # Check student function
  student_fun_name <- paste0("student_", function_name)
  
  if (!exists(student_fun_name, envir = .GlobalEnv, mode = "function")) {
    stop(sprintf("Function '%s' not found.\nDefine: %s <- function(...) { ... }", 
                 student_fun_name, student_fun_name))
  }
  
  student_fun <- get(student_fun_name, envir = .GlobalEnv)
  
  # Run tests
  cat("\n=== Running Tests ===\n")
  passed <- 0
  failed <- 0
  
  for (i in seq_along(test_data$inputs)) {
    input_args <- test_data$inputs[[i]]
    
    student_out <- tryCatch(
      do.call(student_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    expected_out <- tryCatch(
      do.call(instructor_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    if (inherits(student_out, "error")) {
      cat(sprintf("\n[Test %d]: FAIL (Error)\n", i))
      cat(sprintf("  Input: %s\n", paste(deparse(input_args), collapse = " ")))
      cat(sprintf("  Error: %s\n", student_out$error))
      failed <- failed + 1
    } else {
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
  
  cat("\n=== Summary ===\n")
  cat(sprintf("Passed: %d/%d\n", passed, length(test_data$inputs)))
  cat(sprintf("Failed: %d/%d\n", failed, length(test_data$inputs)))
  
  if (passed == length(test_data$inputs)) {
    cat("\n✓ ALL TESTS PASSED!\n")
  } else {
    cat("\n✗ Some tests failed.\n")
  }
  
  invisible(list(passed = passed, failed = failed, total = length(test_data$inputs)))
}
