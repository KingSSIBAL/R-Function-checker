# ============================================================================
# Test: help.R - Help System, Error Solutions, and Diagnostics
# ============================================================================

# ============================================================================
# Section 1: Error Solution Registry
# ============================================================================

test_that(".init_error_solutions populates registry", {
  # Initialize solutions
  .init_error_solutions()
  
  # Check that solutions are registered
  solutions <- ls(envir = .error_solutions)
  expect_true(length(solutions) > 0)
  expect_true("network" %in% solutions)
  expect_true("student_function" %in% solutions)
  expect_true("function_404" %in% solutions)
  expect_true("type_mismatch" %in% solutions)
  expect_true("length_mismatch" %in% solutions)
  expect_true("syntax" %in% solutions)
})

test_that("error solutions have required fields", {
  .init_error_solutions()
  
  for (key in ls(envir = .error_solutions)) {
    solution <- .error_solutions[[key]]
    expect_true("pattern" %in% names(solution), 
                info = paste("Missing pattern in", key))
    expect_true("title" %in% names(solution),
                info = paste("Missing title in", key))
    expect_true("solutions" %in% names(solution),
                info = paste("Missing solutions in", key))
    expect_true(length(solution$solutions) > 0,
                info = paste("Empty solutions in", key))
  }
})

# ============================================================================
# Section 2: get_error_solution
# ============================================================================

test_that("get_error_solution identifies network errors", {
  output <- capture.output({
    result <- get_error_solution("network connection failed", verbose = TRUE)
  })
  
  expect_equal(result$title, "Network Connection Issue")
  expect_true(length(result$solutions) > 0)
  output_text <- paste(output, collapse = "\n")
  expect_match(output_text, "Network")
})

test_that("get_error_solution identifies student function errors", {
  result <- get_error_solution("student_fibonacci not found", verbose = FALSE)
  expect_equal(result$title, "Student Function Not Found")
})

test_that("get_error_solution identifies 404 errors", {
  result <- get_error_solution("Function xyz not found in repository HTTP 404", 
                               verbose = FALSE)
  expect_equal(result$title, "Problem Not Found in Repository")
})

test_that("get_error_solution identifies type mismatch", {
  result <- get_error_solution("Type Error: type mismatch expected numeric got character",
                               verbose = FALSE)
  expect_equal(result$title, "Return Type Mismatch")
})

test_that("get_error_solution identifies length mismatch", {
  result <- get_error_solution("length mismatch: expected length 10 got 5",
                               verbose = FALSE)
  expect_equal(result$title, "Output Length Mismatch")
})

test_that("get_error_solution identifies package loading errors", {
  result <- get_error_solution("package xyz not found", verbose = FALSE)
  expect_equal(result$title, "Package Loading Error")
})

test_that("get_error_solution identifies syntax errors", {
  result <- get_error_solution("unexpected symbol in expression", verbose = FALSE)
  expect_equal(result$title, "R Syntax Error")
})

test_that("get_error_solution identifies runtime errors", {
  result <- get_error_solution("Error in function: runtime error during execution",
                               verbose = FALSE)
  expect_equal(result$title, "Runtime Error in Your Code")
})

test_that("get_error_solution identifies crypto errors", {
  result <- get_error_solution("decrypt failed: authentication error",
                               verbose = FALSE)
  expect_equal(result$title, "Encryption/Authentication Error")
})

test_that("get_error_solution identifies memory errors", {
  result <- get_error_solution("cannot allocate memory", verbose = FALSE)
  expect_equal(result$title, "Memory or Performance Issue")
})

test_that("get_error_solution returns generic for unknown errors", {
  result <- get_error_solution("some completely unique error xyz123", 
                               verbose = FALSE)
  expect_equal(result$title, "General Error")
  expect_true(length(result$solutions) > 0)
})

test_that("get_error_solution handles non-character input", {
  # Should convert to character
  result <- get_error_solution(123, verbose = FALSE)
  expect_type(result, "list")
})

test_that("get_error_solution verbose output is formatted", {
  output <- capture.output({
    get_error_solution("network error", verbose = TRUE)
  })
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Suggested Solutions")
  expect_match(output_text, "\\d\\.")  # numbered list
})

# ============================================================================
# Section 3: autograder_help
# ============================================================================

test_that("autograder_help shows topic list when no argument", {
  output <- capture.output(autograder_help())
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Autograder Help System")
  expect_match(output_text, "getting-started")
  expect_match(output_text, "functions")
  expect_match(output_text, "errors")
  expect_match(output_text, "workflow")
  expect_match(output_text, "tips")
})

test_that("autograder_help shows getting-started topic", {
  output <- capture.output(autograder_help("getting-started"))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Getting Started")
  expect_match(output_text, "list_problems")
  expect_match(output_text, "student_")
  expect_match(output_text, "autograder")
})

test_that("autograder_help shows functions topic", {
  output <- capture.output(autograder_help("functions"))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Functions Reference")
  expect_match(output_text, "autograder")
  expect_match(output_text, "list_problems")
  expect_match(output_text, "preview_tests")
})

test_that("autograder_help shows errors topic", {
  output <- capture.output(autograder_help("errors"))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Errors")
  expect_match(output_text, "not found")
})

test_that("autograder_help shows workflow topic", {
  output <- capture.output(autograder_help("workflow"))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Workflow|workflow")
})

test_that("autograder_help shows tips topic", {
  output <- capture.output(autograder_help("tips"))
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Tips|tips|Success|success", ignore.case = TRUE)
})

test_that("autograder_help errors on unknown topic", {
  expect_error(
    autograder_help("unknown-topic"),
    "Unknown topic"
  )
})

test_that("autograder_help returns invisibly", {
  result <- capture.output(ret <- autograder_help("getting-started"))
  expect_type(ret, "character")
})

# ============================================================================
# Section 4: autograder_diagnose
# ============================================================================

test_that("autograder_diagnose runs without error", {
  output <- capture.output({
    result <- autograder_diagnose()
  })
  
  expect_type(result, "list")
  expect_true("r_version" %in% names(result))
  expect_true("autograder_version" %in% names(result))
})

test_that("autograder_diagnose returns system info", {
  output <- capture.output(result <- autograder_diagnose())
  
  expect_true("r_version" %in% names(result))
  expect_true("has_internet" %in% names(result))
})

test_that("autograder_diagnose output is formatted", {
  output <- capture.output(autograder_diagnose())
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Diagnostic|diagnos", ignore.case = TRUE)
})

# ============================================================================
# Section 5: autograder_quick_ref
# ============================================================================

test_that("autograder_quick_ref displays reference card", {
  output <- capture.output(autograder_quick_ref())
  output_text <- paste(output, collapse = "\n")
  
  expect_match(output_text, "Quick Reference|quick reference", ignore.case = TRUE)
  expect_match(output_text, "autograder")
  expect_match(output_text, "list_problems|preview_tests")
})

test_that("autograder_quick_ref returns invisibly", {
  output <- capture.output(result <- autograder_quick_ref())
  expect_invisible(autograder_quick_ref())
})

# ============================================================================
# Section 6: autograder_check_dependencies
# ============================================================================

test_that("autograder_check_dependencies runs without error", {
  output <- capture.output({
    result <- autograder_check_dependencies()
  })
  
  expect_type(result, "list")
})

test_that("autograder_check_dependencies checks core packages", {
  output <- capture.output(result <- autograder_check_dependencies())
  
  # Result should indicate status of dependencies
  expect_true("all_ok" %in% names(result) || "status" %in% names(result) ||
              length(result) > 0)
})

# ============================================================================
# Section 7: cli_box (shared with enhanced.R)
# ============================================================================

test_that("cli_box creates properly formatted output", {
  result <- cli_box("Test Message")
  
  expect_type(result, "character")
  expect_match(result, "Test Message")
  expect_match(result, "=")
})

test_that("cli_box handles long text", {
  long_text <- paste(rep("a", 100), collapse = "")
  result <- cli_box(long_text)
  
  expect_match(result, long_text)
})
