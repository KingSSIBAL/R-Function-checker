# ============================================================================
# AUTOGRADER PACKAGE - HELP AND DOCUMENTATION UTILITIES
# ============================================================================
#
# File: help.R
# Purpose: Enhanced error messages, documentation discoverability, and
#          automated dependency scanning
#
# This module provides:
#   - Error solution suggestions
#   - Help discovery functions
#   - Dependency validation utilities
#   - Quick reference guides
#
# Author: Reijel Agub (rcagub@up.edu.ph)
# Version: 0.4.0
# License: MIT
#
# ============================================================================

# ============================================================================
# SECTION 1: ERROR SOLUTION REGISTRY
# ============================================================================

# Private environment to store error solutions
.error_solutions <- new.env(parent = emptyenv())

#' Initialize error solutions registry
#' 
#' @description
#' Populates the error solutions registry with common error patterns
#' and their corresponding solutions.
#' 
#' @keywords internal
.init_error_solutions <- function() {
  # Network/Connection errors
  .error_solutions[["network"]] <- list(
    pattern = "network|connection|internet|timeout|could not connect|curl|HTTP",
    title = "Network Connection Issue",
    solutions = c(
      "Check your internet connection",
      "Try again in a few moments (server may be temporarily unavailable)",
      "Check if GitHub is accessible: try visiting https://github.com",
      "If on a corporate/school network, check firewall settings",
      "Try: curl::has_internet() to verify connection"
    ),
    code = 'if (!curl::has_internet()) message("No internet detected")'
  )
  
  # Function not found in global environment
  .error_solutions[["student_function"]] <- list(
    pattern = "student_.*not found|function.*not defined|could not find function",
    title = "Student Function Not Found",
    solutions = c(
      "Make sure you defined your function in the current R session",
      "Function name must be exactly: student_<problem_name>",
      "Run your function definition BEFORE calling autograder()",
      "Check for typos in your function name"
    ),
    code = 'student_fibonacci <- function(n) {\n  # Your implementation here\n}'
  )
  
  # Repository function not found (404)
  .error_solutions[["function_404"]] <- list(
    pattern = "Function.*not found|HTTP 404|does not exist in repository",
    title = "Problem Not Found in Repository",
    solutions = c(
      "Use list_problems() to see available problems",
      "Check spelling of the problem name (case-sensitive)",
      "Verify you're using the correct repository",
      "Ask your instructor if the problem should be available"
    ),
    code = 'list_problems()  # See all available problems'
  )
  
  # Type mismatch errors
  .error_solutions[["type_mismatch"]] <- list(
    pattern = "type.*mismatch|expected.*got|wrong type|Type Error",
    title = "Return Type Mismatch",
    solutions = c(
      "Check the expected return type in the problem description",
      "Use typeof() or class() to inspect your output",
      "Common issue: returning character instead of numeric",
      "Use as.numeric(), as.character(), etc. to convert types"
    ),
    code = 'typeof(your_result)  # Check type\nclass(your_result)  # Check class'
  )
  
  # Length mismatch errors
  .error_solutions[["length_mismatch"]] <- list(
    pattern = "length.*mismatch|expected length|different.*length",
    title = "Output Length Mismatch",
    solutions = c(
      "Check your loop bounds and termination conditions",
      "Verify handling of edge cases (n=0, n=1)",
      "Use length() to check your output size",
      "Review if you're returning the right number of elements"
    ),
    code = 'length(your_result)  # Check output length'
  )
  
  # Package loading errors
  .error_solutions[["package_load"]] <- list(
    pattern = "package.*not found|namespace load failed|there is no package|could not load",
    title = "Package Loading Error",
    solutions = c(
      "Reinstall the autograder package",
      "Check that all dependencies are installed",
      "Run: autograder_check_dependencies() for diagnosis",
      "Clear your R session and restart"
    ),
    code = 'autograder_check_dependencies()  # Check all dependencies'
  )
  
  # Syntax errors
  .error_solutions[["syntax"]] <- list(
    pattern = "unexpected|parse error|syntax error|unexpected symbol",
    title = "R Syntax Error",
    solutions = c(
      "Check for missing or extra parentheses, braces, brackets",
      "Look for missing commas between function arguments",
      "Check for unclosed strings (missing quotes)",
      "Use RStudio's syntax highlighting to spot issues"
    ),
    code = NULL
  )
  
  # Runtime errors in student code
  .error_solutions[["runtime"]] <- list(
    pattern = "Error in.*:|error during execution|runtime error",
    title = "Runtime Error in Your Code",
    solutions = c(
      "Test your function manually with simple inputs first",
      "Check for division by zero or invalid operations",
      "Verify all variables are defined before use",
      "Use traceback() after an error to see where it occurred"
    ),
    code = 'traceback()  # See the call stack after an error'
  )
  
  # Encryption/decryption errors
  .error_solutions[["crypto"]] <- list(
    pattern = "decrypt|encrypt|key|cipher|authentication",
    title = "Encryption/Authentication Error",
    solutions = c(
      "This is usually an internal package issue",
      "Contact your instructor if this persists",
      "Try reinstalling the package",
      "Check if the repository URL is configured correctly"
    ),
    code = NULL
  )
  
  # Memory/performance issues
  .error_solutions[["memory"]] <- list(
    pattern = "memory|allocation|cannot allocate|stack overflow",
    title = "Memory or Performance Issue",
    solutions = c(
      "Your solution may be too slow or use too much memory",
      "Check for infinite loops in your code",
      "Consider a more efficient algorithm",
      "Avoid creating very large intermediate objects"
    ),
    code = NULL
  )
}

#' Get solution suggestions for an error
#' 
#' @description
#' Analyzes an error message and returns relevant solution suggestions.
#' 
#' @param error_message Character. The error message to analyze.
#' @param verbose Logical. If TRUE, print solutions. Default: TRUE.
#' 
#' @return Invisibly returns a list with title, solutions, and code (if any).
#' 
#' @examples
#' \dontrun{
#' tryCatch(
#'   autograder("fibonacci"),
#'   error = function(e) get_error_solution(e$message)
#' )
#' }
#' 
#' @keywords internal
get_error_solution <- function(error_message, verbose = TRUE) {
  # Initialize solutions if not already done
  if (length(ls(envir = .error_solutions)) == 0) {
    .init_error_solutions()
  }
  
  if (!is.character(error_message)) {
    error_message <- as.character(error_message)
  }
  
  # Search for matching solution
  best_match <- NULL
  for (key in ls(envir = .error_solutions)) {
    solution <- .error_solutions[[key]]
    if (grepl(solution$pattern, error_message, ignore.case = TRUE)) {
      best_match <- solution
      break
    }
  }
  
  if (is.null(best_match)) {
    # Generic fallback
    best_match <- list(
      title = "General Error",
      solutions = c(
        "Read the error message carefully for clues",
        "Test your function manually with simple inputs",
        "Use autograder_help() for general guidance",
        "Ask your instructor or classmates for help"
      ),
      code = 'autograder_help()  # See available help topics'
    )
  }
  
  if (verbose) {
    cat("\n", cli_box(paste0("  ", best_match$title)), "\n\n")
    cat("Suggested Solutions:\n")
    cat(sprintf("  %d. %s\n", seq_along(best_match$solutions), best_match$solutions), sep = "")
    if (!is.null(best_match$code)) {
      cat("\nHelpful Code:\n")
      cat("  ", best_match$code, "\n")
    }
    cat("\n")
  }
  
  invisible(best_match)
}

#' Create a simple text box for console output
#' @keywords internal
cli_box <- function(text) {
  width <- nchar(text) + 4
  top_bottom <- paste(rep("=", width), collapse = "")
  paste0(top_bottom, "\n", "| ", text, " |\n", top_bottom)
}

# ============================================================================
# SECTION 2: DOCUMENTATION DISCOVERABILITY
# ============================================================================

#' Autograder Help System
#' 
#' @description
#' Provides quick access to help topics and documentation for the autograder
#' package. Call without arguments to see all available help topics.
#' 
#' @param topic Optional. Specific help topic. Options:
#'   - "getting-started": Quick start guide
#'   - "functions": List of available functions
#'   - "errors": Common errors and solutions
#'   - "workflow": Recommended workflow
#'   - "tips": Tips for success
#'   - NULL: Show all topics
#' 
#' @return Invisibly returns the help content as a character vector.
#' 
#' @examples
#' \dontrun{
#' autograder_help()                    # See all topics
#' autograder_help("getting-started")   # Quick start guide
#' autograder_help("errors")            # Common errors
#' }
#' 
#' @keywords internal
autograder_help <- function(topic = NULL) {
  
  topics <- list(
    "getting-started" = .help_getting_started,
    "functions" = .help_functions,
    "errors" = .help_errors,
    "workflow" = .help_workflow,
    "tips" = .help_tips
  )
  
  if (is.null(topic)) {
    # Show topic list
    cat("\n")
    cat("=== Autograder Help System ===\n\n")
    cat("Available topics:\n")
    cat("  autograder_help('getting-started')  - Quick start guide\n")
    cat("  autograder_help('functions')        - Available functions\n")
    cat("  autograder_help('errors')           - Common errors & solutions\n")
    cat("  autograder_help('workflow')         - Recommended workflow\n")
    cat("  autograder_help('tips')             - Tips for success\n")
    cat("\nOther helpful commands:\n")
    cat("  list_problems()          - See available assignments\n")
    cat("  preview_tests('name')    - Preview test cases\n")
    cat("  autograder_quick_ref()   - Quick reference card\n")
    cat("  autograder_diagnose()    - Diagnose issues\n")
    cat("\n")
    return(invisible(NULL))
  }
  
  if (!topic %in% names(topics)) {
    stop("Unknown topic: '", topic, "'\n",
         "Use autograder_help() to see available topics.", call. = FALSE)
  }
  
  # Call the topic function
  content <- topics[[topic]]()
  cat(content)
  invisible(content)
}

#' @keywords internal
.help_getting_started <- function() {
  "
=== Getting Started with Autograder ===

STEP 1: See available problems
  > list_problems()

STEP 2: Preview test cases
  > preview_tests('fibonacci')

STEP 3: Write your solution
  > student_fibonacci <- function(n) {
  >   # Your implementation here
  > }

STEP 4: Test your solution
  > autograder('fibonacci')

STEP 5: Review feedback and iterate
  - If tests fail, read the feedback carefully

  - Fix issues and run autograder() again

For more help:
  > autograder_help('workflow')
  > autograder_help('tips')

"
}

#' @keywords internal
.help_functions <- function() {
  "
=== Autograder Functions Reference ===

MAIN FUNCTIONS:
  autograder(name)          Run tests for your solution
  list_problems()           List all available problems
  preview_tests(name)       Preview test cases

OPTIONS FOR autograder():
  verbose = TRUE            Show detailed output
  show_hidden = FALSE       Show hidden test results
  show_hints = TRUE         Show hints on failure
  use_parallel = TRUE       Use parallel processing

HELP & DIAGNOSTICS:

  autograder_help()         Show help topics
  autograder_help(topic)    Get help on specific topic
  autograder_quick_ref()    Quick reference card
  autograder_diagnose()     Diagnose common issues
  get_error_solution(msg)   Get solution for error

DEPENDENCY & CACHE:
  autograder_check_dependencies()   Check all dependencies
  cache_info()                      View cache status
  clear_all_caches()                Clear cached data

COMPARISON FUNCTIONS:
  list_comparisons()        List comparison functions
  compare_numeric()         Numeric comparison
  compare_exact()           Exact comparison
  compare_dataframe()       Data frame comparison

PERFORMANCE:
  compare_performance(name) Benchmark your solution

"
}

#' @keywords internal
.help_errors <- function() {
  "
=== Common Errors and Solutions ===

ERROR: 'student_fibonacci' not found
  > Make sure your function is defined in the current session
  > Function name must be: student_<problem_name>
  > Define your function BEFORE calling autograder()

ERROR: Function 'xyz' not found (404)
  > Use list_problems() to see available problems
  > Check spelling (case-sensitive)

ERROR: Type mismatch
  > Check expected return type
  > Use typeof(result) to see your return type
  > Convert with as.numeric(), as.character(), etc.

ERROR: Length mismatch
  > Check loop bounds and edge cases
  > Verify n=0, n=1 handling
  > Use length(result) to check output size

ERROR: Network/connection error
  > Check internet connection
  > Try: curl::has_internet()
  > Wait and retry

ERROR: Package not found
  > Run: autograder_check_dependencies()
  > Reinstall missing packages

For automatic diagnosis:
  > autograder_diagnose()

"
}

#' @keywords internal
.help_workflow <- function() {
  "
=== Recommended Workflow ===

1. EXPLORE
   > list_problems()
   > preview_tests('problem_name')

2. PLAN
   - Read the test descriptions
   - Identify edge cases
   - Plan your approach

3. IMPLEMENT
   > student_problem <- function(...) {
   >   # Start with simple cases
   >   # Build up complexity
   > }

4. TEST MANUALLY
   > student_problem(1)    # Edge case
   > student_problem(5)    # Normal case
   > student_problem(10)   # Larger case

5. RUN AUTOGRADER
   > autograder('problem_name')

6. ITERATE
   - Read feedback carefully
   - Fix one issue at a time
   - Re-run autograder

7. OPTIMIZE (optional)
   > compare_performance('problem_name')

"
}

#' @keywords internal
.help_tips <- function() {
  "
=== Tips for Success ===

TIP 1: Start Simple
  - Test with n=0, n=1 first
  - Build up to complex cases

TIP 2: Read Error Messages
  - They tell you exactly what's wrong
  - Position numbers show where values differ

TIP 3: Handle Edge Cases
  - Empty input (n=0)
  - Single element (n=1)
  - Boundary conditions

TIP 4: Check Your Types
  > typeof(your_result)
  > class(your_result)

TIP 5: Use Verbose Mode
  > autograder('name', verbose = TRUE, show_hints = TRUE)

TIP 6: Test Incrementally
  > student_fib(1)  # Test alone first
  > student_fib(5)  # Then more
  > autograder('fibonacci')  # Finally submit

TIP 7: Save Your Work
  - Keep your solutions in an R script
  - Add comments explaining your logic

TIP 8: Ask for Help
  - Use autograder_help()
  - Check with classmates
  - Ask your instructor

"
}

#' Quick Reference Card
#' 
#' @description
#' Prints a compact quick reference card with the most common commands.
#' 
#' @return Invisibly returns NULL.
#' 
#' @examples
#' \dontrun{
#' autograder_quick_ref()
#' }
#' 
#' @keywords internal
autograder_quick_ref <- function() {
  cat("
+=============================================+
|        AUTOGRADER QUICK REFERENCE           |
+=============================================+
|                                             |
|  DISCOVERY                                  |
|    list_problems()      - See problems      |
|    preview_tests(name)  - Preview tests     |
|                                             |
|  TESTING                                    |
|    autograder(name)     - Grade solution    |
|                                             |
|  HELP                                       |
|    autograder_help()    - Get help          |
|    autograder_diagnose()- Diagnose issues   |
|                                             |
|  NAMING CONVENTION                          |
|    student_<problem>    - Your function     |
|    e.g., student_fibonacci                  |
|                                             |
|  WORKFLOW                                   |
|    1. list_problems()                       |
|    2. preview_tests('name')                 |
|    3. student_name <- function(...) {...}   |
|    4. autograder('name')                    |
|                                             |
+=============================================+
")
  invisible(NULL)
}

# ============================================================================
# SECTION 3: AUTOMATED DEPENDENCY SCANNING
# ============================================================================

#' Check Package Dependencies
#' 
#' @description
#' Scans and validates all package dependencies, reporting any issues.
#' Useful for diagnosing installation problems.
#' 
#' @param fix Logical. If TRUE, attempt to install missing packages. Default: FALSE.
#' @param verbose Logical. If TRUE, show detailed output. Default: TRUE.
#' 
#' @return A list with:
#'   - ok: Logical. TRUE if all dependencies are satisfied.
#'   - missing: Character vector of missing packages.
#'   - outdated: Character vector of outdated packages.
#'   - details: Data frame with full dependency information.
#' 
#' @examples
#' \dontrun{
#' autograder_check_dependencies()
#' autograder_check_dependencies(fix = TRUE)
#' }
#' 
#' @keywords internal
autograder_check_dependencies <- function(fix = FALSE, verbose = TRUE) {
  
  # Get package dependencies from DESCRIPTION
  deps <- list(
    required = c("Rcpp", "curl", "parallel", "memoise", "digest"),
    suggested = c("testthat", "covr", "knitr", "rmarkdown", "openxlsx")
  )
  
  results <- list(
    ok = TRUE,
    missing = character(0),
    outdated = character(0),
    details = data.frame(
      package = character(0),
      required = logical(0),
      installed = logical(0),
      version = character(0),
      status = character(0),
      stringsAsFactors = FALSE
    )
  )
  
  all_deps <- c(deps$required, deps$suggested)
  required_flags <- c(rep(TRUE, length(deps$required)), 
                      rep(FALSE, length(deps$suggested)))
  
  if (verbose) {
    cat("\n=== Autograder Dependency Check ===\n\n")
  }
  
  # Vectorized dependency checking
  installed_vec <- vapply(all_deps, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }, logical(1))
  
  version_vec <- vapply(all_deps, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      as.character(utils::packageVersion(pkg))
    } else {
      ""
    }
  }, character(1))
  
  status_vec <- ifelse(installed_vec, "OK",
                       ifelse(required_flags, "MISSING (required)", "MISSING (optional)"))
  
  # Build results data frame in one operation
  results$details <- data.frame(
    package = all_deps,
    required = required_flags,
    installed = installed_vec,
    version = version_vec,
    status = status_vec,
    stringsAsFactors = FALSE
  )
  
  # Update missing packages
  results$missing <- all_deps[!installed_vec & required_flags]
  results$ok <- length(results$missing) == 0
  
  # Display results if verbose
  if (verbose) {
    icons <- ifelse(installed_vec, "\u2714", 
                    ifelse(required_flags, "\u2718", "-"))
    cat(sprintf("  [%s] %-12s %s\n", icons, all_deps, status_vec), sep = "")
  }
  
  # Check R version
  r_version <- getRversion()
  if (verbose) {
    cat(sprintf("\n  R version: %s\n", r_version))
  }
  
  # Check C++ compiler (for Rcpp)
  if (verbose) {
    has_compiler <- tryCatch({
      # Check if Rtools or compiler is available
      system("where g++", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0 ||
        system("where cl", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
    }, error = function(e) FALSE)
    
    compiler_status <- if (has_compiler) "OK" else "NOT FOUND"
    cat(sprintf("  C++ compiler: %s\n", compiler_status))
  }
  
  if (verbose) {
    cat("\n")
    if (results$ok) {
      cat("\u2714 All required dependencies are installed.\n\n")
    } else {
      cat("\u2718 Missing required packages:", paste(results$missing, collapse = ", "), "\n")
      cat("\nTo install missing packages:\n")
      cat(sprintf("  install.packages(c('%s'))\n\n", 
                  paste(results$missing, collapse = "', '")))
    }
  }
  
  # Attempt to fix if requested
  if (fix && length(results$missing) > 0) {
    if (verbose) cat("Attempting to install missing packages...\n")
    for (pkg in results$missing) {
      tryCatch({
        utils::install.packages(pkg, quiet = TRUE)
        if (verbose) cat(sprintf("  Installed: %s\n", pkg))
      }, error = function(e) {
        if (verbose) cat(sprintf("  Failed to install: %s - %s\n", pkg, e$message))
      })
    }
  }
  
  invisible(results)
}

#' Diagnose Common Issues
#' 
#' @description
#' Runs a comprehensive diagnostic check to identify common issues with
#' the autograder setup.
#' 
#' @return A list with diagnostic results.
#' 
#' @examples
#' \dontrun{
#' autograder_diagnose()
#' }
#' 
#' @keywords internal
autograder_diagnose <- function() {
  cat("\n")
  cat("========================================\n")
  cat("  AUTOGRADER DIAGNOSTIC REPORT\n")
  cat("========================================\n\n")
  
  results <- list()
  
  # 1. Check R version
  cat("1. R Environment\n")
  cat(sprintf("   R version: %s\n", getRversion()))
  cat(sprintf("   Platform: %s\n", R.version$platform))
  results$r_version <- getRversion()
  
  # 2. Check package version
  cat("\n2. Autograder Package\n")
  version <- tryCatch(
    as.character(utils::packageVersion("autograder")),
    error = function(e) "NOT INSTALLED"
  )
  cat(sprintf("   Version: %s\n", version))
  results$autograder_version <- version
  
  # 3. Check dependencies
  cat("\n3. Dependencies\n")
  deps_result <- autograder_check_dependencies(verbose = FALSE)
  if (deps_result$ok) {
    cat("   All required dependencies: OK\n")
  } else {
    cat(sprintf("   Missing packages: %s\n", paste(deps_result$missing, collapse = ", ")))
  }
  results$dependencies <- deps_result
  
  # 4. Check internet connection
  cat("\n4. Network\n")
  has_internet <- tryCatch(
    curl::has_internet(),
    error = function(e) FALSE
  )
  cat(sprintf("   Internet connection: %s\n", if (has_internet) "OK" else "NOT AVAILABLE"))
  results$has_internet <- has_internet
  
  # 5. Check repository access
  cat("\n5. Repository Access\n")
  repo_ok <- tryCatch({
    .cpp_has_internet()
  }, error = function(e) FALSE)
  cat(sprintf("   Repository reachable: %s\n", if (repo_ok) "OK" else "NOT AVAILABLE"))
  results$repo_access <- repo_ok
  
  # 6. Check cache status
  cat("\n6. Cache\n")
  cache <- tryCatch(
    cache_info(),
    error = function(e) list(instructor_cache_size = 0)
  )
  cache_size <- if (!is.null(cache$instructor_cache_size)) cache$instructor_cache_size else 0
  cat(sprintf("   Cache status: %d items cached\n", cache_size))
  results$cache <- cache
  
  # Summary
  cat("\n========================================\n")
  overall <- deps_result$ok && has_internet
  if (overall) {
    cat("  STATUS: All systems operational\n")
  } else {
    cat("  STATUS: Issues detected\n")
    cat("\n  Recommended actions:\n")
    if (!deps_result$ok) {
      cat("    - Run: autograder_check_dependencies(fix = TRUE)\n")
    }
    if (!has_internet) {
      cat("    - Check your internet connection\n")
    }
  }
  cat("========================================\n\n")
  
  invisible(results)
}

#' Check if a function is properly defined
#' 
#' @description
#' Validates that a student function is properly defined and ready for testing.
#' 
#' @param problem_name Character. The problem name (e.g., "fibonacci").
#' 
#' @return Logical. TRUE if the function is properly defined.
#' 
#' @examples
#' \dontrun{
#' student_fibonacci <- function(n) { ... }
#' check_student_function("fibonacci")
#' }
#' 
#' @keywords internal
check_student_function <- function(problem_name) {
  if (!is.character(problem_name) || length(problem_name) != 1) {
    stop("problem_name must be a single character string", call. = FALSE)
  }
  
  fn_name <- paste0("student_", problem_name)
  
  cat(sprintf("\nChecking function: %s\n\n", fn_name))
  
  # Check if exists
  if (!exists(fn_name, envir = .GlobalEnv)) {
    cat("\u2718 Function not found in global environment.\n")
    cat("\n  Solution: Define your function first:\n")
    cat(sprintf("    %s <- function(...) {\n", fn_name))
    cat("      # Your implementation\n")
    cat("    }\n\n")
    return(invisible(FALSE))
  }
  
  fn <- get(fn_name, envir = .GlobalEnv)
  
  # Check if it's a function
  if (!is.function(fn)) {
    cat(sprintf("\u2718 '%s' exists but is not a function.\n", fn_name))
    cat(sprintf("   Current type: %s\n", typeof(fn)))
    cat("\n  Solution: Redefine as a function.\n\n")
    return(invisible(FALSE))
  }
  
  # Get function info
  args <- names(formals(fn))
  cat(sprintf("\u2714 Function found: %s\n", fn_name))
  cat(sprintf("   Arguments: %s\n", 
              if (length(args) == 0) "(none)" else paste(args, collapse = ", ")))
  
  # Quick syntax check
  cat("   Syntax: OK\n")
  cat("\n\u2714 Ready to test with: autograder('", problem_name, "')\n\n", sep = "")
  
  invisible(TRUE)
}
