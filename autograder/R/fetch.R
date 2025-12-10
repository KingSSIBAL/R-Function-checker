# ============================================================================
# AUTOGRADER PACKAGE - FETCH INSTRUCTOR CODE
# ============================================================================
#
# File: fetch.R
# Purpose: Download and extract instructor code from repository
#
# This function is used by both autograder() and preview_tests() to fetch
# instructor code. Centralizing this logic:
#   - Reduces duplication (DRY principle)
#   - Ensures consistent error handling
#   - Makes security updates easier
#   - Simplifies testing
#
# ============================================================================

#' Securely fetch and load instructor code from repository
#' 
#' @description
#' Downloads instructor's reference implementation and test cases from
#' GitHub repository with comprehensive error handling.
#' 
#' Security Measures:
#'   1. Input validation via C++ (prevents path traversal)
#'   2. HTTPS transport (encrypted in transit)
#'   3. Error message sanitization (no path exposure)
#'   4. Timeout handling (30 seconds)
#' 
#' @param function_name Name of function to fetch (validated in C++)
#' 
#' @return Environment containing:
#'   - Instructor's function implementation
#'   - test_cases list with all test data
#' 
#' @details
#' Workflow:
#'   1. Call C++ function to download code
#'   2. Create new isolated environment
#'   3. Parse and evaluate code in that environment
#'   4. Return environment for extraction
#' 
#' Error Handling:
#'   - Invalid name → InvalidInputError
#'   - Network issue → network_error
#'   - 404 error → function_not_found_error
#'   - Other → generic error
#' 
#' @section Side Effects:
#'   Creates temporary file (automatically cleaned up by R)
#' 
#' @keywords internal
fetch_instructor_code <- function(function_name) {
  tryCatch({
    # Call C++ function for secure download
    # C++ handles: validation, URL building, download, content verification
    code <- .cpp_fetch_function_content(function_name)
    
    # Create isolated environment for instructor code
    # This prevents pollution of global environment
    env <- new.env()
    
    # Parse and evaluate code in isolated environment
    # This loads the instructor function and test_cases
    eval(parse(text = code), envir = env)
    
    # Return environment containing loaded code
    env
    
  }, error = function(e) {
    # ===== ERROR CLASSIFICATION AND MESSAGING =====
    
    # Path traversal or invalid characters (various validation error messages)
    if (grepl("Invalid function name|Invalid character|unsafe|contains null", e$message, ignore.case = TRUE)) {
      stop("Invalid function name format. Use only letters, numbers, underscores, and hyphens.",
           call. = FALSE)
    } 
    # Network/connection issues
    else if (grepl("Network error", e$message)) {
      stop(network_error(
        "Unable to connect to the test server. Please check your internet connection and try again."
      ))
    } 
    # Function doesn't exist (404)
    else {
      stop(function_not_found_error(function_name))
    }
  })
}

#' Extract instructor function from loaded environment
#' 
#' @description
#' Searches environment for the first function object. Instructor code
#' typically defines one function, which this extracts.
#' 
#' Search Strategy:
#'   - Iterate through all objects in environment
#'   - Return first object that is.function() == TRUE
#'   - Error if no function found
#' 
#' @param instructor_env Environment loaded from fetch_instructor_code()
#' @param function_name Function name (for error messages)
#' 
#' @return The instructor's function object
#' 
#' @details
#' Why search instead of direct access?
#'   - Function name might differ from file name
#'   - Allows flexibility in instructor code structure
#'   - Handles helper functions gracefully
#' 
#' @keywords internal
extract_instructor_function <- function(instructor_env, function_name) {
  instructor_fun <- NULL
  
  # Search through all objects in environment
  for (name in ls(instructor_env)) {
    obj <- get(name, envir = instructor_env)
    
    # Take the first function found
    if (is.function(obj)) {
      instructor_fun <- obj
      break  # Found it, stop searching
    }
  }
  
  # Validate we found a function
  if (is.null(instructor_fun)) {
    stop(sprintf("No function implementation found for '%s'. Contact instructor.", 
                function_name),
         call. = FALSE)
  }
  
  instructor_fun
}

#' Extract and validate test cases from environment
#' 
#' @description
#' Retrieves test_cases object from instructor environment and validates
#' its structure using validate_test_cases().
#' 
#' Expected Structure:
#'   test_cases <- list(
#'     inputs = list(...),         # Required
#'     descriptions = c(...),      # Optional
#'     hidden = c(...),            # Optional
#'     points = c(...),            # Optional
#'     tolerance = 1e-10,          # Optional
#'     expected_type = "numeric",  # Optional
#'     hints = c(...),             # Optional
#'     comparison_fn = function()  # Optional
#'   )
#' 
#' @param instructor_env Environment loaded from fetch_instructor_code()
#' @param function_name Function name (for error messages)
#' 
#' @return Validated test_data list with all fields normalized
#' 
#' @keywords internal
extract_test_cases <- function(instructor_env, function_name) {
  # Check if test_cases exists
  if (!exists("test_cases", envir = instructor_env)) {
    stop(sprintf("No test cases found for '%s'. Contact instructor.", function_name),
         call. = FALSE)
  }
  
  # Extract and validate
  test_data <- get("test_cases", envir = instructor_env)
  validate_test_cases(test_data, function_name)
}
