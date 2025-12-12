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
# Security:
#   - Rate limiting to prevent API abuse
#   - Code validation before execution
#   - Audit logging of fetch operations
#
# ============================================================================

# Session-level cache for instructor code (avoids redundant network calls)
.instructor_cache <- new.env(parent = emptyenv())

# Curl handle pool for connection reuse
.curl_pool <- new.env(parent = emptyenv())

#' Get a reusable curl handle from the pool
#' 
#' @description
#' Returns a persistent curl handle for connection reuse.
#' This improves performance by avoiding connection setup overhead.
#' 
#' @return A curl handle configured with optimal settings
#' 
#' @keywords internal
get_curl_handle <- function() {
  if (is.null(.curl_pool$handle)) {
    .curl_pool$handle <- curl::new_handle()
    curl::handle_setopt(.curl_pool$handle,
      connecttimeout = 10,
      timeout = 30,
      followlocation = TRUE,
      ssl_verifypeer = TRUE,
      tcp_keepalive = TRUE,
      tcp_keepidle = 60,
      tcp_keepintvl = 30
    )
  }
  .curl_pool$handle
}

#' Reset the curl handle pool
#' 
#' @description
#' Resets the curl handle, useful if connection state becomes invalid.
#' 
#' @return Invisible NULL
#' 
#' @keywords internal
reset_curl_pool <- function() {
  .curl_pool$handle <- NULL
  invisible(NULL)
}

#' Clear the instructor code cache
#' 
#' @description
#' Clears the in-session cache of fetched instructor code.
#' Useful when instructor updates test cases and you want to refetch.
#' 
#' @return Invisible NULL
#' 
#' @keywords internal
clear_instructor_cache <- function() {
  rm(list = ls(.instructor_cache), envir = .instructor_cache)
  invisible(NULL)
}

#' Securely fetch and load instructor code from repository
#' 
#' @description
#' Downloads instructor's reference implementation and test cases from
#' GitHub repository with comprehensive error handling. Results are
#' cached in-session to avoid redundant network calls.
#' 
#' Security Measures:
#'   1. Input validation via C++ (prevents path traversal)
#'   2. HTTPS transport (encrypted in transit)
#'   3. Error message sanitization (no path exposure)
#'   4. Timeout handling (30 seconds)
#' 
#' @param function_name Name of function to fetch (validated in C++)
#' @param use_cache Logical. Use in-session cache? (default: TRUE)
#'   Set to FALSE to force re-download (useful if instructor updated tests)
#' 
#' @return Environment containing:
#'   - Instructor's function implementation
#'   - test_cases list with all test data
#' 
#' @details
#' Workflow:
#'   1. Check in-session cache first
#'   2. If not cached, call C++ function to download code
#'   3. Create new isolated environment
#'   4. Parse and evaluate code in that environment
#'   5. Cache result for future calls
#'   6. Return environment for extraction
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
#' @seealso \code{\link{clear_instructor_cache}} to clear cached data
#' 
#' @keywords internal
fetch_instructor_code <- function(function_name, use_cache = TRUE, max_retries = autograder_max_retries()) {
  # Check rate limit before making API call
  check_rate_limit("fetch_instructor_code")
  
  # Log fetch attempt

  log_security_event("fetch_attempt", list(function_name = function_name))
  
  # Check cache first (avoids redundant network calls)
  if (use_cache && exists(function_name, envir = .instructor_cache)) {
    log_security_event("fetch_cache_hit", list(function_name = function_name))
    return(get(function_name, envir = .instructor_cache))
  }
  
  # Retry logic with exponential backoff for network resilience
  last_error <- NULL
  last_warnings <- character(0)
  
  for (attempt in seq_len(max_retries)) {
    # Capture warnings along with errors to detect 404s
    result <- tryCatch(
      withCallingHandlers({
        # Call C++ function for secure download
        # C++ handles: validation, URL building, download, content verification
        code <- .cpp_fetch_function_content(function_name)
        
        # Collapse character vector to single string if needed
        # (C++ returns lines as vector, validation expects single string)
        if (length(code) > 1) {
          code <- paste(code, collapse = "\n")
        }
        
        # SECURITY: Validate code before execution
        validate_code_safety(code, strict = getOption("autograder.strict_validation", TRUE))
        
        # Log successful validation
        log_security_event("code_validated", list(
          function_name = function_name,
          code_length = nchar(code)
        ))
        
        # Create isolated environment for instructor code
        # This prevents pollution of global environment
        env <- new.env()
        
        # Parse and evaluate code in isolated environment
        # This loads the instructor function and test_cases
        eval(parse(text = code), envir = env)
        
        # Cache the result for future calls
        assign(function_name, env, envir = .instructor_cache)
        
        # Log successful fetch
        log_security_event("fetch_success", list(function_name = function_name))
        
        # Return environment containing loaded code
        return(env)
        
      }, warning = function(w) {
        # Capture warnings (e.g., "HTTP status was '404 Not Found'")
        last_warnings <<- c(last_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
      error = function(e) {
        last_error <<- e
        
        # Check if this is a 404 from captured warnings or error message
        all_messages <- c(e$message, last_warnings)
        is_404 <- any(grepl("404|not found|Function .+ not found", all_messages, ignore.case = TRUE))
        
        # Don't retry validation errors (not transient)
        if (grepl("Invalid function name|Invalid character|unsafe|contains null", e$message, ignore.case = TRUE)) {
          stop("Invalid function name format. Use only letters, numbers, underscores, and hyphens.",
               call. = FALSE)
        }
        
        # Don't retry 404 errors (function doesn't exist)
        if (is_404) {
          stop(function_not_found_error(function_name))
        }
        
        # Network errors - retry with backoff
        if (attempt < max_retries) {
          wait_time <- 2^(attempt - 1)  # 1, 2, 4 seconds
          message(sprintf("Network error, retrying in %d seconds... (attempt %d/%d)", 
                          wait_time, attempt, max_retries))
          Sys.sleep(wait_time)
        }
        
        NULL  # Signal to continue loop
      }
    )
    
    # Reset warnings for next attempt
    last_warnings <- character(0)
    
    # If we got a result, return it
    if (!is.null(result)) {
      return(result)
    }
  }
  
  # All retries exhausted
  stop(network_error(
    "Unable to connect to the test server after multiple attempts. Please check your internet connection and try again."
  ))
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
