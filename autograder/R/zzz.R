# ============================================================================
# AUTOGRADER PACKAGE - PACKAGE STARTUP
# ============================================================================
#
# File: zzz.R (loaded last alphabetically)
# Purpose: Package initialization and startup message
#
# The 'zzz.R' naming convention is used for:
#   - .onLoad hooks (run when package is loaded)
#   - .onAttach hooks (run when package is attached)
#   - .onUnload hooks (run when package is unloaded)
#
# This file is named zzz.R so it loads last alphabetically,
# ensuring all other package components are available.
#
# ============================================================================

#' @useDynLib autograder, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom graphics boxplot mtext
#' @importFrom stats median sd
#' @importFrom utils head tail txtProgressBar setTxtProgressBar
NULL

# ============================================================================
# PACKAGE OPTIONS
# ============================================================================

#' Get default tolerance for numeric comparisons
#' 
#' @description
#' Returns the default tolerance used for comparing numeric values.
#' Can be configured via `options(autograder.tolerance = value)`.
#' 
#' @return Numeric tolerance value (default: 1e-10)
#' 
#' @examples
#' # Get current default tolerance
#' autograder_tolerance()
#' 
#' # Set custom tolerance
#' options(autograder.tolerance = 1e-8)
#' autograder_tolerance()  # Returns 1e-8
#' 
#' @export
autograder_tolerance <- function() {
  getOption("autograder.tolerance", default = 1e-10)
}

#' Get default max retries for network operations
#' 
#' @description
#' Returns the maximum number of retry attempts for network operations.
#' Can be configured via `options(autograder.max_retries = value)`.
#' 
#' @return Integer number of retries (default: 3)
#' 
#' @keywords internal
autograder_max_retries <- function() {
  as.integer(getOption("autograder.max_retries", default = 3L))
}

#' Package load hook
#' 
#' @description
#' Sets default package options when the package is loaded.
#' Initializes memoise cache and registers built-in comparison functions.
#' 
#' @param libname Library name (passed by R)
#' @param pkgname Package name (passed by R)
#' 
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set default options if not already set
  op <- options()
  op.autograder <- list(
    autograder.tolerance = 1e-10,
    autograder.max_retries = 3L,
    autograder.cache_timeout = 3600  # 1 hour cache timeout
  )
  toset <- !(names(op.autograder) %in% names(op))
  if (any(toset)) options(op.autograder[toset])
  
  # Register built-in comparison functions
  .register_builtin_comparisons()
  
  # Initialize memoised functions for network caching
  .init_memoise_cache()
  
  invisible()
}

#' Package startup message
#' 
#' @description
#' Displays welcome message when package is loaded via library() or require().
#' 
#' @param libname Library name (passed by R)
#' @param pkgname Package name (passed by R)
#' 
#' @details
#' Message includes:
#'   - Package version
#'   - Quick start instructions (list_problems, preview_tests, autograder)
#'   - New features in this version
#' 
#' Uses packageStartupMessage() which can be suppressed with:
#'   suppressPackageStartupMessages(library(autograder))
#' 
#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Get current package version
  version <- utils::packageVersion("autograder")
  
  # Display friendly startup message
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
