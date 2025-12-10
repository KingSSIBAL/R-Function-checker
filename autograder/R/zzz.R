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
