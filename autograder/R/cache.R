# ============================================================================
# AUTOGRADER PACKAGE - MEMOIZATION CACHING
# ============================================================================
#
# File: cache.R
# Purpose: Memoise-based caching for expensive operations
#
# This module provides:
#   - Automatic caching of network requests
#   - Time-based cache expiration
#   - Manual cache clearing
#
# Author: Reijel Agub (rcagub@up.edu.ph)
# Version: 0.4.0
# License: MIT
#
# ============================================================================

# Private environment to store memoised functions
.memoise_cache <- new.env(parent = emptyenv())

#' Initialize memoise cache
#' 
#' @description
#' Sets up memoised versions of expensive functions.
#' Called during package load.
#' 
#' @keywords internal
.init_memoise_cache <- function() {
  # Get cache timeout from options (default: 1 hour)
  cache_timeout <- getOption("autograder.cache_timeout", default = 3600)
  
  # Create a file-system cache for persistence across sessions
  cache_dir <- file.path(tempdir(), "autograder_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # Memoise the problems list fetcher
  .memoise_cache$fetch_problems <- memoise::memoise(
    function() {
      .cpp_fetch_problems_list()
    },
    cache = memoise::cache_filesystem(cache_dir),
    ~memoise::timeout(cache_timeout)
  )
  
  # Memoise the data file fetcher
  .memoise_cache$fetch_data_file <- memoise::memoise(
    function(filename) {
      .cpp_fetch_data_file(filename)
    },
    cache = memoise::cache_filesystem(cache_dir),
    ~memoise::timeout(cache_timeout)
  )
  
  invisible(TRUE)
}

#' Get memoised problems list fetcher
#' @keywords internal
.get_memoised_fetch_problems <- function() {
  if (exists("fetch_problems", envir = .memoise_cache)) {
    return(.memoise_cache$fetch_problems)
  }
  # Fallback to direct call if memoise not initialized
  .cpp_fetch_problems_list
}

#' Get memoised data file fetcher
#' @keywords internal
.get_memoised_fetch_data <- function() {
  if (exists("fetch_data_file", envir = .memoise_cache)) {
    return(.memoise_cache$fetch_data_file)
  }
  # Fallback to direct call if memoise not initialized
  .cpp_fetch_data_file
}

#' Clear all caches
#' 
#' @description
#' Clears all cached data including:
#' - Memoised network results
#' - In-session instructor code cache
#' 
#' Use this when:
#' - Instructor updates test cases
#' - You want to force fresh data
#' - Debugging cache-related issues
#' 
#' @return Invisibly returns TRUE on success.
#' 
#' @examples
#' \dontrun{
#' # Clear all caches and refetch everything
#' clear_all_caches()
#' autograder("fibonacci")
#' }
#' 
#' @keywords internal
clear_all_caches <- function() {
  # Clear instructor code cache
  clear_instructor_cache()
  
  # Clear memoise caches
  if (exists("fetch_problems", envir = .memoise_cache)) {
    memoise::forget(.memoise_cache$fetch_problems)
  }
  
  if (exists("fetch_data_file", envir = .memoise_cache)) {
    memoise::forget(.memoise_cache$fetch_data_file)
  }
  
  # Clear filesystem cache
  cache_dir <- file.path(tempdir(), "autograder_cache")
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    dir.create(cache_dir, recursive = TRUE)
  }
  
  message("All caches cleared.")
  invisible(TRUE)
}

#' Get cache statistics
#' 
#' @description
#' Returns information about the current cache state.
#' 
#' @return A list with cache statistics:
#' \itemize{
#'   \item \code{instructor_cache_size}: Number of cached instructor environments
#'   \item \code{cache_dir}: Path to filesystem cache
#'   \item \code{cache_dir_size}: Size of cache directory in bytes
#'   \item \code{timeout}: Cache timeout in seconds
#' }
#' 
#' @examples
#' \dontrun{
#' cache_info()
#' }
#' 
#' @keywords internal
cache_info <- function() {
  cache_dir <- file.path(tempdir(), "autograder_cache")
  
  # Calculate cache directory size
  cache_size <- 0
  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE)
    cache_size <- sum(file.info(files)$size, na.rm = TRUE)
  }
  
  list(
    instructor_cache_size = length(ls(.instructor_cache)),
    cache_dir = cache_dir,
    cache_dir_size_bytes = cache_size,
    cache_timeout_seconds = getOption("autograder.cache_timeout", 3600)
  )
}

#' Set cache timeout
#' 
#' @description
#' Sets the timeout for cached data. After this time, cached data
#' will be refreshed on next access.
#' 
#' @param seconds Numeric. Timeout in seconds. Use Inf for no expiration.
#' 
#' @return Invisibly returns the previous timeout value.
#' 
#' @examples
#' \dontrun{
#' # Set cache to expire after 5 minutes
#' set_cache_timeout(300)
#' 
#' # Disable cache expiration
#' set_cache_timeout(Inf)
#' }
#' 
#' @keywords internal
set_cache_timeout <- function(seconds) {
  if (!is.numeric(seconds) || length(seconds) != 1 || seconds < 0) {
    stop("'seconds' must be a non-negative number", call. = FALSE)
  }
  
  old_value <- getOption("autograder.cache_timeout", 3600)
  options(autograder.cache_timeout = seconds)
  
  # Re-initialize memoise with new timeout
  .init_memoise_cache()
  
  invisible(old_value)
}
