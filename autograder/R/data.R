# ============================================================================
# AUTOGRADER PACKAGE - DATA FILE LOADING
# ============================================================================
#
# File: data.R
# Purpose: Functions for loading and injecting external data files
#
# Functions:
#   - fetch_data(): Download and load a data file from repository
#   - prefetch_data_files(): Cache all data files before test execution
#   - inject_data_into_inputs(): Replace file references with loaded data
#
# Supported Formats:
#   - CSV: read.csv() -> data.frame
#   - RDS: readRDS() -> any R object
#   - RData/rda: load() -> environment objects
#   - TXT: readLines() -> character vector
#   - XLSX/XLS: readxl::read_excel() -> data.frame
#
# Security:
#   - Filenames validated in C++ (no path traversal)
#   - Only allowed extensions processed
#   - Temporary files cleaned up after loading
#
# ============================================================================

#' Fetch a data file from the repository
#' 
#' @description
#' Downloads a data file from the repository's data/ folder and
#' loads it into R.
#' 
#' Supported Formats:
#'   - **CSV**: Loaded with read.csv() into data.frame
#'   - **RDS**: Loaded with readRDS() (any R object)
#'   - **RData/rda**: Loaded with load() into environment
#'   - **TXT**: Loaded with readLines() into character vector
#'   - **XLSX/XLS**: Loaded with readxl::read_excel() into tibble/data.frame
#' 
#' Security:
#'   - Filename validated in C++ (no path traversal)
#'   - Only allowed extensions: csv, rds, rda, RData, txt, xlsx, xls
#'   - Downloaded to temporary file (cleaned up by R)
#' 
#' @param filename Name of the data file (e.g., "dataset.csv")
#' 
#' @return The loaded data object (type depends on file format)
#' 
#' @examples
#' \dontrun{
#' # Fetch a CSV file
#' df <- fetch_data("sample_data.csv")
#' 
#' # Fetch an RDS file
#' model <- fetch_data("trained_model.rds")
#' }
#' 
#' @keywords internal
fetch_data <- function(filename) {
  # Call C++ to download the file
  temp_path <- tryCatch(
    .cpp_fetch_data_file(filename),
    error = function(e) {
      stop(sprintf("Failed to fetch data file '%s': %s", filename, e$message),
           call. = FALSE)
    }
  )
  
  # Get file extension
  ext <- tolower(tools::file_ext(filename))
  
  # Load based on extension
  result <- switch(ext,
    "csv" = utils::read.csv(temp_path, stringsAsFactors = FALSE),
    "rds" = readRDS(temp_path),
    "rdata" = , "rda" = {
      env <- new.env()
      load(temp_path, envir = env)
      # Return first object in environment
      objs <- ls(env)
      if (length(objs) == 1) {
        get(objs[1], envir = env)
      } else {
        # Return list of all objects
        mget(objs, envir = env)
      }
    },
    "txt" = readLines(temp_path),
    "xlsx" = , "xls" = {
      # Check if readxl is available
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required to read Excel files. Install with: install.packages('readxl')",
             call. = FALSE)
      }
      # Read Excel file and convert to data.frame
      as.data.frame(readxl::read_excel(temp_path))
    },
    stop(sprintf("Unsupported data file format: %s", ext), call. = FALSE)
  )
  
  # Clean up temp file
  unlink(temp_path)
  
  result
}

#' Prefetch and cache data files for test cases
#' 
#' @description
#' Downloads all data files specified in test_data$data_files and
#' stores them in a named list for quick access during test execution.
#' 
#' @param test_data Validated test case data
#' 
#' @return Named list of loaded data objects (keyed by filename)
#' 
#' @details
#' This function is called once before running tests to avoid
#' downloading the same file multiple times. The returned cache
#' is passed to the test runner.
#' 
#' @keywords internal
prefetch_data_files <- function(test_data) {
  # Check if data_files field exists
  if (is.null(test_data$data_files) || length(test_data$data_files) == 0) {
    return(list())
  }
  
  data_cache <- list()
  
  for (filename in test_data$data_files) {
    cat(sprintf("  Loading data: %s...\n", filename))
    data_cache[[filename]] <- fetch_data(filename)
  }
  
  data_cache
}

#' Inject data into test case inputs
#' 
#' @description
#' Replaces data file references in test inputs with actual loaded data.
#' 
#' @param input_args List of arguments for a single test case
#' @param data_cache Named list of loaded data objects
#' 
#' @return Modified input_args with data objects injected
#' 
#' @details
#' Data can be specified in two ways:
#'   1. As a named `data` argument: list(data = "file.csv")
#'   2. As any argument value: list(df = "file.csv")
#' 
#' The function checks each argument to see if it's a string that
#' matches a cached data filename, and replaces it with the loaded data.
#' 
#' @keywords internal
inject_data_into_inputs <- function(input_args, data_cache) {
  if (length(data_cache) == 0) {
    return(input_args)
  }
  
  # Process each argument
  for (arg_name in names(input_args)) {
    arg_value <- input_args[[arg_name]]
    
    # Check if this argument is a data file reference
    if (is.character(arg_value) && length(arg_value) == 1) {
      if (arg_value %in% names(data_cache)) {
        # Replace with loaded data
        input_args[[arg_name]] <- data_cache[[arg_value]]
      }
    }
  }
  
  input_args
}

# ============================================================================
# END OF FILE
# ============================================================================
