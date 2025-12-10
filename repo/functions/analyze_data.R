# ============================================================================
# ANALYZE DATA - Function that requires external data files
# ============================================================================
#
# This function demonstrates the data loading capability of the autograder.
# It can analyze different types of data from various file formats.
#
# ============================================================================

#' Analyze Data
#' 
#' Performs various analyses on data depending on the type parameter.
#' 
#' @param data A data.frame or list containing the data to analyze
#' @param type Character. Type of analysis to perform:
#'   - "mean": Calculate mean of numeric columns
#'   - "sum": Calculate sum of numeric columns  
#'   - "count": Count number of rows
#'   - "summary": Return summary statistics
#' 
#' @return Depends on analysis type:
#'   - "mean": Named numeric vector of column means
#'   - "sum": Named numeric vector of column sums
#'   - "count": Single integer (number of rows)
#'   - "summary": Named list with min, max, mean for each numeric column
#'
#' @examples
#' df <- data.frame(a = 1:5, b = 6:10)
#' analyze_data(df, "mean")   # Returns c(a = 3, b = 8)
#' analyze_data(df, "count")  # Returns 5
#'
analyze_data <- function(data, type = "mean") {
  
  # Handle list input (from RData files with multiple objects)
  if (is.list(data) && !is.data.frame(data)) {
    # If it's a list with a data component, use that
    if ("data" %in% names(data)) {
      data <- data$data
    } else if (length(data) == 1) {
      data <- data[[1]]
    }
  }
  
  # Convert to data.frame if needed
  if (!is.data.frame(data)) {
    if (is.numeric(data)) {
      # Numeric vector - wrap in data.frame
      data <- data.frame(value = data)
    } else {
      stop("Input must be a data.frame or numeric vector", call. = FALSE)
    }
  }
  
  # Get numeric columns only
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  if (ncol(numeric_data) == 0) {
    stop("No numeric columns found in data", call. = FALSE)
  }
  
  # Perform analysis based on type
  result <- switch(type,
    "mean" = {
      sapply(numeric_data, mean, na.rm = TRUE)
    },
    "sum" = {
      sapply(numeric_data, sum, na.rm = TRUE)
    },
    "count" = {
      nrow(data)
    },
    "summary" = {
      lapply(numeric_data, function(x) {
        list(
          min = min(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          mean = mean(x, na.rm = TRUE)
        )
      })
    },
    stop(sprintf("Unknown analysis type: %s", type), call. = FALSE)
  )
  
  result
}

# ============================================================================
# TEST CASES
# ============================================================================
#
# These test cases use external data files stored in the data/ folder.
# The autograder will automatically fetch and load these files.
#
# ============================================================================

test_cases <- list(
  
  # List of data files to prefetch
  data_files = c(
    "sample_scores.csv",
    "measurements.rds",
    "inventory.xlsx"
  ),
  
  inputs = list(
    # Test 1: CSV file - calculate mean scores
    list(data = "sample_scores.csv", type = "mean"),
    
    # Test 2: CSV file - count students
    list(data = "sample_scores.csv", type = "count"),
    
    # Test 3: CSV file - sum of scores
    list(data = "sample_scores.csv", type = "sum"),
    
    # Test 4: RDS file - mean of measurements
    list(data = "measurements.rds", type = "mean"),
    
    # Test 5: RDS file - summary statistics
    list(data = "measurements.rds", type = "summary"),
    
    # Test 6: Excel file - inventory analysis
    list(data = "inventory.xlsx", type = "sum"),
    
    # Test 7: Excel file - count items
    list(data = "inventory.xlsx", type = "count"),
    
    # Test 8: Direct data input (no file)
    list(data = data.frame(x = 1:10, y = 11:20), type = "mean")
  ),
  
  descriptions = c(
    "CSV: Mean of student scores",
    "CSV: Count of students",
    "CSV: Sum of all scores",
    "RDS: Mean of measurements",
    "RDS: Summary statistics",
    "Excel: Sum of inventory values",
    "Excel: Count inventory items",
    "Direct: Mean of simple data"
  ),
  
  hidden = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
  
  points = c(2, 1, 2, 2, 3, 2, 1, 1),
  
  tolerance = 1e-6,
  
  hints = c(
    "Check that you're calculating the mean of numeric columns only",
    "Use nrow() to count rows in a data.frame",
    "Use sum() on each numeric column",
    "RDS files load directly as data.frames",
    "Return a list with min, max, mean for each column",
    "Excel files are loaded as data.frames",
    "Count works the same for all file types",
    "Handle direct data.frame input"
  )
)
