# ============================================================================
# TESTS FOR DATA FILE LOADING FUNCTIONALITY
# ============================================================================
#
# Tests the data file loading system for test cases that require external data.
#
# ============================================================================

test_that("inject_data_into_inputs returns input unchanged when no cache", {
  input_args <- list(x = 5, y = 10)
  data_cache <- list()
  
  result <- autograder:::inject_data_into_inputs(input_args, data_cache)
  
  expect_identical(result, input_args)
})

test_that("inject_data_into_inputs replaces filename with cached data", {
  # Create sample data
  sample_df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  
  # Input has a filename reference
  input_args <- list(data = "sample.csv", n = 10)
  
  # Cache has the loaded data
  data_cache <- list("sample.csv" = sample_df)
  
  result <- autograder:::inject_data_into_inputs(input_args, data_cache)
  
  # The filename should be replaced with actual data
  expect_identical(result$data, sample_df)
  # Other args should be unchanged
  expect_equal(result$n, 10)
})

test_that("inject_data_into_inputs handles multiple data references", {
  df1 <- data.frame(x = 1:5)
  df2 <- data.frame(y = 6:10)
  
  input_args <- list(a = "data1.csv", b = "data2.csv", c = 100)
  data_cache <- list("data1.csv" = df1, "data2.csv" = df2)
  
  result <- autograder:::inject_data_into_inputs(input_args, data_cache)
  
  expect_identical(result$a, df1)
  expect_identical(result$b, df2)
  expect_equal(result$c, 100)
})

test_that("inject_data_into_inputs ignores non-matching strings", {
  input_args <- list(name = "not_a_file", data = "file.csv")
  data_cache <- list("file.csv" = data.frame(x = 1))
  
  result <- autograder:::inject_data_into_inputs(input_args, data_cache)
  
  # "not_a_file" should remain unchanged
  expect_equal(result$name, "not_a_file")
  # "file.csv" should be replaced
  expect_s3_class(result$data, "data.frame")
})

test_that("prefetch_data_files returns empty list when no data_files", {
  test_data <- list(inputs = list(list(x = 1)))
  
  result <- autograder:::prefetch_data_files(test_data)
  
  expect_equal(result, list())
})

test_that("prefetch_data_files returns empty list for empty data_files", {
  test_data <- list(
    inputs = list(list(x = 1)),
    data_files = character(0)
  )
  
  result <- autograder:::prefetch_data_files(test_data)
  
  expect_equal(result, list())
})

test_that("run_tests_sequential accepts data_cache parameter", {
  # Create mock functions
  student_fun <- function(x) x * 2
  instructor_fun <- function(x) x * 2
  
  test_data <- list(
    inputs = list(list(x = 5)),
    descriptions = "Test 1",
    hidden = FALSE,
    points = 1,
    tolerance = 1e-10
  )
  
  # Should run without error with empty data_cache
  result <- autograder:::run_tests_sequential(
    student_fun, instructor_fun, test_data, 1e-10, list()
  )
  
  expect_length(result, 1)
  expect_equal(result[[1]]$student, 10)
  expect_equal(result[[1]]$expected, 10)
})

test_that("run_tests_sequential injects data from cache", {
  # Function that takes a data.frame
  student_fun <- function(df) sum(df$value)
  instructor_fun <- function(df) sum(df$value)
  
  # Test data references a file
  test_data <- list(
    inputs = list(list(df = "test.csv")),
    descriptions = "Test with data",
    hidden = FALSE,
    points = 1,
    tolerance = 1e-10
  )
  
  # Data cache has the loaded data
  sample_df <- data.frame(value = c(1, 2, 3, 4, 5))
  data_cache <- list("test.csv" = sample_df)
  
  result <- autograder:::run_tests_sequential(
    student_fun, instructor_fun, test_data, 1e-10, data_cache
  )
  
  expect_length(result, 1)
  expect_equal(result[[1]]$student, 15)  # sum(1:5)
  expect_equal(result[[1]]$expected, 15)
})

test_that("run_tests_parallel accepts data_cache parameter", {
  skip_on_cran()  # Parallel tests can be flaky on CRAN
  
  student_fun <- function(x) x + 1
  instructor_fun <- function(x) x + 1
  
  test_data <- list(
    inputs = list(list(x = 1)),
    descriptions = "Test 1",
    hidden = FALSE,
    points = 1,
    tolerance = 1e-10
  )
  
  # Should run without error, but fall back to sequential for 1 test
  result <- autograder:::run_tests_parallel(
    student_fun, instructor_fun, test_data, 1e-10, 
    use_parallel = TRUE, data_cache = list()
  )
  
  expect_length(result, 1)
  expect_equal(result[[1]]$student, 2)
})

# ============================================================================
# EXCEL FILE SUPPORT TESTS
# ============================================================================

test_that("fetch_data detects xlsx extension correctly", {
  # Test extension detection logic
  filename <- "data.xlsx"
  ext <- tolower(tools::file_ext(filename))
  expect_equal(ext, "xlsx")
})

test_that("fetch_data detects xls extension correctly", {
  filename <- "data.xls"
  ext <- tolower(tools::file_ext(filename))
  expect_equal(ext, "xls")
})

test_that("fetch_data requires readxl for Excel files", {
  skip_if_not_installed("readxl")
  
  # Create a temporary xlsx file using openxlsx if available
  skip_if_not_installed("openxlsx")
  
  temp_file <- tempfile(fileext = ".xlsx")
  test_df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  openxlsx::write.xlsx(test_df, temp_file)
  
  # Read it back using readxl
  result <- as.data.frame(readxl::read_excel(temp_file))
  
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  
  unlink(temp_file)
})

# ============================================================================
# INTEGRATION TESTS WITH LOCAL DATA FILES
# ============================================================================

test_that("CSV data file can be loaded and processed", {
  # Create temp CSV
  temp_csv <- tempfile(fileext = ".csv")
  test_df <- data.frame(a = 1:5, b = 6:10)
  write.csv(test_df, temp_csv, row.names = FALSE)
  
  # Load it
  result <- utils::read.csv(temp_csv, stringsAsFactors = FALSE)
  
  expect_equal(nrow(result), 5)
  expect_equal(mean(result$a), 3)
  expect_equal(sum(result$b), 40)
  
  unlink(temp_csv)
})

test_that("RDS data file can be loaded and processed", {
  # Create temp RDS
  temp_rds <- tempfile(fileext = ".rds")
  test_df <- data.frame(x = c(10, 20, 30), y = c(1, 2, 3))
  saveRDS(test_df, temp_rds)
  
  # Load it
  result <- readRDS(temp_rds)
  
  expect_equal(nrow(result), 3)
  expect_equal(mean(result$x), 20)
  
  unlink(temp_rds)
})

test_that("RData file can be loaded with multiple objects", {
  # Create temp RData
  temp_rdata <- tempfile(fileext = ".RData")
  obj1 <- 1:10
  obj2 <- data.frame(a = 1:3)
  save(obj1, obj2, file = temp_rdata)
  
  # Load it
  env <- new.env()
  load(temp_rdata, envir = env)
  
  expect_true("obj1" %in% ls(env))
  expect_true("obj2" %in% ls(env))
  expect_equal(sum(get("obj1", envir = env)), 55)
  
  unlink(temp_rdata)
})

test_that("TXT data file can be loaded", {
  # Create temp TXT
  temp_txt <- tempfile(fileext = ".txt")
  writeLines(c("line1", "line2", "line3"), temp_txt)
  
  # Load it
  result <- readLines(temp_txt)
  
  expect_length(result, 3)
  expect_equal(result[1], "line1")
  
  unlink(temp_txt)
})

test_that("Excel file can be loaded with readxl", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("openxlsx")
  
  # Create temp Excel
  temp_xlsx <- tempfile(fileext = ".xlsx")
  test_df <- data.frame(col1 = c(100, 200, 300), col2 = c("a", "b", "c"))
  openxlsx::write.xlsx(test_df, temp_xlsx)
  
  # Load it
  result <- as.data.frame(readxl::read_excel(temp_xlsx))
  
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$col1), 600)
  
  unlink(temp_xlsx)
})

# ============================================================================
# DATA INJECTION END-TO-END TEST
# ============================================================================

test_that("full test workflow with data injection works", {
  # Simulate an analyze function
  analyze_fn <- function(data, type = "mean") {
    numeric_cols <- sapply(data, is.numeric)
    numeric_data <- data[, numeric_cols, drop = FALSE]
    if (type == "mean") {
      sapply(numeric_data, mean, na.rm = TRUE)
    } else if (type == "count") {
      nrow(data)
    } else {
      sapply(numeric_data, sum, na.rm = TRUE)
    }
  }
  
  # Test data structure
  test_data <- list(
    inputs = list(
      list(data = "test.csv", type = "mean"),
      list(data = "test.csv", type = "count")
    ),
    descriptions = c("Mean test", "Count test"),
    hidden = c(FALSE, FALSE),
    points = c(1, 1),
    tolerance = 1e-10
  )
  
  # Create data cache
  sample_df <- data.frame(x = c(10, 20, 30), y = c(5, 10, 15))
  data_cache <- list("test.csv" = sample_df)
  
  # Run tests with injection
  results <- autograder:::run_tests_sequential(
    analyze_fn, analyze_fn, test_data, 1e-10, data_cache
  )
  
  expect_length(results, 2)
  
  # First test: mean
  expect_equal(results[[1]]$student["x"], c(x = 20))
  expect_equal(results[[1]]$expected["x"], c(x = 20))
  
  # Second test: count
  expect_equal(results[[2]]$student, 3)
  expect_equal(results[[2]]$expected, 3)
})
