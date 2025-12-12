# ============================================================================
# Test: data.R - Data File Loading and Injection
# ============================================================================

# ============================================================================
# Section 1: fetch_data Input Validation
# ============================================================================

test_that("fetch_data validates filename parameter", {
  expect_error(fetch_data(), "filename must be")
  expect_error(fetch_data(NULL), "filename must be")
  expect_error(fetch_data(""), "filename must be")
  expect_error(fetch_data(123), "filename must be")
  expect_error(fetch_data(c("a.csv", "b.csv")), "filename must be")
})

test_that("fetch_data validates non-empty filename", {
  expect_error(fetch_data(""), "filename must be a non-empty")
})

# ============================================================================
# Section 2: inject_data_into_inputs
# ============================================================================

test_that("inject_data_into_inputs returns unchanged when cache empty", {
  inputs <- list(x = 1, y = 2)
  result <- inject_data_into_inputs(inputs, list())
  
  expect_equal(result, inputs)
})

test_that("inject_data_into_inputs replaces data file references", {
  inputs <- list(data = "test.csv", n = 10)
  cache <- list("test.csv" = data.frame(a = 1:3, b = 4:6))
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_true(is.data.frame(result$data))
  expect_equal(result$data, cache[["test.csv"]])
  expect_equal(result$n, 10)
})

test_that("inject_data_into_inputs handles multiple data files", {
  inputs <- list(df1 = "file1.csv", df2 = "file2.csv")
  cache <- list(
    "file1.csv" = data.frame(x = 1:2),
    "file2.csv" = data.frame(y = 3:4)
  )
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_equal(result$df1, cache[["file1.csv"]])
  expect_equal(result$df2, cache[["file2.csv"]])
})

test_that("inject_data_into_inputs preserves non-matching strings", {
  inputs <- list(filename = "not_in_cache.csv", value = 5)
  cache <- list("other.csv" = data.frame(a = 1))
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_equal(result$filename, "not_in_cache.csv")
  expect_equal(result$value, 5)
})

test_that("inject_data_into_inputs handles mixed input types", {
  inputs <- list(
    data = "test.csv",
    num = 42,
    vec = c(1, 2, 3),
    char = "regular string",
    flag = TRUE
  )
  cache <- list("test.csv" = data.frame(x = 1:3))
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_true(is.data.frame(result$data))
  expect_equal(result$num, 42)
  expect_equal(result$vec, c(1, 2, 3))
  expect_equal(result$char, "regular string")
  expect_true(result$flag)
})

test_that("inject_data_into_inputs handles NULL cache", {
  inputs <- list(x = 1)
  # Empty list behaves same as NULL in length check
  result <- inject_data_into_inputs(inputs, list())
  expect_equal(result, inputs)
})

test_that("inject_data_into_inputs handles RDS data", {
  inputs <- list(model = "model.rds")
  cache <- list("model.rds" = list(coef = c(1, 2, 3), type = "linear"))
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_type(result$model, "list")
  expect_equal(result$model$type, "linear")
})

# ============================================================================
# Section 3: prefetch_data_files
# ============================================================================

test_that("prefetch_data_files returns empty list when no data_files", {
  test_data <- list(test_cases = list())
  result <- prefetch_data_files(test_data)
  
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("prefetch_data_files returns empty for NULL data_files", {
  test_data <- list(test_cases = list(), data_files = NULL)
  result <- prefetch_data_files(test_data)
  
  expect_length(result, 0)
})

test_that("prefetch_data_files returns empty for empty data_files", {
  test_data <- list(test_cases = list(), data_files = character(0))
  result <- prefetch_data_files(test_data)
  
  expect_length(result, 0)
})

# ============================================================================
# Section 4: File Extension Handling (mocked)
# ============================================================================

test_that("file extension detection works correctly", {
  # Test that tools::file_ext works as expected
  expect_equal(tolower(tools::file_ext("data.csv")), "csv")
  expect_equal(tolower(tools::file_ext("data.CSV")), "csv")
  expect_equal(tolower(tools::file_ext("model.rds")), "rds")
  expect_equal(tolower(tools::file_ext("data.RData")), "rdata")
  expect_equal(tolower(tools::file_ext("file.xlsx")), "xlsx")
  expect_equal(tolower(tools::file_ext("notes.txt")), "txt")
  expect_equal(tolower(tools::file_ext("archive.rda")), "rda")
})

test_that("complex filenames are handled", {
  expect_equal(tolower(tools::file_ext("my.data.file.csv")), "csv")
  expect_equal(tolower(tools::file_ext("path/to/data.csv")), "csv")
  expect_equal(tolower(tools::file_ext("file_with_underscore.rds")), "rds")
})

# ============================================================================
# Section 5: Data Cache Integration
# ============================================================================

test_that("data cache preserves data types", {
  cache <- list(
    "df.csv" = data.frame(a = 1:3, b = c("x", "y", "z")),
    "vec.rds" = c(1, 2, 3, 4, 5),
    "model.rds" = list(type = "lm", coef = c(1.5, 2.5))
  )
  
  inputs <- list(data = "df.csv", vector = "vec.rds", m = "model.rds")
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_s3_class(result$data, "data.frame")
  expect_type(result$vector, "double")
  expect_type(result$m, "list")
})

test_that("data cache handles special characters in values", {
  cache <- list(
    "text.csv" = data.frame(
      text = c("hello, world", "line\nbreak", "tab\there")
    )
  )
  
  inputs <- list(data = "text.csv")
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_equal(nrow(result$data), 3)
  expect_match(result$data$text[1], "hello, world")
})

# ============================================================================
# ADDITIONAL VALIDATION TESTS
# ============================================================================

test_that("fetch_data validates NA filename", {
  expect_error(fetch_data(NA_character_))
})

test_that("fetch_data validates list filename", {
  expect_error(fetch_data(list("file.csv")), "non-empty character string")
})

test_that("fetch_data validates logical filename", {
  expect_error(fetch_data(TRUE), "non-empty character string")
})

test_that("inject_data_into_inputs handles NULL in inputs", {
  inputs <- list(x = NULL, data = "test.csv")
  cache <- list("test.csv" = data.frame(a = 1))
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_null(result$x)
  expect_true(is.data.frame(result$data))
})

test_that("inject_data_into_inputs handles empty inputs", {
  inputs <- list()
  cache <- list("test.csv" = data.frame(a = 1))
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_equal(length(result), 0)
})

test_that("inject_data_into_inputs handles multi-element character vectors", {
  inputs <- list(names = c("test.csv", "other.csv"))
  cache <- list("test.csv" = data.frame(a = 1))
  
  result <- inject_data_into_inputs(inputs, cache)
  expect_equal(result$names, c("test.csv", "other.csv"))
})

test_that("inject preserves data frame attributes", {
  df <- data.frame(a = 1:3, b = 4:6)
  attr(df, "custom") <- "my_attribute"
  
  cache <- list("test.csv" = df)
  inputs <- list(data = "test.csv")
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_equal(attr(result$data, "custom"), "my_attribute")
})
# ============================================================================
# ADDITIONAL COVERAGE TESTS FOR data.R
# ============================================================================

test_that("inject_data_into_inputs handles numeric input names", {
  inputs <- list("1" = "test.csv", "2" = 10)
  cache <- list("test.csv" = data.frame(a = 1))
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_true(is.data.frame(result[["1"]]))
  expect_equal(result[["2"]], 10)
})

test_that("inject_data_into_inputs handles nested list data", {
  nested_data <- list(
    level1 = list(
      level2 = c(1, 2, 3)
    )
  )
  
  cache <- list("nested.rds" = nested_data)
  inputs <- list(data = "nested.rds")
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_type(result$data, "list")
  expect_equal(result$data$level1$level2, c(1, 2, 3))
})

test_that("inject_data_into_inputs handles matrix data", {
  mat <- matrix(1:6, nrow = 2)
  
  cache <- list("matrix.rds" = mat)
  inputs <- list(m = "matrix.rds")
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_true(is.matrix(result$m))
  expect_equal(dim(result$m), c(2, 3))
})

test_that("inject_data_into_inputs handles factor data", {
  df <- data.frame(
    category = factor(c("A", "B", "C")),
    stringsAsFactors = TRUE
  )
  
  cache <- list("factors.csv" = df)
  inputs <- list(data = "factors.csv")
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_true(is.factor(result$data$category))
})

test_that("inject_data_into_inputs handles large data frame", {
  large_df <- data.frame(
    a = 1:1000,
    b = rnorm(1000),
    c = sample(letters, 1000, replace = TRUE)
  )
  
  cache <- list("large.csv" = large_df)
  inputs <- list(data = "large.csv")
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_equal(nrow(result$data), 1000)
})

test_that("prefetch_data_files handles list input", {
  test_data <- list(
    inputs = list(list(data = "file.csv")),
    data_files = list()  # Empty list instead of character(0)
  )
  
  result <- prefetch_data_files(test_data)
  expect_length(result, 0)
})

test_that("file extension detection handles uppercase", {
  expect_equal(tolower(tools::file_ext("data.CSV")), "csv")
  expect_equal(tolower(tools::file_ext("data.RDS")), "rds")
  expect_equal(tolower(tools::file_ext("data.RDATA")), "rdata")
  expect_equal(tolower(tools::file_ext("data.TXT")), "txt")
  expect_equal(tolower(tools::file_ext("data.XLSX")), "xlsx")
  expect_equal(tolower(tools::file_ext("data.XLS")), "xls")
})

test_that("file extension detection handles no extension", {
  expect_equal(tools::file_ext("noextension"), "")
})

test_that("file extension detection handles hidden files", {
  # .hidden is treated as having extension "hidden" by tools::file_ext
  expect_equal(tools::file_ext(".hidden"), "hidden")
  expect_equal(tools::file_ext(".hidden.csv"), "csv")
})

test_that("inject handles identical filename in cache and input", {
  cache <- list("file.csv" = data.frame(x = 1))
  inputs <- list(file = "file.csv")
  
  result <- inject_data_into_inputs(inputs, cache)
  
  expect_true(is.data.frame(result$file))
})

test_that("inject handles case sensitivity in filenames", {
  cache <- list("File.csv" = data.frame(x = 1))
  inputs <- list(data = "file.csv")  # Different case
  
  result <- inject_data_into_inputs(inputs, cache)
  
  # Should NOT match due to case difference
  expect_equal(result$data, "file.csv")
})

test_that("fetch_data validates path traversal attempts", {
  expect_error(fetch_data("../../../etc/passwd"))
  expect_error(fetch_data("..\\..\\windows\\system32"))
})

test_that("fetch_data handles unsupported extension", {
  # Mock the memoised fetch to return a temp file
  local_mock <- function(...) tempfile(fileext = ".xyz")
  
  # This test verifies the error path for unsupported formats
  expect_error(
    fetch_data("file.invalid_extension_xyz"),
    "fetch|format|extension|Failed",
    ignore.case = TRUE
  )
})