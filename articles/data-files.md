# Working with Data Files

## Overview

The autograder supports external data files in test cases. This allows
instructors to create realistic data analysis problems where students
work with CSV files, Excel spreadsheets, RDS objects, and more.

## Supported Formats

| Extension        | Loader                                                                           | Returns          |
|------------------|----------------------------------------------------------------------------------|------------------|
| `.csv`           | [`read.csv()`](https://rdrr.io/r/utils/read.table.html)                          | data.frame       |
| `.xlsx`, `.xls`  | [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html) | data.frame       |
| `.rds`           | [`readRDS()`](https://rdrr.io/r/base/readRDS.html)                               | Any R object     |
| `.RData`, `.rda` | [`load()`](https://rdrr.io/r/base/load.html)                                     | List of objects  |
| `.txt`           | [`readLines()`](https://rdrr.io/r/base/readLines.html)                           | Character vector |

## For Students

### How Data Files Work

When a test case uses data files, the autograder:

1.  **Downloads** the data file from the instructor’s repository
2.  **Loads** it using the appropriate R function
3.  **Passes** the loaded data to your function

You receive the **already-loaded data**, not a file path!

### Example: Writing a Data Analysis Function

``` r
# Your function receives a data frame directly
student_analyze_scores <- function(data, statistic = "mean") {
  # data is already loaded - don't use read.csv()!
  
  # Find numeric columns
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Calculate the requested statistic
  switch(statistic,
    "mean" = sapply(numeric_data, mean, na.rm = TRUE),
    "median" = sapply(numeric_data, median, na.rm = TRUE),
    "sum" = sapply(numeric_data, sum, na.rm = TRUE),
    "count" = nrow(data)
  )
}
```

### Common Mistakes

``` r
# ❌ WRONG - Don't try to read the file yourself
student_analyze <- function(data) {
  df <- read.csv(data)  # data is already a data frame!
  mean(df$value)
}

# ✅ CORRECT - Use the data directly
student_analyze <- function(data) {
  mean(data$value)
}
```

### Testing Locally

If you want to test your function locally with sample data:

``` r
# Create sample data
sample_data <- data.frame(
  id = 1:10,
  score = c(85, 92, 78, 95, 88, 72, 91, 84, 89, 76)
)

# Test your function
student_analyze_scores(sample_data, "mean")
```

## For Instructors

### Repository Structure

Place data files in a `data/` folder within your repository:

    your-repo/
    ├── functions/
    │   ├── _problems.R
    │   └── analyze_data.R
    └── data/
        ├── sample_scores.csv
        ├── measurements.rds
        └── inventory.xlsx

### Creating Data Files

``` r
# CSV
scores <- data.frame(
  student_id = 1:10,
  score = sample(60:100, 10),
  grade = sample(c("A", "B", "C"), 10, replace = TRUE)
)
write.csv(scores, "data/sample_scores.csv", row.names = FALSE)

# RDS (any R object)
model <- lm(mpg ~ wt, data = mtcars)
saveRDS(model, "data/fitted_model.rds")

# RData (multiple objects)
x <- 1:100
y <- x^2
save(x, y, file = "data/xy_data.RData")

# Excel
library(openxlsx)
write.xlsx(scores, "data/scores.xlsx")
```

### Referencing Data in Test Cases

Use the `data_files` field to declare required data files:

``` r
# functions/analyze_data.R

analyze_data <- function(data, type = "mean") {
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  switch(type,
    "mean" = sapply(numeric_data, mean),
    "sum" = sapply(numeric_data, sum),
    "count" = nrow(data)
  )
}

test_cases <- list(
  # Declare all data files needed
  data_files = c("sample_scores.csv", "measurements.rds", "inventory.xlsx"),
  
  inputs = list(
    # String filename is replaced with loaded data at runtime
    list(data = "sample_scores.csv", type = "mean"),
    list(data = "measurements.rds", type = "sum"),
    list(data = "inventory.xlsx", type = "count"),
    # You can also use inline data
    list(data = data.frame(x = 1:5, y = 6:10), type = "mean")
  ),
  
  descriptions = c(
    "Calculate mean from CSV",
    "Calculate sum from RDS",
    "Count rows in Excel",
    "Inline data test"
  ),
  
  points = c(2, 2, 2, 1),
  hidden = c(FALSE, FALSE, TRUE, FALSE)
)
```

### How Data Injection Works

When a test runs:

1.  **Prefetching**: All files in `data_files` are downloaded and cached
2.  **Injection**: For each input, if a value is a string matching a
    data file, it’s replaced with the loaded data
3.  **Execution**: The student function receives the actual data

``` r
# What the student function sees:
# Instead of: student_analyze(data = "sample_scores.csv")
# It receives: student_analyze(data = <data.frame with scores>)
```

### Parallel Execution

Data files are cached and shared across parallel workers for efficiency:

``` r
# Cache is created once, shared with all workers
autograder("analyze_data", use_parallel = TRUE)
```

## Best Practices

### For Data Files

1.  **Keep files small** - Large files slow down test execution
2.  **Use descriptive names** - `student_grades_2024.csv` not
    `data1.csv`
3.  **Document expected structure** - Comments in problem file
4.  **Test your own data** - Verify files load correctly

### For Test Cases

1.  **Mix data sources** - Test with different file types
2.  **Include edge cases** - Empty data, single row, missing values
3.  **Hide sensitive tests** - Use `hidden = TRUE` for edge cases

### Example Complete Problem

``` r
# functions/summarize_sales.R

# Reference implementation
summarize_sales <- function(sales_data, by = "product", metric = "total") {
  if (nrow(sales_data) == 0) return(data.frame())
  
  result <- aggregate(
    sales_data$amount,
    by = list(category = sales_data[[by]]),
    FUN = switch(metric,
      "total" = sum,
      "average" = mean,
      "count" = length
    )
  )
  names(result) <- c(by, metric)
  result
}

test_cases <- list(
  data_files = c("sales_2024.csv", "sales_empty.csv"),
  
  inputs = list(
    list(sales_data = "sales_2024.csv", by = "product", metric = "total"),
    list(sales_data = "sales_2024.csv", by = "region", metric = "average"),
    list(sales_data = "sales_empty.csv", by = "product", metric = "total")
  ),
  
  descriptions = c(
    "Total sales by product",
    "Average sales by region",
    "Handle empty data"
  ),
  
  points = c(3, 3, 2),
  hidden = c(FALSE, TRUE, TRUE),
  
  hints = c(
    "Use aggregate() to group and summarize",
    "Make sure to handle different grouping columns",
    "Return empty data frame for empty input"
  )
)
```
