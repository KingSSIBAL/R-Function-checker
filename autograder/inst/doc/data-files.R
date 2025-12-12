## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# # Your function receives a data frame directly
# student_analyze_scores <- function(data, statistic = "mean") {
#   # data is already loaded - don't use read.csv()!
# 
#   # Find numeric columns
#   numeric_cols <- sapply(data, is.numeric)
#   numeric_data <- data[, numeric_cols, drop = FALSE]
# 
#   # Calculate the requested statistic
#   switch(statistic,
#     "mean" = sapply(numeric_data, mean, na.rm = TRUE),
#     "median" = sapply(numeric_data, median, na.rm = TRUE),
#     "sum" = sapply(numeric_data, sum, na.rm = TRUE),
#     "count" = nrow(data)
#   )
# }

## -----------------------------------------------------------------------------
# # ❌ WRONG - Don't try to read the file yourself
# student_analyze <- function(data) {
#   df <- read.csv(data)  # data is already a data frame!
#   mean(df$value)
# }
# 
# # ✅ CORRECT - Use the data directly
# student_analyze <- function(data) {
#   mean(data$value)
# }

## -----------------------------------------------------------------------------
# # Create sample data
# sample_data <- data.frame(
#   id = 1:10,
#   score = c(85, 92, 78, 95, 88, 72, 91, 84, 89, 76)
# )
# 
# # Test your function
# student_analyze_scores(sample_data, "mean")

## -----------------------------------------------------------------------------
# # CSV
# scores <- data.frame(
#   student_id = 1:10,
#   score = sample(60:100, 10),
#   grade = sample(c("A", "B", "C"), 10, replace = TRUE)
# )
# write.csv(scores, "data/sample_scores.csv", row.names = FALSE)
# 
# # RDS (any R object)
# model <- lm(mpg ~ wt, data = mtcars)
# saveRDS(model, "data/fitted_model.rds")
# 
# # RData (multiple objects)
# x <- 1:100
# y <- x^2
# save(x, y, file = "data/xy_data.RData")
# 
# # Excel
# library(openxlsx)
# write.xlsx(scores, "data/scores.xlsx")

## -----------------------------------------------------------------------------
# # functions/analyze_data.R
# 
# analyze_data <- function(data, type = "mean") {
#   numeric_cols <- sapply(data, is.numeric)
#   numeric_data <- data[, numeric_cols, drop = FALSE]
# 
#   switch(type,
#     "mean" = sapply(numeric_data, mean),
#     "sum" = sapply(numeric_data, sum),
#     "count" = nrow(data)
#   )
# }
# 
# test_cases <- list(
#   # Declare all data files needed
#   data_files = c("sample_scores.csv", "measurements.rds", "inventory.xlsx"),
# 
#   inputs = list(
#     # String filename is replaced with loaded data at runtime
#     list(data = "sample_scores.csv", type = "mean"),
#     list(data = "measurements.rds", type = "sum"),
#     list(data = "inventory.xlsx", type = "count"),
#     # You can also use inline data
#     list(data = data.frame(x = 1:5, y = 6:10), type = "mean")
#   ),
# 
#   descriptions = c(
#     "Calculate mean from CSV",
#     "Calculate sum from RDS",
#     "Count rows in Excel",
#     "Inline data test"
#   ),
# 
#   points = c(2, 2, 2, 1),
#   hidden = c(FALSE, FALSE, TRUE, FALSE)
# )

## -----------------------------------------------------------------------------
# # What the student function sees:
# # Instead of: student_analyze(data = "sample_scores.csv")
# # It receives: student_analyze(data = <data.frame with scores>)

## -----------------------------------------------------------------------------
# # Cache is created once, shared with all workers
# autograder("analyze_data", use_parallel = TRUE)

## -----------------------------------------------------------------------------
# # functions/summarize_sales.R
# 
# # Reference implementation
# summarize_sales <- function(sales_data, by = "product", metric = "total") {
#   if (nrow(sales_data) == 0) return(data.frame())
# 
#   result <- aggregate(
#     sales_data$amount,
#     by = list(category = sales_data[[by]]),
#     FUN = switch(metric,
#       "total" = sum,
#       "average" = mean,
#       "count" = length
#     )
#   )
#   names(result) <- c(by, metric)
#   result
# }
# 
# test_cases <- list(
#   data_files = c("sales_2024.csv", "sales_empty.csv"),
# 
#   inputs = list(
#     list(sales_data = "sales_2024.csv", by = "product", metric = "total"),
#     list(sales_data = "sales_2024.csv", by = "region", metric = "average"),
#     list(sales_data = "sales_empty.csv", by = "product", metric = "total")
#   ),
# 
#   descriptions = c(
#     "Total sales by product",
#     "Average sales by region",
#     "Handle empty data"
#   ),
# 
#   points = c(3, 3, 2),
#   hidden = c(FALSE, TRUE, TRUE),
# 
#   hints = c(
#     "Use aggregate() to group and summarize",
#     "Make sure to handle different grouping columns",
#     "Return empty data frame for empty input"
#   )
# )

