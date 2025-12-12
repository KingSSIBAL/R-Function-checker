## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(autograder)
# list_problems()

## -----------------------------------------------------------------------------
# preview_tests("fibonacci")

## -----------------------------------------------------------------------------
# # For the "fibonacci" problem, create student_fibonacci
# student_fibonacci <- function(n) {
#   # Your implementation here
# }

## -----------------------------------------------------------------------------
# result <- autograder("fibonacci")

## -----------------------------------------------------------------------------
# result$passed     # Number of tests passed
# result$failed     # Number of tests failed
# result$total      # Total tests
# result$score      # Points earned
# result$max_score  # Maximum points
# result$pass_rate  # Percentage passed

## -----------------------------------------------------------------------------
# # Test your function manually with the failing input
# student_fibonacci(5)
# # Compare with expected output
# # Fix the bug
# # Run autograder again
# autograder("fibonacci")

## -----------------------------------------------------------------------------
# # Test edge cases first
# student_fibonacci(0)
# student_fibonacci(1)
# student_fibonacci(2)
# 
# # Then larger inputs
# student_fibonacci(10)

## -----------------------------------------------------------------------------
# autograder("fibonacci", verbose = TRUE, show_hints = TRUE)

## -----------------------------------------------------------------------------
# # Wrong: returning string instead of number
# return("5")
# 
# # Right: returning number
# return(5)
# 
# # Wrong: returning list instead of vector
# return(list(1, 2, 3))
# 
# # Right: returning vector
# return(c(1, 2, 3))

## -----------------------------------------------------------------------------
# # ✅ CORRECT - data is already a data frame
# student_analyze <- function(data, stat = "mean") {
#   sapply(data[sapply(data, is.numeric)],
#          switch(stat, mean = mean, sum = sum))
# }
# 
# # ❌ WRONG - don't try to read the file
# student_analyze <- function(data, stat = "mean") {
#   df <- read.csv(data)  # data is NOT a filename!
#   # ...
# }

## -----------------------------------------------------------------------------
# result <- compare_performance("fibonacci", n_runs = 100)
# print(result)
# plot(result)

