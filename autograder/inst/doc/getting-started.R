## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("KingSSIBAL/R-Function-checker/autograder")

## -----------------------------------------------------------------------------
# library(autograder)

## -----------------------------------------------------------------------------
# list_problems()

## -----------------------------------------------------------------------------
# preview_tests("fibonacci")

## -----------------------------------------------------------------------------
# student_fibonacci <- function(n) {
#   if (n <= 0) return(numeric(0))
#   if (n == 1) return(1)
# 
#   fib <- c(1, 1)
#   for (i in 3:n) {
#     fib[i] <- fib[i-1] + fib[i-2]
#   }
#   fib
# }

## -----------------------------------------------------------------------------
# autograder("fibonacci")

## -----------------------------------------------------------------------------
# # A buggy implementation
# student_fibonacci <- function(n) {
#   rep(0, n)  # Wrong!
# }
# 
# autograder("fibonacci")

## -----------------------------------------------------------------------------
# autograder("fibonacci",
#   verbose = TRUE,       # Show detailed output (default: TRUE)
#   show_hidden = FALSE,  # Show hidden test details (default: FALSE)
#   show_progress = TRUE, # Show progress bar (default: FALSE)
#   use_parallel = TRUE,  # Use parallel execution (default: TRUE)
#   show_hints = TRUE     # Show hints for failures (default: TRUE)
# )

## -----------------------------------------------------------------------------
# result <- compare_performance("fibonacci", n_runs = 100)

## -----------------------------------------------------------------------------
# plot(result)

