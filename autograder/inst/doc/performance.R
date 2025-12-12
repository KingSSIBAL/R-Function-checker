## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(autograder)
# 
# # First, ensure your function passes all tests
# result <- autograder("fibonacci")
# 
# # Then benchmark performance
# perf <- compare_performance("fibonacci", n_runs = 100)

## -----------------------------------------------------------------------------
# perf$student_median   # Median execution time for your function
# perf$instructor_median # Median execution time for reference
# perf$student_mean     # Mean execution time for your function
# perf$instructor_mean  # Mean execution time for reference
# perf$ratio            # How many times slower/faster you are
# perf$n_runs           # Number of benchmark iterations

## -----------------------------------------------------------------------------
# plot(perf)

## -----------------------------------------------------------------------------
# # Growing a vector in a loop is O(n²)
# result <- c()
# for (i in 1:n) {
#   result <- c(result, compute(i))
# }

## -----------------------------------------------------------------------------
# # Pre-allocate the vector - O(n)
# result <- numeric(n)
# for (i in 1:n) {
#   result[i] <- compute(i)
# }

## -----------------------------------------------------------------------------
# # Calculating mean inside loop
# for (i in 1:n) {
#   deviation[i] <- x[i] - mean(x)  # mean(x) calculated n times!
# }

## -----------------------------------------------------------------------------
# # Calculate once, use many times
# m <- mean(x)
# for (i in 1:n) {
#   deviation[i] <- x[i] - m
# }

## -----------------------------------------------------------------------------
# # Loop-based sum
# total <- 0
# for (i in 1:length(x)) {
#   total <- total + x[i]
# }

## -----------------------------------------------------------------------------
# # Vectorized
# total <- sum(x)

## -----------------------------------------------------------------------------
# # Naive recursive Fibonacci - O(2^n)
# fib <- function(n) {
#   if (n <= 2) return(1)
#   fib(n-1) + fib(n-2)
# }

## -----------------------------------------------------------------------------
# # Iterative Fibonacci - O(n)
# fib <- function(n) {
#   if (n <= 0) return(integer(0))
#   if (n == 1) return(1L)
# 
#   result <- integer(n)
#   result[1:2] <- 1L
#   for (i in 3:n) {
#     result[i] <- result[i-1] + result[i-2]
#   }
#   result
# }

## -----------------------------------------------------------------------------
# # Instead of manual implementation
# x_sorted <- x[order(x)]
# 
# # Use built-in sort (implemented in C)
# x_sorted <- sort(x)

## -----------------------------------------------------------------------------
# # Instead of loop
# squares <- numeric(length(x))
# for (i in seq_along(x)) {
#   squares[i] <- x[i]^2
# }
# 
# # Use vectorization
# squares <- x^2

## -----------------------------------------------------------------------------
# # Instead of loop over rows
# for (i in 1:nrow(mat)) {
#   row_sums[i] <- sum(mat[i, ])
# }
# 
# # Use apply
# row_sums <- apply(mat, 1, sum)
# 
# # Even better: use rowSums
# row_sums <- rowSums(mat)

## -----------------------------------------------------------------------------
# # Quick check
# compare_performance("fibonacci", n_runs = 10)
# 
# # Reliable benchmark
# compare_performance("fibonacci", n_runs = 1000)

## -----------------------------------------------------------------------------
# for (i in 1:3) {
#   print(compare_performance("fibonacci", n_runs = 100))
# }

## -----------------------------------------------------------------------------
# library(autograder)
# 
# # 1. Check available problems
# list_problems()
# 
# # 2. Preview test cases
# preview_tests("fibonacci")
# 
# # 3. Create your function
# student_fibonacci <- function(n) {
#   if (n <= 0) return(integer(0))
#   if (n == 1) return(1L)
# 
#   fib <- integer(n)
#   fib[1:2] <- 1L
#   for (i in 3:n) {
#     fib[i] <- fib[i-1] + fib[i-2]
#   }
#   fib
# }
# 
# # 4. Test for correctness
# result <- autograder("fibonacci")
# print(result)
# 
# # 5. If all tests pass, benchmark performance
# if (result$pass_rate == 100) {
#   perf <- compare_performance("fibonacci", n_runs = 100)
#   print(perf)
#   plot(perf)
# }

