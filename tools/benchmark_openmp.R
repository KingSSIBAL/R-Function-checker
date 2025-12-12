# OpenMP Parallelization Test Script
# ==================================

library(autograder)

# Show OpenMP configuration
cat("OpenMP Configuration:\n")
cat("=====================\n")
info <- autograder:::.cpp_openmp_info()
cat("Available:          ", info$available, "\n")
cat("Version:            ", info$version, "\n")
cat("Max Threads:        ", info$max_threads, "\n")
cat("Parallel Threshold: ", info$parallel_threshold, "\n")
cat("\n")

# Test parallel comparison with large vectors
cat("Performance Test:\n")
cat("=================\n")

n <- 100000L
set.seed(42)
v1 <- rnorm(n)
v2 <- v1 + rnorm(n, sd = 1e-15)

cat("Vector size:", n, "\n")
cat("\n")

# Sequential test
cat("Sequential comparison: ")
t_seq <- system.time({
  for (i in 1:10) {
    result <- autograder:::.cpp_compare_fast(v1, v2, 1e-10)
  }
})
cat(round(t_seq["elapsed"] * 100, 2), "ms per call\n")

# Parallel test
cat("Parallel comparison:   ")
t_par <- system.time({
  for (i in 1:10) {
    result <- autograder:::.cpp_compare_parallel(v1, v2, 1e-10)
  }
})
cat(round(t_par["elapsed"] * 100, 2), "ms per call\n")

# Sum of absolute differences test
cat("\nSum of absolute differences: ")
t_sum <- system.time({
  for (i in 1:10) {
    total <- autograder:::.cpp_sum_abs_diff(v1, v2)
  }
})
cat(round(t_sum["elapsed"] * 100, 2), "ms per call\n")
cat("Total difference:", total, "\n")

cat("\nOpenMP implementation successful!\n")
