# ============================================================================
# AUTOGRADER - BENCHMARK INTEGRATION TESTS
# ============================================================================
#
# Tests for the benchmark integration in autograder():
#   - benchmark parameter validation
#   - benchmark execution when all tests pass
#   - benchmark skipped when tests fail
#   - benchmark bonus calculation
#
# ============================================================================

# ============================================================================
# BENCHMARK PARAMETER VALIDATION TESTS
# ============================================================================

test_that("autograder validates benchmark parameter", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) 1:n, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  expect_error(autograder("fibonacci", benchmark = "yes"), "flag|logical|Must be")
  expect_error(autograder("fibonacci", benchmark = 1), "flag|logical|Must be")
  expect_error(autograder("fibonacci", benchmark = c(TRUE, FALSE)), "flag|logical|length 1")
})

test_that("autograder validates benchmark_runs parameter", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) 1:n, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  expect_error(autograder("fibonacci", benchmark = TRUE, benchmark_runs = 0), 
               "Element 1 is not|positive|lower|>= 1")
  expect_error(autograder("fibonacci", benchmark = TRUE, benchmark_runs = -5),
               "Element 1 is not|positive|lower|>= 1")
  expect_error(autograder("fibonacci", benchmark = TRUE, benchmark_runs = "fifty"),
               "integerish|numeric|integer|Must be")
})

test_that("autograder validates benchmark_bonus parameter", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) 1:n, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  expect_error(autograder("fibonacci", benchmark_bonus = -5), 
               "lower|negative|>= 0")
  expect_error(autograder("fibonacci", benchmark_bonus = 150),
               "upper|<= 100|exceed")
  expect_error(autograder("fibonacci", benchmark_bonus = "ten"),
               "number|numeric")
})

# ============================================================================
# BENCHMARK EXECUTION TESTS
# ============================================================================

test_that("autograder includes benchmark in result when benchmark=TRUE and tests pass", {
  skip_on_cran()
  skip_if_offline()
  
  # Define a correct fibonacci function
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- suppressMessages(
    autograder("fibonacci", verbose = FALSE, benchmark = TRUE, benchmark_runs = 10)
  )
  
  # Check that benchmark results are included

  expect_true(!is.null(result$details$benchmark))
  expect_true(is.list(result$details$benchmark))
  expect_true("ratio" %in% names(result$details$benchmark))
  expect_true("verdict" %in% names(result$details$benchmark))
  expect_true("student_median" %in% names(result$details$benchmark))
  expect_true("instructor_median" %in% names(result$details$benchmark))
})

test_that("autograder skips benchmark when tests fail", {
  skip_on_cran()
  skip_if_offline()
  
  # Define an incorrect fibonacci function
  assign("student_fibonacci", function(n) {
    rep(0, n)  # Wrong implementation
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- suppressMessages(
    capture.output(
      autograder("fibonacci", verbose = FALSE, benchmark = TRUE, benchmark_runs = 10)
    )
  )
  
  # Check output mentions skipping benchmark
  expect_true(any(grepl("Skipped|All tests must pass", result)))
})

test_that("autograder returns NULL benchmark when benchmark=FALSE", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- suppressMessages(
    autograder("fibonacci", verbose = FALSE, benchmark = FALSE)
  )
  
  # Benchmark should be NULL when not requested
  expect_null(result$details$benchmark)
})

# ============================================================================
# BENCHMARK BONUS TESTS
# ============================================================================

test_that("autograder awards bonus for faster implementation", {
  skip_on_cran()
  skip_if_offline()
  
  # Define a correct fibonacci function (should be comparable speed)
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- suppressMessages(
    autograder("fibonacci", verbose = FALSE, benchmark = TRUE, 
               benchmark_runs = 10, benchmark_bonus = 10)
  )
  
  # Check bonus is recorded (may be 0 if not faster)
  expect_true(!is.null(result$details$benchmark_bonus))
  expect_true(is.numeric(result$details$benchmark_bonus))
  expect_true(result$details$benchmark_bonus >= 0)
  
  # Base score should be tracked separately
  expect_true(!is.null(result$details$base_score))
  expect_true(!is.null(result$details$base_max))
})

test_that("autograder calculates max_score correctly with benchmark_bonus", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- suppressMessages(
    autograder("fibonacci", verbose = FALSE, benchmark = TRUE, 
               benchmark_runs = 10, benchmark_bonus = 10)
  )
  
  # max_score should include the potential bonus
  base_max <- result$details$base_max
  expected_max <- base_max + base_max * 0.10  # 10% bonus
  
  expect_equal(result$max_score, expected_max)
})

# ============================================================================
# PRINT/SUMMARY METHOD TESTS WITH BENCHMARK
# ============================================================================

test_that("print.autograder_result shows benchmark info", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- suppressMessages(
    autograder("fibonacci", verbose = FALSE, benchmark = TRUE, benchmark_runs = 10)
  )
  
  output <- capture.output(print(result))
  
  # Should contain benchmark info
  expect_true(any(grepl("Benchmark|ratio|FASTER|SLOWER|COMPARABLE", output)))
})

test_that("summary.autograder_result shows benchmark details", {
  skip_on_cran()
  skip_if_offline()
  
  assign("student_fibonacci", function(n) {
    if (n <= 0) return(numeric(0))
    if (n == 1) return(1)
    fib <- c(1, 1)
    if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
    fib
  }, envir = .GlobalEnv)
  on.exit(rm("student_fibonacci", envir = .GlobalEnv), add = TRUE)
  
  result <- suppressMessages(
    autograder("fibonacci", verbose = FALSE, benchmark = TRUE, benchmark_runs = 10)
  )
  
  output <- capture.output(summary(result))
  
  # Should contain benchmark section
  expect_true(any(grepl("Benchmark|ratio|Performance|median", output)))
})
