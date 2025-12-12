# ============================================================================
# AUTOGRADER PACKAGE - PERFORMANCE COMPARISON
# ============================================================================
#
# File: performance.R
# Purpose: Benchmark student vs instructor implementations
#
# Functions:
#   - compare_performance(): Main benchmarking function
#   - print.performance_comparison(): S3 print method
#   - plot.performance_comparison(): S3 plot method
#
# Provides:
#   - Execution time comparison
#   - Statistical summary of performance
#   - Visual representation of results
#
# ============================================================================

#' Compare Performance of Student vs Instructor Implementation
#' 
#' @description
#' Benchmarks the student's function implementation against the instructor's
#' reference solution to evaluate computational efficiency.
#' 
#' This function measures:
#'   - Execution time (median, mean, min, max)
#'   - Relative performance (student vs instructor ratio)
#'   - Consistency (standard deviation)
#' 
#' @param function_name Character string. Name of the problem/function to benchmark.
#' @param n_runs Integer. Number of benchmark iterations (default: 100).
#' @param warmup Integer. Number of warmup runs before timing (default: 5).
#' @param test_inputs List. Custom test inputs for benchmarking. If NULL, uses
#'   test cases from the problem definition.
#' @param verbose Logical. Whether to print detailed output (default: TRUE).
#' 
#' @return A list with class "performance_comparison" containing:
#'   \item{function_name}{The function being tested}
#'   \item{n_runs}{Number of benchmark iterations}
#'   \item{student_times}{Vector of student execution times (seconds)}
#'   \item{instructor_times}{Vector of instructor execution times (seconds)}
#'   \item{student_stats}{Summary statistics for student (median, mean, sd, min, max)}
#'   \item{instructor_stats}{Summary statistics for instructor}
#'   \item{ratio}{Performance ratio (student/instructor, >1 means slower)}
#'   \item{passed_correctness}{Whether student output matches instructor}
#'   \item{verdict}{Human-readable performance assessment}
#' 
#' @examples
#' \dontrun{
#' # Define student function
#' student_fibonacci <- function(n) {
#'   if (n <= 0) return(numeric(0))
#'   if (n == 1) return(1)
#'   fib <- c(1, 1)
#'   for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
#'   fib
#' }
#' 
#' # Run performance comparison
#' result <- compare_performance("fibonacci")
#' 
#' # Custom number of runs
#' result <- compare_performance("fibonacci", n_runs = 500)
#' 
#' # Custom test inputs
#' result <- compare_performance("fibonacci", 
#'   test_inputs = list(list(20), list(30), list(50)))
#' }
#' 
#' @keywords internal
compare_performance <- function(function_name, 
                                 n_runs = 100L,
                                 warmup = 5L,
                                 test_inputs = NULL,
                                 verbose = TRUE) {
  
  # ==========================================================================
  # INPUT VALIDATION
  # ==========================================================================
  
  if (!is.character(function_name) || length(function_name) != 1) {
    stop("function_name must be a single character string.", call. = FALSE)
  }
  
  if (!is.numeric(n_runs) || n_runs < 1) {
    stop("n_runs must be a positive integer.", call. = FALSE)
  }
  n_runs <- as.integer(n_runs)
  
  if (!is.numeric(warmup) || warmup < 0) {
    stop("warmup must be a non-negative integer.", call. = FALSE)
  }
  warmup <- as.integer(warmup)
  
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("verbose must be TRUE or FALSE.", call. = FALSE)
  }
  
  # ==========================================================================
  # LOAD FUNCTIONS
  # ==========================================================================
  
  if (verbose) {
    message(sprintf("Loading %s...", function_name))
  }
  
  # Get student function
  student_fn_name <- paste0("student_", function_name)
  if (!exists(student_fn_name, envir = .GlobalEnv)) {
    stop(
      sprintf("Student function '%s' not found in global environment.\n", student_fn_name),
      "Define your function first:\n",
      sprintf("  %s <- function(...) { ... }", student_fn_name),
      call. = FALSE
    )
  }
  student_fn <- get(student_fn_name, envir = .GlobalEnv)
  
  if (!is.function(student_fn)) {
    stop(sprintf("'%s' exists but is not a function.", student_fn_name), call. = FALSE)
  }
  
  # Fetch instructor code
  instructor_env <- tryCatch(
    fetch_instructor_code(function_name),
    error = function(e) {
      stop(
        sprintf("Could not load instructor code for '%s'.\n", function_name),
        e$message,
        call. = FALSE
      )
    }
  )
  
  instructor_fn <- extract_instructor_function(instructor_env, function_name)
  
  # ==========================================================================
  # PREPARE TEST INPUTS
  # ==========================================================================
  
  if (is.null(test_inputs)) {
    # Extract from problem definition
    test_data <- extract_test_cases(instructor_env, function_name)
    test_inputs <- test_data$inputs
    
    # Use non-hidden tests only for performance
    if (!is.null(test_data$hidden)) {
      visible_indices <- which(!test_data$hidden)
      if (length(visible_indices) > 0) {
        test_inputs <- test_inputs[visible_indices]
      }
    }
  }
  
  if (length(test_inputs) == 0) {
    stop("No test inputs available for benchmarking.", call. = FALSE)
  }
  
  if (verbose) {
    message(sprintf("Benchmarking with %d test inputs, %d runs each...", 
                    length(test_inputs), n_runs))
  }
  
  # ==========================================================================
  # CORRECTNESS CHECK
  # ==========================================================================
  
  passed_correctness <- TRUE
  for (i in seq_along(test_inputs)) {
    student_out <- tryCatch(
      do.call(student_fn, test_inputs[[i]]),
      error = function(e) NULL
    )
    instructor_out <- do.call(instructor_fn, test_inputs[[i]])
    
    if (!.cpp_compare_fast(student_out, instructor_out, 1e-10)) {
      passed_correctness <- FALSE
      if (verbose) {
        warning(sprintf("Test %d: Student output does not match instructor.", i), 
                call. = FALSE)
      }
      break
    }
  }
  
  # ==========================================================================
  # WARMUP PHASE
  # ==========================================================================
  
  if (warmup > 0 && verbose) {
    message(sprintf("Warming up (%d runs)...", warmup))
  }
  
  for (w in seq_len(warmup)) {
    for (input in test_inputs) {
      suppressWarnings(do.call(student_fn, input))
      suppressWarnings(do.call(instructor_fn, input))
    }
  }
  
  # ==========================================================================
  # BENCHMARK PHASE (using C++ high-precision timing)
  # ==========================================================================
  
  if (verbose) {
    message("Running benchmarks with high-precision timing...")
  }
  
  # Wrap functions to suppress warnings during benchmarking
  student_fn_quiet <- function(...) suppressWarnings(student_fn(...))
  instructor_fn_quiet <- function(...) suppressWarnings(instructor_fn(...))
  
  # Use C++ benchmark for higher precision timing
  student_times <- .cpp_benchmark(student_fn_quiet, test_inputs, n_runs)
  instructor_times <- .cpp_benchmark(instructor_fn_quiet, test_inputs, n_runs)
  
  # ==========================================================================
  # COMPUTE STATISTICS
  # ==========================================================================
  
  compute_stats <- function(times) {
    list(
      median = median(times),
      mean = mean(times),
      sd = sd(times),
      min = min(times),
      max = max(times)
    )
  }
  
  student_stats <- compute_stats(student_times)
  instructor_stats <- compute_stats(instructor_times)
  
  # Ratio based on median (more robust than mean)
  ratio <- student_stats$median / instructor_stats$median
  
  # Performance verdict
  verdict <- if (ratio < 0.9) {
    "FASTER - Your implementation is faster than the reference!"
  } else if (ratio <= 1.1) {
    "COMPARABLE - Your implementation has similar performance."
  } else if (ratio <= 2.0) {
    "SLOWER - Your implementation is somewhat slower."
  } else if (ratio <= 5.0) {
    "MUCH SLOWER - Consider optimizing your implementation."
  } else {
    "VERY SLOW - Your implementation needs significant optimization."
  }
  
  # ==========================================================================
  # BUILD RESULT
  # ==========================================================================
  
  result <- list(
    function_name = function_name,
    n_runs = n_runs,
    n_inputs = length(test_inputs),
    student_times = student_times,
    instructor_times = instructor_times,
    student_stats = student_stats,
    instructor_stats = instructor_stats,
    ratio = ratio,
    passed_correctness = passed_correctness,
    verdict = verdict
  )
  
  class(result) <- c("performance_comparison", "list")
  
  # ==========================================================================
  # PRINT RESULTS
  # ==========================================================================
  
  if (verbose) {
    print(result)
  }
  
  invisible(result)
}

#' Print Performance Comparison Results
#' 
#' @description
#' Formats and displays performance comparison results in a readable format.
#' 
#' @param x A performance_comparison object
#' @param ... Additional arguments (ignored)
#' 
#' @return Invisibly returns the input object
#' 
#' @keywords internal
print.performance_comparison <- function(x, ...) {
  cat("\n")
  cat("=== Performance Comparison ===\n")
  cat(sprintf("Function: %s\n", x$function_name))
  cat(sprintf("Runs: %d | Inputs: %d\n", x$n_runs, x$n_inputs))
  cat("\n")
  
  # Correctness status
  if (x$passed_correctness) {
    cat("\342\234\223 Correctness: PASSED\n")
  } else {
    cat("\342\234\227 Correctness: FAILED (results may not be meaningful)\n")
  }
  cat("\n")
  
  # Timing table
  cat("Timing (seconds per batch):\n")
  cat(sprintf("  %-12s %12s %12s\n", "", "Student", "Instructor"))
  cat(sprintf("  %-12s %12.6f %12.6f\n", "Median:", x$student_stats$median, x$instructor_stats$median))
  cat(sprintf("  %-12s %12.6f %12.6f\n", "Mean:", x$student_stats$mean, x$instructor_stats$mean))
  cat(sprintf("  %-12s %12.6f %12.6f\n", "Std Dev:", x$student_stats$sd, x$instructor_stats$sd))
  cat(sprintf("  %-12s %12.6f %12.6f\n", "Min:", x$student_stats$min, x$instructor_stats$min))
  cat(sprintf("  %-12s %12.6f %12.6f\n", "Max:", x$student_stats$max, x$instructor_stats$max))
  cat("\n")
  
  # Ratio and verdict
  cat(sprintf("Performance Ratio: %.2fx (student/instructor)\n", x$ratio))
  cat(sprintf("Verdict: %s\n", x$verdict))
  cat("\n")
  
  invisible(x)
}

#' Plot Performance Comparison Results
#' 
#' @description
#' Creates a visualization of performance comparison results.
#' 
#' @param x A performance_comparison object
#' @param ... Additional arguments passed to boxplot
#' 
#' @return Invisibly returns the input object
#' 
#' @keywords internal
plot.performance_comparison <- function(x, ...) {
  # Convert times to milliseconds for readability
  student_ms <- x$student_times * 1000
  instructor_ms <- x$instructor_times * 1000
  
  # Create side-by-side boxplots
  boxplot(
    list(Student = student_ms, Instructor = instructor_ms),
    main = sprintf("Performance: %s", x$function_name),
    ylab = "Time (milliseconds)",
    col = c("#E69F00", "#56B4E9"),
    ...
  )
  
  # Add ratio annotation
  mtext(sprintf("Ratio: %.2fx", x$ratio), side = 3, line = 0.5, cex = 0.8)
  
  invisible(x)
}

# ============================================================================
# OPENMP PARALLEL PROCESSING
# ============================================================================

#' Check OpenMP Parallel Processing Status
#' 
#' @description
#' Reports the status of OpenMP parallel processing support in the
#' autograder package's C++ backend.
#' 
#' OpenMP is used to accelerate comparisons for large vectors (>10,000 elements).
#' This function shows whether OpenMP is available and configured.
#' 
#' @return A list with class "openmp_status" containing:
#'   \item{available}{Logical. Whether OpenMP is compiled in}
#'   \item{version}{Character. OpenMP version (e.g., "2015.11" for OpenMP 4.5)}
#'   \item{max_threads}{Integer. Maximum number of parallel threads available}
#'   \item{parallel_threshold}{Integer. Minimum vector size for parallelization}
#' 
#' @keywords internal
openmp_status <- function() {
  info <- .cpp_openmp_info()
  class(info) <- "openmp_status"
  info
}

#' Print OpenMP Status
#' 
#' @param x An openmp_status object
#' @param ... Additional arguments (ignored)
#' 
#' @keywords internal
print.openmp_status <- function(x, ...) {
  cat("\n")
  cat(cli::col_cyan(cli::style_bold("OpenMP Parallel Processing Status")), "\n")
  cat(cli::col_grey(strrep("-", 40)), "\n\n")
  
  if (x$available) {
    cat(cli::col_green("\u2713"), " OpenMP:    ", 
        cli::col_green("Enabled"), "\n", sep = "")
    cat("  Version:   ", x$version, "\n", sep = "")
    cat("  Threads:   ", x$max_threads, " available\n", sep = "")
    cat("  Threshold: ", format(x$parallel_threshold, big.mark = ","), 
        " elements\n", sep = "")
    cat("\n")
    cat(cli::col_grey("Vectors with >"), 
        cli::col_cyan(format(x$parallel_threshold, big.mark = ",")),
        cli::col_grey(" elements use parallel processing.\n"))
  } else {
    cat(cli::col_yellow("\u26A0"), " OpenMP:    ", 
        cli::col_yellow("Not Available"), "\n", sep = "")
    cat("\n")
    cat(cli::col_grey("Package will use sequential processing.\n"))
    cat(cli::col_grey("This is still fast for most use cases.\n"))
  }
  
  cat("\n")
  invisible(x)
}
