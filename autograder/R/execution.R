# ============================================================================
# AUTOGRADER PACKAGE - TEST EXECUTION
# ============================================================================
#
# File: execution.R
# Purpose: Sequential and parallel test execution
#
# Performance Optimization for Large Test Sets
#
# Decision Tree:
#   - < 10 tests: Use sequential (overhead not worth it)
#   - ≥ 10 tests AND use_parallel=TRUE: Use parallel
#   - use_parallel=FALSE: Always use sequential
#
# Parallel Benefits:
#   - Tests run independently (embarassingly parallel)
#   - Speedup: ~2-4x on multi-core systems
#   - No shared state = no synchronization overhead
#
# Parallel Overhead:
#   - Cluster setup: ~100-200ms
#   - Data serialization: ~10ms per test
#   - Worth it for: 10+ tests
#   - Not worth it for: <10 tests
#
# Implementation Notes:
#   - Uses snow-style cluster (portable across OS)
#   - Limits cores to min(available-1, 4) to keep system responsive
#   - Exports necessary objects explicitly
#   - Proper cleanup with on.exit()
#
# ============================================================================

#' Run tests in parallel using multiple CPU cores
#' 
#' @description
#' Executes test cases in parallel for improved performance on multi-core
#' systems. Automatically falls back to sequential for small test sets.
#' 
#' Parallel Execution Strategy:
#'   - Create cluster with n cores (leave 1 free for system)
#'   - Export student_fun, instructor_fun, tolerance to workers
#'   - Distribute test indices across workers
#'   - Each worker: runs test, catches errors, returns result
#'   - Main process: collects results, cleans up cluster
#' 
#' @param student_fun Student's function implementation
#' @param instructor_fun Instructor's reference implementation
#' @param test_data Validated test case data
#' @param tolerance Numeric tolerance for comparisons
#' @param use_parallel Whether to actually use parallel (TRUE) or fall back (FALSE)
#' @param data_cache Named list of loaded data objects (optional)
#' 
#' @return List of test results, each containing:
#'   - student: student output or error object
#'   - expected: expected output or error object
#'   - index: test number (1-based)
#' 
#' @details
#' Performance Characteristics:
#'   - Setup cost: ~100-200ms (cluster creation)
#'   - Per-test overhead: ~10ms (serialization)
#'   - Speedup: typically 2-4x on 4-core system
#'   - Worth it for: ≥10 tests
#' 
#' Core Allocation Strategy:
#'   - Detect available cores
#'   - Use cores - 1 (keep system responsive)
#'   - Cap at 4 cores (diminishing returns beyond this)
#'   - Example: 8-core system → use 4 cores
#' 
#' @section Resource Management:
#'   Cluster is always stopped via on.exit(), even if errors occur.
#'   This prevents orphaned R processes.
#' 
#' @keywords internal
run_tests_parallel <- function(student_fun, instructor_fun, test_data, tolerance, use_parallel = TRUE, data_cache = list()) {
  
  n_tests <- length(test_data$inputs)
  
  # ===== DECISION: Use parallel or not? =====
  # Parallel has overhead; only worth it for many tests
  if (!use_parallel || n_tests < 10) {
    return(run_tests_sequential(student_fun, instructor_fun, test_data, tolerance, data_cache))
  }
  
  # ===== SETUP PARALLEL CLUSTER =====
  # Determine optimal number of cores
  # Strategy: use available-1, capped at 4
  n_cores <- min(
    parallel::detectCores() - 1,  # Leave one core for system
    4                              # Cap at 4 (diminishing returns)
  )
  
  # Create cluster (starts background R processes)
  cl <- parallel::makeCluster(n_cores)
  
  # Ensure cluster is stopped even if error occurs
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  # Get timeout from options
  timeout <- getOption("autograder.test_timeout", 30)
  
  # Export necessary objects to worker processes
  # Workers are independent R processes with empty environments
  # Must explicitly export anything they need
  parallel::clusterExport(
    cl, 
    c("student_fun", "instructor_fun", "tolerance", "data_cache", 
      "inject_data_into_inputs", "timeout"),
    envir = environment()
  )
  
  # ===== RUN TESTS IN PARALLEL =====
  # parLapply distributes indices across workers
  # Each worker processes a subset of test indices
  results <- parallel::parLapply(cl, seq_along(test_data$inputs), function(i) {
    input_args <- test_data$inputs[[i]]
    
    # Inject data if needed
    if (length(data_cache) > 0) {
      input_args <- inject_data_into_inputs(input_args, data_cache)
    }
    
    # Run student function with error catching and timeout
    student_out <- tryCatch({
      setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
      on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
      do.call(student_fun, input_args)
    }, error = function(e) {
      if (grepl("time limit|reached elapsed|reached CPU", e$message, ignore.case = TRUE)) {
        structure(list(error = sprintf("Timeout: exceeded %d seconds", timeout)), 
                  class = c("timeout_error", "error"))
      } else {
        structure(list(error = e$message), class = "error")
      }
    })
    
    # Run instructor function with error catching
    # If instructor function errors, it's a config problem
    expected_out <- tryCatch(
      do.call(instructor_fun, input_args),
      error = function(e) structure(list(error = e$message), class = "error")
    )
    
    # Return structured result
    list(
      student = student_out,
      expected = expected_out,
      index = i
    )
  })
  
  # Cluster is automatically stopped by on.exit()
  results
}

#' Run tests sequentially (one after another)
#' 
#' @description
#' Executes test cases one at a time in order. Used for:
#'   - Small test sets (<10 tests)
#'   - When use_parallel = FALSE
#'   - Debugging (easier to trace)
#' 
#' Advantages over Parallel:
#'   - No setup overhead
#'   - Easier debugging (linear execution)
#'   - More predictable
#'   - Better for small test sets
#' 
#' @param student_fun Student's function
#' @param instructor_fun Instructor's reference function
#' @param test_data Validated test case data
#' @param tolerance Numeric tolerance
#' @param data_cache Named list of loaded data objects (optional)
#' 
#' @return List of test results (same structure as parallel version)
#' 
#' @details
#' Execution Flow:
#'   For each test:
#'     1. Extract input arguments
#'     2. Inject data if needed
#'     3. Call student function (catch errors)
#'     4. Call instructor function (catch errors)
#'     5. Package results
#'     6. Move to next test
#' 
#' Error Handling:
#'   Errors are captured, not thrown. This allows:
#'   - All tests to run (one failure doesn't stop others)
#'   - Detailed error reporting for each test
#'   - Graceful handling of student code errors
#' 
#' @keywords internal
run_tests_sequential <- function(student_fun, instructor_fun, test_data, tolerance, data_cache = list()) {
  
  # Get timeout from options (default: 30 seconds per test)
  timeout <- getOption("autograder.test_timeout", 30)
  
  # Simple lapply - process tests one by one
  results <- lapply(seq_along(test_data$inputs), function(i) {
    input_args <- test_data$inputs[[i]]
    
    # ===== INJECT DATA IF NEEDED =====
    # Replace data file references with actual loaded data
    if (length(data_cache) > 0) {
      input_args <- inject_data_into_inputs(input_args, data_cache)
    }
    
    # ===== RUN STUDENT FUNCTION WITH TIMEOUT =====
    # Wrap in tryCatch with timeout to prevent infinite loops
    student_out <- tryCatch({
      setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
      on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
      do.call(student_fun, input_args)
    }, error = function(e) {
      # Check for timeout error
      if (grepl("time limit|reached elapsed|reached CPU", e$message, ignore.case = TRUE)) {
        structure(list(error = sprintf("Timeout: exceeded %d seconds", timeout)), 
                  class = c("timeout_error", "error"))
      } else {
        structure(list(error = e$message), class = "error")
      }
    })
    
    # ===== RUN INSTRUCTOR FUNCTION =====
    # Should never error (instructor code should be tested)
    # But catch anyway for robustness
    expected_out <- tryCatch(
      do.call(instructor_fun, input_args),
      error = function(e) {
        structure(list(error = e$message), class = "error")
      }
    )
    
    # ===== PACKAGE RESULTS =====
    # Return list with outputs and index
    # Index is needed for matching with test metadata later
    list(
      student = student_out,
      expected = expected_out,
      index = i
    )
  })
  
  results
}
