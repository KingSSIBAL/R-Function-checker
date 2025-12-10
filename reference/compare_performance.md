# Compare Performance of Student vs Instructor Implementation

Benchmarks the student's function implementation against the
instructor's reference solution to evaluate computational efficiency.

This function measures:

- Execution time (median, mean, min, max)

- Relative performance (student vs instructor ratio)

- Consistency (standard deviation)

## Usage

``` r
compare_performance(
  function_name,
  n_runs = 100L,
  warmup = 5L,
  test_inputs = NULL,
  verbose = TRUE
)
```

## Arguments

- function_name:

  Character string. Name of the problem/function to benchmark.

- n_runs:

  Integer. Number of benchmark iterations (default: 100).

- warmup:

  Integer. Number of warmup runs before timing (default: 5).

- test_inputs:

  List. Custom test inputs for benchmarking. If NULL, uses test cases
  from the problem definition.

- verbose:

  Logical. Whether to print detailed output (default: TRUE).

## Value

A list with class "performance_comparison" containing:

- function_name:

  The function being tested

- n_runs:

  Number of benchmark iterations

- student_times:

  Vector of student execution times (seconds)

- instructor_times:

  Vector of instructor execution times (seconds)

- student_stats:

  Summary statistics for student (median, mean, sd, min, max)

- instructor_stats:

  Summary statistics for instructor

- ratio:

  Performance ratio (student/instructor, \>1 means slower)

- passed_correctness:

  Whether student output matches instructor

- verdict:

  Human-readable performance assessment

## Examples

``` r
if (FALSE) { # \dontrun{
# Define student function
student_fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  fib <- c(1, 1)
  for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
  fib
}

# Run performance comparison
result <- compare_performance("fibonacci")

# Custom number of runs
result <- compare_performance("fibonacci", n_runs = 500)

# Custom test inputs
result <- compare_performance("fibonacci", 
  test_inputs = list(list(20), list(30), list(50)))
} # }
```
