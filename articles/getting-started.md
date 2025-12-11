# Getting Started with Autograder

## Introduction

The `autograder` package provides an automated testing framework for R
programming assignments. It’s designed to help students learn by
providing immediate, detailed feedback on their code.

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("KingSSIBAL/R-Function-checker/autograder")
```

## Quick Start

### 1. Load the Package

``` r
library(autograder)
```

### 2. See Available Problems

``` r
list_problems()
```

This will show you which functions you can implement and test:

    Available problems:

      - fibonacci
      - factorial
      - sum_vector

    Usage:
      student_<function_name> <- function(...) { ... }
      autograder('<function_name>')

### 3. Preview Test Cases

Before implementing, see what tests your function needs to pass:

``` r
preview_tests("fibonacci")
```

Output:

    === Test Cases Preview ===

    [Test 1] Base case: n = 1 (1 pt)
      Input: list(1)

    [Test 2] Small input: n = 5 (2 pt)
      Input: list(5)

    [Test 3] Medium input: n = 10 (2 pt)
      Input: list(10)

    ...

### 4. Implement Your Solution

Create your function with the prefix `student_`:

``` r
student_fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  
  fib <- c(1, 1)
  for (i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  fib
}
```

### 5. Run the Autograder

``` r
autograder("fibonacci")
```

If all tests pass:

    === Running Tests ===
    [Test 1] Base case: n = 1 (1 pt): PASS
    [Test 2] Small input: n = 5 (2 pt): PASS
    [Test 3] Medium input: n = 10 (2 pt): PASS
    [Test 4] Larger input: n = 15 (3 pt): PASS
    [Test 5] (1 pt): PASS
    [Test 6] (1 pt): PASS

    === Summary ===
    Score: 10/10 points (100.0%)
    Tests: 6/6 passed (100.0%)

    ✓ ALL TESTS PASSED! Excellent work!

## Understanding Feedback

When a test fails, the autograder provides detailed feedback:

``` r
# A buggy implementation
student_fibonacci <- function(n) {
  rep(0, n)  # Wrong!
}

autograder("fibonacci")
```

Output:

    [Test 2] Small input: n = 5 (2 pt): FAIL
      Input:    list(5)
      Expected: c(1, 1, 2, 3, 5)
      Got:      c(0, 0, 0, 0, 0)

      Feedback:
        * Differences at positions: 1, 2, 3, 4, 5

## Autograder Options

The
[`autograder()`](https://kingssibal.github.io/R-Function-checker/reference/autograder.md)
function has several options:

``` r
autograder("fibonacci", 
  verbose = TRUE,       # Show detailed output (default: TRUE)
  show_hidden = FALSE,  # Show hidden test details (default: FALSE)
  show_progress = TRUE, # Show progress bar (default: FALSE)
  use_parallel = TRUE,  # Use parallel execution (default: TRUE)
  show_hints = TRUE     # Show hints for failures (default: TRUE)
)
```

## Comparing Performance

After passing all tests, you can compare your implementation’s
performance against the reference solution:

``` r
result <- compare_performance("fibonacci", n_runs = 100)
```

    === Performance Comparison ===
    Function: fibonacci
    Runs: 100 | Inputs: 4

    ✓ Correctness: PASSED

    Timing (seconds per batch):
                        Student   Instructor
      Median:          0.000150     0.000120
      Mean:            0.000155     0.000125
      ...

    Performance Ratio: 1.25x (student/instructor)
    Verdict: SLOWER - Your implementation is somewhat slower.

You can also visualize the comparison:

``` r
plot(result)
```

## Next Steps

- Read the [Student
  Guide](https://kingssibal.github.io/R-Function-checker/articles/student-guide.md)
  for more detailed usage
- See [Instructor
  Guide](https://kingssibal.github.io/R-Function-checker/articles/instructor-guide.md)
  if you’re creating test cases
- Learn about [Data
  Files](https://kingssibal.github.io/R-Function-checker/articles/data-files.md)
  for data analysis problems
- Understand
  [Authentication](https://kingssibal.github.io/R-Function-checker/articles/authentication.md)
  for private repositories
- Check out [Performance
  Benchmarking](https://kingssibal.github.io/R-Function-checker/articles/performance.md)
  for optimization tips
