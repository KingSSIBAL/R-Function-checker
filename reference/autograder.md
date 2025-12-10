# Run autograder by function name

Tests student implementation against reference outputs with support for
points, descriptions, hidden tests, type checking, tolerance, parallel
execution, and detailed feedback.

This is the main function students use to test their work.

## Usage

``` r
autograder(
  function_name,
  verbose = TRUE,
  show_hidden = FALSE,
  show_progress = FALSE,
  use_parallel = TRUE,
  show_hints = TRUE
)
```

## Arguments

- function_name:

  Character. Name of the function to test. Must match a function in the
  repository (see list_problems()). Example: "fibonacci", "factorial",
  "sum_vector"

- verbose:

  Logical. Show detailed output for each test? TRUE (default): Shows
  inputs, expected, actual output for failures FALSE: Shows only test
  result (PASS/FAIL) and summary

- show_hidden:

  Logical. Show details for hidden tests? FALSE (default): Hidden tests
  show only PASS/FAIL TRUE: Hidden tests show full details (for
  instructor review)

- show_progress:

  Logical. Show progress bar for many tests? FALSE (default): No
  progress bar TRUE: Shows progress bar if ≥6 tests

- use_parallel:

  Logical. Use parallel processing? TRUE (default): Automatic parallel
  for ≥10 tests FALSE: Always use sequential execution

- show_hints:

  Logical. Show hints for failed tests? TRUE (default): Display
  instructor hints when available FALSE: Hide hints (for
  exams/assessments)

## Value

Invisibly returns a list with test results:

- `passed`: Integer. Number of tests passed

- `failed`: Integer. Number of tests failed

- `total`: Integer. Total number of tests

- `score`: Numeric. Points earned

- `max_score`: Numeric. Maximum possible points

- `pass_rate`: Numeric. Percentage of tests passed (0-100)

## Usage Workflow

1.  See available problems:
    [`list_problems()`](https://kingsibal.github.io/R-Function-checker/reference/list_problems.md)

2.  Preview test cases: `preview_tests("function_name")`

3.  Define your function:
    `student_function_name <- function(...) { ... }`

4.  Run autograder: `autograder("function_name")`

5.  Review feedback and improve your code

6.  Repeat until all tests pass

## Performance

- Sequential mode: ~100-500ms per test (depends on function complexity)

- Parallel mode: ~50-200ms per test on 4-core system

- Network fetch: ~1-3 seconds (cached by OS after first call)

## Error Messages

The function provides specific, actionable error messages:

- "Function not found" → Use list_problems()

- "Type mismatch" → Check your return type

- "Length mismatch" → Check loop bounds or edge cases

- "Network error" → Check internet connection

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Define your solution
student_fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  fib <- c(1, 1)
  for (i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  fib
}

# 2. Run autograder
result <- autograder("fibonacci")

# 3. Check your score
print(result$pass_rate)  # 100 if all passed

# 4. Run with different options
autograder("fibonacci", verbose = FALSE)           # Minimal output
autograder("fibonacci", show_hints = TRUE)         # Show hints
autograder("fibonacci", use_parallel = FALSE)      # Force sequential
} # }
```
