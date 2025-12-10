# Instructor Guide

## Overview

This guide explains how instructors can create and manage programming
assignments using the autograder package. The system allows you to:

- Create reference functions with test cases
- Host problems on GitHub for easy distribution
- Track student performance with point-weighted tests
- Include hidden tests for comprehensive evaluation

## Repository Structure

Set up a GitHub repository with this structure:

    your-course-repo/
    ├── README.md
    └── functions/
        ├── _problems.R          # Registry of available problems
        ├── fibonacci.R          # Problem: fibonacci sequence
        ├── factorial.R          # Problem: factorial function
        └── sum_vector.R         # Problem: vector summation

## Creating the Problem Registry

Create `functions/_problems.R` to list all available problems:

``` r
# functions/_problems.R
# List of all available problems in this repository
problems <- c(
  "fibonacci",
  "factorial", 
  "sum_vector",
  "binary_search",
  "matrix_multiply"
)
```

Students use
[`list_problems()`](https://kingsibal.github.io/R-Function-checker/reference/list_problems.md)
to see this list.

## Creating a Problem File

Each problem file contains: 1. **Reference function** (instructor’s
solution) 2. **Test cases** with points and descriptions 3. **Optional
hidden tests**

### Basic Structure

``` r
# functions/fibonacci.R

# Reference function (instructor's solution)
instructor_fibonacci <- function(n) {
  if (n <= 0) return(integer(0))
  if (n == 1) return(1L)
  if (n == 2) return(c(1L, 1L))
  
  fib <- integer(n)
  fib[1:2] <- 1L
  for (i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  fib
}

# Test cases
test_cases <- list(
  # Visible tests (students see inputs and expected outputs)
  list(
    description = "Edge case: n = 0 returns empty vector",
    input = list(0),
    points = 1
  ),
  list(
    description = "Base case: n = 1",
    input = list(1),
    points = 1
  ),
  list(
    description = "Base case: n = 2", 
    input = list(2),
    points = 1
  ),
  list(
    description = "Small sequence: n = 5",
    input = list(5),
    points = 2
  ),
  list(
    description = "Medium sequence: n = 10",
    input = list(10),
    points = 2
  ),
  
  # Hidden tests (students don't see these)
  list(
    description = "Hidden: Large sequence",
    input = list(20),
    points = 3,
    hidden = TRUE
  ),
  list(
    description = "Hidden: Edge case handling",
    input = list(-1),
    points = 2,
    hidden = TRUE
  )
)
```

## Test Case Components

### Required Fields

| Field         | Type    | Description                       |
|---------------|---------|-----------------------------------|
| `description` | string  | What this test checks             |
| `input`       | list    | Arguments to pass to the function |
| `points`      | integer | Point value (weight)              |

### Optional Fields

| Field      | Type    | Description                                |
|------------|---------|--------------------------------------------|
| `hidden`   | logical | If TRUE, test details hidden from students |
| `expected` | any     | Override expected output (advanced use)    |

## Writing Effective Test Cases

### 1. Start with Edge Cases

``` r
# Empty input
list(description = "Empty vector", input = list(c()), points = 1)

# Single element
list(description = "Single element", input = list(c(5)), points = 1)

# Zero/null values
list(description = "Zero input", input = list(0), points = 1)
```

### 2. Cover Normal Cases

``` r
# Typical inputs
list(description = "Small input", input = list(c(1, 2, 3)), points = 2)
list(description = "Medium input", input = list(1:10), points = 2)
```

### 3. Include Stress Tests

``` r
# Large inputs (often hidden)
list(
  description = "Large input performance", 
  input = list(1:1000), 
  points = 3,
  hidden = TRUE
)
```

### 4. Test Type Handling

``` r
# Different input types
list(description = "Integer input", input = list(5L), points = 1)
list(description = "Double input", input = list(5.0), points = 1)
list(description = "Negative input", input = list(-5), points = 1)
```

## Multiple Arguments

For functions with multiple arguments:

``` r
# Reference function
instructor_power <- function(base, exponent) {
  base ^ exponent
}

# Test cases
test_cases <- list(
  list(
    description = "Simple power: 2^3",
    input = list(2, 3),  # base = 2, exponent = 3
    points = 1
  ),
  list(
    description = "Negative exponent: 2^-1",
    input = list(2, -1),  # base = 2, exponent = -1
    points = 2
  ),
  list(
    description = "Zero exponent: 5^0",
    input = list(5, 0),
    points = 1
  )
)
```

## Point Weighting Strategy

Recommended point distribution:

| Test Type           | Points | Percentage |
|---------------------|--------|------------|
| Edge cases          | 1-2    | ~20%       |
| Basic functionality | 2-3    | ~40%       |
| Advanced cases      | 3-4    | ~25%       |
| Hidden stress tests | 3-5    | ~15%       |

Example for a 20-point problem:

``` r
test_cases <- list(
  # Edge cases (4 points)
  list(description = "Empty input", input = list(c()), points = 2),
  list(description = "Single element", input = list(c(1)), points = 2),
  
  # Basic functionality (8 points)
  list(description = "Small array", input = list(1:5), points = 2),
  list(description = "Medium array", input = list(1:10), points = 3),
  list(description = "Negative numbers", input = list(-5:-1), points = 3),
  
  # Advanced (5 points)
  list(description = "Mixed values", input = list(c(-2, 0, 2)), points = 2),
  list(description = "Duplicates", input = list(c(1, 1, 2, 2)), points = 3),
  
  # Hidden (3 points)
  list(description = "Large input", input = list(1:1000), points = 3, hidden = TRUE)
)
```

## Hosting on GitHub

### 1. Create Repository

Create a public GitHub repository (e.g., `stat101-assignments`).

### 2. Upload Problem Files

Push your problem files to the repository.

### 3. Configure Autograder

Students point the autograder to your repository:

``` r
library(autograder)

# Set your course repository
options(autograder_repo = "username/stat101-assignments")

# Now students can list and test problems
list_problems()
autograder("fibonacci")
```

### 4. Repository Settings

The default base URL is:

    https://raw.githubusercontent.com/{repo}/main/functions/

You can customize this in the package configuration.

## Security Considerations

### Private Repositories

For maximum security, use a private repository with token
authentication:

1.  Create a private GitHub repository
2.  Generate a fine-grained personal access token
3.  Configure secure mode (see [Authentication
    Guide](https://kingsibal.github.io/R-Function-checker/articles/authentication.md))

### Hidden Tests

Hidden tests protect your grading criteria:

``` r
list(
  description = "Hidden: Stress test",
  input = list(1:10000),
  points = 5,
  hidden = TRUE
)
```

Students see: `[Test 6] Hidden test (5 pts): PASS/FAIL` But NOT the
actual input or expected output.

### Code Encryption

The autograder encrypts function code during transit using RSA
encryption. Reference implementations are protected from direct
inspection.

### Best Practices

1.  **Use hidden tests** for edge cases you don’t want revealed
2.  **Rotate problems** each semester to prevent sharing
3.  **Include unique test values** that are hard to guess
4.  **Monitor for suspicious patterns** in submissions
5.  **Use private repositories** for exams (see
    [Authentication](https://kingsibal.github.io/R-Function-checker/articles/authentication.md))

## Data Files

For problems requiring external data (CSV, Excel, RDS), see the [Data
Files
Guide](https://kingsibal.github.io/R-Function-checker/articles/data-files.md).
Quick example:

``` r
test_cases <- list(
  # Declare data files
  data_files = c("scores.csv", "inventory.xlsx"),
  
  inputs = list(
    # Filename is replaced with loaded data
    list(data = "scores.csv", type = "mean"),
    list(data = "inventory.xlsx", type = "sum")
  ),
  
  descriptions = c("CSV analysis", "Excel analysis"),
  points = c(2, 2)
)
```

## Troubleshooting

### “Problem not found”

Check that: 1. The problem is listed in `_problems.R` 2. The filename
matches (e.g., `fibonacci.R` for “fibonacci”) 3. The repository is
accessible

### “Invalid test cases”

Ensure each test case has: - `description` (string) - `input` (list) -
`points` (positive integer)

### “Network error”

Check: - Repository is public - GitHub is accessible - Base URL is
correct

## Example Complete Problem

``` r
# functions/mean_absolute_deviation.R

# Reference function
instructor_mean_absolute_deviation <- function(x) {
  if (length(x) == 0) return(NA_real_)
  mean(abs(x - mean(x)))
}

# Test cases
test_cases <- list(
  # Edge cases
  list(
    description = "Empty vector returns NA",
    input = list(c()),
    points = 2
  ),
  list(
    description = "Single element returns 0",
    input = list(c(5)),
    points = 2
  ),
  
  # Basic functionality
  list(
    description = "Simple case: c(1, 2, 3)",
    input = list(c(1, 2, 3)),
    points = 3
  ),
  list(
    description = "Larger range: 1:10",
    input = list(1:10),
    points = 3
  ),
  
  # Advanced cases
  list(
    description = "Negative values",
    input = list(c(-3, -1, 0, 1, 3)),
    points = 3
  ),
  list(
    description = "Floating point values",
    input = list(c(1.5, 2.5, 3.5)),
    points = 2
  ),
  
  # Hidden tests
  list(
    description = "Hidden: Large vector",
    input = list(rnorm(1000)),
    points = 3,
    hidden = TRUE
  ),
  list(
    description = "Hidden: All same values",
    input = list(rep(5, 100)),
    points = 2,
    hidden = TRUE
  )
)
```
