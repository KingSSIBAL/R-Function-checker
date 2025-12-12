# Test Functions

Test case definitions for autograder problems.

## Files

| File | Problem |
|------|---------|
| `_problems.R` | Problem registry |
| `fibonacci.R` | Fibonacci sequence |
| `factorial.R` | Factorial calculation |
| `sum_vector.R` | Vector sum |
| `analyze_data.R` | Data analysis with files |

## Format

```r
# Reference solution
my_function <- function(x) { ... }

# Test cases
test_cases <- list(
  inputs = list(list(x = 1), list(x = 5)),
  descriptions = c("Basic", "Edge case"),
  hidden = c(FALSE, TRUE),
  points = c(1, 2)
)
```

## Adding Problems

1. Create `new_function.R` with solution and `test_cases`
2. Add to `_problems.R`: `problems <- c(..., "new_function")`
