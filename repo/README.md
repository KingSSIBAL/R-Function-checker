# Test Case Repository

Test cases and data files for the autograder.

## Structure

```
repo/
├── functions/       # Test case files (.R)
│   ├── _problems.R  # Problem list
│   ├── fibonacci.R
│   └── ...
└── data/            # Data files (CSV, RDS, etc.)
```

## Creating Test Cases

```r
# repo/functions/my_function.R

my_function <- function(x, y) { x + y }  # Reference solution

test_cases <- list(
  inputs = list(
    list(x = 1, y = 2),
    list(x = -5, y = 10)
  ),
  descriptions = c("Basic", "Negative"),
  hidden = c(FALSE, TRUE),
  points = c(1, 2)
)
```

Register in `_problems.R`:

```r
problems <- c("fibonacci", "factorial", "my_function")
```

## Data Files

Reference data in test cases:

```r
test_cases <- list(
  inputs = list(list(data = "scores.csv")),
  data_files = c("scores.csv")
)
```

Supported: CSV, RDS, RData, Excel, TXT
