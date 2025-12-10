# Test Case Repository

This directory contains test cases, reference implementations, and data files used by the autograder.

## 📁 Structure

```
repo/
├── README.md              # This file
├── functions/
│   ├── _problems.R        # List of available problems
│   ├── factorial.R        # Test cases for factorial
│   ├── fibonacci.R        # Test cases for fibonacci
│   ├── sum_vector.R       # Test cases for sum_vector
│   ├── analyze_data.R     # Test cases using data files
│   └── your_function.R    # Add your own!
└── data/
    ├── sample_scores.csv  # CSV data file
    ├── measurements.rds   # RDS data file
    ├── growth_data.RData  # RData file with multiple objects
    ├── inventory.xlsx     # Excel data file
    └── sales_data.txt     # Text data file
```

## 🚀 Quick Start (Instructors)

### Step 1: Create a New Function File

Create `repo/functions/my_function.R`:

```r
# ============================================================================
# Reference Implementation (Instructor's Solution)
# ============================================================================

my_function <- function(x, y) {
    # Your correct implementation
    x + y
}

# ============================================================================
# Test Cases
# ============================================================================

test_cases <- list(
    # Required: list of input sets
    inputs = list(
        list(x = 1, y = 2),
        list(x = 0, y = 0),
        list(x = -5, y = 10),
        list(x = 1.5, y = 2.5)
    ),
    
    # Optional: descriptions for each test
    descriptions = c(
        "Basic addition",
        "Zero case",
        "Negative numbers",
        "Decimal numbers"
    ),
    
    # Optional: hide some tests from students
    hidden = c(FALSE, FALSE, TRUE, TRUE),
    
    # Optional: point values
    points = c(1, 1, 2, 2),
    
    # Optional: tolerance for numeric comparison
    tolerance = 1e-10,
    
    # Optional: expected return type
    expected_type = "numeric",
    
    # Optional: hints for failed tests
    hints = c(
        "Check basic arithmetic",
        "Consider edge cases",
        "Remember: negative + positive can be positive or negative",
        "Floating point should work the same way"
    )
)
```

### Step 2: Register the Problem

Add to `repo/functions/_problems.R`:

```r
problems <- c(
    "factorial",
    "fibonacci",
    "sum_vector",
    "my_function"  # Add your new function
)
```

### Step 3: Test Locally

```r
library(autograder)

# Define a student solution
student_my_function <- function(x, y) {
    x + y
}

# Test it
autograder("my_function")
```

## 📝 Test Case Format

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `inputs` | `list` | List of input argument lists |

### Optional Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `descriptions` | `character` | Auto-generated | Test descriptions |
| `hidden` | `logical` | `FALSE` | Hide test from students |
| `points` | `numeric` | `1` | Point value per test |
| `tolerance` | `numeric` | `1e-10` | Floating-point tolerance |
| `expected_type` | `character` | Auto-detect | Expected return type |
| `hints` | `character` | `NULL` | Hints for failed tests |
| `data_files` | `character` | `NULL` | Data files to prefetch |

## 📊 Data File Support

Test cases can use external data files stored in the `data/` folder.

### Supported Formats

| Extension | Loader | Returns |
|-----------|--------|---------|
| `.csv` | `read.csv()` | data.frame |
| `.xlsx`, `.xls` | `readxl::read_excel()` | data.frame |
| `.rds` | `readRDS()` | Any R object |
| `.RData`, `.rda` | `load()` | List of objects |
| `.txt` | `readLines()` | Character vector |

### Example with Data Files

```r
# Function that analyzes data
analyze_data <- function(data, type = "mean") {
  numeric_cols <- sapply(data, is.numeric)
  if (type == "mean") sapply(data[, numeric_cols], mean)
  else if (type == "count") nrow(data)
  else sapply(data[, numeric_cols], sum)
}

test_cases <- list(
  # Data files to prefetch (downloaded once)
  data_files = c("sample_scores.csv", "inventory.xlsx"),
  
  inputs = list(
    # Reference filename - replaced with loaded data at runtime
    list(data = "sample_scores.csv", type = "mean"),
    list(data = "inventory.xlsx", type = "sum"),
    # Can also use direct data (no file)
    list(data = data.frame(x = 1:5), type = "count")
  ),
  
  descriptions = c("CSV mean", "Excel sum", "Direct data"),
  points = c(2, 2, 1)
)
```

## 🎓 Pedagogical Best Practices

### 1. Mix Visible and Hidden Tests

```r
hidden = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
```

- **Visible tests**: Help students understand the problem
- **Hidden tests**: Prevent hardcoding solutions

### 2. Progressive Difficulty

```r
inputs = list(
    list(n = 1),   # Easy: base case
    list(n = 5),   # Medium: small input
    list(n = 100)  # Hard: large input
)
points = c(1, 2, 3)  # More points for harder tests
```

### 3. Test Edge Cases

- Empty inputs: `list()`, `c()`, `""`
- Single elements: `list(x = 1)`
- Negative numbers: `list(x = -5)`
- Large numbers: `list(x = 1e10)`
- Special values: `NA`, `NULL`, `Inf`, `NaN`

### 4. Helpful Hints

```r
hints = c(
    "Check your base case",
    "Are you handling negative inputs?",
    "Consider using vectorization for efficiency"
)
```

## 🔒 Security Notes

1. **URL Encryption**: Use `tools/encrypt_url_helper.R` to encrypt your repository URL
2. **Hidden Tests**: Keep critical test cases hidden to prevent cheating
3. **Private Repository**: Consider hosting on a private GitHub repo

## ✅ Validation Checklist

Before deploying, verify:

- [ ] Reference function produces correct outputs
- [ ] All test cases run without errors
- [ ] `_problems.R` includes the new function
- [ ] Point values sum to expected total
- [ ] Hidden tests cover edge cases
- [ ] Tolerance is appropriate for floating-point results

## Support

Contact: rcagub@up.edu.ph
