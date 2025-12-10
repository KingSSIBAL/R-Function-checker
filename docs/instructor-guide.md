# Instructor Guide for R-Function-checker

## Overview

R Function Checker is an autograding system for R programming courses that provides instant feedback to students while ensuring assessment integrity with secure test case management, private repository authentication, and data file support.

As an instructor, your role is to:
1. Create test cases for your assignments
2. Configure authentication (public or private repository)
3. Create data files for tests that need external data
4. Build and distribute the package to students

## 📁 Repository Structure

```
R-Function-checker/
├── autograder/              # The R package (distribute to students)
│   ├── R/                   # R source files
│   ├── src/                 # C++ source (modular architecture)
│   │   ├── core/           # Types, exceptions
│   │   ├── crypto/         # URL encryption, auth config
│   │   ├── validation/     # Input validation
│   │   ├── compare/        # Fast comparison
│   │   ├── network/        # Secure fetching with auth
│   │   └── format/         # Output formatting
│   └── tests/              # 2000+ unit tests
├── repo/                    # Test case repository
│   ├── functions/          # Your test case files
│   └── data/               # Data files (CSV, Excel, RDS, etc.)
├── tools/                   # Instructor utilities
│   ├── encrypt_url_helper.R # Main setup script
│   ├── lib/                # Modular helper functions
│   └── .env                # Credentials (gitignored)
└── docs/                    # Documentation
```

## 🚀 Quick Start

### Option 1: Public Repository (Legacy Mode)

For open courses where test cases can be publicly accessible:

```r
source("tools/encrypt_url_helper.R")
setup_legacy_mode()
```

### Option 2: Private Repository (Secure Mode)

For exams or when test cases must be protected:

1. **Create a `.env` file** in `tools/`:
```env
AUTH_MODE=secure
BASE_URL=https://raw.githubusercontent.com/YourOrg/PrivateRepo/main
GITHUB_TOKEN=github_pat_xxxxxxxxxxxx
```

2. **Get a GitHub token:**
   - Go to: GitHub → Settings → Developer Settings → Personal Access Tokens
   - Create a fine-grained token with `Contents: Read-only` permission
   - Scope it to your private repository

3. **Run setup:**
```r
source("tools/encrypt_url_helper.R")
test_token()       # Verify token works
setup_secure_mode()
```

4. **Build and distribute:**
```bash
cd autograder
R CMD build .
R CMD INSTALL autograder_*.tar.gz
```

## 🔐 Authentication Modes

| Mode | Use Case | Security Level |
|------|----------|----------------|
| **Legacy** | Open courses, shared test cases | Low (public) |
| **Secure** | Exams, private courses | High (token auth) |

### Security Notes

- **Token is embedded** in the built package (encrypted)
- **Each build is unique** with different encryption
- **Hidden tests still matter** - use `hidden = TRUE` for critical tests
- **Expire tokens** after the semester

## 📝 Creating Test Cases

### Test Case Template

Create `repo/functions/your_function.R`:

```r
# ============================================================================
# FUNCTION: your_function
# Description: Brief description of what the function should do
# ============================================================================

# Reference implementation (instructor's solution)
your_function <- function(x, y) {
    # Correct implementation
    x + y
}

# Test cases
test_cases <- list(
    # Required: input sets
    inputs = list(
        list(x = 1, y = 2),
        list(x = 0, y = 0),
        list(x = -5, y = 10),
        list(x = 1.5, y = 2.5)
    ),
    
    # Optional: test descriptions
    descriptions = c(
        "Basic addition",
        "Zero case",
        "Negative numbers",
        "Decimal numbers"
    ),
    
    # Optional: hide tests from students
    hidden = c(FALSE, FALSE, TRUE, TRUE),
    
    # Optional: point values
    points = c(1, 1, 2, 2),
    
    # Optional: comparison tolerance
    tolerance = 1e-10,
    
    # Optional: expected return type
    expected_type = "numeric",
    
    # Optional: hints for failed tests
    hints = c(
        "Check basic arithmetic",
        "Consider edge cases",
        "Negative + positive can be either",
        "Decimals work the same way"
    )
)
```

### Register the Problem

Add to `repo/functions/_problems.R`:

```r
problems <- c(
    "factorial",
    "fibonacci",
    "sum_vector",
    "your_function"  # Add new function here
)
```

## 🎓 Pedagogical Best Practices

### 1. Balance Visible and Hidden Tests

```r
# 3 visible (students can see), 3 hidden (students cannot see)
hidden = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
```

- **Visible tests**: Help students understand the problem
- **Hidden tests**: Prevent hardcoding, test edge cases

### 2. Progressive Difficulty

```r
inputs = list(
    list(n = 1),    # Easy
    list(n = 5),    # Medium
    list(n = 100)   # Hard
)
points = c(1, 2, 3)  # Higher points for harder tests
```

### 3. Test Edge Cases

Common edge cases to include:
- Empty inputs: `c()`, `list()`, `""`
- Single elements: `c(1)`
- Negative numbers: `-5`

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

### Creating Data Files

Place data files in `repo/data/`:

```r
# CSV
write.csv(my_data, "repo/data/scores.csv", row.names = FALSE)

# RDS (any R object)
saveRDS(my_model, "repo/data/model.rds")

# Excel
openxlsx::write.xlsx(my_data, "repo/data/inventory.xlsx")
```

### Using Data in Test Cases

Reference data files in your test case inputs:

```r
test_cases <- list(
  # Files to prefetch (downloaded once)
  data_files = c("scores.csv", "inventory.xlsx"),
  
  inputs = list(
    # Filename is replaced with loaded data at runtime
    list(data = "scores.csv", type = "mean"),
    list(data = "inventory.xlsx", type = "sum"),
    # Can mix with direct data
    list(data = data.frame(x = 1:5), type = "count")
  ),
  
  descriptions = c("CSV analysis", "Excel analysis", "Direct data"),
  points = c(2, 2, 1)
)
```

### Example: Data Analysis Function

```r
# repo/functions/analyze_data.R

analyze_data <- function(data, type = "mean") {
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  switch(type,
    "mean" = sapply(numeric_data, mean),
    "sum" = sapply(numeric_data, sum),
    "count" = nrow(data)
  )
}

test_cases <- list(
  data_files = c("sample_scores.csv", "measurements.rds", "inventory.xlsx"),
  
  inputs = list(
    list(data = "sample_scores.csv", type = "mean"),
    list(data = "measurements.rds", type = "summary"),
    list(data = "inventory.xlsx", type = "sum")
  ),
  
  descriptions = c("CSV mean", "RDS summary", "Excel sum"),
  hidden = c(FALSE, TRUE, FALSE),
  points = c(2, 3, 2)
)
```

### 3. Test Edge Cases

Common edge cases to include:
- Empty inputs: `c()`, `list()`, `""`
- Single elements: `c(1)`
- Negative numbers: `-5`
- Large numbers: `1e10`
- Special values: `NA`, `NULL`, `Inf`, `NaN`
- Boundary values: `0`, `1`, max/min

### 4. Provide Helpful Hints

```r
hints = c(
    "Check your base case",
    "Are you handling negative inputs?",
    "Consider using vectorization"
)
```

Hints should guide without giving away the answer.

## 🔧 Testing Your Test Cases

Before deploying, verify your test cases work:

```r
library(autograder)

# Load your test file
source("repo/functions/your_function.R")

# Test with a correct implementation
student_your_function <- your_function  # Use reference

# This should pass all tests
autograder("your_function")

# Test with an incorrect implementation
student_your_function <- function(x, y) x * y  # Wrong!

# This should fail some tests
autograder("your_function")
```

## 📊 Grading Workflow

### For In-Class Use

1. Students install your pre-built package
2. Students write solutions in their own R session
3. Students run `autograder("problem_name")`
4. Students see immediate feedback
5. Final grades can be collected via Canvas, etc.

### For Automated Grading

```r
# Run all problems and collect results
results <- list()
for (problem in list_problems()) {
    result <- autograder(problem, verbose = FALSE)
    results[[problem]] <- result
}

# Calculate total score
total_points <- sum(sapply(results, function(r) r$score))
max_points <- sum(sapply(results, function(r) r$max_score))
percentage <- total_points / max_points * 100
```

## ❓ Troubleshooting

### "Function not found"
- Check that the function is listed in `_problems.R`
- Verify the file name matches the function name
- Ensure the repository is accessible

### Test case validation errors
- Check that `inputs` is a list of lists
- Verify all optional vectors have the same length as `inputs`
- Run validation: `autograder:::validate_test_cases(test_cases)`

### Encryption issues
- Re-run `encrypt_url_helper.R` after URL changes
- Rebuild the package after encryption
- Check that key factors match between encryption and package

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/KingSSIBAL/R-Function-checker/issues)
- **Email**: rcagub@up.edu.ph
- **Documentation**: See other files in `docs/`
