# R Function Checker (Autograder)

[![R-CMD-check](https://github.com/KingSSIBAL/R-Function-checker/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/KingSSIBAL/R-Function-checker/actions/workflows/R-CMD-check.yml)
[![pkgdown](https://github.com/KingSSIBAL/R-Function-checker/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/KingSSIBAL/R-Function-checker/actions/workflows/pkgdown.yml)
[![R Package](https://img.shields.io/badge/R-Package-blue.svg)](https://www.r-project.org/)
[![C++17](https://img.shields.io/badge/C++-17-green.svg)](https://isocpp.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> Automated grading system for R programming assignments with a **modern modular C++ backend**, parallel execution, intelligent feedback, secure authentication, and data file support.

## ✨ Features

- **Modular C++ Architecture**: Clean separation of concerns with specialized modules
- **High-Performance Comparison**: 10-100x faster than R's `identical()` for common types
- **Secure Authentication**: Support for private repositories with GitHub token authentication
- **Data File Support**: Load CSV, Excel, RDS, RData, and TXT files for test cases
- **AES-Based Encryption**: Secure encryption helpers for test case protection
- **Parallel Execution**: Automatic parallel processing for large test sets
- **Smart Feedback**: Type-aware feedback to help students learn

## 📁 Project Structure


```
R-Function-checker/
├── autograder/              # R Package
│   ├── R/                   # R source files
│   ├── src/                 # C++ backend (flat structure)
│   │   ├── autograder.cpp   # Main entry points
│   │   ├── autograder.h     # Public header
│   │   ├── comparator.h     # Fast comparison engine
│   │   ├── encryption.h     # AES encryption
│   │   ├── validator.h      # Input validation
│   │   ├── fetcher.h        # Network fetching with auth
│   │   ├── formatter.h      # Output formatting
│   │   ├── types.h          # Core types
│   │   └── exceptions.h     # Custom exceptions
│   └── tests/testthat/      # 2000+ unit tests
├── repo/                    # Test case repository
│   ├── functions/           # Function files with test cases
│   └── data/                # Data files for test cases
├── tools/                   # Instructor utilities
│   ├── encrypt_url_helper.R # Main setup script
│   ├── lib/                 # Modular helper functions
│   └── .env                 # Secure credentials (gitignored)
└── docs/                    # Documentation
```

## 🚀 Quick Start (Students)

```r
# Install from source
install.packages("autograder_0.4.0.tar.gz", repos = NULL, type = "source")

# Or from GitHub
devtools::install_github("KingSSIBAL/R-Function-checker/autograder")

library(autograder)

# See available problems
list_problems()

# Preview test cases
preview_tests("fibonacci")

# Define your solution
student_fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  fib <- c(1, 1)
  for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
  fib
}

# Run autograder
autograder("fibonacci")
```

## 📊 Available Problems

| Problem | Description | Points |
|---------|-------------|--------|
| `fibonacci` | Generate Fibonacci sequence | 10 |
| `factorial` | Calculate factorial | 8 |
| `sum_vector` | Sum elements of a vector | 6 |
| `analyze_data` | Analyze data from files (CSV, Excel, RDS) | 14 |

## 🔐 Security Features

### Authentication Modes

| Mode | Description | Use Case |
|------|-------------|----------|
| **Legacy** | Public repository, no authentication | Open courses |
| **Secure** | Private repo + GitHub token | Exam security |

### Data File Support

Test cases can use external data files:

```r
test_cases <- list(
  inputs = list(
    list(data = "scores.csv", type = "mean"),
    list(data = "inventory.xlsx", type = "sum")
  ),
  data_files = c("scores.csv", "inventory.xlsx")
)
```

**Supported formats:** CSV, Excel (xlsx/xls), RDS, RData, TXT

## 🔧 C++ Modules


| Header         | Purpose                        | Key Functions |
|--------------- |--------------------------------|---------------|
| `types.h`      | Core types                     | `Config`, `CryptoResult`, `TestResult` |
| `exceptions.h` | Error handling                 | `AutograderException`, custom errors |
| `encryption.h` | AES encryption                 | `encrypt()`, `decrypt()`, `derive_key()` |
| `validator.h`  | Input validation               | `is_valid_function_name()`, security checks |
| `comparator.h` | Fast comparison                | `compare()`, tolerance handling |
| `fetcher.h`    | Network fetching               | Token auth, data file download |
| `formatter.h`  | Output display                 | Smart truncation, formatting |

## 📊 Performance

| Operation | R (identical) | C++ (compare) | Speedup |
|-----------|---------------|---------------|---------|
| 1M numeric vector | ~150ms | ~2ms | **75x** |
| Different vectors | ~150ms | <1ms | **150x+** |

## 🛠️ Instructor Setup

### Option 1: Public Repository (Legacy Mode)

```r
source("tools/encrypt_url_helper.R")
setup_legacy_mode()
```

### Option 2: Private Repository (Secure Mode)

1. Create a `.env` file in `tools/`:
```env
AUTH_MODE=secure
BASE_URL=https://raw.githubusercontent.com/YourOrg/PrivateRepo/main
GITHUB_TOKEN=github_pat_xxxxxxxxxxxx
```

2. Run setup:
```r
source("tools/encrypt_url_helper.R")
test_token()  # Verify token works
setup_secure_mode()
```

3. Build and distribute:
```bash
R CMD build autograder
R CMD INSTALL autograder_*.tar.gz
```

See [Instructor Guide](docs/instructor-guide.md) for detailed instructions.

## 📦 Building the Package

```bash
cd autograder
R CMD build .
R CMD check autograder_0.4.0.tar.gz
R CMD INSTALL autograder_0.4.0.tar.gz
```

## 🧪 Running Tests

```r
# Run all tests (2000+)
devtools::test()

# Run specific test file
devtools::test(filter = "data-loading")
devtools::test(filter = "auth")
```

## 📄 License

MIT License © 2025 Reijel Agub

## 🔗 Links

- [Student Guide](docs/student-guide.md)
- [Instructor Guide](docs/instructor-guide.md)
- [Tools Documentation](tools/README.md)
- [Repository Structure](repo/README.md)
- [C++ Source Guide](autograder/src/README.md)
- [Issues](https://github.com/KingSSIBAL/R-Function-checker/issues)

## 📧 Contact

Reijel Agub - rcagub@up.edu.ph