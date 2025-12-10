# Autograder R Package

[![Version](https://img.shields.io/badge/version-0.4.0-blue)](https://github.com/KingSSIBAL/R-Function-checker)
[![C++17](https://img.shields.io/badge/C++-17-green)](https://isocpp.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> Automated grading system for R programming assignments with a **modular C++ backend**, parallel execution, intelligent feedback, secure authentication, and data file support.

## ✨ Features

- **Modular C++ Architecture**: Clean separation into specialized modules
- **High-Performance Comparison**: 10-100x faster than R's `identical()`
- **Secure Authentication**: Private repository support with GitHub token auth
- **Data File Support**: CSV, Excel, RDS, RData, TXT file loading
- **AES-Based Encryption**: Secure URL encryption for test case protection
- **Parallel Execution**: Automatic parallel processing for large test sets
- **Smart Feedback**: Type-aware feedback to help students learn

## 📁 Package Structure

```
autograder/
├── R/
│   ├── autograder.R        # Main R interface & data loading
│   └── RcppExports.R       # Auto-generated Rcpp exports
├── src/                    # Modular C++ backend (flat structure)
│   ├── autograder.cpp      # Main entry points
│   ├── autograder.h        # Public header
│   ├── comparator.h        # Fast comparison engine
│   ├── encryption.h        # AES encryption
│   ├── validator.h         # Input validation
│   ├── fetcher.h           # Network fetching with auth
│   ├── formatter.h         # Output formatting
│   ├── types.h             # Core types
│   └── exceptions.h        # Custom exceptions
├── man/                    # Documentation
├── vignettes/              # Articles and guides
└── tests/testthat/         # 2000+ unit tests
```

## 🚀 Quick Start

### For Students

```r
# Install the package (provided by instructor)
install.packages("autograder_0.4.0.tar.gz", repos = NULL, type = "source")

library(autograder)

# See available problems
list_problems()

# Preview test cases
preview_tests("fibonacci")

# Write your solution
student_fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  fib <- c(1, 1)
  for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
  fib
}

# Test your solution
autograder("fibonacci")
```

### For Instructors

See the [Instructor Guide](https://kingssibal.github.io/R-Function-checker/articles/instructor-guide.html) for:
- Creating test cases
- Encrypting custom repository URLs
- Building and distributing the package

## 🔐 Encryption API

The package provides encryption helpers for securing sensitive data:

```r
# Encrypt/decrypt text
encrypted <- encrypt_text("my secret data")
decrypted <- decrypt_text(encrypted)

# Use custom key
my_key <- generate_key()
encrypted <- encrypt_text("secret", key = my_key)

# Derive key from factors
key <- derive_key("username", "password", "salt")
```

## 🔧 C++ Modules

| Header | Purpose | Key Functions |
|--------|---------|---------------|
| `types.h` | Core types | `Config`, `CryptoResult`, `TestResult` |
| `exceptions.h` | Error handling | `AutograderException`, custom errors |
| `encryption.h` | AES encryption | `encrypt()`, `decrypt()`, `derive_key()` |
| `validator.h` | Input validation | `is_valid_function_name()`, security checks |
| `comparator.h` | Fast comparison | `compare()`, tolerance handling |
| `fetcher.h` | Network fetching | Token auth, data file download |
| `formatter.h` | Output display | Smart truncation, formatting |

## 📊 Available Problems

| Problem | Description | Data Files |
|---------|-------------|------------|
| `fibonacci` | Generate Fibonacci sequence | None |
| `factorial` | Calculate factorial | None |
| `sum_vector` | Sum elements of a vector | None |
| `analyze_data` | Analyze data from files | CSV, Excel, RDS |

## 📊 Performance

| Operation | R `identical()` | C++ `compare` | Speedup |
|-----------|-----------------|---------------|---------|
| 1M numeric vector | ~150ms | ~2ms | **75x** |
| Mismatch at start | ~150ms | <1ms | **150x+** |
| Large data.frame | ~500ms | ~10ms | **50x** |

## 🛠️ Development

### Prerequisites

- R >= 4.0.0
- C++17 compatible compiler
- Rcpp package

### Building from Source

```bash
# Clone the repository
git clone https://github.com/KingSSIBAL/R-Function-checker.git
cd R-Function-checker/autograder

# Build and install
R CMD build .
R CMD INSTALL autograder_*.tar.gz

# Or using devtools
devtools::install()
```

### Running Tests

```r
devtools::test()
```

### Updating Encrypted URL

To use a custom repository URL:

```r
# From project root
source("tools/encrypt_url_helper.R")
# Script auto-updates src/crypto/encrypted_config.hpp
```

Then rebuild the package.

## 📚 Documentation

| Document | Description |
|----------|-------------|
| [Getting Started](https://kingssibal.github.io/R-Function-checker/articles/getting-started.html) | Quick introduction to the package |
| [Student Guide](https://kingssibal.github.io/R-Function-checker/articles/student-guide.html) | Installation and usage for students |
| [Instructor Guide](https://kingssibal.github.io/R-Function-checker/articles/instructor-guide.html) | Creating tests, encryption, distribution |
| [Data Files](https://kingssibal.github.io/R-Function-checker/articles/data-files.html) | Working with CSV, Excel, RDS files |
| [Performance](https://kingssibal.github.io/R-Function-checker/articles/performance.html) | Benchmarking and optimization |

## 🔗 Exported Functions

### Main API

| Function | Description |
|----------|-------------|
| `autograder(name)` | Run tests for a function |
| `list_problems()` | List available problems |
| `preview_tests(name)` | Preview test cases |

### Encryption API

| Function | Description |
|----------|-------------|
| `encrypt_text(text, key, encoding)` | Encrypt text (hex/base64) |
| `decrypt_text(ciphertext, key, encoding)` | Decrypt text |
| `generate_key(length)` | Generate random key |
| `derive_key(..., length)` | Derive key from factors |

## 📄 License

MIT License © 2025 Reijel Agub

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/KingSSIBAL/R-Function-checker/issues)
- **Email**: rcagub@up.edu.ph

