# R Function Checker

[![R Package](https://img.shields.io/badge/R%20Package-v0.3.0-blue)](https://github.com/KingSSIBAL/R-Function-checker)
[![Tests](https://img.shields.io/badge/tests-325%20passing-success)](https://github.com/KingSSIBAL/R-Function-checker)
[![Coverage](https://img.shields.io/badge/coverage-60.18%25-green)](https://github.com/KingSSIBAL/R-Function-checker)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/KingSSIBAL/R-Function-checker)

> **Automated grading system for R programming assignments** with parallel execution, intelligent feedback, and secure test case management.

## 🌟 Overview

R Function Checker is a comprehensive autograding system designed for R programming courses. It provides instant feedback to students while maintaining assessment integrity through secure test case management and hidden tests.

### Key Features

- 🔒 **Secure**: AES-inspired encryption, input sanitization, path traversal prevention
- ⚡ **Fast**: Parallel test execution (2-4x speedup), C++ optimization (10-100x faster comparisons)
- 📚 **Educational**: Detailed feedback with hints, type checking, position-specific error messages
- 🎯 **Flexible**: Weighted scoring, hidden tests, custom comparison functions, tolerance settings
- ✅ **Reliable**: 325+ comprehensive tests, 60% code coverage, zero R CMD check errors
- 🔧 **Well-Documented**: Comprehensive inline comments, roxygen2 documentation, examples

## 📦 Project Structure

```
R-Function-checker/
├── autograder/          # R package (student-facing)
│   ├── R/               # R source code
│   ├── src/             # C++ source code
│   ├── tests/           # 325+ comprehensive tests
│   ├── man/             # Documentation
│   └── README.md        # Package documentation
│
├── repo/                # Test case repository (instructor-facing)
│   ├── functions/       # Function implementations and test cases
│   └── README.md        # Repository documentation
│
└── README.md            # This file - project overview
```

## 🚀 Quick Start

### For Students

```r
# 1. Install the package
remotes::install_github("KingSSIBAL/R-Function-checker", subdir = "autograder")

# 2. Load the package
library(autograder)

# 3. See available assignments
list_problems()

# 4. Preview test cases
preview_tests("fibonacci")

# 5. Write your solution
student_fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  fib <- c(1, 1)
  for (i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  fib
}

# 6. Get instant feedback
autograder("fibonacci")
```

**Output:**
```
=== Running Tests ===
[Test 1] Base case: n = 1 (1 pt): PASS
[Test 2] Small input: n = 5 (2 pt): PASS
[Test 3] Medium input: n = 10 (2 pt): PASS
...

=== Summary ===
Score: 10/10 points (100.0%)
Tests: 6/6 passed (100.0%)

✓ ALL TESTS PASSED! Excellent work!
```

### For Instructors

See [`repo/README.md`](repo/README.md) for detailed instructions on creating test cases.

## 📖 Documentation

- **[Autograder Package Guide](autograder/README.md)** - Complete package documentation
- **[Test Case Repository Guide](repo/README.md)** - Creating and managing test cases
- **[Student Tutorial](docs/student-guide.md)** - Getting started guide
- **[Instructor Guide](docs/instructor-guide.md)** - Advanced configuration

## 🎓 Available Problems

Currently implemented:
- **fibonacci** - Generate Fibonacci sequence
- **factorial** - Calculate factorial
- **sum_vector** - Sum vector elements

More problems can be added to the repository by instructors.

## 🏗️ Architecture

### Component Overview

```
┌─────────────────────────────────────────────────────────┐
│                      STUDENT                             │
└────────────────────┬────────────────────────────────────┘
                     │
                     │ autograder("fibonacci")
                     ▼
┌─────────────────────────────────────────────────────────┐
│              R PACKAGE (autograder)                      │
│                                                          │
│  ┌──────────────┐         ┌─────────────┐               │
│  │  R Functions │◄────────┤  C++ Core   │               │
│  │              │         │             │               │
│  │ • Workflow   │         │ • Fast      │               │
│  │ • Feedback   │         │   Compare   │               │
│  │ • Parallel   │         │ • Secure    │               │
│  │              │         │   Fetch     │               │
│  └──────┬───────┘         │ • Validate  │               │
│         │                 └─────────────┘               │
│         │                                                │
└─────────┼────────────────────────────────────────────────┘
          │
          │ HTTPS (secure)
          ▼
┌─────────────────────────────────────────────────────────┐
│           GITHUB REPOSITORY (repo/)                      │
│                                                          │
│  ┌─────────────────────────────────────────┐            │
│  │  functions/                              │            │
│  │  ├── fibonacci.R (implementation + tests)│            │
│  │  ├── factorial.R                         │            │
│  │  ├── sum_vector.R                        │            │
│  │  └── _problems.R (available functions)   │            │
│  └─────────────────────────────────────────┘            │
└─────────────────────────────────────────────────────────┘
```

### Technology Stack

- **R**: Main interface, workflow orchestration, feedback generation
- **C++/Rcpp**: Performance-critical operations (comparison, validation)
- **GitHub**: Secure test case storage and distribution
- **parallel**: Multi-core test execution

### Performance Characteristics

| Operation | Sequential | Parallel | Speedup |
|-----------|-----------|----------|---------|
| < 10 tests | ~1-2s | N/A | - |
| 20 tests | ~4-5s | ~2-3s | 1.7x |
| 50 tests | ~10-15s | ~4-6s | 2.5x |
| 100 tests | ~20-30s | ~8-12s | 2.5-3x |

**C++ vs R Comparison:**
- Small vectors (n=100): 10x faster
- Large vectors (n=10,000): 50x faster
- Huge vectors (n=1,000,000): 100x faster

## 🔐 Security Features

1. **Input Sanitization**
   - Path traversal prevention (`../`, `~`)
   - Character whitelist (alphanumeric + `_` + `-` only)
   - Length limits (1-100 characters)

2. **AES-Inspired Encryption**
   - S-box transformation for URL obfuscation
   - 256-bit key derivation
   - Multi-factor key generation

3. **Network Security**
   - HTTPS-only transport
   - 30-second timeouts
   - Error message sanitization

4. **Code Isolation**
   - Separate environments for instructor/student code
   - No global namespace pollution
   - Temp file auto-cleanup

## 📊 Test Coverage

- **Overall Coverage**: 60.18%
- **R Code**: 55.29%
- **C++ Code**: 75.97%
- **Total Tests**: 325 passing

Run coverage report:
```r
covr::package_coverage()
covr::report()  # Opens HTML report
```

## 🧪 Development

### Setup Development Environment

```bash
# Clone repository
git clone https://github.com/KingSSIBAL/R-Function-checker.git
cd R-Function-checker

# Install package in development mode
cd autograder
```

```r
# In R
devtools::install_deps(dependencies = TRUE)
devtools::load_all()
```

### Run Tests

```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test_file("tests/testthat/test-autograder.R")

# Check package
devtools::check()
```

### Build Package

```r
# Generate documentation
devtools::document()

# Build package
devtools::build()

# Install locally
devtools::install()
```

## 🤝 Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

Areas for contribution:
- 📝 Add more example problems
- 🧪 Increase test coverage
- 📖 Improve documentation
- 🌐 Add internationalization
- 🔧 Performance optimizations

## 📋 Requirements

### System Requirements
- **R**: ≥ 3.5.0
- **OS**: Windows, macOS, or Linux
- **Internet**: Required for fetching test cases
- **Compiler**: C++11 or later (for building from source)

### R Package Dependencies
- `Rcpp` (≥ 1.0.0)
- `parallel`
- `curl`
- `utils`

## 📝 Version History

### v0.3.0 (Current)
- ✅ Parallel test execution (2-4x speedup)
- ✅ Enhanced error handling with custom error classes
- ✅ Improved feedback system with hints
- ✅ Input sanitization and security hardening
- ✅ 325+ comprehensive tests (60% coverage)
- ✅ Performance optimization (C++ comparison)

### v0.2.0
- ✅ AES-inspired encryption
- ✅ Basic parallel execution
- ✅ Custom error messages

### v0.1.0
- ✅ Initial release
- ✅ Basic autograding functionality

## 📄 License

MIT License © 2025 Reijel Agub

See [LICENSE](LICENSE) for full text.

## 👤 Author

**Reijel Agub**
- Email: rcagub@up.edu.ph
- GitHub: [@KingSSIBAL](https://github.com/KingSSIBAL)
- Affiliation: University of the Philippines

## 🐛 Issues & Support

### Reporting Bugs

Found a bug? [Open an issue](https://github.com/KingSSIBAL/R-Function-checker/issues/new) with:
- Description of the problem
- Minimal reproducible example
- Expected vs actual behavior
- R session info (`sessionInfo()`)

### Getting Help

- 📖 Check [documentation](autograder/README.md)
- 💬 [Open a discussion](https://github.com/KingSSIBAL/R-Function-checker/discussions)
- 📧 Email: rcagub@up.edu.ph

## 🙏 Acknowledgments

Built with:
- [Rcpp](https://www.rcpp.org/) - R and C++ integration
- [testthat](https://testthat.r-lib.org/) - Testing framework
- [roxygen2](https://roxygen2.r-lib.org/) - Documentation
- [devtools](https://devtools.r-lib.org/) - Development tools

## 🔗 Links

- **Repository**: https://github.com/KingSSIBAL/R-Function-checker
- **Issues**: https://github.com/KingSSIBAL/R-Function-checker/issues
- **Discussions**: https://github.com/KingSSIBAL/R-Function-checker/discussions

## 📈 Project Stats

![GitHub stars](https://img.shields.io/github/stars/KingSSIBAL/R-Function-checker?style=social)
![GitHub forks](https://img.shields.io/github/forks/KingSSIBAL/R-Function-checker?style=social)
![GitHub watchers](https://img.shields.io/github/watchers/KingSSIBAL/R-Function-checker?style=social)

---

**⭐ If you find this project useful, please star it on GitHub!**
