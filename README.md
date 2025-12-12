# R Function Checker

[![R-CMD-check](https://github.com/KingSSIBAL/R-Function-checker/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/KingSSIBAL/R-Function-checker/actions/workflows/R-CMD-check.yml)
[![codecov](https://codecov.io/gh/KingSSIBAL/R-Function-checker/graph/badge.svg)](https://codecov.io/gh/KingSSIBAL/R-Function-checker)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Automated grading system for R programming assignments with C++ backend.

## Features

- C++ comparison engine (10-100x faster than `identical()`)
- Private repo authentication with encrypted tokens
- Data file support (CSV, Excel, RDS, RData, TXT)
- Parallel test execution
- Smart feedback with hints

## Structure

```
autograder/   # R package with C++ backend
repo/         # Test cases and data files
tools/        # Instructor utilities
docs/         # Documentation
```

## Quick Start

```r
# Install
devtools::install_github("KingSSIBAL/R-Function-checker/autograder")

library(autograder)
list_problems()
preview_tests("fibonacci")
autograder("fibonacci")
```

## Documentation

- [Student Guide](docs/student-guide.md)
- [Instructor Guide](docs/instructor-guide.md)
- [Full Documentation](https://kingssibal.github.io/R-Function-checker/)

## Navigation

| Folder | README | Description |
|--------|--------|-------------|
| [autograder/](autograder/) | [README](autograder/README.md) | R package source |
| [repo/](repo/) | [README](repo/README.md) | Test cases |
| [repo/functions/](repo/functions/) | [README](repo/functions/README.md) | Problem definitions |
| [repo/data/](repo/data/) | [README](repo/data/README.md) | Data files |
| [tools/](tools/) | [README](tools/README.md) | Instructor utilities |
| [docs/](docs/) | [README](docs/README.md) | Documentation |

## License

MIT © 2025 Reijel Agub