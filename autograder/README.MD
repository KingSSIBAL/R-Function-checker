# autograder

[![R-CMD-check](https://github.com/KingSSIBAL/R-Function-checker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KingSSIBAL/R-Function-checker/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Automated grading for R programming assignments. C++ backend, secure test distribution, instant feedback.

## Installation

```r
# From GitHub
remotes::install_github("KingSSIBAL/R-Function-checker/autograder")

# From source
install.packages("autograder_0.5.0.tar.gz", repos = NULL, type = "source")
```

**Requirements:** R ≥ 4.0, C++17 compiler

## Usage

```r
library(autograder)
list_problems()
preview_tests("fibonacci")
autograder("fibonacci")
```

## Instructor Setup

```r
autograder_configure(
  url = "https://api.github.com/repos/YourOrg/tests/contents",
  token = "github_pat_xxxx",
  install = TRUE
)
```

## Documentation

- [Instructor Guide](articles/instructor-guide.html)
- [Student Guide](articles/student-guide.html)
- [Full Docs](https://kingssibal.github.io/R-Function-checker/)

## License

MIT © 2025 Reijel Agub
