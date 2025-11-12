# R Function Checker

> Automated grading system for R programming assignments with parallel execution, intelligent feedback, and secure test case management.

## Overview

An autograding system for R courses providing instant feedback, secure test case management, and hidden tests to ensure assessment integrity.

## Key Features

- Secure: Input sanitization, AES-inspired encryption
- Fast: Parallel test execution and C++ optimizations
- Educational: Detailed feedback and helpful hints
- Flexible: Weighted scoring, hidden tests, tolerance settings
- Reliable: 325+ tests and 60% code coverage

## Project Structure

```
R-Function-checker/
├── autograder/         # Student-facing R package
├── repo/               # Instructor test case repository
└── README.md           # Project overview
```

## Quick Start (Students)

```r
install.packages("C:/Users/YourUsername/Downloads/autograder_0.3.0.tar.gz", 
                 repos = NULL, 
                 type = "source")
library(autograder)
list_problems()
autograder("fibonacci")
```

## License

MIT License © 2025 Reijel Agub

---