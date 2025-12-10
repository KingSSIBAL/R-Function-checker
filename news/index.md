# Changelog

## autograder 0.4.0

### Major Changes

- Complete modular refactoring of the R codebase into 11 separate files
  for better maintainability
- C++ backend split into modular components (executor, comparer, hasher,
  encryption)
- Improved parallel test execution with better error handling
- Added dual authentication modes: legacy (public) and secure (private
  repositories)
- Added data file support for test cases (CSV, Excel, RDS, RData, TXT)

### New Features

- Added
  [`compare_performance()`](https://kingsibal.github.io/R-Function-checker/reference/compare_performance.md)
  for benchmarking student vs instructor implementations
- Added
  [`preview_tests()`](https://kingsibal.github.io/R-Function-checker/reference/preview_tests.md)
  to let students see test cases before coding
- Added
  [`list_problems()`](https://kingsibal.github.io/R-Function-checker/reference/list_problems.md)
  to discover available problems
- Added encryption utilities:
  [`encrypt_text()`](https://kingsibal.github.io/R-Function-checker/reference/encrypt_text.md),
  [`decrypt_text()`](https://kingsibal.github.io/R-Function-checker/reference/decrypt_text.md),
  [`generate_key()`](https://kingsibal.github.io/R-Function-checker/reference/generate_key.md),
  [`derive_key()`](https://kingsibal.github.io/R-Function-checker/reference/derive_key.md)
- Added comprehensive pkgdown documentation site
- Added
  [`fetch_data()`](https://kingsibal.github.io/R-Function-checker/reference/fetch_data.md)
  for loading external data files in test cases
- Added
  [`prefetch_data_files()`](https://kingsibal.github.io/R-Function-checker/reference/prefetch_data_files.md)
  for efficient data caching
- Added
  [`inject_data_into_inputs()`](https://kingsibal.github.io/R-Function-checker/reference/inject_data_into_inputs.md)
  for automatic data injection
- Added `.cpp_get_auth_info()` and `.cpp_get_auth_mode()` for
  authentication status
- Added GitHub token authentication for private repository access
- Added Excel file support (.xlsx, .xls) via readxl package

### Internal Improvements

- Test coverage improved to 55%+ with 2015 unit tests
- Added vignettes for students, instructors, data files, authentication,
  and performance
- Improved error messages and feedback formatting
- Data files are cached and shared across parallel workers

## autograder 0.3.0

### New Features

- Added parallel test execution support via
  [`run_tests_parallel()`](https://kingsibal.github.io/R-Function-checker/reference/run_tests_parallel.md)
- Implemented C++ backend for performance-critical operations
- Added RSA encryption for secure code transmission

### Bug Fixes

- Fixed edge case handling for empty test suites
- Improved network error messages

## autograder 0.2.0

### New Features

- Added hidden test support
- Added point-weighted test cases
- Improved feedback formatting with detailed diff output

## autograder 0.1.0

- Initial release
- Basic autograder functionality
- GitHub-based test case fetching
- Support for single-function testing
