# autograder 0.4.0

## Major Changes

* Complete modular refactoring of the R codebase into 11 separate files for better maintainability
* C++ backend split into modular components (executor, comparer, hasher, encryption)
* Improved parallel test execution with better error handling
* Added dual authentication modes: legacy (public) and secure (private repositories)
* Added data file support for test cases (CSV, Excel, RDS, RData, TXT)

## New Features

* Added `compare_performance()` for benchmarking student vs instructor implementations
* Added `preview_tests()` to let students see test cases before coding
* Added `list_problems()` to discover available problems
* Added encryption utilities: `encrypt_text()`, `decrypt_text()`, `generate_key()`, `derive_key()`
* Added comprehensive pkgdown documentation site
* Added `fetch_data()` for loading external data files in test cases
* Added `prefetch_data_files()` for efficient data caching
* Added `inject_data_into_inputs()` for automatic data injection
* Added `.cpp_get_auth_info()` and `.cpp_get_auth_mode()` for authentication status
* Added GitHub token authentication for private repository access
* Added Excel file support (.xlsx, .xls) via readxl package

## Internal Improvements

* Test coverage improved to 55%+ with 2015 unit tests
* Added vignettes for students, instructors, data files, authentication, and performance
* Improved error messages and feedback formatting
* Data files are cached and shared across parallel workers

# autograder 0.3.0

## New Features

* Added parallel test execution support via `run_tests_parallel()`
* Implemented C++ backend for performance-critical operations
* Added RSA encryption for secure code transmission

## Bug Fixes

* Fixed edge case handling for empty test suites
* Improved network error messages

# autograder 0.2.0

## New Features

* Added hidden test support
* Added point-weighted test cases
* Improved feedback formatting with detailed diff output

# autograder 0.1.0

* Initial release
* Basic autograder functionality
* GitHub-based test case fetching
* Support for single-function testing
