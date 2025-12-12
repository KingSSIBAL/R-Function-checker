# autograder 0.4.0

## Major Changes

* Complete modular refactoring of the R codebase into 11 separate files for better maintainability
* C++ backend split into modular components (executor, comparer, hasher, encryption)
* Improved parallel test execution with better error handling
* Added secure authentication with GitHub token for private repositories
* Added data file support for test cases (CSV, Excel, RDS, RData, TXT)
* **Integrated performance benchmarking** into `autograder()` with optional bonus points

## Performance Benchmarking (NEW)

* Added `benchmark` parameter to `autograder()` - Run performance comparison after tests pass
* Added `benchmark_runs` parameter - Configure number of benchmark iterations (default: 50)
* Added `benchmark_bonus` parameter - Award bonus points for fast implementations (0-100%)
* Benchmark results now included in `autograder_result` object
* Updated `print()` and `summary()` methods to display benchmark data
* Performance bonus scales based on how much faster the student's implementation is

## Instructor Tools (NEW)

* Added `autograder_configure()` - One-command package setup for instructors
* Added `autograder_setup_wizard()` - Interactive step-by-step configuration wizard
* Added `autograder_test_token()` - Validate GitHub tokens before building
* Added `autograder_check_build_tools()` - Check for Rtools/Xcode installation
* Added RStudio Addin for graphical configuration (Addins menu > Configure Autograder)
* Added GitHub Actions workflow for automated multi-platform builds

## Security Enhancements

* Added rate limiting for API calls (30 calls/minute default)
* Added code validation to detect dangerous patterns before execution
* Added optional audit logging for security events
* Added secure memory clearing for sensitive data
* Added connection pooling for HTTP requests with TCP keep-alive

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
* Added `show_diff()` for detailed side-by-side comparison of failed tests
* Added `autograder_log_config()` and `autograder_log_history()` for audit logging

## Code Quality

* Added checkmate for robust input validation
* Added cli for better console output formatting
* Added hedgehog to Suggests for property-based testing
* Added microbenchmark to Suggests for performance testing
* Added architecture and security vignettes
* Added GitHub Actions CI/CD workflow

## Internal Improvements

* Test coverage improved to 55%+ with 2015 unit tests
* Added vignettes for students, instructors, data files, authentication, and performance
* Improved error messages and feedback formatting
* Data files are cached and shared across parallel workers
* Dynamic OpenMP threshold adjustment based on core count

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
