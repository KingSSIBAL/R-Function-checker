# Package index

## Main Functions

Primary functions for students to test their work

- [`autograder()`](https://kingsibal.github.io/R-Function-checker/reference/autograder.md)
  : Run autograder by function name
- [`list_problems()`](https://kingsibal.github.io/R-Function-checker/reference/list_problems.md)
  : List available problems/functions to test
- [`preview_tests()`](https://kingsibal.github.io/R-Function-checker/reference/preview_tests.md)
  : Preview test cases without running them

## Data Loading

Functions for loading external data files in test cases

- [`fetch_data()`](https://kingsibal.github.io/R-Function-checker/reference/fetch_data.md)
  : Fetch a data file from the repository
- [`prefetch_data_files()`](https://kingsibal.github.io/R-Function-checker/reference/prefetch_data_files.md)
  : Prefetch and cache data files for test cases
- [`inject_data_into_inputs()`](https://kingsibal.github.io/R-Function-checker/reference/inject_data_into_inputs.md)
  : Inject data into test case inputs

## Performance Analysis

Compare your implementation against the reference

- [`compare_performance()`](https://kingsibal.github.io/R-Function-checker/reference/compare_performance.md)
  : Compare Performance of Student vs Instructor Implementation
- [`print(`*`<performance_comparison>`*`)`](https://kingsibal.github.io/R-Function-checker/reference/print.performance_comparison.md)
  : Print Performance Comparison Results
- [`plot(`*`<performance_comparison>`*`)`](https://kingsibal.github.io/R-Function-checker/reference/plot.performance_comparison.md)
  : Plot Performance Comparison Results

## Encryption Utilities

Functions for encrypting test cases and sensitive data

- [`encrypt_text()`](https://kingsibal.github.io/R-Function-checker/reference/encrypt_text.md)
  : Encrypt text
- [`decrypt_text()`](https://kingsibal.github.io/R-Function-checker/reference/decrypt_text.md)
  : Decrypt text
- [`generate_key()`](https://kingsibal.github.io/R-Function-checker/reference/generate_key.md)
  : Generate encryption key
- [`derive_key()`](https://kingsibal.github.io/R-Function-checker/reference/derive_key.md)
  : Derive key from factors

## Internal Functions

Internal functions used by the package. These are not intended for
direct use but are documented for completeness.

- [`autograder()`](https://kingsibal.github.io/R-Function-checker/reference/autograder.md)
  : Run autograder by function name
- [`compare_performance()`](https://kingsibal.github.io/R-Function-checker/reference/compare_performance.md)
  : Compare Performance of Student vs Instructor Implementation
- [`decrypt_text()`](https://kingsibal.github.io/R-Function-checker/reference/decrypt_text.md)
  : Decrypt text
- [`derive_key()`](https://kingsibal.github.io/R-Function-checker/reference/derive_key.md)
  : Derive key from factors
- [`encrypt_text()`](https://kingsibal.github.io/R-Function-checker/reference/encrypt_text.md)
  : Encrypt text
- [`generate_key()`](https://kingsibal.github.io/R-Function-checker/reference/generate_key.md)
  : Generate encryption key
- [`list_problems()`](https://kingsibal.github.io/R-Function-checker/reference/list_problems.md)
  : List available problems/functions to test
- [`plot(`*`<performance_comparison>`*`)`](https://kingsibal.github.io/R-Function-checker/reference/plot.performance_comparison.md)
  : Plot Performance Comparison Results
- [`preview_tests()`](https://kingsibal.github.io/R-Function-checker/reference/preview_tests.md)
  : Preview test cases without running them
- [`print(`*`<performance_comparison>`*`)`](https://kingsibal.github.io/R-Function-checker/reference/print.performance_comparison.md)
  : Print Performance Comparison Results
- [`fetch_instructor_code()`](https://kingsibal.github.io/R-Function-checker/reference/fetch_instructor_code.md)
  : Securely fetch and load instructor code from repository
- [`extract_instructor_function()`](https://kingsibal.github.io/R-Function-checker/reference/extract_instructor_function.md)
  : Extract instructor function from loaded environment
- [`extract_test_cases()`](https://kingsibal.github.io/R-Function-checker/reference/extract_test_cases.md)
  : Extract and validate test cases from environment
- [`validate_test_cases()`](https://kingsibal.github.io/R-Function-checker/reference/validate_test_cases.md)
  : Validate and normalize test case structure
- [`provide_feedback()`](https://kingsibal.github.io/R-Function-checker/reference/provide_feedback.md)
  : Analyze differences and generate helpful feedback
- [`print_feedback()`](https://kingsibal.github.io/R-Function-checker/reference/print_feedback.md)
  : Display feedback messages in formatted style
- [`format_output()`](https://kingsibal.github.io/R-Function-checker/reference/format_output.md)
  : Intelligent output formatting with smart truncation
- [`run_tests_parallel()`](https://kingsibal.github.io/R-Function-checker/reference/run_tests_parallel.md)
  : Run tests in parallel using multiple CPU cores
- [`run_tests_sequential()`](https://kingsibal.github.io/R-Function-checker/reference/run_tests_sequential.md)
  : Run tests sequentially (one after another)
- [`network_error()`](https://kingsibal.github.io/R-Function-checker/reference/network_error.md)
  : Custom error for network-related issues
- [`function_not_found_error()`](https://kingsibal.github.io/R-Function-checker/reference/function_not_found_error.md)
  : Custom error for missing functions
- [`test_execution_error()`](https://kingsibal.github.io/R-Function-checker/reference/test_execution_error.md)
  : Custom error for test execution failures
