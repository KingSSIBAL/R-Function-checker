# Validate and normalize test case structure

Performs comprehensive validation of test case data structure with
strict error checking. Ensures all test components are properly
formatted and consistent.

Required Fields:

- **inputs**: list of input argument lists (REQUIRED)

Optional Fields (with defaults):

- **descriptions**: character vector of test descriptions

- **hidden**: logical vector indicating hidden tests

- **points**: numeric vector of point values per test

- **tolerance**: single numeric for floating-point comparison

- **expected_type**: single character specifying expected output type

- **hints**: character vector of hints for failed tests

- **comparison_fn**: custom comparison function

## Usage

``` r
validate_test_cases(test_data, function_name)
```

## Arguments

- test_data:

  List containing test case configuration

- function_name:

  Name of function (for error messages)

## Value

Validated and normalized test_data with all fields present

## Details

Validation Strategy:

**Critical Fields (STOP on error):**

- Missing inputs: Hard error (can't proceed)

- Mismatched lengths: Hard error (data corruption)

- Invalid types: Hard error (wrong configuration)

**Optional Fields (WARN and fix):**

- Missing: Fill with defaults

- Wrong format: Warn and ignore

- Can proceed: Yes

Error Messages:

- Clear indication of what's wrong

- Tells user to contact instructor (config issue)

- Never blames the student for instructor errors

## Examples

``` r
if (FALSE) { # \dontrun{
# Valid test case
test_data <- list(
  inputs = list(list(1), list(2)),
  descriptions = c("Test 1", "Test 2"),
  points = c(1, 2)
)
validated <- validate_test_cases(test_data, "my_function")

# Missing optional fields - filled with defaults
test_data_minimal <- list(
  inputs = list(list(1))
)
validated <- validate_test_cases(test_data_minimal, "my_function")
# Returns: descriptions = "Test 1", hidden = FALSE, points = 1, tolerance = 1e-10
} # }
```
