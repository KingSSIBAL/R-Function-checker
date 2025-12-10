# Extract and validate test cases from environment

Retrieves test_cases object from instructor environment and validates
its structure using validate_test_cases().

Expected Structure: test_cases \<- list( inputs = list(...), \# Required
descriptions = c(...), \# Optional hidden = c(...), \# Optional points =
c(...), \# Optional tolerance = 1e-10, \# Optional expected_type =
"numeric", \# Optional hints = c(...), \# Optional comparison_fn =
function() \# Optional )

## Usage

``` r
extract_test_cases(instructor_env, function_name)
```

## Arguments

- instructor_env:

  Environment loaded from fetch_instructor_code()

- function_name:

  Function name (for error messages)

## Value

Validated test_data list with all fields normalized
