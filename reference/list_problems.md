# List available problems/functions to test

Displays all functions available for testing. Shows usage instructions
and how to preview test cases.

Problem Discovery:

1.  Attempts to fetch from repository (\_problems.R)

2.  Falls back to default list if fetch fails

3.  Returns problems invisibly for programmatic access

## Usage

``` r
list_problems()
```

## Value

Invisibly returns character vector of problem names

## Display Format

    Available problems:

      - fibonacci
      - factorial
      - sum_vector

    Usage:
      student_<function_name> <- function(...) { ... }
      autograder('<function_name>')

    Preview tests:
      preview_tests('<function_name>')

## Examples

``` r
if (FALSE) { # \dontrun{
# See available functions
list_problems()

# Capture list programmatically
problems <- list_problems()
print(problems)  # c("fibonacci", "factorial", ...)
} # }
```
