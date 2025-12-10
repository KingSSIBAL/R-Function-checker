# Preview test cases without running them

Shows test case inputs, descriptions, and point values without executing
any code. Hidden tests show only summary information to maintain
assessment integrity.

Use Case: Students can review test cases to understand requirements
before implementing their solution.

## Usage

``` r
preview_tests(function_name)
```

## Arguments

- function_name:

  Character. Name of the function to preview.

## Value

Invisibly returns the test_data structure (for programmatic access)

## Display Format

    === Test Cases Preview ===

    [Test 1] Base case: n = 1 (1 pt)
      Input: list(1)

    [Test 2] Small input: n = 5 (2 pt)
      Input: list(5)

    [Test 3] [HIDDEN TEST] (2 pt)

    === Summary ===
    Total tests: 3
    Visible tests: 2
    Hidden tests: 1
    Total points: 5

## Examples

``` r
if (FALSE) { # \dontrun{
# See what tests exist for fibonacci
preview_tests("fibonacci")

# Capture test data for analysis
test_data <- preview_tests("fibonacci")
print(test_data$points)  # See point distribution
} # }
```
