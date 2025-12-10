# Custom error for test execution failures

Creates a structured error when a specific test case fails to execute.
Includes test number for precise debugging.

## Usage

``` r
test_execution_error(message, test_number, call = NULL)
```

## Arguments

- message:

  Description of what went wrong

- test_number:

  Which test failed (1-based index)

- call:

  The call that generated the error

## Value

An S3 object of class c("test_execution_error", "error", "condition")

## Details

Used when:

- Test setup fails

- Input arguments are invalid

- Unexpected runtime error in test harness
