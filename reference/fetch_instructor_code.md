# Securely fetch and load instructor code from repository

Downloads instructor's reference implementation and test cases from
GitHub repository with comprehensive error handling.

Security Measures:

1.  Input validation via C++ (prevents path traversal)

2.  HTTPS transport (encrypted in transit)

3.  Error message sanitization (no path exposure)

4.  Timeout handling (30 seconds)

## Usage

``` r
fetch_instructor_code(function_name)
```

## Arguments

- function_name:

  Name of function to fetch (validated in C++)

## Value

Environment containing:

- Instructor's function implementation

- test_cases list with all test data

## Details

Workflow:

1.  Call C++ function to download code

2.  Create new isolated environment

3.  Parse and evaluate code in that environment

4.  Return environment for extraction

Error Handling:

- Invalid name → InvalidInputError

- Network issue → network_error

- 404 error → function_not_found_error

- Other → generic error

## Side Effects

Creates temporary file (automatically cleaned up by R)
