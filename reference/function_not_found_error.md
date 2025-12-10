# Custom error for missing functions

Creates a structured error when a requested function doesn't exist in
the repository. Provides helpful guidance on what to do next.

## Usage

``` r
function_not_found_error(function_name, call = NULL)
```

## Arguments

- function_name:

  Name of the function that wasn't found

- call:

  The call that generated the error (usually NULL)

## Value

An S3 object of class c("function_not_found_error", "error",
"condition")

## Details

This error includes:

- Function name that was requested

- Suggestion to use list_problems()

- Common troubleshooting steps

User Experience: Instead of: "Error: HTTP 404" Students see: "Function
'fibonaci' not found. Use list_problems() to see available functions."
