# Extract instructor function from loaded environment

Searches environment for the first function object. Instructor code
typically defines one function, which this extracts.

Search Strategy:

- Iterate through all objects in environment

- Return first object that is.function() == TRUE

- Error if no function found

## Usage

``` r
extract_instructor_function(instructor_env, function_name)
```

## Arguments

- instructor_env:

  Environment loaded from fetch_instructor_code()

- function_name:

  Function name (for error messages)

## Value

The instructor's function object

## Details

Why search instead of direct access?

- Function name might differ from file name

- Allows flexibility in instructor code structure

- Handles helper functions gracefully
