# Student Guide

## Overview

This guide explains how to use the autograder package to test your R
programming assignments. The autograder provides immediate feedback on
your code, helping you learn and improve.

## Workflow Summary

1.  **Discover** available problems with
    [`list_problems()`](https://kingssibal.github.io/R-Function-checker/reference/list_problems.md)
2.  **Preview** test cases with `preview_tests("function_name")`
3.  **Implement** your solution as `student_function_name`
4.  **Test** with `autograder("function_name")`
5.  **Iterate** based on feedback until all tests pass
6.  **Optimize** (optional) using
    [`compare_performance()`](https://kingssibal.github.io/R-Function-checker/reference/compare_performance.md)

## Step-by-Step Guide

### Step 1: List Available Problems

``` r
library(autograder)
list_problems()
```

This shows all functions available for testing in your course.

### Step 2: Preview Test Cases

Before writing code, understand what’s expected:

``` r
preview_tests("fibonacci")
```

This shows: - **Test descriptions** - What each test checks - **Input
values** - What arguments are passed - **Point values** - How much each
test is worth - **Hidden tests** - Existence noted, but details hidden

### Step 3: Create Your Function

Name your function with the `student_` prefix:

``` r
# For the "fibonacci" problem, create student_fibonacci
student_fibonacci <- function(n) {
  # Your implementation here
}
```

### Step 4: Run the Autograder

``` r
result <- autograder("fibonacci")
```

The autograder returns a results object:

``` r
result$passed     # Number of tests passed
result$failed     # Number of tests failed
result$total      # Total tests
result$score      # Points earned
result$max_score  # Maximum points
result$pass_rate  # Percentage passed
```

### Step 5: Interpret Feedback

#### Passing Test

    [Test 1] Base case: n = 1 (1 pt): PASS

#### Failing Test

    [Test 2] Small input: n = 5 (2 pt): FAIL
      Input:    list(5)
      Expected: c(1, 1, 2, 3, 5)
      Got:      c(1, 1, 2, 3, 4)

      Feedback:
        * Differences at positions: 5

#### Type Error

    [Test 1] Base case (1 pt): FAIL (Type Error)
      Expected type: numeric
      Got type: character

#### Runtime Error

    [Test 1] Base case (1 pt): FAIL (Error)
      Input: list(5)
      Error: object 'x' not found

### Step 6: Debug and Retry

Use the feedback to fix your code:

``` r
# Test your function manually with the failing input
student_fibonacci(5)
# Compare with expected output
# Fix the bug
# Run autograder again
autograder("fibonacci")
```

## Tips for Success

### 1. Start Simple

Test your function manually before running the autograder:

``` r
# Test edge cases first
student_fibonacci(0)
student_fibonacci(1)
student_fibonacci(2)

# Then larger inputs
student_fibonacci(10)
```

### 2. Read Error Messages Carefully

The autograder provides specific feedback: - **Position differences**
tell you exactly where output differs - **Type errors** indicate wrong
return type - **Runtime errors** show R error messages from your
function

### 3. Handle Edge Cases

Many problems have edge cases: - Empty input (`n = 0`) - Single element
(`n = 1`) - Negative numbers - Very large inputs

### 4. Use Verbose Mode

Get detailed output for debugging:

``` r
autograder("fibonacci", verbose = TRUE, show_hints = TRUE)
```

### 5. Check Your Return Type

Common type issues:

``` r
# Wrong: returning string instead of number
return("5")

# Right: returning number
return(5)

# Wrong: returning list instead of vector
return(list(1, 2, 3))

# Right: returning vector
return(c(1, 2, 3))
```

## Common Errors and Solutions

### “Function not found”

    Error: Function 'student_fibonacci' not found in your environment.

**Solution:** Make sure your function is named correctly with `student_`
prefix.

### “Type mismatch”

    Type mismatch: Expected numeric but got character

**Solution:** Check that you’re returning numbers, not strings.

### “Length mismatch”

    Length mismatch: Expected length 10 but got 9

**Solution:** Check your loop bounds for off-by-one errors.

### “Network error”

    No internet connection detected.

**Solution:** The autograder needs internet to fetch test cases. Check
your connection.

## Working with Data Files

Some problems require your function to analyze data from files (CSV,
Excel, RDS). The autograder handles data loading automatically!

### How It Works

When a test uses data files: 1. The autograder downloads the data file
2. Loads it into R (as data frame, list, etc.) 3. Passes the loaded data
to your function

**Your function receives the data directly, not a file path!**

### Example

``` r
# ✅ CORRECT - data is already a data frame
student_analyze <- function(data, stat = "mean") {
  sapply(data[sapply(data, is.numeric)], 
         switch(stat, mean = mean, sum = sum))
}

# ❌ WRONG - don't try to read the file
student_analyze <- function(data, stat = "mean") {
  df <- read.csv(data)  # data is NOT a filename!
  # ...
}
```

For more details, see [Working with Data
Files](https://kingssibal.github.io/R-Function-checker/articles/data-files.md).

## Performance Comparison (Optional)

After passing all tests, optimize your code:

``` r
result <- compare_performance("fibonacci", n_runs = 100)
print(result)
plot(result)
```

This shows how your implementation compares to the reference solution in
terms of speed.

## Getting Help

If you’re stuck: 1. Re-read the test case descriptions with
[`preview_tests()`](https://kingssibal.github.io/R-Function-checker/reference/preview_tests.md)
2. Test your function manually with simple inputs 3. Check the function
requirements/documentation 4. Ask your instructor for help
