# Instructor Guide for R-Function-checker

## Overview

R Function Checker is an autograding system for R programming courses that provides instant feedback to students while ensuring assessment integrity with secure test case management and hidden tests.

As an instructor, your role is to create and manage all test cases that students will run against using this system.

## Repository Structure for Instructors

- The **repo/functions/** directory is where you create reference implementations and test cases.
- The **autograder/** directory contains the student-facing R package.
- The **repo/functions/_problems.R** file lists all the problems available for students.

## Creating Test Cases

### Steps to Add a New Test Case

1. Create a new R file in `repo/functions/` with the function implementation and test cases.
   
2. Define the function (reference implementation) and the `test_cases` object detailing inputs, expected behavior, and test metadata.

3. Add the new function name to the `problems` vector inside `repo/functions/_problems.R` to expose it to students.

4. Commit and push your changes to the GitHub repository.

5. Test the new case locally with the `autograder` package to ensure it works correctly.

### Example Template of a Test Case File

============================================================================
FUNCTION: function_name
Description: Brief description
============================================================================
function_name <- function(param1, param2) {

Reference implementation
...
}

test_cases <- list(
inputs = list(
list(arg1_val, arg2_val),
list(arg1_val, arg2_val)
),
descriptions = c("Description for test 1", "Description for test 2"),
hidden = c(FALSE, TRUE), # Hide some tests for integrity
points = c(1, 2), # Weighted points per test
tolerance = 1e-10, # For numerical comparisons
expected_type = "numeric", # Validates output type
hints = c("Hint for test 1", "Hint for test 2")
)

text

## Pedagogical Best Practices

- Use a balanced mix of visible and hidden tests for fairness and to prevent hardcoding.
- Provide meaningful descriptions and hints to foster learning.
- Use weighted scoring to emphasize important tests and edge cases.
- Design tests following patterns like incremental complexity or category-based testing.

## Security

- The system includes AES-inspired encryption for URL obfuscation, though URLs are currently plaintext for simplicity.
- Hidden tests maintain assessment integrity by preventing students from seeing critical test cases.
- Students run tests on their own computers, and feedback is provided instantly with detailed hints.

## Testing and Validation

- Validate test cases locally by running them and verifying the reference implementation outputs.
- Use the autograder to simulate student runs and confirm 100% passing rates.
- Debug and adjust tests as necessary to ensure reliability.

## Recommended Workflow Summary

Create your problem file
Add function and tests
Edit repo/functions/_problems.R to include new problem
Commit and push your changes
Test with autograder package
text

## Additional Support

- Contact: rcagub@up.edu.ph
- Project discussions and issues on the GitHub repository
Feel free to ask if you want help with anything else related to this guide!