# Test Case Repository - Instructor Guide

This directory contains test cases and reference implementations used by the autograder.

## Quick Start

1. Create a new function file with a reference implementation and test cases in `repo/functions/`.
2. Add the function name to `repo/functions/_problems.R`.
3. Commit and push.
4. Test locally with the autograder package.

## Test Case Template

```r
function_name <- function(param1, param2) {
  # Reference implementation
}

test_cases <- list(
  inputs = list(
    list(...), 
    list(...)
  ),
  descriptions = c("Test 1", "Test 2"),
  hidden = c(FALSE, TRUE),
  points = c(1, 2),
  tolerance = 1e-10,
  expected_type = "numeric",
  hints = c("Hint 1", "Hint 2")
)
```

## Pedagogical Tips

- Provide visible and hidden tests.
- Use descriptions and hints.
- Employ weighted scoring.
- Test incremental difficulty.

## Testing Your Test Cases

- Load and verify test outputs.
- Validate test structure with autograder.
- Fix mismatched test vectors or strict tolerances.

## Security

- URLs are plaintext for simplicity.
- Hidden tests maintain integrity.

## Support

Contact: rcagub@up.edu.ph
