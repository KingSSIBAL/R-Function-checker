# Analyze differences and generate helpful feedback

Compares student output with expected output and generates specific,
actionable feedback to help students understand their mistakes.

Analysis Categories:

1.  **Type Analysis:** Checks if output type matches expectation Common
    student errors:

    - Returning character "5" instead of numeric 5

    - Returning list instead of vector

    - Returning data.frame instead of matrix

2.  **Dimension Analysis:** For vectors: checks length Common student
    errors:

    - Off-by-one errors (length 9 instead of 10)

    - Not handling edge cases (empty input)

    - Starting from wrong index

3.  **Value Analysis:** For numeric vectors: identifies which positions
    differ Common student errors:

    - Logic errors in loop

    - Wrong formula

    - Incorrect initial values

4.  **Hint Integration:** Adds instructor-provided hints when available

## Usage

``` r
provide_feedback(student_out, expected_out, input_args, hint = NULL)
```

## Arguments

- student_out:

  Student's function output

- expected_out:

  Expected (correct) output

- input_args:

  Input arguments used for this test

- hint:

  Optional instructor hint (NULL or character string)

## Value

Named list of feedback messages, may be empty if outputs match

## Details

Return Structure: list( type_issue = "Type mismatch: ...", \# If types
differ length_issue = "Length mismatch: ...", \# If lengths differ
diff_positions = "Differences at...", \# If values differ hint = "Hint:
..." \# If hint provided )

## Performance

Time: O(n) for vectors, O(1) for type/length checks Space: O(k) where k
= number of feedback messages (typically 1-3)

## Examples

``` r
if (FALSE) { # \dontrun{
# Type mismatch
provide_feedback("5", 5, list(x = 5))
# Returns: list(type_issue = "Type mismatch: Expected numeric but got character")

# Length mismatch
provide_feedback(1:5, 1:10, list(n = 10))
# Returns: list(length_issue = "Length mismatch: Expected length 10 but got 5")

# Value differences
provide_feedback(c(1, 5, 3), c(1, 2, 3), list())
# Returns: list(diff_positions = "Differences at positions: 2")

# With hint
provide_feedback(5, 10, list(x = 2), "Try multiplying by 2")
# Returns: list(hint = "Hint: Try multiplying by 2")
} # }
```
