# Intelligent output formatting with smart truncation

Formats R objects for display with type-aware strategies to prevent
overwhelming output while preserving essential information.

Design Goals:

- Show enough information to understand the object

- Prevent screen overflow for large objects

- Preserve structure for small objects

- Handle all common R types gracefully

## Usage

``` r
format_output(obj, max_length = 200, preserve_structure = TRUE)
```

## Arguments

- obj:

  The R object to format (any type)

- max_length:

  Maximum characters in output (default: 200)

- preserve_structure:

  Keep full structure for lists? (default: TRUE)

## Value

Character string representation, never exceeds max_length + 3

## Details

Formatting Strategy by Type:

**NULL:** Returns: "NULL"

**Lists (small, ≤3 elements):** Shows: Full structure Example: list(a =
1, b = 2, c = 3)

**Lists (large, \>3 elements):** Shows: list(length=100, first 3: ...,
...)

**Vectors (≤10 elements):** Shows: Full deparsed representation Example:
c(1, 2, 3, 4, 5)

**Vectors (\>10 elements):** Shows:
`type[1:n] = first, second, third ... type[n] = last` Example:
`integer[1:100] = 1, 2, 3 ... integer[100] = 100`

**Matrices:** Shows: `matrix[rows x cols]: first_values ...` Example:
`matrix[3x4]: 1 2 3 ...`

**Data Frames:** Shows:
`data.frame[rows x cols] columns: col1, col2, col3` Example:
`data.frame[10 x 3] columns: x, y, z`

## Performance

Time: O(1) for most types (uses head/tail) Space: O(k) where k =
min(object_size, max_length)

## Examples

``` r
if (FALSE) { # \dontrun{
# Small vector - full output
format_output(1:5)
# "1:5"

# Large vector - truncated
format_output(1:1000)
# "integer[1:1000] = 1, 2, 3 ... integer[1000] = 1000"

# Data frame - summary
format_output(mtcars)
# "data.frame[32 x 11] columns: mpg, cyl, disp, hp, ..."

# Matrix - dimensions
format_output(matrix(1:100, 10, 10))
# "matrix[10x10]: 1 2 3 ..."
} # }
```
