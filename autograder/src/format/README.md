# Format Module

Intelligent output formatting for displaying R objects.

## Files

| File | Purpose |
|------|---------|
| `formatter.hpp` | Formatting API |
| `formatter.cpp` | Implementation |

## Purpose

This module formats R objects for display in feedback:

- **Smart truncation** for large objects
- **Type-aware formatting** (vectors, matrices, data frames)
- **Configurable length limits**
- **Human-readable output**

## Components

### Formatter Class

```cpp
class Formatter {
public:
    explicit Formatter(size_t max_length = 200);
    
    // Format any R object
    std::string format(SEXP obj);
    
    // Type-specific formatting
    std::string format_vector(Rcpp::NumericVector v);
    std::string format_vector(Rcpp::IntegerVector v);
    std::string format_vector(Rcpp::CharacterVector v);
    std::string format_vector(Rcpp::LogicalVector v);
    std::string format_matrix(Rcpp::NumericMatrix m);
    std::string format_list(Rcpp::List l);
    std::string format_dataframe(Rcpp::DataFrame df);
};
```

### FormatConfig Struct

```cpp
struct FormatConfig {
    size_t max_length = 200;      // Maximum output length
    size_t max_elements = 10;     // Max elements to show before truncating
    size_t max_rows = 5;          // Max rows for matrices/data frames
    size_t max_cols = 5;          // Max columns for matrices/data frames
    bool show_types = false;      // Include type annotations
    std::string truncation = "...";  // Truncation indicator
};
```

## Usage

### Basic Formatting

```cpp
#include "format/formatter.hpp"

using namespace autograder::format;

Formatter fmt(200);  // max 200 characters

Rcpp::NumericVector v = {1, 2, 3, 4, 5};
std::string output = fmt.format(v);
// "c(1, 2, 3, 4, 5)"
```

### With Configuration

```cpp
FormatConfig config;
config.max_length = 100;
config.max_elements = 5;

Formatter fmt(config);
std::string output = fmt.format(large_object);
```

### Large Objects

```cpp
// Vector with 1000 elements
Rcpp::NumericVector large = Rcpp::seq(1, 1000);

Formatter fmt(50);
std::string output = fmt.format(large);
// "c(1, 2, 3, 4, 5, ... [1000 elements])"
```

## Output Examples

### Vectors

| Input | Output |
|-------|--------|
| `c(1, 2, 3)` | `c(1, 2, 3)` |
| `1:100` | `c(1, 2, 3, ... [100 elements])` |
| `c("a", "b")` | `c("a", "b")` |
| `c(TRUE, FALSE)` | `c(TRUE, FALSE)` |

### Matrices

```
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    4    5    6
... [10x10 matrix]
```

### Data Frames

```
  x    y     z
1 1  "a"  TRUE
2 2  "b" FALSE
... [100 rows x 5 cols]
```

### Lists

```
list(
  a = 1,
  b = c(1, 2, 3),
  c = ...
)
```

### Special Values

| R Value | Output |
|---------|--------|
| `NULL` | `NULL` |
| `NA` | `NA` |
| `NaN` | `NaN` |
| `Inf` | `Inf` |
| `""` | `""` (empty string) |

## Integration

The format module is used in:

1. **Feedback generation**: Show expected vs actual values
2. **Error messages**: Display problematic values
3. **Test case preview**: Show test inputs/outputs

```cpp
// In feedback generation
std::string expected_str = fmt.format(expected);
std::string actual_str = fmt.format(actual);

std::cout << "Expected: " << expected_str << std::endl;
std::cout << "Actual:   " << actual_str << std::endl;
```

## Performance

Formatting is done lazily and only when needed:

- Truncation happens during formatting, not after
- Large objects are not fully traversed
- String building uses efficient concatenation
