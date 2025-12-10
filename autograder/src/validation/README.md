# Validation Module

Input sanitization and security validation for the autograder.

## Files

| File | Purpose |
|------|---------|
| `validator.hpp` | Validation API |
| `validator.cpp` | Implementation |

## Purpose

This module ensures all user inputs are safe before processing:

- **Function names**: Prevent path traversal attacks
- **File paths**: Validate allowed characters
- **URLs**: Ensure proper format

## Components

### Validator Class

```cpp
class Validator {
public:
    // Check if function name is valid (alphanumeric + underscore)
    static bool is_valid_function_name(const std::string& name);
    
    // Full validation with detailed result
    static ValidationResult validate_function_name(const std::string& name);
    
    // Sanitize input (remove dangerous characters)
    static std::string sanitize(const std::string& input);
    
    // Validate file path
    static ValidationResult validate_path(const std::string& path);
    
    // Check for path traversal attempts
    static bool has_path_traversal(const std::string& input);
};
```

## Usage

### Function Name Validation

```cpp
#include "validation/validator.hpp"

using namespace autograder::validation;

// Simple check
if (Validator::is_valid_function_name("fibonacci")) {
    // Valid
}

// Detailed validation
auto result = Validator::validate_function_name("../../../etc/passwd");
if (!result.valid) {
    std::cerr << result.error_message << std::endl;
    // "Invalid function name: contains path traversal"
}
```

### Sanitization

```cpp
std::string clean = Validator::sanitize("user<script>input");
// Returns: "userscriptinput"
```

### Path Traversal Detection

```cpp
if (Validator::has_path_traversal("../secret")) {
    // Reject input
}
```

## Validation Rules

### Valid Function Names

- Start with letter or underscore
- Contain only: `a-z`, `A-Z`, `0-9`, `_`, `.`
- Maximum 100 characters
- No path separators (`/`, `\`)
- No path traversal (`..`)

### Examples

| Input | Valid | Reason |
|-------|-------|--------|
| `fibonacci` | ✅ | Valid name |
| `my_function` | ✅ | Underscores allowed |
| `calc.sum` | ✅ | Dots allowed |
| `../hack` | ❌ | Path traversal |
| `foo/bar` | ❌ | Path separator |
| `<script>` | ❌ | Invalid characters |
| `` (empty) | ❌ | Empty string |

## Security

This module prevents:

- **Path traversal attacks**: `../../../etc/passwd`
- **Null byte injection**: `file%00.R`
- **Directory escape**: `foo/../bar`
- **Invalid characters**: `<>|&;`
