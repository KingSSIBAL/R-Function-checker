# Core Module

Foundation types and exception hierarchy for the autograder C++ backend.

## Files

| File | Purpose |
|------|---------|
| `types.hpp` | Central type definitions used across all modules |
| `exceptions.hpp` | Exception class hierarchy for error handling |

## Types (types.hpp)

### Configuration

```cpp
struct Config {
    std::string base_url;        // Repository base URL
    int timeout_seconds;         // Network timeout
    double tolerance;            // Comparison tolerance
    size_t max_output_length;    // Max formatted output length
    bool use_parallel;           // Enable parallel processing
};
```

### Result Types

```cpp
struct CryptoResult {
    bool success;
    std::string data;
    std::string error_message;
};

struct ComparisonResult {
    bool equal;
    size_t first_diff_index;
    std::string diff_description;
};

struct ValidationResult {
    bool valid;
    std::string error_message;
    std::string sanitized;
};

struct FetchResult {
    bool success;
    std::string content;
    std::string error_message;
    int status_code;
};
```

## Exceptions (exceptions.hpp)

### Hierarchy

```
AutograderException (base)
├── NetworkException
├── ValidationException  
├── CryptoException
├── ComparisonException
├── FunctionNotFoundException
└── InvalidInputException
```

### Usage

```cpp
try {
    // Code that may throw
} catch (const autograder::FunctionNotFoundException& e) {
    // Handle missing function
} catch (const autograder::NetworkException& e) {
    // Handle network error
} catch (const autograder::AutograderException& e) {
    // Handle any autograder error
}
```
