# Network Module

Secure network operations for fetching test cases from remote repositories.

## Files

| File | Purpose |
|------|---------|
| `fetcher.hpp` | Network API |
| `fetcher.cpp` | Implementation |

## Purpose

This module handles:

- **Secure downloads** from remote repositories
- **URL construction** with proper validation
- **Repository operations** (fetch functions, list problems)
- **Error handling** for network failures

## Components

### Fetcher Class

```cpp
class Fetcher {
public:
    Fetcher();
    explicit Fetcher(const Config& config);
    
    // Check internet connectivity
    static bool has_internet();
    
    // Download content to temp file and read
    Rcpp::CharacterVector download_to_temp(const std::string& url);
    
    // Fetch function content from repository
    Rcpp::CharacterVector fetch_function_content(const std::string& function_name);
    
    // Fetch list of available problems
    Rcpp::CharacterVector fetch_problems_list();
    
    // Generic download
    std::string download(const std::string& url);
    bool download_to_file(const std::string& url, const std::string& local_path);
};
```

### Repository Class

```cpp
class Repository {
public:
    Repository();  // Uses encrypted default URL
    explicit Repository(const std::string& base_url);
    
    // Get function file content
    Rcpp::CharacterVector get_function(const std::string& name);
    
    // Get list of available problems
    std::vector<std::string> list_problems();
    
    // Check if function exists
    bool has_function(const std::string& name);
    
    // Refresh cached data
    void refresh();
};
```

## Usage

### Fetching Function Content

```cpp
#include "network/fetcher.hpp"

using namespace autograder::network;

Fetcher fetcher;

// Check connectivity first
if (Fetcher::has_internet()) {
    auto content = fetcher.fetch_function_content("fibonacci");
    // content is CharacterVector with lines of the R file
}
```

### Using Repository

```cpp
Repository repo;  // Uses encrypted URL from config

// Get available problems
auto problems = repo.list_problems();

// Fetch specific function
auto content = repo.get_function("factorial");
```

### Custom Configuration

```cpp
Config config;
config.base_url = "https://my-server.edu/repo";
config.timeout_seconds = 30;

Fetcher fetcher(config);
auto content = fetcher.download("https://my-server.edu/repo/functions/test.R");
```

## URL Construction

The fetcher builds URLs from the base URL:

| Resource | URL Pattern |
|----------|-------------|
| Function file | `{base_url}/functions/{name}.R` |
| Problems list | `{base_url}/functions/_problems.R` |

## Error Handling

The module throws specific exceptions:

```cpp
try {
    auto content = fetcher.fetch_function_content("nonexistent");
} catch (const FunctionNotFoundException& e) {
    // Function doesn't exist in repository
} catch (const NetworkException& e) {
    // Network error (timeout, no connection, etc.)
} catch (const InvalidInputException& e) {
    // Invalid function name (path traversal attempt, etc.)
}
```

## Security

### Input Validation

All function names are validated before URL construction:

```cpp
// This is safe - fetcher validates internally
fetcher.fetch_function_content("../../../etc/passwd");
// Throws InvalidInputException, doesn't make network request
```

### URL Encryption

The default repository URL is encrypted in `crypto/encrypted_config.hpp`:

```cpp
// In Repository constructor
Repository::Repository() 
    : base_url_(config::get_repository_url())  // Decrypts at runtime
    , problems_cached_(false) {}
```

## Dependencies

- **crypto module**: For URL decryption
- **validation module**: For input sanitization
- **curl package** (R): For `has_internet()` check
