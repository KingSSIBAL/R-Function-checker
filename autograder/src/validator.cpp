// ============================================================================
// AUTOGRADER - INPUT VALIDATION MODULE IMPLEMENTATION
// ============================================================================
//
// File: validation/validator.cpp
// Purpose: Implementation of input validation and sanitization
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#include "validator.h"
#include <algorithm>
#include <cctype>

namespace autograder {
namespace validation {

// ============================================================================
// STATIC DATA
// ============================================================================

const std::vector<std::string> Validator::DANGEROUS_PATTERNS = {
    "..",           // Parent directory traversal
    "./",           // Current directory
    "//",           // Double slash
    "\\",           // Windows path separator
    "~",            // Home directory
    "${",           // Variable expansion
    "$((",          // Arithmetic expansion
    "`",            // Command substitution
    ";",            // Command separator
    "|",            // Pipe
    "&",            // Background/and
    ">",            // Redirect
    "<",            // Redirect
    "\n",           // Newline
    "\r"            // Carriage return
    // Note: null byte check must be done separately with strlen/contains_null
};

// ============================================================================
// VALIDATOR IMPLEMENTATION
// ============================================================================

ValidationResult Validator::validate_function_name(const std::string& name) {
    // Empty name check
    if (name.empty()) {
        return ValidationResult("Function name cannot be empty");
    }
    
    // Length check
    if (name.length() > MAX_NAME_LENGTH) {
        return ValidationResult("Function name too long (max " + 
                               std::to_string(MAX_NAME_LENGTH) + " characters)");
    }
    
    // Null byte check (done separately since '\0' in std::string is tricky)
    if (name.find('\0') != std::string::npos) {
        return ValidationResult("Invalid function name: contains null byte");
    }
    
    // Path traversal check
    if (contains_path_traversal(name)) {
        return ValidationResult("Invalid function name: contains unsafe characters");
    }
    
    // Character whitelist check
    for (char c : name) {
        if (!is_allowed_char(c)) {
            return ValidationResult("Invalid character '" + std::string(1, c) + 
                                   "' in function name. Use only letters, numbers, "
                                   "underscores, and hyphens.");
        }
    }
    
    // First character must be a letter or underscore
    if (!std::isalpha(static_cast<unsigned char>(name[0])) && name[0] != '_') {
        return ValidationResult("Function name must start with a letter or underscore");
    }
    
    return ValidationResult(true, name);
}

bool Validator::is_valid_function_name(const std::string& name) {
    return validate_function_name(name).valid;
}

ValidationResult Validator::validate_url(const std::string& url) {
    if (url.empty()) {
        return ValidationResult("URL cannot be empty");
    }
    
    // Check for valid URL prefix
    if (url.substr(0, 7) != "http://" && url.substr(0, 8) != "https://") {
        return ValidationResult("URL must start with http:// or https://");
    }
    
    // Check for dangerous patterns
    std::string lower_url = Sanitizer::to_lower(url);
    for (const auto& pattern : {"javascript:", "data:", "vbscript:", "file:"}) {
        if (lower_url.find(pattern) != std::string::npos) {
            return ValidationResult("URL contains unsafe protocol");
        }
    }
    
    return ValidationResult(true, url);
}

ValidationResult Validator::validate_path(const std::string& path) {
    if (path.empty()) {
        return ValidationResult("Path cannot be empty");
    }
    
    if (contains_path_traversal(path)) {
        return ValidationResult("Path contains unsafe traversal patterns");
    }
    
    // Sanitize the path
    std::string sanitized = sanitize_for_path(path);
    
    return ValidationResult(true, sanitized);
}

std::string Validator::sanitize_for_url(const std::string& input) {
    std::string result;
    result.reserve(input.length());
    
    for (char c : input) {
        if (std::isalnum(static_cast<unsigned char>(c)) || 
            c == '-' || c == '_' || c == '.' || c == '~') {
            result += c;
        } else if (c == ' ') {
            result += "%20";
        } else {
            // URL encode other characters
            char hex[4];
            snprintf(hex, sizeof(hex), "%%%02X", static_cast<unsigned char>(c));
            result += hex;
        }
    }
    
    return result;
}

std::string Validator::sanitize_for_path(const std::string& path) {
    std::string result;
    result.reserve(path.length());
    
    for (char c : path) {
        if (std::isalnum(static_cast<unsigned char>(c)) || 
            c == '-' || c == '_' || c == '.' || c == '/') {
            result += c;
        }
    }
    
    // Remove any remaining path traversal
    while (result.find("..") != std::string::npos) {
        result = Sanitizer::replace_all(result, "..", "");
    }
    
    return result;
}

bool Validator::contains_path_traversal(const std::string& path) {
    for (const auto& pattern : DANGEROUS_PATTERNS) {
        if (path.find(pattern) != std::string::npos) {
            return true;
        }
    }
    return false;
}

bool Validator::is_allowed_char(char c) {
    return std::isalnum(static_cast<unsigned char>(c)) || 
           c == '_' || 
           c == '-';
}

ValidationResult Validator::validate_test_cases(const std::string& test_data) {
    // Placeholder for test case validation
    // In actual implementation, this would validate R test case structure
    if (test_data.empty()) {
        return ValidationResult("Test data cannot be empty");
    }
    return ValidationResult(true, test_data);
}

// ============================================================================
// SANITIZER IMPLEMENTATION
// ============================================================================

std::string Sanitizer::remove_special_chars(const std::string& input, 
                                            const std::string& allowed) {
    std::string result;
    result.reserve(input.length());
    
    for (char c : input) {
        if (std::isalnum(static_cast<unsigned char>(c)) || 
            allowed.find(c) != std::string::npos) {
            result += c;
        }
    }
    
    return result;
}

std::string Sanitizer::escape_shell(const std::string& input) {
    std::string result;
    result.reserve(input.length() * 2);
    
    for (char c : input) {
        switch (c) {
            case '\'':
                result += "'\\''";
                break;
            case '\\':
            case '"':
            case '$':
            case '`':
            case '!':
                result += '\\';
                result += c;
                break;
            default:
                result += c;
        }
    }
    
    return result;
}

std::string Sanitizer::trim(const std::string& input) {
    size_t start = input.find_first_not_of(" \t\n\r\f\v");
    if (start == std::string::npos) {
        return "";
    }
    
    size_t end = input.find_last_not_of(" \t\n\r\f\v");
    return input.substr(start, end - start + 1);
}

std::string Sanitizer::to_lower(const std::string& input) {
    std::string result = input;
    std::transform(result.begin(), result.end(), result.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    return result;
}

std::string Sanitizer::to_upper(const std::string& input) {
    std::string result = input;
    std::transform(result.begin(), result.end(), result.begin(),
                   [](unsigned char c) { return std::toupper(c); });
    return result;
}

std::string Sanitizer::replace_all(const std::string& input,
                                   const std::string& from,
                                   const std::string& to) {
    if (from.empty()) {
        return input;
    }
    
    std::string result;
    size_t start = 0;
    size_t pos;
    
    while ((pos = input.find(from, start)) != std::string::npos) {
        result += input.substr(start, pos - start);
        result += to;
        start = pos + from.length();
    }
    
    result += input.substr(start);
    return result;
}

} // namespace validation
} // namespace autograder
