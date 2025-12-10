// ============================================================================
// AUTOGRADER - INPUT VALIDATION MODULE HEADER
// ============================================================================
//
// File: validation/validator.hpp
// Purpose: Input validation and sanitization functions
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_VALIDATOR_HPP
#define AUTOGRADER_VALIDATOR_HPP

#include <string>
#include <vector>
#include <regex>
#include "types.hpp"
#include "exceptions.hpp"

namespace autograder {
namespace validation {

// ============================================================================
// VALIDATOR CLASS
// ============================================================================

/**
 * @class Validator
 * @brief Input validation and sanitization
 * 
 * Provides comprehensive input validation to prevent:
 *   - Path traversal attacks
 *   - Command injection
 *   - Invalid function names
 *   - Malformed input data
 */
class Validator {
public:
    /**
     * @brief Validate a function name
     * 
     * Rules:
     *   - Length: 1-100 characters
     *   - No path separators: / \
     *   - No parent directory: ..
     *   - No home directory: ~
     *   - Only alphanumeric, underscore, hyphen
     * 
     * @param name The function name to validate
     * @return ValidationResult with status and sanitized name
     */
    static ValidationResult validate_function_name(const std::string& name);
    
    /**
     * @brief Check if a function name is valid
     * @param name The function name to check
     * @return true if valid, false otherwise
     */
    static bool is_valid_function_name(const std::string& name);
    
    /**
     * @brief Validate a URL
     * @param url The URL to validate
     * @return ValidationResult with status
     */
    static ValidationResult validate_url(const std::string& url);
    
    /**
     * @brief Validate a file path
     * @param path The file path to validate
     * @return ValidationResult with status and sanitized path
     */
    static ValidationResult validate_path(const std::string& path);
    
    /**
     * @brief Sanitize a string for safe use in URLs
     * @param input The string to sanitize
     * @return URL-safe string
     */
    static std::string sanitize_for_url(const std::string& input);
    
    /**
     * @brief Sanitize a string for safe use in file paths
     * @param input The string to sanitize
     * @return Path-safe string
     */
    static std::string sanitize_for_path(const std::string& input);
    
    /**
     * @brief Check for path traversal attempts
     * @param path The path to check
     * @return true if path traversal detected
     */
    static bool contains_path_traversal(const std::string& path);
    
    /**
     * @brief Check if character is allowed in function names
     * @param c The character to check
     * @return true if allowed
     */
    static bool is_allowed_char(char c);
    
    /**
     * @brief Validate test case structure
     * @param test_data The test data (placeholder for R object)
     * @return ValidationResult
     */
    static ValidationResult validate_test_cases(const std::string& test_data);
    
private:
    // Maximum allowed function name length
    static constexpr size_t MAX_NAME_LENGTH = 100;
    
    // Dangerous path patterns
    static const std::vector<std::string> DANGEROUS_PATTERNS;
};

// ============================================================================
// SANITIZER CLASS
// ============================================================================

/**
 * @class Sanitizer
 * @brief String sanitization utilities
 */
class Sanitizer {
public:
    /**
     * @brief Remove all non-alphanumeric characters except allowed ones
     * @param input Input string
     * @param allowed Additional allowed characters
     * @return Sanitized string
     */
    static std::string remove_special_chars(const std::string& input, 
                                            const std::string& allowed = "_-");
    
    /**
     * @brief Escape special characters for shell commands
     * @param input Input string
     * @return Escaped string
     */
    static std::string escape_shell(const std::string& input);
    
    /**
     * @brief Trim whitespace from both ends
     * @param input Input string
     * @return Trimmed string
     */
    static std::string trim(const std::string& input);
    
    /**
     * @brief Convert to lowercase
     * @param input Input string
     * @return Lowercase string
     */
    static std::string to_lower(const std::string& input);
    
    /**
     * @brief Convert to uppercase
     * @param input Input string
     * @return Uppercase string
     */
    static std::string to_upper(const std::string& input);
    
    /**
     * @brief Replace all occurrences of a substring
     * @param input Input string
     * @param from Substring to replace
     * @param to Replacement substring
     * @return Modified string
     */
    static std::string replace_all(const std::string& input,
                                   const std::string& from,
                                   const std::string& to);
};

} // namespace validation
} // namespace autograder

#endif // AUTOGRADER_VALIDATOR_HPP
