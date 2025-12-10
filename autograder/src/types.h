// ============================================================================
// AUTOGRADER - TYPE DEFINITIONS
// ============================================================================
//
// File: core/types.hpp
// Purpose: Common type definitions and forward declarations
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_TYPES_HPP
#define AUTOGRADER_TYPES_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <memory>
#include <functional>

namespace autograder {

// ============================================================================
// TYPE ALIASES
// ============================================================================

using ByteVector = std::vector<uint8_t>;
using StringVector = std::vector<std::string>;

// ============================================================================
// ENCRYPTION TYPES
// ============================================================================

/**
 * @brief Encryption algorithm identifier
 */
enum class EncryptionAlgorithm {
    NONE,           // No encryption
    XOR_SIMPLE,     // Simple XOR cipher
    AES_SBOX,       // AES S-box based encryption
    AES_256_CBC     // Full AES-256 CBC mode
};

/**
 * @brief Result of encryption/decryption operation
 */
struct CryptoResult {
    bool success;
    std::string data;
    std::string error_message;
    
    CryptoResult() : success(false) {}
    CryptoResult(bool s, const std::string& d) : success(s), data(d) {}
    CryptoResult(const std::string& err) : success(false), error_message(err) {}
};

// ============================================================================
// COMPARISON TYPES
// ============================================================================

/**
 * @brief Result of comparison operation
 */
struct ComparisonResult {
    bool equal;
    size_t first_diff_index;
    std::string diff_description;
    
    ComparisonResult() : equal(true), first_diff_index(0) {}
    ComparisonResult(bool eq) : equal(eq), first_diff_index(0) {}
    ComparisonResult(size_t idx, const std::string& desc) 
        : equal(false), first_diff_index(idx), diff_description(desc) {}
};

// ============================================================================
// VALIDATION TYPES
// ============================================================================

/**
 * @brief Result of validation operation
 */
struct ValidationResult {
    bool valid;
    std::string error_message;
    std::string sanitized_input;
    
    ValidationResult() : valid(false) {}
    explicit ValidationResult(bool v) : valid(v) {}
    ValidationResult(bool v, const std::string& sanitized) 
        : valid(v), sanitized_input(sanitized) {}
    ValidationResult(const std::string& err) : valid(false), error_message(err) {}
    // Add const char* constructor to avoid implicit bool conversion
    ValidationResult(const char* err) : valid(false), error_message(err) {}
};

// ============================================================================
// NETWORK TYPES
// ============================================================================

/**
 * @brief HTTP response structure
 */
struct HttpResponse {
    int status_code;
    std::string body;
    std::string content_type;
    bool success;
    std::string error_message;
    
    HttpResponse() : status_code(0), success(false) {}
    HttpResponse(int code, const std::string& b) 
        : status_code(code), body(b), success(code >= 200 && code < 300) {}
};

// ============================================================================
// CONFIGURATION TYPES
// ============================================================================

/**
 * @brief Authentication mode for repository access
 */
enum class AuthMode {
    NONE,           // No authentication (public repo, legacy mode)
    GITHUB_TOKEN,   // GitHub Personal Access Token
    OAUTH           // OAuth2 flow (future)
};

/**
 * @brief Autograder configuration structure
 */
struct Config {
    std::string base_url;
    int timeout_seconds;
    double default_tolerance;
    bool enable_parallel;
    int max_threads;
    EncryptionAlgorithm encryption;
    AuthMode auth_mode;
    std::string auth_token;  // Encrypted token, decrypted at runtime
    
    Config() 
        : base_url("https://raw.githubusercontent.com/KingSSIBAL/R-Function-checker/main/repo")
        , timeout_seconds(30)
        , default_tolerance(1e-10)
        , enable_parallel(true)
        , max_threads(4)
        , encryption(EncryptionAlgorithm::AES_SBOX)
        , auth_mode(AuthMode::NONE)
        , auth_token("") {}
};

} // namespace autograder

#endif // AUTOGRADER_TYPES_HPP
