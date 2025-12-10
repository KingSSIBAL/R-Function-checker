// ============================================================================
// AUTOGRADER - MAIN IMPLEMENTATION AND RCPP EXPORTS
// ============================================================================
//
// File: autograder_main.cpp
// Purpose: Main implementation file with Rcpp exports
//
// This file provides:
//   - Global configuration management
//   - Rcpp exported functions for R interface
//   - Convenience wrappers for the modular components
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#include <Rcpp.h>
#include "autograder.hpp"
#include "crypto/encrypted_config.hpp"

using namespace Rcpp;

namespace autograder {

// ============================================================================
// GLOBAL CONFIGURATION
// ============================================================================

static Config global_config;

Config& get_config() {
    return global_config;
}

void set_config(const Config& config) {
    global_config = config;
}

void reset_config() {
    global_config = Config();
}

// ============================================================================
// CONVENIENCE FUNCTIONS IMPLEMENTATION
// ============================================================================

bool compare_objects(SEXP obj1, SEXP obj2, double tolerance) {
    compare::Comparator comp(tolerance);
    return comp.equal(obj1, obj2);
}

bool validate_function_name(const std::string& name) {
    return validation::Validator::is_valid_function_name(name);
}

std::string encrypt_string(const std::string& plaintext) {
    return crypto::quick_encrypt(plaintext);
}

std::string decrypt_string(const std::string& ciphertext) {
    return crypto::quick_decrypt(ciphertext);
}

std::string format_object(SEXP obj, size_t max_length) {
    format::Formatter fmt(max_length);
    return fmt.format(obj);
}

} // namespace autograder

// ============================================================================
// RCPP EXPORTED FUNCTIONS
// ============================================================================

/**
 * @brief High-performance comparison of R objects
 * 
 * @param obj1 First R object (typically student output)
 * @param obj2 Second R object (typically expected output)
 * @param tolerance Numeric tolerance for floating-point comparisons
 * 
 * @return LogicalVector of length 1: TRUE if match, FALSE otherwise
 */
// [[Rcpp::export(".cpp_compare_fast")]]
LogicalVector cpp_compare_fast(SEXP obj1, SEXP obj2, double tolerance = 1e-10) {
    autograder::compare::Comparator comp(tolerance);
    return LogicalVector::create(comp.equal(obj1, obj2));
}

/**
 * @brief Legacy comparison wrapper for backward compatibility
 */
// [[Rcpp::export(".cpp_compare_identical")]]
LogicalVector cpp_compare_identical(SEXP obj1, SEXP obj2) {
    return cpp_compare_fast(obj1, obj2, 1e-10);
}

/**
 * @brief Securely fetch function content from remote repository
 * 
 * @param function_name Name of function to fetch (must be validated)
 * @return CharacterVector where each element is one line of the R file
 */
// [[Rcpp::export(".cpp_fetch_function_content")]]
CharacterVector cpp_fetch_function_content(const std::string& function_name) {
    autograder::network::Fetcher fetcher;
    return fetcher.fetch_function_content(function_name);
}

/**
 * @brief Fetch list of available problems from repository
 * 
 * @return CharacterVector containing R code that defines problems list
 */
// [[Rcpp::export(".cpp_fetch_problems_list")]]
CharacterVector cpp_fetch_problems_list() {
    autograder::network::Fetcher fetcher;
    return fetcher.fetch_problems_list();
}

/**
 * @brief Fetch a data file from the repository
 * 
 * Downloads a data file (CSV, RDS, RData) from the data/ folder.
 * 
 * @param filename Name of the data file (e.g., "dataset.csv")
 * @return CharacterVector with path to the downloaded temporary file
 */
// [[Rcpp::export(".cpp_fetch_data_file")]]
CharacterVector cpp_fetch_data_file(const std::string& filename) {
    autograder::network::Fetcher fetcher;
    std::string path = fetcher.fetch_data_file(filename);
    return CharacterVector::create(path);
}

/**
 * @brief Validate a function name for security
 * 
 * @param name The function name to validate
 * @return LogicalVector indicating if name is valid
 */
// [[Rcpp::export(".cpp_validate_function_name")]]
LogicalVector cpp_validate_function_name(const std::string& name) {
    return LogicalVector::create(
        autograder::validation::Validator::is_valid_function_name(name)
    );
}

/**
 * @brief Check if internet connection is available
 * 
 * @return LogicalVector indicating if connected
 */
// [[Rcpp::export(".cpp_has_internet")]]
LogicalVector cpp_has_internet() {
    return LogicalVector::create(autograder::network::Fetcher::has_internet());
}

/**
 * @brief Format an R object for display
 * 
 * @param obj R object to format
 * @param max_length Maximum output length
 * @return CharacterVector with formatted output
 */
// [[Rcpp::export(".cpp_format_output")]]
CharacterVector cpp_format_output(SEXP obj, int max_length = 200) {
    autograder::format::Formatter fmt(static_cast<size_t>(max_length));
    return CharacterVector::create(fmt.format(obj));
}

/**
 * @brief Get comparison result with details
 * 
 * @param obj1 First R object
 * @param obj2 Second R object
 * @param tolerance Comparison tolerance
 * @return List with equal (logical), first_diff (integer), description (character)
 */
// [[Rcpp::export(".cpp_compare_detailed")]]
List cpp_compare_detailed(SEXP obj1, SEXP obj2, double tolerance = 1e-10) {
    autograder::compare::Comparator comp(tolerance);
    autograder::ComparisonResult result = comp.compare(obj1, obj2);
    
    return List::create(
        Named("equal") = result.equal,
        Named("first_diff") = static_cast<int>(result.first_diff_index + 1),  // 1-based
        Named("description") = result.diff_description
    );
}

/**
 * @brief Find positions where two numeric vectors differ
 * 
 * @param v1 First numeric vector
 * @param v2 Second numeric vector
 * @param tolerance Comparison tolerance
 * @param max_diffs Maximum differences to return
 * @return IntegerVector of differing positions (1-based)
 */
// [[Rcpp::export(".cpp_find_differences")]]
IntegerVector cpp_find_differences(NumericVector v1, NumericVector v2,
                                    double tolerance = 1e-10, int max_diffs = 10) {
    std::vector<R_xlen_t> diffs = autograder::compare::find_differences(
        v1, v2, tolerance, static_cast<size_t>(max_diffs));
    
    IntegerVector result(diffs.size());
    for (size_t i = 0; i < diffs.size(); ++i) {
        result[i] = static_cast<int>(diffs[i] + 1);  // 1-based indexing
    }
    
    return result;
}

// ============================================================================
// ENCRYPTION HELPER FUNCTIONS (NEW)
// ============================================================================

/**
 * @brief Encrypt a string using AES S-box encryption
 * 
 * @param plaintext Text to encrypt
 * @param key Optional encryption key (uses default if empty)
 * @return CharacterVector with hex-encoded encrypted string
 */
// [[Rcpp::export(".cpp_encrypt")]]
CharacterVector cpp_encrypt(const std::string& plaintext, 
                            const std::string& key = "") {
    try {
        std::string result = autograder::crypto::quick_encrypt(plaintext, key);
        return CharacterVector::create(result);
    } catch (const std::exception& e) {
        stop("Encryption failed: %s", e.what());
    }
}

/**
 * @brief Decrypt a hex-encoded encrypted string
 * 
 * @param ciphertext Hex-encoded encrypted string
 * @param key Optional encryption key (uses default if empty)
 * @return CharacterVector with decrypted text
 */
// [[Rcpp::export(".cpp_decrypt")]]
CharacterVector cpp_decrypt(const std::string& ciphertext,
                            const std::string& key = "") {
    try {
        std::string result = autograder::crypto::quick_decrypt(ciphertext, key);
        return CharacterVector::create(result);
    } catch (const std::exception& e) {
        stop("Decryption failed: %s", e.what());
    }
}

/**
 * @brief Encrypt a string to base64 encoding
 * 
 * @param plaintext Text to encrypt
 * @param key Optional encryption key
 * @return CharacterVector with base64-encoded encrypted string
 */
// [[Rcpp::export(".cpp_encrypt_base64")]]
CharacterVector cpp_encrypt_base64(const std::string& plaintext,
                                    const std::string& key = "") {
    try {
        autograder::crypto::Encryptor enc(
            key.empty() ? autograder::crypto::KeyDeriver::derive_default() : key
        );
        std::string result = enc.encrypt_to_base64(plaintext);
        return CharacterVector::create(result);
    } catch (const std::exception& e) {
        stop("Encryption failed: %s", e.what());
    }
}

/**
 * @brief Decrypt a base64-encoded encrypted string
 * 
 * @param ciphertext Base64-encoded encrypted string
 * @param key Optional encryption key
 * @return CharacterVector with decrypted text
 */
// [[Rcpp::export(".cpp_decrypt_base64")]]
CharacterVector cpp_decrypt_base64(const std::string& ciphertext,
                                    const std::string& key = "") {
    try {
        autograder::crypto::Encryptor enc(
            key.empty() ? autograder::crypto::KeyDeriver::derive_default() : key
        );
        std::string result = enc.decrypt_from_base64(ciphertext);
        return CharacterVector::create(result);
    } catch (const std::exception& e) {
        stop("Decryption failed: %s", e.what());
    }
}

/**
 * @brief Derive a key from multiple factors
 * 
 * @param factors Character vector of key factors
 * @param key_length Desired key length (default 32 for 256-bit)
 * @return CharacterVector with hex-encoded derived key
 */
// [[Rcpp::export(".cpp_derive_key")]]
CharacterVector cpp_derive_key(CharacterVector factors, int key_length = 32) {
    try {
        std::vector<std::string> factor_vec;
        for (int i = 0; i < factors.size(); ++i) {
            factor_vec.push_back(as<std::string>(factors[i]));
        }
        
        std::string key = autograder::crypto::KeyDeriver::derive(
            factor_vec, static_cast<size_t>(key_length));
        std::string hex_key = autograder::crypto::Encoder::to_hex(key);
        
        return CharacterVector::create(hex_key);
    } catch (const std::exception& e) {
        stop("Key derivation failed: %s", e.what());
    }
}

/**
 * @brief Generate a random encryption key
 * 
 * @param length Key length in bytes (default 32)
 * @return CharacterVector with hex-encoded random key
 */
// [[Rcpp::export(".cpp_generate_key")]]
CharacterVector cpp_generate_key(int length = 32) {
    try {
        std::string key = autograder::crypto::generate_random_key(
            static_cast<size_t>(length));
        std::string hex_key = autograder::crypto::Encoder::to_hex(key);
        return CharacterVector::create(hex_key);
    } catch (const std::exception& e) {
        stop("Key generation failed: %s", e.what());
    }
}

/**
 * @brief Encrypt a URL for storage
 * 
 * @param url The URL to encrypt
 * @return CharacterVector with encrypted URL
 */
// [[Rcpp::export(".cpp_encrypt_url")]]
CharacterVector cpp_encrypt_url(const std::string& url) {
    try {
        autograder::crypto::UrlEncryptor enc;
        std::string result = enc.encrypt_url(url);
        return CharacterVector::create(result);
    } catch (const std::exception& e) {
        stop("URL encryption failed: %s", e.what());
    }
}

/**
 * @brief Decrypt an encrypted URL
 * 
 * @param encrypted_url The encrypted URL
 * @return CharacterVector with decrypted URL
 */
// [[Rcpp::export(".cpp_decrypt_url")]]
CharacterVector cpp_decrypt_url(const std::string& encrypted_url) {
    try {
        autograder::crypto::UrlEncryptor enc;
        std::string result = enc.decrypt_url(encrypted_url);
        return CharacterVector::create(result);
    } catch (const std::exception& e) {
        stop("URL decryption failed: %s", e.what());
    }
}

/**
 * @brief Get the autograder version
 * 
 * @return CharacterVector with version string
 */
// [[Rcpp::export(".cpp_get_version")]]
CharacterVector cpp_get_version() {
    return CharacterVector::create(autograder::get_version());
}

/**
 * @brief Get R object type name
 * 
 * @param obj R object
 * @return CharacterVector with type name
 */
// [[Rcpp::export(".cpp_get_type")]]
CharacterVector cpp_get_type(SEXP obj) {
    return CharacterVector::create(autograder::compare::get_type_name(obj));
}

// ============================================================================
// AUTHENTICATION FUNCTIONS
// ============================================================================

/**
 * @brief Check if authentication is enabled
 * 
 * @return LogicalVector TRUE if private repo mode enabled
 */
// [[Rcpp::export(".cpp_is_auth_enabled")]]
LogicalVector cpp_is_auth_enabled() {
    return LogicalVector::create(autograder::config::USE_AUTHENTICATION);
}

/**
 * @brief Get authentication mode as string
 * 
 * @return CharacterVector "secure" or "legacy"
 */
// [[Rcpp::export(".cpp_get_auth_mode")]]
CharacterVector cpp_get_auth_mode() {
    return CharacterVector::create(
        autograder::config::USE_AUTHENTICATION ? "secure" : "legacy"
    );
}

/**
 * @brief Check if authentication token is configured
 * 
 * @return LogicalVector TRUE if token is present (not empty)
 */
// [[Rcpp::export(".cpp_has_auth_token")]]
LogicalVector cpp_has_auth_token() {
    return LogicalVector::create(
        autograder::config::USE_AUTHENTICATION && 
        autograder::config::ENCRYPTED_TOKEN_LEN > 0
    );
}

/**
 * @brief Get authentication info (for diagnostics)
 * 
 * @return List with mode, has_token, url_encrypted
 */
// [[Rcpp::export(".cpp_get_auth_info")]]
List cpp_get_auth_info() {
    return List::create(
        Named("mode") = autograder::config::USE_AUTHENTICATION ? "secure" : "legacy",
        Named("has_token") = autograder::config::USE_AUTHENTICATION && 
                             autograder::config::ENCRYPTED_TOKEN_LEN > 0,
        Named("url_length") = static_cast<int>(autograder::config::ENCRYPTED_URL_LEN),
        Named("token_length") = static_cast<int>(autograder::config::ENCRYPTED_TOKEN_LEN)
    );
}
