// ============================================================================
// AUTOGRADER - MAIN HEADER (PUBLIC API)
// ============================================================================
//
// File: autograder.hpp
// Purpose: Main public header file for autograder library
//
// This is the main include file that brings together all components
// of the modular autograder C++ library.
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_HPP
#define AUTOGRADER_HPP

// Core components
#include "types.h"
#include "exceptions.h"

// Cryptography
#include "encryption.h"

// Validation
#include "validator.h"

// Comparison
#include "comparator.h"

// Network
#include "fetcher.h"

// Formatting
#include "formatter.h"

namespace autograder {

// ============================================================================
// VERSION INFORMATION
// ============================================================================

constexpr const char* VERSION = "0.4.0";
constexpr int VERSION_MAJOR = 0;
constexpr int VERSION_MINOR = 4;
constexpr int VERSION_PATCH = 0;

/**
 * @brief Get version string
 * @return Version string (e.g., "0.4.0")
 */
inline const char* get_version() {
    return VERSION;
}

// ============================================================================
// GLOBAL CONFIGURATION
// ============================================================================

/**
 * @brief Get global configuration
 * @return Reference to global config
 */
Config& get_config();

/**
 * @brief Set global configuration
 * @param config New configuration
 */
void set_config(const Config& config);

/**
 * @brief Reset configuration to defaults
 */
void reset_config();

// ============================================================================
// CONVENIENCE FUNCTIONS
// ============================================================================

/**
 * @brief Quick comparison of two R objects
 * @param obj1 First object
 * @param obj2 Second object
 * @param tolerance Comparison tolerance
 * @return true if equal
 */
bool compare_objects(SEXP obj1, SEXP obj2, double tolerance = 1e-10);

/**
 * @brief Validate a function name
 * @param name Function name
 * @return true if valid
 */
bool validate_function_name(const std::string& name);

/**
 * @brief Encrypt a string using default key
 * @param plaintext Text to encrypt
 * @return Encrypted hex string
 */
std::string encrypt_string(const std::string& plaintext);

/**
 * @brief Decrypt a string using default key
 * @param ciphertext Encrypted hex string
 * @return Decrypted text
 */
std::string decrypt_string(const std::string& ciphertext);

/**
 * @brief Format an R object for display
 * @param obj R object
 * @param max_length Maximum output length
 * @return Formatted string
 */
std::string format_object(SEXP obj, size_t max_length = 200);

} // namespace autograder

#endif // AUTOGRADER_HPP
