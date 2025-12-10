// ============================================================================
// AUTOGRADER - CUSTOM EXCEPTION CLASSES
// ============================================================================
//
// File: core/exceptions.hpp
// Purpose: Exception hierarchy for autograder error handling
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_EXCEPTIONS_HPP
#define AUTOGRADER_EXCEPTIONS_HPP

#include <stdexcept>
#include <string>

namespace autograder {

// ============================================================================
// BASE EXCEPTION
// ============================================================================

/**
 * @class AutograderException
 * @brief Base exception for all autograder-specific errors
 * 
 * Provides a common base for all autograder exceptions,
 * enabling catch-all handling for autograder errors.
 */
class AutograderException : public std::runtime_error {
public:
    explicit AutograderException(const std::string& msg) 
        : std::runtime_error(msg) {}
    
    virtual const char* type() const noexcept { return "AutograderException"; }
};

// ============================================================================
// NETWORK EXCEPTIONS
// ============================================================================

/**
 * @class NetworkException
 * @brief Exception for network-related failures
 * 
 * Thrown when:
 *   - Internet connection is unavailable
 *   - Download times out (>30 seconds)
 *   - Server returns error code (5xx)
 *   - DNS resolution fails
 *   - SSL/TLS handshake fails
 */
class NetworkException : public AutograderException {
public:
    explicit NetworkException(const std::string& msg) 
        : AutograderException(msg) {}
    
    const char* type() const noexcept override { return "NetworkException"; }
};

/**
 * @class TimeoutException
 * @brief Exception for timeout errors
 */
class TimeoutException : public NetworkException {
public:
    explicit TimeoutException(const std::string& msg) 
        : NetworkException(msg) {}
    
    const char* type() const noexcept override { return "TimeoutException"; }
};

/**
 * @class ConnectionException
 * @brief Exception for connection failures
 */
class ConnectionException : public NetworkException {
public:
    explicit ConnectionException(const std::string& msg) 
        : NetworkException(msg) {}
    
    const char* type() const noexcept override { return "ConnectionException"; }
};

// ============================================================================
// VALIDATION EXCEPTIONS
// ============================================================================

/**
 * @class ValidationException
 * @brief Exception for input validation failures
 * 
 * Security-critical exception indicating potential malicious input.
 */
class ValidationException : public AutograderException {
public:
    explicit ValidationException(const std::string& msg) 
        : AutograderException(msg) {}
    
    const char* type() const noexcept override { return "ValidationException"; }
};

/**
 * @class InvalidInputException
 * @brief Exception for invalid user input
 */
class InvalidInputException : public ValidationException {
public:
    explicit InvalidInputException(const std::string& msg) 
        : ValidationException(msg) {}
    
    const char* type() const noexcept override { return "InvalidInputException"; }
};

/**
 * @class PathTraversalException
 * @brief Exception for path traversal attack attempts
 */
class PathTraversalException : public ValidationException {
public:
    explicit PathTraversalException(const std::string& msg) 
        : ValidationException(msg) {}
    
    const char* type() const noexcept override { return "PathTraversalException"; }
};

// ============================================================================
// RESOURCE EXCEPTIONS
// ============================================================================

/**
 * @class ResourceException
 * @brief Base exception for resource-related errors
 */
class ResourceException : public AutograderException {
public:
    explicit ResourceException(const std::string& msg) 
        : AutograderException(msg) {}
    
    const char* type() const noexcept override { return "ResourceException"; }
};

/**
 * @class FunctionNotFoundException
 * @brief Exception for missing/nonexistent functions
 */
class FunctionNotFoundException : public ResourceException {
private:
    std::string function_name_;
    
public:
    explicit FunctionNotFoundException(const std::string& function_name) 
        : ResourceException("Function '" + function_name + "' not found.\n"
                          "Use list_problems() to see available functions.\n\n"
                          "Common issues:\n"
                          "  * Check spelling\n"
                          "  * Ensure the function exists in the repository\n"
                          "  * Verify your internet connection")
        , function_name_(function_name) {}
    
    const char* type() const noexcept override { return "FunctionNotFoundException"; }
    
    const std::string& function_name() const noexcept { return function_name_; }
};

/**
 * @class TestCaseException
 * @brief Exception for test case configuration errors
 */
class TestCaseException : public ResourceException {
public:
    explicit TestCaseException(const std::string& msg) 
        : ResourceException(msg) {}
    
    const char* type() const noexcept override { return "TestCaseException"; }
};

// ============================================================================
// ENCRYPTION EXCEPTIONS
// ============================================================================

/**
 * @class CryptoException
 * @brief Base exception for cryptographic errors
 */
class CryptoException : public AutograderException {
public:
    explicit CryptoException(const std::string& msg) 
        : AutograderException(msg) {}
    
    const char* type() const noexcept override { return "CryptoException"; }
};

/**
 * @class EncryptionException
 * @brief Exception for encryption failures
 */
class EncryptionException : public CryptoException {
public:
    explicit EncryptionException(const std::string& msg) 
        : CryptoException(msg) {}
    
    const char* type() const noexcept override { return "EncryptionException"; }
};

/**
 * @class DecryptionException
 * @brief Exception for decryption failures
 */
class DecryptionException : public CryptoException {
public:
    explicit DecryptionException(const std::string& msg) 
        : CryptoException(msg) {}
    
    const char* type() const noexcept override { return "DecryptionException"; }
};

/**
 * @class InvalidKeyException
 * @brief Exception for invalid encryption keys
 */
class InvalidKeyException : public CryptoException {
public:
    explicit InvalidKeyException(const std::string& msg) 
        : CryptoException(msg) {}
    
    const char* type() const noexcept override { return "InvalidKeyException"; }
};

// ============================================================================
// EXECUTION EXCEPTIONS
// ============================================================================

/**
 * @class ExecutionException
 * @brief Exception for test execution failures
 */
class ExecutionException : public AutograderException {
private:
    int test_number_;
    
public:
    ExecutionException(const std::string& msg, int test_number) 
        : AutograderException(msg), test_number_(test_number) {}
    
    const char* type() const noexcept override { return "ExecutionException"; }
    
    int test_number() const noexcept { return test_number_; }
};

} // namespace autograder

#endif // AUTOGRADER_EXCEPTIONS_HPP
