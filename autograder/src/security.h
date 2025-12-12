// ============================================================================
// AUTOGRADER - SECURITY MODULE HEADER
// ============================================================================
//
// File: security.h
// Purpose: Security utilities including code validation, rate limiting,
//          and secure random number generation
//
// Security Features:
//   - Code pattern validation (dangerous function detection)
//   - Rate limiting state management
//   - Cryptographically secure random byte generation
//   - Audit event tracking
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_SECURITY_H
#define AUTOGRADER_SECURITY_H

#include <string>
#include <vector>
#include <array>
#include <cstdint>
#include <chrono>
#include <mutex>
#include <atomic>
#include "types.h"

namespace autograder {
namespace security {

// ============================================================================
// DANGEROUS CODE PATTERNS
// ============================================================================

/**
 * @brief Severity levels for dangerous patterns
 */
enum class PatternSeverity {
    CRITICAL,   // Must block immediately (system exec, quit)
    HIGH,       // Should block (file delete, env modify)
    MEDIUM      // Warn or block depending on mode
};

/**
 * @brief Information about a dangerous pattern
 */
struct DangerousPattern {
    const char* name;
    const char* pattern;
    const char* description;
    PatternSeverity severity;
};

/**
 * @brief Result of code safety validation
 */
struct CodeValidationResult {
    bool safe;
    std::vector<std::string> violations;
    std::vector<std::string> severities;
    std::string error_message;
    
    CodeValidationResult() : safe(true) {}
    CodeValidationResult(bool s) : safe(s) {}
};

// ============================================================================
// CODE VALIDATOR CLASS
// ============================================================================

/**
 * @class CodeValidator
 * @brief Validates R code for dangerous patterns
 * 
 * Scans code for potentially dangerous operations that should not
 * appear in instructor code. Patterns are compiled into the binary
 * for security (not visible in R source).
 */
class CodeValidator {
public:
    /**
     * @brief Validate code for dangerous patterns
     * @param code R code as string
     * @param strict If true, reject all severity levels. If false, only critical/high.
     * @return CodeValidationResult with details
     */
    static CodeValidationResult validate(const std::string& code, bool strict = true);
    
    /**
     * @brief Check if code contains any dangerous pattern
     * @param code R code as string
     * @return true if dangerous patterns found
     */
    static bool contains_dangerous_patterns(const std::string& code);
    
    /**
     * @brief Get list of all dangerous pattern names (for testing)
     * @return Vector of pattern names
     */
    static std::vector<std::string> get_pattern_names();

private:
    // Static array of dangerous patterns (compiled into binary)
    static const std::array<DangerousPattern, 10> DANGEROUS_PATTERNS;
    
    // Check single pattern against code
    static bool matches_pattern(const std::string& code, const char* pattern);
};

// ============================================================================
// RATE LIMITER CLASS
// ============================================================================

/**
 * @class RateLimiter
 * @brief Thread-safe rate limiting for API calls
 * 
 * Implements a sliding window rate limiter to prevent API abuse.
 * State is maintained in C++ for harder manipulation.
 */
class RateLimiter {
public:
    /**
     * @brief Get singleton instance
     */
    static RateLimiter& instance();
    
    /**
     * @brief Configure rate limiting parameters
     * @param max_calls Maximum calls per window
     * @param window_seconds Window duration in seconds
     */
    void configure(int max_calls, int window_seconds);
    
    /**
     * @brief Check if request is within rate limits
     * @param action Action name for logging
     * @return true if within limits, false if exceeded
     */
    bool check_limit(const std::string& action = "api_call");
    
    /**
     * @brief Get current rate limit status
     * @return Tuple of (calls_remaining, window_remaining_seconds)
     */
    std::pair<int, int> get_status() const;
    
    /**
     * @brief Reset rate limiter state
     */
    void reset();
    
    /**
     * @brief Get current configuration
     * @return Tuple of (max_calls, window_seconds)
     */
    std::pair<int, int> get_config() const;

private:
    RateLimiter();
    ~RateLimiter() = default;
    RateLimiter(const RateLimiter&) = delete;
    RateLimiter& operator=(const RateLimiter&) = delete;
    
    mutable std::mutex mutex_;
    std::chrono::steady_clock::time_point window_start_;
    std::atomic<int> call_count_;
    int max_calls_;
    int window_seconds_;
    bool initialized_;
};

// ============================================================================
// SECURE RANDOM CLASS
// ============================================================================

/**
 * @class SecureRandom
 * @brief Cryptographically secure random number generation
 * 
 * Uses platform-specific secure random sources:
 *   - Windows: CryptGenRandom / BCryptGenRandom
 *   - Unix: /dev/urandom
 *   - Fallback: std::random_device with seeding
 */
class SecureRandom {
public:
    /**
     * @brief Generate secure random bytes
     * @param count Number of bytes to generate
     * @return Vector of random bytes
     */
    static std::vector<uint8_t> generate_bytes(size_t count);
    
    /**
     * @brief Generate secure random bytes as hex string
     * @param count Number of bytes to generate
     * @return Hex-encoded string of random bytes
     */
    static std::string generate_hex(size_t count);
    
    /**
     * @brief Generate a random integer in range [min, max]
     * @param min Minimum value (inclusive)
     * @param max Maximum value (inclusive)
     * @return Random integer
     */
    static int generate_int(int min, int max);
    
private:
    // Platform-specific implementation
    static bool fill_random_bytes(uint8_t* buffer, size_t count);
};

// ============================================================================
// AUDIT LOGGER CLASS
// ============================================================================

/**
 * @brief Single audit log entry
 */
struct AuditEntry {
    std::string timestamp;
    std::string event;
    std::string details;
    int session_pid;
    std::string user;
};

/**
 * @class AuditLogger
 * @brief Security event audit logging
 * 
 * Maintains an in-memory log of security events for forensics.
 * Thread-safe implementation.
 */
class AuditLogger {
public:
    /**
     * @brief Get singleton instance
     */
    static AuditLogger& instance();
    
    /**
     * @brief Enable or disable logging
     * @param enabled Whether to enable logging
     * @param max_entries Maximum entries to keep in memory
     */
    void configure(bool enabled, size_t max_entries = 1000);
    
    /**
     * @brief Log a security event
     * @param event Event type
     * @param details Event details
     */
    void log_event(const std::string& event, const std::string& details = "");
    
    /**
     * @brief Get recent log entries
     * @param count Number of entries to retrieve (0 = all)
     * @return Vector of audit entries
     */
    std::vector<AuditEntry> get_entries(size_t count = 0) const;
    
    /**
     * @brief Clear all log entries
     */
    void clear();
    
    /**
     * @brief Check if logging is enabled
     */
    bool is_enabled() const;

private:
    AuditLogger();
    ~AuditLogger() = default;
    AuditLogger(const AuditLogger&) = delete;
    AuditLogger& operator=(const AuditLogger&) = delete;
    
    mutable std::mutex mutex_;
    std::vector<AuditEntry> entries_;
    size_t max_entries_;
    bool enabled_;
};

} // namespace security
} // namespace autograder

#endif // AUTOGRADER_SECURITY_H
