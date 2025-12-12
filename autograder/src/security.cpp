// ============================================================================
// AUTOGRADER - SECURITY MODULE IMPLEMENTATION
// ============================================================================
//
// File: security.cpp
// Purpose: Implementation of security utilities
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#include "security.h"
#include <regex>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <ctime>
#include <random>

#ifdef _WIN32
#include <windows.h>
#include <wincrypt.h>
#pragma comment(lib, "advapi32.lib")
#else
#include <fstream>
#include <unistd.h>
#endif

namespace autograder {
namespace security {

// ============================================================================
// DANGEROUS PATTERNS DEFINITION
// ============================================================================
// These patterns are compiled into the binary, making them harder to discover
// or bypass compared to R source code definitions.

const std::array<DangerousPattern, 10> CodeValidator::DANGEROUS_PATTERNS = {{
    // System command execution - CRITICAL
    {
        "system_exec",
        "\\b(system|system2|shell|shell\\.exec)\\s*\\(",
        "System command execution",
        PatternSeverity::CRITICAL
    },
    // R session termination - CRITICAL
    {
        "quit_r",
        "\\b(quit|q)\\s*\\(\\s*\\)",
        "R session termination",
        PatternSeverity::CRITICAL
    },
    // File system modifications - HIGH
    {
        "file_delete",
        "\\b(unlink|file\\.remove|file\\.rename|dir\\.create)\\s*\\(",
        "File system modification",
        PatternSeverity::HIGH
    },
    // Environment manipulation - HIGH
    {
        "env_modify",
        "\\b(Sys\\.setenv|Sys\\.unsetenv)\\s*\\(",
        "Environment variable modification",
        PatternSeverity::HIGH
    },
    // Dynamic code loading - HIGH
    {
        "dynamic_load",
        "\\b(dyn\\.load|library\\.dynam)\\s*\\(",
        "Dynamic library loading",
        PatternSeverity::HIGH
    },
    // Registry manipulation (Windows) - HIGH
    {
        "registry",
        "\\b(readRegistry|setRegistry)\\s*\\(",
        "Windows registry access",
        PatternSeverity::HIGH
    },
    // Network downloads to disk - MEDIUM
    {
        "download",
        "\\b(download\\.file|curl_download)\\s*\\(",
        "File download to disk",
        PatternSeverity::MEDIUM
    },
    // Connection to external resources - MEDIUM
    {
        "external_conn",
        "\\b(socketConnection|pipe|fifo)\\s*\\(",
        "External connection creation",
        PatternSeverity::MEDIUM
    },
    // Dangerous eval patterns - HIGH
    {
        "eval_parse",
        "\\beval\\s*\\(\\s*parse\\s*\\(",
        "Dynamic code evaluation",
        PatternSeverity::HIGH
    },
    // Source external files - HIGH
    {
        "source_external",
        "\\bsource\\s*\\(\\s*[\"']https?://",
        "Remote code sourcing",
        PatternSeverity::HIGH
    }
}};

// ============================================================================
// CODE VALIDATOR IMPLEMENTATION
// ============================================================================

bool CodeValidator::matches_pattern(const std::string& code, const char* pattern) {
    try {
        std::regex re(pattern, std::regex::ECMAScript | std::regex::icase);
        return std::regex_search(code, re);
    } catch (const std::regex_error&) {
        // If regex fails, do simple substring search as fallback
        return code.find(pattern) != std::string::npos;
    }
}

CodeValidationResult CodeValidator::validate(const std::string& code, bool strict) {
    CodeValidationResult result;
    result.safe = true;
    
    for (const auto& pattern : DANGEROUS_PATTERNS) {
        // In strict mode, check all severities
        // In non-strict mode, only check CRITICAL and HIGH
        bool should_check = strict || 
                           (pattern.severity == PatternSeverity::CRITICAL ||
                            pattern.severity == PatternSeverity::HIGH);
        
        if (should_check && matches_pattern(code, pattern.pattern)) {
            result.safe = false;
            result.violations.push_back(pattern.name);
            
            switch (pattern.severity) {
                case PatternSeverity::CRITICAL:
                    result.severities.push_back("critical");
                    break;
                case PatternSeverity::HIGH:
                    result.severities.push_back("high");
                    break;
                case PatternSeverity::MEDIUM:
                    result.severities.push_back("medium");
                    break;
            }
        }
    }
    
    if (!result.safe) {
        std::ostringstream oss;
        oss << "Code contains potentially dangerous operations:\n";
        for (size_t i = 0; i < result.violations.size(); ++i) {
            // Find the pattern description
            for (const auto& p : DANGEROUS_PATTERNS) {
                if (result.violations[i] == p.name) {
                    oss << "  - " << p.description << " (" << result.severities[i] << " severity)\n";
                    break;
                }
            }
        }
        result.error_message = oss.str();
    }
    
    return result;
}

bool CodeValidator::contains_dangerous_patterns(const std::string& code) {
    for (const auto& pattern : DANGEROUS_PATTERNS) {
        if (matches_pattern(code, pattern.pattern)) {
            return true;
        }
    }
    return false;
}

std::vector<std::string> CodeValidator::get_pattern_names() {
    std::vector<std::string> names;
    names.reserve(DANGEROUS_PATTERNS.size());
    for (const auto& pattern : DANGEROUS_PATTERNS) {
        names.push_back(pattern.name);
    }
    return names;
}

// ============================================================================
// RATE LIMITER IMPLEMENTATION
// ============================================================================

RateLimiter& RateLimiter::instance() {
    static RateLimiter instance;
    return instance;
}

RateLimiter::RateLimiter() 
    : call_count_(0)
    , max_calls_(30)
    , window_seconds_(60)
    , initialized_(false) {}

void RateLimiter::configure(int max_calls, int window_seconds) {
    std::lock_guard<std::mutex> lock(mutex_);
    max_calls_ = max_calls;
    window_seconds_ = window_seconds;
}

bool RateLimiter::check_limit(const std::string& action) {
    std::lock_guard<std::mutex> lock(mutex_);
    
    auto now = std::chrono::steady_clock::now();
    
    // Initialize window on first call
    if (!initialized_) {
        window_start_ = now;
        call_count_ = 0;
        initialized_ = true;
    }
    
    // Check if window has expired
    auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(
        now - window_start_).count();
    
    if (elapsed >= window_seconds_) {
        // Reset window
        window_start_ = now;
        call_count_ = 0;
    }
    
    // Increment counter
    ++call_count_;
    
    // Check limit
    if (call_count_ > max_calls_) {
        // Log the rate limit event
        AuditLogger::instance().log_event("rate_limit_exceeded", 
            "action=" + action + ";count=" + std::to_string(call_count_.load()));
        return false;
    }
    
    return true;
}

std::pair<int, int> RateLimiter::get_status() const {
    std::lock_guard<std::mutex> lock(mutex_);
    
    if (!initialized_) {
        return {max_calls_, window_seconds_};
    }
    
    auto now = std::chrono::steady_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(
        now - window_start_).count();
    
    if (elapsed >= window_seconds_) {
        return {max_calls_, window_seconds_};
    }
    
    int remaining_calls = std::max(0, max_calls_ - call_count_.load());
    int remaining_seconds = static_cast<int>(window_seconds_ - elapsed);
    
    return {remaining_calls, remaining_seconds};
}

void RateLimiter::reset() {
    std::lock_guard<std::mutex> lock(mutex_);
    call_count_ = 0;
    initialized_ = false;
}

std::pair<int, int> RateLimiter::get_config() const {
    std::lock_guard<std::mutex> lock(mutex_);
    return {max_calls_, window_seconds_};
}

// ============================================================================
// SECURE RANDOM IMPLEMENTATION
// ============================================================================

bool SecureRandom::fill_random_bytes(uint8_t* buffer, size_t count) {
#ifdef _WIN32
    // Windows implementation using CryptGenRandom
    HCRYPTPROV hProvider = 0;
    if (!CryptAcquireContextW(&hProvider, NULL, NULL, PROV_RSA_FULL, 
                               CRYPT_VERIFYCONTEXT | CRYPT_SILENT)) {
        return false;
    }
    
    bool success = CryptGenRandom(hProvider, static_cast<DWORD>(count), buffer) != FALSE;
    CryptReleaseContext(hProvider, 0);
    return success;
#else
    // Unix implementation using /dev/urandom
    std::ifstream urandom("/dev/urandom", std::ios::binary);
    if (!urandom) {
        return false;
    }
    
    urandom.read(reinterpret_cast<char*>(buffer), count);
    return urandom.good();
#endif
}

std::vector<uint8_t> SecureRandom::generate_bytes(size_t count) {
    std::vector<uint8_t> bytes(count);
    
    if (!fill_random_bytes(bytes.data(), count)) {
        // Fallback to std::random_device with high-entropy seeding
        std::random_device rd;
        std::mt19937_64 gen(rd() ^ 
            static_cast<uint64_t>(std::chrono::high_resolution_clock::now()
                .time_since_epoch().count()));
        std::uniform_int_distribution<int> dist(0, 255);
        
        for (size_t i = 0; i < count; ++i) {
            bytes[i] = static_cast<uint8_t>(dist(gen));
        }
    }
    
    return bytes;
}

std::string SecureRandom::generate_hex(size_t count) {
    auto bytes = generate_bytes(count);
    
    std::ostringstream oss;
    oss << std::hex << std::setfill('0');
    for (uint8_t byte : bytes) {
        oss << std::setw(2) << static_cast<int>(byte);
    }
    
    return oss.str();
}

int SecureRandom::generate_int(int min, int max) {
    if (min > max) {
        std::swap(min, max);
    }
    
    auto bytes = generate_bytes(sizeof(uint32_t));
    uint32_t value = 0;
    for (size_t i = 0; i < sizeof(uint32_t); ++i) {
        value = (value << 8) | bytes[i];
    }
    
    // Map to range [min, max]
    uint64_t range = static_cast<uint64_t>(max) - min + 1;
    return min + static_cast<int>(value % range);
}

// ============================================================================
// AUDIT LOGGER IMPLEMENTATION
// ============================================================================

AuditLogger& AuditLogger::instance() {
    static AuditLogger instance;
    return instance;
}

AuditLogger::AuditLogger() 
    : max_entries_(1000)
    , enabled_(false) {}

void AuditLogger::configure(bool enabled, size_t max_entries) {
    std::lock_guard<std::mutex> lock(mutex_);
    enabled_ = enabled;
    max_entries_ = max_entries;
}

void AuditLogger::log_event(const std::string& event, const std::string& details) {
    if (!enabled_) {
        return;
    }
    
    std::lock_guard<std::mutex> lock(mutex_);
    
    // Get current timestamp
    auto now = std::chrono::system_clock::now();
    auto time = std::chrono::system_clock::to_time_t(now);
    std::tm tm_buf;
#ifdef _WIN32
    localtime_s(&tm_buf, &time);
#else
    localtime_r(&time, &tm_buf);
#endif
    
    std::ostringstream ts;
    ts << std::put_time(&tm_buf, "%Y-%m-%d %H:%M:%S");
    
    AuditEntry entry;
    entry.timestamp = ts.str();
    entry.event = event;
    entry.details = details;
#ifdef _WIN32
    entry.session_pid = static_cast<int>(GetCurrentProcessId());
#else
    entry.session_pid = static_cast<int>(getpid());
#endif
    
    entries_.push_back(entry);
    
    // Trim if over limit
    if (entries_.size() > max_entries_) {
        entries_.erase(entries_.begin(), 
                      entries_.begin() + (entries_.size() - max_entries_));
    }
}

std::vector<AuditEntry> AuditLogger::get_entries(size_t count) const {
    std::lock_guard<std::mutex> lock(mutex_);
    
    if (count == 0 || count >= entries_.size()) {
        return entries_;
    }
    
    return std::vector<AuditEntry>(entries_.end() - count, entries_.end());
}

void AuditLogger::clear() {
    std::lock_guard<std::mutex> lock(mutex_);
    entries_.clear();
}

bool AuditLogger::is_enabled() const {
    return enabled_;
}

} // namespace security
} // namespace autograder

// ============================================================================
// RCPP EXPORTS
// ============================================================================

#include <Rcpp.h>
using namespace Rcpp;

//' Validate R code for dangerous patterns (C++)
//' 
//' @param code R code as string
//' @param strict If TRUE, check all severity levels
//' @return List with validation results
//' @keywords internal
// [[Rcpp::export(.cpp_validate_code_safety)]]
List cpp_validate_code_safety(const std::string& code, bool strict = true) {
    auto result = autograder::security::CodeValidator::validate(code, strict);
    
    return List::create(
        Named("safe") = result.safe,
        Named("violations") = wrap(result.violations),
        Named("severities") = wrap(result.severities),
        Named("error_message") = result.error_message
    );
}

//' Check if code contains dangerous patterns
//' 
//' @param code R code as string
//' @return TRUE if dangerous patterns found
//' @keywords internal
// [[Rcpp::export(.cpp_contains_dangerous_patterns)]]
LogicalVector cpp_contains_dangerous_patterns(const std::string& code) {
    return LogicalVector::create(
        autograder::security::CodeValidator::contains_dangerous_patterns(code)
    );
}

//' Configure rate limiter (C++)
//' 
//' @param max_calls Maximum calls per window
//' @param window_seconds Window duration
//' @keywords internal
// [[Rcpp::export(.cpp_configure_rate_limit)]]
void cpp_configure_rate_limit(int max_calls, int window_seconds) {
    autograder::security::RateLimiter::instance().configure(max_calls, window_seconds);
}

//' Check rate limit (C++)
//' 
//' @param action Action name for logging
//' @return TRUE if within limits
//' @keywords internal
// [[Rcpp::export(.cpp_check_rate_limit)]]
List cpp_check_rate_limit(const std::string& action = "api_call") {
    auto& limiter = autograder::security::RateLimiter::instance();
    bool within_limits = limiter.check_limit(action);
    auto status = limiter.get_status();
    auto config = limiter.get_config();
    
    return List::create(
        Named("within_limits") = within_limits,
        Named("calls_remaining") = status.first,
        Named("window_remaining") = status.second,
        Named("max_calls") = config.first,
        Named("window_seconds") = config.second
    );
}

//' Reset rate limiter (C++)
//' 
//' @keywords internal
// [[Rcpp::export(.cpp_reset_rate_limit)]]
void cpp_reset_rate_limit() {
    autograder::security::RateLimiter::instance().reset();
}

//' Get rate limit status (C++)
//' 
//' @return List with rate limit status
//' @keywords internal
// [[Rcpp::export(.cpp_rate_limit_status)]]
List cpp_rate_limit_status() {
    auto& limiter = autograder::security::RateLimiter::instance();
    auto status = limiter.get_status();
    auto config = limiter.get_config();
    
    return List::create(
        Named("calls_remaining") = status.first,
        Named("window_remaining_seconds") = status.second,
        Named("limit") = config.first,
        Named("window_seconds") = config.second
    );
}

//' Generate secure random bytes (C++)
//' 
//' @param n Number of bytes to generate
//' @return Raw vector of random bytes
//' @keywords internal
// [[Rcpp::export(.cpp_secure_random_bytes)]]
RawVector cpp_secure_random_bytes(int n) {
    if (n < 1 || n > 4096) {
        stop("n must be between 1 and 4096");
    }
    
    auto bytes = autograder::security::SecureRandom::generate_bytes(static_cast<size_t>(n));
    
    RawVector result(n);
    std::copy(bytes.begin(), bytes.end(), result.begin());
    
    return result;
}

//' Generate secure random hex string (C++)
//' 
//' @param n Number of bytes (result will be 2*n characters)
//' @return Hex-encoded string
//' @keywords internal
// [[Rcpp::export(.cpp_secure_random_hex)]]
CharacterVector cpp_secure_random_hex(int n) {
    if (n < 1 || n > 4096) {
        stop("n must be between 1 and 4096");
    }
    
    return CharacterVector::create(
        autograder::security::SecureRandom::generate_hex(static_cast<size_t>(n))
    );
}

//' Configure audit logging (C++)
//' 
//' @param enabled Enable or disable logging
//' @param max_entries Maximum entries to keep
//' @keywords internal
// [[Rcpp::export(.cpp_configure_audit_logging)]]
void cpp_configure_audit_logging(bool enabled, int max_entries = 1000) {
    autograder::security::AuditLogger::instance().configure(enabled, 
        static_cast<size_t>(max_entries));
}

//' Log security event (C++)
//' 
//' @param event Event type
//' @param details Event details
//' @keywords internal
// [[Rcpp::export(.cpp_log_security_event)]]
void cpp_log_security_event(const std::string& event, const std::string& details = "") {
    autograder::security::AuditLogger::instance().log_event(event, details);
}

//' Get audit log entries (C++)
//' 
//' @param n Number of entries (0 = all)
//' @return Data frame of log entries
//' @keywords internal
// [[Rcpp::export(.cpp_get_audit_log)]]
DataFrame cpp_get_audit_log(int n = 0) {
    auto entries = autograder::security::AuditLogger::instance().get_entries(
        static_cast<size_t>(n));
    
    CharacterVector timestamps(entries.size());
    CharacterVector events(entries.size());
    CharacterVector details(entries.size());
    IntegerVector pids(entries.size());
    
    for (size_t i = 0; i < entries.size(); ++i) {
        timestamps[i] = entries[i].timestamp;
        events[i] = entries[i].event;
        details[i] = entries[i].details;
        pids[i] = entries[i].session_pid;
    }
    
    return DataFrame::create(
        Named("timestamp") = timestamps,
        Named("event") = events,
        Named("details") = details,
        Named("session") = pids,
        Named("stringsAsFactors") = false
    );
}

//' Clear audit log (C++)
//' 
//' @keywords internal
// [[Rcpp::export(.cpp_clear_audit_log)]]
void cpp_clear_audit_log() {
    autograder::security::AuditLogger::instance().clear();
}

//' Check if audit logging is enabled (C++)
//' 
//' @return TRUE if enabled
//' @keywords internal
// [[Rcpp::export(.cpp_audit_logging_enabled)]]
LogicalVector cpp_audit_logging_enabled() {
    return LogicalVector::create(
        autograder::security::AuditLogger::instance().is_enabled()
    );
}
