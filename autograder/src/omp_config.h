// ============================================================================
// AUTOGRADER - OPENMP CONFIGURATION
// ============================================================================
//
// File: omp_config.h
// Purpose: OpenMP configuration and portability macros
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_OMP_CONFIG_H
#define AUTOGRADER_OMP_CONFIG_H

#include <Rcpp.h>  // For R_xlen_t
#include <string>
#include <algorithm>  // For std::min
#include <cstring>    // For memset

// Include OpenMP if available
#ifdef _OPENMP
    #include <omp.h>
    #define AUTOGRADER_PARALLEL 1
#else
    #define AUTOGRADER_PARALLEL 0
    // Provide stub definitions for systems without OpenMP
    inline int omp_get_max_threads() { return 1; }
    inline int omp_get_thread_num() { return 0; }
    inline int omp_get_num_threads() { return 1; }
    inline void omp_set_num_threads(int) {}
#endif

namespace autograder {

// ============================================================================
// SECURE MEMORY UTILITIES
// ============================================================================
namespace security {

/**
 * @brief Securely clear sensitive data from memory
 * 
 * Uses volatile pointer to prevent compiler optimization from
 * removing the clearing operation.
 * 
 * @param data Pointer to data to clear
 * @param size Size in bytes to clear
 */
inline void secure_clear(void* data, size_t size) {
    if (data != nullptr && size > 0) {
        volatile unsigned char* p = static_cast<volatile unsigned char*>(data);
        for (size_t i = 0; i < size; ++i) {
            p[i] = 0;
        }
    }
}

/**
 * @brief Securely clear a string
 * 
 * @param s String to clear
 */
inline void secure_clear_string(std::string& s) {
    if (!s.empty()) {
        secure_clear(&s[0], s.size());
        s.clear();
    }
}

/**
 * @brief RAII wrapper for secure string clearing
 * 
 * Automatically clears the string when destroyed.
 */
class SecureString {
public:
    SecureString() = default;
    explicit SecureString(const std::string& s) : data_(s) {}
    explicit SecureString(std::string&& s) : data_(std::move(s)) {}
    
    ~SecureString() {
        secure_clear_string(data_);
    }
    
    // Prevent copying (security measure)
    SecureString(const SecureString&) = delete;
    SecureString& operator=(const SecureString&) = delete;
    
    // Allow moving
    SecureString(SecureString&& other) noexcept : data_(std::move(other.data_)) {
        other.data_.clear();
    }
    
    SecureString& operator=(SecureString&& other) noexcept {
        if (this != &other) {
            secure_clear_string(data_);
            data_ = std::move(other.data_);
            other.data_.clear();
        }
        return *this;
    }
    
    const std::string& get() const { return data_; }
    std::string& get() { return data_; }
    size_t size() const { return data_.size(); }
    bool empty() const { return data_.empty(); }
    
private:
    std::string data_;
};

} // namespace security

// ============================================================================
// OPENMP PARALLEL CONFIGURATION
// ============================================================================
namespace parallel {

// Maximum threads to use (prevent oversubscription)
constexpr int MAX_THREADS = 8;

// Minimum elements per thread for efficiency
constexpr R_xlen_t MIN_ELEMENTS_PER_THREAD = 2500;

/**
 * @brief Get dynamically computed parallel threshold
 * 
 * Adjusts threshold based on available cores for optimal performance.
 * More cores = lower threshold is beneficial.
 * 
 * @return Optimal minimum vector size for parallelization
 */
inline R_xlen_t get_parallel_threshold() {
#ifdef _OPENMP
    static R_xlen_t cached_threshold = 0;
    if (cached_threshold == 0) {
        int cores = omp_get_max_threads();
        // Base threshold of 10000, reduced for more cores
        // Formula: max(5000, 50000 / cores)
        // 4 cores -> 12500, 8 cores -> 6250, 16 cores -> 5000
        cached_threshold = std::max(static_cast<R_xlen_t>(5000), 
                                    static_cast<R_xlen_t>(50000 / cores));
    }
    return cached_threshold;
#else
    return 10000;  // Default for non-OpenMP builds
#endif
}

// Keep PARALLEL_THRESHOLD for backward compatibility
constexpr R_xlen_t PARALLEL_THRESHOLD = 10000;

/**
 * @brief Get optimal thread count for given workload
 * 
 * @param n Size of data to process
 * @return Number of threads to use
 */
inline int get_num_threads(R_xlen_t n) {
#ifdef _OPENMP
    R_xlen_t threshold = get_parallel_threshold();
    if (n < threshold) {
        return 1;  // Too small for parallelization
    }
    int available = omp_get_max_threads();
    int optimal = std::min(available, MAX_THREADS);
    
    // Ensure we have enough work per thread
    int max_useful = static_cast<int>(n / MIN_ELEMENTS_PER_THREAD);
    return std::max(1, std::min(optimal, max_useful));
#else
    return 1;
#endif
}

/**
 * @brief Check if OpenMP is available
 * 
 * @return true if OpenMP support is compiled in
 */
inline bool is_available() {
#ifdef _OPENMP
    return true;
#else
    return false;
#endif
}

/**
 * @brief Get OpenMP version string
 * 
 * @return OpenMP version or "not available"
 */
inline std::string version() {
#ifdef _OPENMP
    int version = _OPENMP;
    // Format: YYYYMM
    int year = version / 100;
    int month = version % 100;
    return std::to_string(year) + "." + std::to_string(month);
#else
    return "not available";
#endif
}

} // namespace parallel
} // namespace autograder

#endif // AUTOGRADER_OMP_CONFIG_H
