// ============================================================================
// AUTOGRADER - FORMATTING MODULE HEADER
// ============================================================================
//
// File: format/formatter.hpp
// Purpose: Output formatting and display utilities
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_FORMATTER_HPP
#define AUTOGRADER_FORMATTER_HPP

#include <Rcpp.h>
#include <string>
#include <vector>
#include "types.h"

namespace autograder {
namespace format {

// ============================================================================
// FORMATTER CLASS
// ============================================================================

/**
 * @class Formatter
 * @brief Output formatting utilities
 * 
 * Provides intelligent formatting for R objects with:
 *   - Smart truncation
 *   - Type-aware display
 *   - Configurable output limits
 */
class Formatter {
public:
    /**
     * @brief Default constructor
     */
    Formatter();
    
    /**
     * @brief Constructor with max length
     * @param max_length Maximum output length
     */
    explicit Formatter(size_t max_length);
    
    /**
     * @brief Set maximum output length
     * @param max_length Maximum length
     */
    void set_max_length(size_t max_length);
    
    /**
     * @brief Get maximum output length
     * @return Maximum length
     */
    size_t get_max_length() const;
    
    /**
     * @brief Format an R object for display
     * @param obj R object to format
     * @return Formatted string
     */
    std::string format(SEXP obj) const;
    
    /**
     * @brief Format a numeric vector
     * @param vec Numeric vector
     * @return Formatted string
     */
    std::string format_numeric(const Rcpp::NumericVector& vec) const;
    
    /**
     * @brief Format an integer vector
     * @param vec Integer vector
     * @return Formatted string
     */
    std::string format_integer(const Rcpp::IntegerVector& vec) const;
    
    /**
     * @brief Format a character vector
     * @param vec Character vector
     * @return Formatted string
     */
    std::string format_character(const Rcpp::CharacterVector& vec) const;
    
    /**
     * @brief Format a logical vector
     * @param vec Logical vector
     * @return Formatted string
     */
    std::string format_logical(const Rcpp::LogicalVector& vec) const;
    
    /**
     * @brief Format a list
     * @param lst List
     * @return Formatted string
     */
    std::string format_list(const Rcpp::List& lst) const;
    
    /**
     * @brief Format a matrix
     * @param mat Matrix
     * @return Formatted string
     */
    std::string format_matrix(const Rcpp::NumericMatrix& mat) const;
    
    /**
     * @brief Format a data frame
     * @param df Data frame
     * @return Formatted string
     */
    std::string format_dataframe(const Rcpp::DataFrame& df) const;
    
    /**
     * @brief Truncate string if too long
     * @param str String to truncate
     * @param max_len Maximum length
     * @return Truncated string with "..." if needed
     */
    static std::string truncate(const std::string& str, size_t max_len);
    
    /**
     * @brief Format a number with specified precision
     * @param value Numeric value
     * @param precision Decimal places
     * @return Formatted string
     */
    static std::string format_number(double value, int precision = 6);

private:
    size_t max_length_;
    static constexpr size_t DEFAULT_MAX_LENGTH = 200;
    static constexpr size_t MAX_VECTOR_ELEMENTS = 10;
    static constexpr size_t MAX_LIST_ELEMENTS = 3;
};

// ============================================================================
// FEEDBACK FORMATTER
// ============================================================================

/**
 * @class FeedbackFormatter
 * @brief Formats student feedback messages
 */
class FeedbackFormatter {
public:
    /**
     * @brief Format type mismatch message
     * @param expected Expected type
     * @param actual Actual type
     * @return Formatted message
     */
    static std::string type_mismatch(const std::string& expected, 
                                     const std::string& actual);
    
    /**
     * @brief Format length mismatch message
     * @param expected Expected length
     * @param actual Actual length
     * @return Formatted message
     */
    static std::string length_mismatch(size_t expected, size_t actual);
    
    /**
     * @brief Format value difference message
     * @param indices Indices where values differ
     * @return Formatted message
     */
    static std::string value_differences(const std::vector<size_t>& indices);
    
    /**
     * @brief Format hint message
     * @param hint The hint text
     * @return Formatted message
     */
    static std::string hint(const std::string& hint_text);
    
    /**
     * @brief Format test result header
     * @param test_num Test number
     * @param description Test description
     * @param points Point value
     * @param passed Whether test passed
     * @return Formatted header
     */
    static std::string test_header(int test_num, const std::string& description,
                                   int points, bool passed);
    
    /**
     * @brief Format summary section
     * @param passed Number passed
     * @param total Total tests
     * @param score Points earned
     * @param max_score Maximum points
     * @return Formatted summary
     */
    static std::string summary(int passed, int total, int score, int max_score);
};

// ============================================================================
// COLOR CODES (for terminal output)
// ============================================================================

namespace colors {
    const std::string RESET   = "\033[0m";
    const std::string RED     = "\033[31m";
    const std::string GREEN   = "\033[32m";
    const std::string YELLOW  = "\033[33m";
    const std::string BLUE    = "\033[34m";
    const std::string MAGENTA = "\033[35m";
    const std::string CYAN    = "\033[36m";
    const std::string BOLD    = "\033[1m";
}

} // namespace format
} // namespace autograder

#endif // AUTOGRADER_FORMATTER_HPP
