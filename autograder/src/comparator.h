// ============================================================================
// AUTOGRADER - COMPARISON MODULE HEADER
// ============================================================================
//
// File: comparator.h
// Purpose: High-performance comparison functions for R objects
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_COMPARATOR_H
#define AUTOGRADER_COMPARATOR_H

#include <Rcpp.h>
#include <string>
#include <vector>
#include <cmath>
#include <functional>
#include "types.h"

namespace autograder {
namespace compare {

// ============================================================================
// COMPARATOR CLASS
// ============================================================================

/**
 * @class Comparator
 * @brief High-performance comparison for R objects
 * 
 * Provides optimized comparison functions that are 10-100x faster than
 * R's identical() function for common cases.
 * 
 * Features:
 *   - Early termination on first difference
 *   - Type-specific optimizations
 *   - Floating-point tolerance handling
 *   - NA value handling
 */
class Comparator {
public:
    /**
     * @brief Default constructor
     */
    Comparator();
    
    /**
     * @brief Constructor with tolerance
     * @param tolerance Numeric tolerance for floating-point comparisons
     */
    explicit Comparator(double tolerance);
    
    /**
     * @brief Set comparison tolerance
     * @param tolerance Numeric tolerance
     */
    void set_tolerance(double tolerance);
    
    /**
     * @brief Get current tolerance
     * @return Current tolerance value
     */
    double get_tolerance() const;
    
    /**
     * @brief Compare two R objects
     * @param obj1 First R object
     * @param obj2 Second R object
     * @return ComparisonResult with match status and details
     */
    ComparisonResult compare(SEXP obj1, SEXP obj2) const;
    
    /**
     * @brief Compare two R objects (boolean result)
     * @param obj1 First R object
     * @param obj2 Second R object
     * @return true if objects are equal within tolerance
     */
    bool equal(SEXP obj1, SEXP obj2) const;
    
    /**
     * @brief Compare two numeric vectors
     * @param v1 First vector
     * @param v2 Second vector
     * @return ComparisonResult
     */
    ComparisonResult compare_numeric(const Rcpp::NumericVector& v1,
                                     const Rcpp::NumericVector& v2) const;
    
    /**
     * @brief Compare two integer vectors
     * @param v1 First vector
     * @param v2 Second vector
     * @return ComparisonResult
     */
    ComparisonResult compare_integer(const Rcpp::IntegerVector& v1,
                                     const Rcpp::IntegerVector& v2) const;
    
    /**
     * @brief Compare two character vectors
     * @param v1 First vector
     * @param v2 Second vector
     * @return ComparisonResult
     */
    ComparisonResult compare_character(const Rcpp::CharacterVector& v1,
                                       const Rcpp::CharacterVector& v2) const;
    
    /**
     * @brief Compare two logical vectors
     * @param v1 First vector
     * @param v2 Second vector
     * @return ComparisonResult
     */
    ComparisonResult compare_logical(const Rcpp::LogicalVector& v1,
                                     const Rcpp::LogicalVector& v2) const;
    
    /**
     * @brief Compare two matrices
     * @param m1 First matrix
     * @param m2 Second matrix
     * @return ComparisonResult
     */
    ComparisonResult compare_matrix(const Rcpp::NumericMatrix& m1,
                                    const Rcpp::NumericMatrix& m2) const;
    
    /**
     * @brief Compare two lists
     * @param l1 First list
     * @param l2 Second list
     * @return ComparisonResult
     */
    ComparisonResult compare_list(const Rcpp::List& l1,
                                  const Rcpp::List& l2) const;

private:
    double tolerance_;
    
    // Check if two doubles are equal within tolerance
    bool doubles_equal(double a, double b) const;
    
    // Check if value is NA
    static bool is_na_double(double x);
    static bool is_na_int(int x);
};

// ============================================================================
// COMPARISON UTILITIES
// ============================================================================

/**
 * @brief Get type name of R object
 * @param obj R object
 * @return Type name as string
 */
std::string get_type_name(SEXP obj);

/**
 * @brief Check if two R objects have the same type
 * @param obj1 First object
 * @param obj2 Second object
 * @return true if same type
 */
bool same_type(SEXP obj1, SEXP obj2);

/**
 * @brief Get the size/length of an R object
 * @param obj R object
 * @return Size/length
 */
R_xlen_t get_length(SEXP obj);

/**
 * @brief Find indices where two vectors differ
 * @param v1 First vector
 * @param v2 Second vector
 * @param tolerance Comparison tolerance
 * @param max_diffs Maximum differences to return
 * @return Vector of differing indices (0-based)
 */
std::vector<R_xlen_t> find_differences(const Rcpp::NumericVector& v1,
                                       const Rcpp::NumericVector& v2,
                                       double tolerance = 1e-10,
                                       size_t max_diffs = 10);

} // namespace compare
} // namespace autograder

#endif // AUTOGRADER_COMPARATOR_H
