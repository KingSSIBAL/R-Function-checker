// ============================================================================
// AUTOGRADER - COMPARISON MODULE IMPLEMENTATION
// ============================================================================
//
// File: compare/comparator.cpp
// Purpose: Implementation of high-performance comparison functions
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#include "comparator.h"
#include <limits>

namespace autograder {
namespace compare {

// ============================================================================
// COMPARATOR IMPLEMENTATION
// ============================================================================

Comparator::Comparator() 
    : tolerance_(1e-10) {}

Comparator::Comparator(double tolerance) 
    : tolerance_(tolerance) {}

void Comparator::set_tolerance(double tolerance) {
    tolerance_ = tolerance;
}

double Comparator::get_tolerance() const {
    return tolerance_;
}

bool Comparator::doubles_equal(double a, double b) const {
    // Handle NA values
    if (is_na_double(a) && is_na_double(b)) {
        return true;
    }
    if (is_na_double(a) || is_na_double(b)) {
        return false;
    }
    
    // Handle infinity
    if (std::isinf(a) && std::isinf(b)) {
        return (a > 0) == (b > 0);  // Same sign infinity
    }
    
    // Handle NaN
    if (std::isnan(a) && std::isnan(b)) {
        return true;
    }
    if (std::isnan(a) || std::isnan(b)) {
        return false;
    }
    
    // Standard tolerance comparison
    return std::abs(a - b) <= tolerance_;
}

bool Comparator::is_na_double(double x) {
    return Rcpp::NumericVector::is_na(x);
}

bool Comparator::is_na_int(int x) {
    return Rcpp::IntegerVector::is_na(x);
}

ComparisonResult Comparator::compare(SEXP obj1, SEXP obj2) const {
    // Type check first (O(1) operation)
    int type1 = TYPEOF(obj1);
    int type2 = TYPEOF(obj2);
    
    if (type1 != type2) {
        return ComparisonResult(0, "Type mismatch: " + 
                               get_type_name(obj1) + " vs " + 
                               get_type_name(obj2));
    }
    
    // Dispatch based on type
    switch (type1) {
        case REALSXP:
            return compare_numeric(Rcpp::NumericVector(obj1), 
                                  Rcpp::NumericVector(obj2));
        
        case INTSXP:
            return compare_integer(Rcpp::IntegerVector(obj1),
                                  Rcpp::IntegerVector(obj2));
        
        case STRSXP:
            return compare_character(Rcpp::CharacterVector(obj1),
                                    Rcpp::CharacterVector(obj2));
        
        case LGLSXP:
            return compare_logical(Rcpp::LogicalVector(obj1),
                                  Rcpp::LogicalVector(obj2));
        
        case VECSXP:
            return compare_list(Rcpp::List(obj1), Rcpp::List(obj2));
        
        default:
            // Fall back to R's identical for complex types
            Rcpp::Function identical("identical");
            bool result = Rcpp::as<bool>(identical(obj1, obj2));
            if (result) {
                return ComparisonResult(true);
            } else {
                return ComparisonResult(0, "Objects differ (complex type comparison)");
            }
    }
}

bool Comparator::equal(SEXP obj1, SEXP obj2) const {
    return compare(obj1, obj2).equal;
}

ComparisonResult Comparator::compare_numeric(const Rcpp::NumericVector& v1,
                                             const Rcpp::NumericVector& v2) const {
    // Size check
    if (v1.size() != v2.size()) {
        return ComparisonResult(0, "Length mismatch: " + 
                               std::to_string(v1.size()) + " vs " + 
                               std::to_string(v2.size()));
    }
    
    // Element-wise comparison with early termination
    for (R_xlen_t i = 0; i < v1.size(); ++i) {
        if (!doubles_equal(v1[i], v2[i])) {
            std::string desc = "Difference at index " + std::to_string(i + 1) +
                              ": " + std::to_string(v1[i]) + " vs " + 
                              std::to_string(v2[i]);
            return ComparisonResult(static_cast<size_t>(i), desc);
        }
    }
    
    return ComparisonResult(true);
}

ComparisonResult Comparator::compare_integer(const Rcpp::IntegerVector& v1,
                                             const Rcpp::IntegerVector& v2) const {
    // Size check
    if (v1.size() != v2.size()) {
        return ComparisonResult(0, "Length mismatch: " + 
                               std::to_string(v1.size()) + " vs " + 
                               std::to_string(v2.size()));
    }
    
    // Element-wise comparison
    for (R_xlen_t i = 0; i < v1.size(); ++i) {
        // Handle NA values
        bool na1 = is_na_int(v1[i]);
        bool na2 = is_na_int(v2[i]);
        
        if (na1 && na2) continue;
        if (na1 || na2 || v1[i] != v2[i]) {
            std::string desc = "Difference at index " + std::to_string(i + 1);
            return ComparisonResult(static_cast<size_t>(i), desc);
        }
    }
    
    return ComparisonResult(true);
}

ComparisonResult Comparator::compare_character(const Rcpp::CharacterVector& v1,
                                               const Rcpp::CharacterVector& v2) const {
    // Size check
    if (v1.size() != v2.size()) {
        return ComparisonResult(0, "Length mismatch: " + 
                               std::to_string(v1.size()) + " vs " + 
                               std::to_string(v2.size()));
    }
    
    // Element-wise comparison
    for (R_xlen_t i = 0; i < v1.size(); ++i) {
        if (v1[i] != v2[i]) {
            std::string desc = "Difference at index " + std::to_string(i + 1) +
                              ": '" + Rcpp::as<std::string>(v1[i]) + "' vs '" + 
                              Rcpp::as<std::string>(v2[i]) + "'";
            return ComparisonResult(static_cast<size_t>(i), desc);
        }
    }
    
    return ComparisonResult(true);
}

ComparisonResult Comparator::compare_logical(const Rcpp::LogicalVector& v1,
                                             const Rcpp::LogicalVector& v2) const {
    // Size check
    if (v1.size() != v2.size()) {
        return ComparisonResult(0, "Length mismatch: " + 
                               std::to_string(v1.size()) + " vs " + 
                               std::to_string(v2.size()));
    }
    
    // Element-wise comparison
    for (R_xlen_t i = 0; i < v1.size(); ++i) {
        if (v1[i] != v2[i]) {
            std::string desc = "Difference at index " + std::to_string(i + 1);
            return ComparisonResult(static_cast<size_t>(i), desc);
        }
    }
    
    return ComparisonResult(true);
}

ComparisonResult Comparator::compare_matrix(const Rcpp::NumericMatrix& m1,
                                            const Rcpp::NumericMatrix& m2) const {
    // Dimension check
    if (m1.nrow() != m2.nrow() || m1.ncol() != m2.ncol()) {
        return ComparisonResult(0, "Dimension mismatch: [" + 
                               std::to_string(m1.nrow()) + "x" + 
                               std::to_string(m1.ncol()) + "] vs [" +
                               std::to_string(m2.nrow()) + "x" + 
                               std::to_string(m2.ncol()) + "]");
    }
    
    // Element-wise comparison (column-major order)
    for (int j = 0; j < m1.ncol(); ++j) {
        for (int i = 0; i < m1.nrow(); ++i) {
            if (!doubles_equal(m1(i, j), m2(i, j))) {
                std::string desc = "Difference at [" + std::to_string(i + 1) + 
                                  ", " + std::to_string(j + 1) + "]";
                return ComparisonResult(static_cast<size_t>(i * m1.ncol() + j), desc);
            }
        }
    }
    
    return ComparisonResult(true);
}

ComparisonResult Comparator::compare_list(const Rcpp::List& l1,
                                          const Rcpp::List& l2) const {
    // Size check
    if (l1.size() != l2.size()) {
        return ComparisonResult(0, "List length mismatch: " + 
                               std::to_string(l1.size()) + " vs " + 
                               std::to_string(l2.size()));
    }
    
    // Element-wise comparison
    for (R_xlen_t i = 0; i < l1.size(); ++i) {
        ComparisonResult elem_result = compare(l1[i], l2[i]);
        if (!elem_result.equal) {
            return ComparisonResult(static_cast<size_t>(i), 
                                   "Difference in list element " + 
                                   std::to_string(i + 1) + ": " + 
                                   elem_result.diff_description);
        }
    }
    
    return ComparisonResult(true);
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

std::string get_type_name(SEXP obj) {
    switch (TYPEOF(obj)) {
        case NILSXP:    return "NULL";
        case SYMSXP:    return "symbol";
        case LISTSXP:   return "pairlist";
        case CLOSXP:    return "function";
        case ENVSXP:    return "environment";
        case PROMSXP:   return "promise";
        case LANGSXP:   return "language";
        case SPECIALSXP: return "special";
        case BUILTINSXP: return "builtin";
        case CHARSXP:   return "char";
        case LGLSXP:    return "logical";
        case INTSXP:    return "integer";
        case REALSXP:   return "numeric";
        case CPLXSXP:   return "complex";
        case STRSXP:    return "character";
        case DOTSXP:    return "...";
        case ANYSXP:    return "any";
        case VECSXP:    return "list";
        case EXPRSXP:   return "expression";
        case BCODESXP:  return "bytecode";
        case EXTPTRSXP: return "externalptr";
        case WEAKREFSXP: return "weakref";
        case RAWSXP:    return "raw";
        case S4SXP:     return "S4";
        default:        return "unknown";
    }
}

bool same_type(SEXP obj1, SEXP obj2) {
    return TYPEOF(obj1) == TYPEOF(obj2);
}

R_xlen_t get_length(SEXP obj) {
    return Rf_xlength(obj);
}

std::vector<R_xlen_t> find_differences(const Rcpp::NumericVector& v1,
                                       const Rcpp::NumericVector& v2,
                                       double tolerance,
                                       size_t max_diffs) {
    std::vector<R_xlen_t> diffs;
    diffs.reserve(max_diffs);
    
    R_xlen_t len = std::min(v1.size(), v2.size());
    
    for (R_xlen_t i = 0; i < len && diffs.size() < max_diffs; ++i) {
        if (std::abs(v1[i] - v2[i]) > tolerance) {
            diffs.push_back(i);
        }
    }
    
    return diffs;
}

} // namespace compare
} // namespace autograder
