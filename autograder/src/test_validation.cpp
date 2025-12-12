// ============================================================================
// AUTOGRADER - TEST CASE VALIDATION MODULE (C++)
// ============================================================================
//
// File: test_validation.cpp
// Purpose: High-performance test case validation
// 
// STATUS: DEPRECATED - Moved to pure R implementation in R/validation.R
//         This file is kept for reference and potential future use.
//         The R implementation is sufficient for list operations.
//
// Provides fast validation of test case structures with:
//   - Type checking
//   - Length consistency validation
//   - Default value generation
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#include <Rcpp.h>
#include <string>
#include <vector>

using namespace Rcpp;

// ============================================================================
// VALIDATION RESULT STRUCTURE
// ============================================================================

struct TestValidationResult {
    bool valid;
    std::string error_message;
    
    TestValidationResult() : valid(true) {}
    TestValidationResult(const std::string& err) : valid(false), error_message(err) {}
};

// ============================================================================
// VALIDATION FUNCTIONS (DEPRECATED - kept for reference)
// ============================================================================

// NOTE: These functions are no longer exported to R.
// The pure R implementations in R/validation.R are used instead.
// List operations are R's native strength, so there's no performance benefit.

//' Validate test case structure (C++)
//' 
//' @description
//' Performs fast validation of test case structure with comprehensive
//' error checking. Returns validated and normalized test data.
//' 
//' @param test_data List containing test case configuration
//' @param function_name Name of function (for error messages)
//' 
//' @return List with validated test_data and any error messages
//' @keywords internal
// DEPRECATED: Now implemented in R - [[Rcpp::export(.cpp_validate_test_cases)]]
List cpp_validate_test_cases(List test_data, std::string function_name) {
    
    // Result structure
    std::string error_message = "";
    
    // ===== REQUIRED FIELD: inputs =====
    if (!test_data.containsElementNamed("inputs")) {
        return List::create(
            Named("valid") = false,
            Named("error") = "CRITICAL: Test cases for '" + function_name + 
                            "' are missing required 'inputs' field.\n" +
                            "Contact instructor to fix test configuration.",
            Named("data") = R_NilValue
        );
    }
    
    SEXP inputs_sexp = test_data["inputs"];
    if (TYPEOF(inputs_sexp) != VECSXP || Rf_xlength(inputs_sexp) == 0) {
        return List::create(
            Named("valid") = false,
            Named("error") = "CRITICAL: Test cases for '" + function_name + 
                            "' must have at least one test.\n" +
                            "Contact instructor to fix test configuration.",
            Named("data") = R_NilValue
        );
    }
    
    List inputs(inputs_sexp);
    R_xlen_t n_tests = inputs.size();
    
    // ===== OPTIONAL FIELD: descriptions =====
    if (test_data.containsElementNamed("descriptions")) {
        SEXP desc_sexp = test_data["descriptions"];
        if (TYPEOF(desc_sexp) == STRSXP) {
            if (Rf_xlength(desc_sexp) != n_tests) {
                return List::create(
                    Named("valid") = false,
                    Named("error") = "CRITICAL: Test case descriptions length (" + 
                                    std::to_string(Rf_xlength(desc_sexp)) + 
                                    ") doesn't match inputs length (" + 
                                    std::to_string(n_tests) + ").\n" +
                                    "Contact instructor to fix test configuration.",
                    Named("data") = R_NilValue
                );
            }
        }
    } else {
        // Generate default descriptions
        CharacterVector descriptions(n_tests);
        for (R_xlen_t i = 0; i < n_tests; ++i) {
            descriptions[i] = "Test " + std::to_string(i + 1);
        }
        test_data["descriptions"] = descriptions;
    }
    
    // ===== OPTIONAL FIELD: hidden =====
    if (test_data.containsElementNamed("hidden")) {
        SEXP hidden_sexp = test_data["hidden"];
        if (TYPEOF(hidden_sexp) == LGLSXP) {
            if (Rf_xlength(hidden_sexp) != n_tests) {
                return List::create(
                    Named("valid") = false,
                    Named("error") = "CRITICAL: 'hidden' field length (" + 
                                    std::to_string(Rf_xlength(hidden_sexp)) + 
                                    ") doesn't match inputs length (" + 
                                    std::to_string(n_tests) + ").\nContact instructor.",
                    Named("data") = R_NilValue
                );
            }
        } else {
            return List::create(
                Named("valid") = false,
                Named("error") = "CRITICAL: 'hidden' field must be logical (TRUE/FALSE). Contact instructor.",
                Named("data") = R_NilValue
            );
        }
    } else {
        // Default: all tests visible
        LogicalVector hidden(n_tests, false);
        test_data["hidden"] = hidden;
    }
    
    // ===== OPTIONAL FIELD: points =====
    if (test_data.containsElementNamed("points")) {
        SEXP points_sexp = test_data["points"];
        if (TYPEOF(points_sexp) == REALSXP || TYPEOF(points_sexp) == INTSXP) {
            if (Rf_xlength(points_sexp) != n_tests) {
                return List::create(
                    Named("valid") = false,
                    Named("error") = "CRITICAL: 'points' field length (" + 
                                    std::to_string(Rf_xlength(points_sexp)) + 
                                    ") doesn't match inputs length (" + 
                                    std::to_string(n_tests) + ").\nContact instructor.",
                    Named("data") = R_NilValue
                );
            }
            
            // Check for negative values
            NumericVector points(points_sexp);
            for (R_xlen_t i = 0; i < points.size(); ++i) {
                if (points[i] < 0) {
                    return List::create(
                        Named("valid") = false,
                        Named("error") = "CRITICAL: 'points' must be non-negative numeric values. Contact instructor.",
                        Named("data") = R_NilValue
                    );
                }
            }
        } else {
            return List::create(
                Named("valid") = false,
                Named("error") = "CRITICAL: 'points' must be numeric values. Contact instructor.",
                Named("data") = R_NilValue
            );
        }
    } else {
        // Default: all tests worth 1 point
        NumericVector points(n_tests, 1.0);
        test_data["points"] = points;
    }
    
    // ===== OPTIONAL FIELD: tolerance =====
    if (test_data.containsElementNamed("tolerance")) {
        SEXP tol_sexp = test_data["tolerance"];
        if (TYPEOF(tol_sexp) == REALSXP || TYPEOF(tol_sexp) == INTSXP) {
            if (Rf_xlength(tol_sexp) != 1) {
                return List::create(
                    Named("valid") = false,
                    Named("error") = "CRITICAL: 'tolerance' must be a single numeric value. Contact instructor.",
                    Named("data") = R_NilValue
                );
            }
            double tol = Rcpp::as<double>(tol_sexp);
            if (tol < 0) {
                return List::create(
                    Named("valid") = false,
                    Named("error") = "CRITICAL: 'tolerance' must be non-negative. Contact instructor.",
                    Named("data") = R_NilValue
                );
            }
        }
    } else {
        // Default: 1e-10
        test_data["tolerance"] = 1e-10;
    }
    
    // ===== OPTIONAL FIELD: hints =====
    if (test_data.containsElementNamed("hints")) {
        SEXP hints_sexp = test_data["hints"];
        if (TYPEOF(hints_sexp) == STRSXP && Rf_xlength(hints_sexp) != n_tests) {
            // Length mismatch - remove hints with warning (handled in R)
            test_data["hints"] = R_NilValue;
        }
    }
    
    // Return validated data
    return List::create(
        Named("valid") = true,
        Named("error") = "",
        Named("data") = test_data
    );
}

//' Quick validation check for test case inputs
//' 
//' @param test_data List containing test case configuration
//' @return TRUE if basic structure is valid
//' @keywords internal
// DEPRECATED: Now implemented in R - [[Rcpp::export(.cpp_quick_validate_inputs)]]
LogicalVector cpp_quick_validate_inputs(List test_data) {
    // Check inputs exists and is non-empty list
    if (!test_data.containsElementNamed("inputs")) {
        return LogicalVector::create(false);
    }
    
    SEXP inputs_sexp = test_data["inputs"];
    if (TYPEOF(inputs_sexp) != VECSXP || Rf_xlength(inputs_sexp) == 0) {
        return LogicalVector::create(false);
    }
    
    return LogicalVector::create(true);
}

//' Get test count from test data
//' 
//' @param test_data List containing test case configuration
//' @return Number of tests, or -1 if invalid
//' @keywords internal
// DEPRECATED: Now implemented in R - [[Rcpp::export(.cpp_get_test_count)]]
IntegerVector cpp_get_test_count(List test_data) {
    if (!test_data.containsElementNamed("inputs")) {
        return IntegerVector::create(-1);
    }
    
    SEXP inputs_sexp = test_data["inputs"];
    if (TYPEOF(inputs_sexp) != VECSXP) {
        return IntegerVector::create(-1);
    }
    
    return IntegerVector::create(static_cast<int>(Rf_xlength(inputs_sexp)));
}

//' Validate field lengths match test count
//' 
//' @param test_data List containing test case configuration
//' @param n_tests Expected number of tests
//' @return LogicalVector with validation results for each field
//' @keywords internal
// DEPRECATED: Now implemented in R - [[Rcpp::export(.cpp_validate_field_lengths)]]
LogicalVector cpp_validate_field_lengths(List test_data, int n_tests) {
    LogicalVector results = LogicalVector::create(
        Named("descriptions") = true,
        Named("hidden") = true,
        Named("points") = true,
        Named("hints") = true
    );
    
    // Check each field
    if (test_data.containsElementNamed("descriptions")) {
        SEXP desc = test_data["descriptions"];
        results["descriptions"] = (Rf_xlength(desc) == n_tests);
    }
    
    if (test_data.containsElementNamed("hidden")) {
        SEXP hidden = test_data["hidden"];
        results["hidden"] = (Rf_xlength(hidden) == n_tests);
    }
    
    if (test_data.containsElementNamed("points")) {
        SEXP points = test_data["points"];
        results["points"] = (Rf_xlength(points) == n_tests);
    }
    
    if (test_data.containsElementNamed("hints")) {
        SEXP hints = test_data["hints"];
        results["hints"] = (Rf_xlength(hints) == n_tests);
    }
    
    return results;
}
