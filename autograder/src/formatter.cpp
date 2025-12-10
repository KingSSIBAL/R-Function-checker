// ============================================================================
// AUTOGRADER - FORMATTING MODULE IMPLEMENTATION
// ============================================================================
//
// File: format/formatter.cpp
// Purpose: Implementation of output formatting utilities
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#include "formatter.h"
#include <sstream>
#include <iomanip>
#include <algorithm>

namespace autograder {
namespace format {

// ============================================================================
// FORMATTER IMPLEMENTATION
// ============================================================================

Formatter::Formatter() 
    : max_length_(DEFAULT_MAX_LENGTH) {}

Formatter::Formatter(size_t max_length) 
    : max_length_(max_length) {}

void Formatter::set_max_length(size_t max_length) {
    max_length_ = max_length;
}

size_t Formatter::get_max_length() const {
    return max_length_;
}

std::string Formatter::truncate(const std::string& str, size_t max_len) {
    if (str.length() <= max_len) {
        return str;
    }
    return str.substr(0, max_len - 3) + "...";
}

std::string Formatter::format_number(double value, int precision) {
    std::ostringstream ss;
    ss << std::setprecision(precision) << value;
    return ss.str();
}

std::string Formatter::format(SEXP obj) const {
    if (Rf_isNull(obj)) {
        return "NULL";
    }
    
    switch (TYPEOF(obj)) {
        case REALSXP:
            return format_numeric(Rcpp::NumericVector(obj));
        case INTSXP:
            return format_integer(Rcpp::IntegerVector(obj));
        case STRSXP:
            return format_character(Rcpp::CharacterVector(obj));
        case LGLSXP:
            return format_logical(Rcpp::LogicalVector(obj));
        case VECSXP:
            return format_list(Rcpp::List(obj));
        default: {
            // Use R's deparse for other types
            Rcpp::Function deparse("deparse");
            Rcpp::CharacterVector result = deparse(obj, 
                Rcpp::Named("width.cutoff", static_cast<int>(max_length_)));
            std::string output;
            for (int i = 0; i < result.size(); ++i) {
                if (i > 0) output += " ";
                output += Rcpp::as<std::string>(result[i]);
            }
            return truncate(output, max_length_);
        }
    }
}

std::string Formatter::format_numeric(const Rcpp::NumericVector& vec) const {
    if (vec.size() == 0) {
        return "numeric(0)";
    }
    
    std::ostringstream ss;
    
    if (vec.size() <= MAX_VECTOR_ELEMENTS) {
        // Show all elements
        ss << "c(";
        for (R_xlen_t i = 0; i < vec.size(); ++i) {
            if (i > 0) ss << ", ";
            if (Rcpp::NumericVector::is_na(vec[i])) {
                ss << "NA";
            } else {
                ss << format_number(vec[i]);
            }
        }
        ss << ")";
    } else {
        // Show head ... tail pattern
        ss << "numeric[1:" << vec.size() << "] = ";
        for (R_xlen_t i = 0; i < 3; ++i) {
            if (i > 0) ss << ", ";
            if (Rcpp::NumericVector::is_na(vec[i])) {
                ss << "NA";
            } else {
                ss << format_number(vec[i]);
            }
        }
        ss << " ... ";
        ss << "numeric[" << vec.size() << "] = ";
        if (Rcpp::NumericVector::is_na(vec[vec.size() - 1])) {
            ss << "NA";
        } else {
            ss << format_number(vec[vec.size() - 1]);
        }
    }
    
    return truncate(ss.str(), max_length_);
}

std::string Formatter::format_integer(const Rcpp::IntegerVector& vec) const {
    if (vec.size() == 0) {
        return "integer(0)";
    }
    
    std::ostringstream ss;
    
    if (vec.size() <= MAX_VECTOR_ELEMENTS) {
        ss << "c(";
        for (R_xlen_t i = 0; i < vec.size(); ++i) {
            if (i > 0) ss << ", ";
            if (Rcpp::IntegerVector::is_na(vec[i])) {
                ss << "NA";
            } else {
                ss << vec[i] << "L";
            }
        }
        ss << ")";
    } else {
        ss << "integer[1:" << vec.size() << "] = ";
        for (R_xlen_t i = 0; i < 3; ++i) {
            if (i > 0) ss << ", ";
            if (Rcpp::IntegerVector::is_na(vec[i])) {
                ss << "NA";
            } else {
                ss << vec[i];
            }
        }
        ss << " ... ";
        ss << "integer[" << vec.size() << "] = ";
        if (Rcpp::IntegerVector::is_na(vec[vec.size() - 1])) {
            ss << "NA";
        } else {
            ss << vec[vec.size() - 1];
        }
    }
    
    return truncate(ss.str(), max_length_);
}

std::string Formatter::format_character(const Rcpp::CharacterVector& vec) const {
    if (vec.size() == 0) {
        return "character(0)";
    }
    
    std::ostringstream ss;
    
    if (vec.size() <= MAX_VECTOR_ELEMENTS) {
        ss << "c(";
        for (R_xlen_t i = 0; i < vec.size(); ++i) {
            if (i > 0) ss << ", ";
            ss << "\"" << Rcpp::as<std::string>(vec[i]) << "\"";
        }
        ss << ")";
    } else {
        ss << "character[1:" << vec.size() << "] = \"";
        ss << Rcpp::as<std::string>(vec[0]) << "\", ...";
    }
    
    return truncate(ss.str(), max_length_);
}

std::string Formatter::format_logical(const Rcpp::LogicalVector& vec) const {
    if (vec.size() == 0) {
        return "logical(0)";
    }
    
    std::ostringstream ss;
    
    if (vec.size() <= MAX_VECTOR_ELEMENTS) {
        ss << "c(";
        for (R_xlen_t i = 0; i < vec.size(); ++i) {
            if (i > 0) ss << ", ";
            if (Rcpp::LogicalVector::is_na(vec[i])) {
                ss << "NA";
            } else {
                ss << (vec[i] ? "TRUE" : "FALSE");
            }
        }
        ss << ")";
    } else {
        ss << "logical[1:" << vec.size() << "]";
    }
    
    return truncate(ss.str(), max_length_);
}

std::string Formatter::format_list(const Rcpp::List& lst) const {
    if (lst.size() == 0) {
        return "list()";
    }
    
    std::ostringstream ss;
    
    if (lst.size() <= MAX_LIST_ELEMENTS) {
        ss << "list(";
        Rcpp::CharacterVector names = lst.names();
        for (R_xlen_t i = 0; i < lst.size(); ++i) {
            if (i > 0) ss << ", ";
            if (names.size() > i && names[i] != "") {
                ss << Rcpp::as<std::string>(names[i]) << " = ";
            }
            ss << format(lst[i]);
        }
        ss << ")";
    } else {
        ss << "list(length=" << lst.size() << ", first 3: ";
        for (R_xlen_t i = 0; i < 3; ++i) {
            if (i > 0) ss << ", ";
            ss << format(lst[i]);
        }
        ss << ", ...)";
    }
    
    return truncate(ss.str(), max_length_);
}

std::string Formatter::format_matrix(const Rcpp::NumericMatrix& mat) const {
    std::ostringstream ss;
    ss << "matrix[" << mat.nrow() << "x" << mat.ncol() << "]: ";
    
    // Show first few elements
    int count = 0;
    for (int i = 0; i < mat.nrow() && count < 3; ++i) {
        for (int j = 0; j < mat.ncol() && count < 3; ++j) {
            if (count > 0) ss << ", ";
            ss << format_number(mat(i, j));
            ++count;
        }
    }
    ss << " ...";
    
    return truncate(ss.str(), max_length_);
}

std::string Formatter::format_dataframe(const Rcpp::DataFrame& df) const {
    std::ostringstream ss;
    ss << "data.frame[" << df.nrows() << " x " << df.size() << "] columns: ";
    
    Rcpp::CharacterVector names = df.names();
    for (R_xlen_t i = 0; i < names.size() && i < 5; ++i) {
        if (i > 0) ss << ", ";
        ss << Rcpp::as<std::string>(names[i]);
    }
    if (names.size() > 5) {
        ss << ", ...";
    }
    
    return truncate(ss.str(), max_length_);
}

// ============================================================================
// FEEDBACK FORMATTER IMPLEMENTATION
// ============================================================================

std::string FeedbackFormatter::type_mismatch(const std::string& expected, 
                                              const std::string& actual) {
    return "Type mismatch: Expected " + expected + " but got " + actual;
}

std::string FeedbackFormatter::length_mismatch(size_t expected, size_t actual) {
    return "Length mismatch: Expected length " + std::to_string(expected) + 
           " but got " + std::to_string(actual);
}

std::string FeedbackFormatter::value_differences(const std::vector<size_t>& indices) {
    std::ostringstream ss;
    ss << "Differences at positions: ";
    
    for (size_t i = 0; i < indices.size() && i < 5; ++i) {
        if (i > 0) ss << ", ";
        ss << (indices[i] + 1);  // Convert to 1-based indexing
    }
    
    if (indices.size() > 5) {
        ss << " ...";
    }
    
    return ss.str();
}

std::string FeedbackFormatter::hint(const std::string& hint_text) {
    if (hint_text.empty()) {
        return "";
    }
    return "Hint: " + hint_text;
}

std::string FeedbackFormatter::test_header(int test_num, const std::string& description,
                                            int points, bool passed) {
    std::ostringstream ss;
    ss << "[Test " << test_num << "] " << description 
       << " (" << points << " pt): " 
       << (passed ? "PASS" : "FAIL");
    return ss.str();
}

std::string FeedbackFormatter::summary(int passed, int total, int score, int max_score) {
    std::ostringstream ss;
    ss << "\n=== Summary ===\n";
    ss << "Score: " << score << "/" << max_score << " points (";
    ss << std::fixed << std::setprecision(1) 
       << (static_cast<double>(score) / max_score * 100) << "%)\n";
    ss << "Tests: " << passed << "/" << total << " passed (";
    ss << std::fixed << std::setprecision(1)
       << (static_cast<double>(passed) / total * 100) << "%)";
    return ss.str();
}

} // namespace format
} // namespace autograder
