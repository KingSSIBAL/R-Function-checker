#include <Rcpp.h>
#include <string>

using namespace Rcpp;

// ============================================================================
// GITHUB CONFIGURATION - Hidden in compiled binary
// ============================================================================

static const std::string GITHUB_BASE = "https://raw.githubusercontent.com/KingSSIBAL/instructor-repo/main";

// ============================================================================
// GET GITHUB URLS - Hidden in compiled binary
// ============================================================================

//' Get GitHub URL for instructor function (hidden in compiled binary)
//' @keywords internal
// [[Rcpp::export(".cpp_get_function_url")]]
std::string cpp_get_function_url(const std::string& function_name) {
  return GITHUB_BASE + "/functions/" + function_name + ".R";
}

//' Get GitHub URL for test data (hidden in compiled binary)
//' @keywords internal
// [[Rcpp::export(".cpp_get_testdata_url")]]
std::string cpp_get_testdata_url() {
  return GITHUB_BASE + "/test_data.rds";
}

// ============================================================================
// COMPARISON - Hidden in compiled binary
// ============================================================================

//' Compare two outputs for identical match (hidden in compiled binary)
//'
//' @param obj1 First object
//' @param obj2 Second object
//' @return Logical vector. TRUE if identical, FALSE otherwise
//' @keywords internal
// [[Rcpp::export(".cpp_compare_identical")]]
LogicalVector cpp_compare_identical(SEXP obj1, SEXP obj2) {
  Function identical("identical");
  LogicalVector result = LogicalVector::create(as<bool>(identical(obj1, obj2)));
  return result;
}
