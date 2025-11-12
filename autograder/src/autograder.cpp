#include <Rcpp.h>
#include <string>
#include <vector>
#include <curl/curl.h>

using namespace Rcpp;

// ============================================================================
// GITHUB CONFIGURATION - Hidden in compiled binary
// ============================================================================

// These URLs are compiled into binary and NOT visible to students
static const std::string GITHUB_BASE = "https://raw.githubusercontent.com/KingSSIBAL/instructor-repo/main";

// ============================================================================
// CURL CALLBACK
// ============================================================================

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
  userp->append((char*)contents, size * nmemb);
  return size * nmemb;
}

// ============================================================================
// INTERNAL HELPER - Get GitHub URL for function (hidden in binary)
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
// FETCH FROM GITHUB - Hidden in compiled binary
// ============================================================================

//' Fetch file from GitHub (hidden in compiled binary)
//'
//' @param url Character. URL to fetch from
//' @return Raw vector containing file contents
//' @keywords internal
// [[Rcpp::export(".cpp_fetch_from_github")]]
RawVector cpp_fetch_from_github(const std::string& url) {
  CURL* curl = curl_easy_init();
  
  if (!curl) {
    throw std::runtime_error("Failed to initialize CURL");
  }
  
  std::string response;
  struct curl_slist* headers = NULL;
  headers = curl_slist_append(headers, "User-Agent: R-autograder/1.0");
  
  curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
  curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
  curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 1L);
  curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 2L);
  
  CURLcode res = curl_easy_perform(curl);
  
  if (res != CURLE_OK) {
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    std::string error_msg = std::string("CURL error: ") + curl_easy_strerror(res);
    throw std::runtime_error(error_msg);
  }
  
  long http_code = 0;
  curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);
  
  curl_slist_free_all(headers);
  curl_easy_cleanup(curl);
  
  if (http_code != 200) {
    throw std::runtime_error(std::string("HTTP error: ") + std::to_string(http_code));
  }
  
  RawVector result(response.begin(), response.end());
  return result;
}

// ============================================================================
// COMPARISON - Hidden in compiled binary
// ============================================================================

//' Compare two outputs (hidden in compiled binary)
//' @keywords internal
// [[Rcpp::export(".cpp_compare_identical")]]
LogicalVector cpp_compare_identical(SEXP obj1, SEXP obj2) {
  LogicalVector result(1);
  result[0] = Rf_identical(obj1, obj2, FALSE);
  return result;
}
