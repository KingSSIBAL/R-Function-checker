#include <Rcpp.h>
#include <string>
#include <vector>

using namespace Rcpp;

// ============================================================================
// OBFUSCATION - GitHub URL is split and encoded
// ============================================================================

// Split URL into multiple parts to make it harder to find in binary
// Original: https://raw.githubusercontent.com/KingSSIBAL/R-Function-checker/main/repo
static const std::string URL_PART_1 = "aHR0cHM6Ly9y";           // https://r
static const std::string URL_PART_2 = "YXcuZ2l0aHVi";           // aw.github
static const std::string URL_PART_3 = "dXNlcmNvbnRl";           // userconte
static const std::string URL_PART_4 = "bnQuY29tL0tp";           // nt.com/Ki
static const std::string URL_PART_5 = "bmdTU0lCQUwv";           // ngSSIBAL/
static const std::string URL_PART_6 = "Ui1GdW5jdGlv";           // R-Functio
static const std::string URL_PART_7 = "bi1jaGVja2Vy";           // n-checker
static const std::string URL_PART_8 = "L21haW4vcmVwbw==";       // /main/repo

// Simple base64 decoder
std::string base64_decode(const std::string& encoded) {
  static const std::string base64_chars = 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  
  std::string decoded;
  std::vector<int> T(256, -1);
  for (int i = 0; i < 64; i++) T[base64_chars[i]] = i;
  
  int val = 0, valb = -8;
  for (unsigned char c : encoded) {
    if (T[c] == -1) break;
    val = (val << 6) + T[c];
    valb += 6;
    if (valb >= 0) {
      decoded.push_back(char((val >> valb) & 0xFF));
      valb -= 8;
    }
  }
  return decoded;
}

// Reconstruct URL at runtime
std::string get_github_base() {
  std::string encoded = URL_PART_1 + URL_PART_2 + URL_PART_3 + URL_PART_4 + 
                       URL_PART_5 + URL_PART_6 + URL_PART_7 + URL_PART_8;
  return base64_decode(encoded);
}

// ============================================================================
// SECURE FETCH - Does NOT expose URL to R environment
// ============================================================================

//' Securely fetch function content (URL hidden)
//' @keywords internal
// [[Rcpp::export(".cpp_fetch_function_content")]]
CharacterVector cpp_fetch_function_content(const std::string& function_name) {
  
  // Build URL internally - never exposed to R
  std::string base_url = get_github_base();
  std::string url = base_url + "/functions/" + function_name + ".R";
  
  // Get R functions
  Function download_file("download.file");
  Function readLines("readLines");
  Function tempfile("tempfile");
  
  // Create temp file
  CharacterVector temp = tempfile();
  std::string temp_path = as<std::string>(temp);
  
  // Download with error handling
  try {
    // Try to download
    download_file(url, temp_path, 
                 Named("mode", "w"), 
                 Named("quiet", true));
    
    // Read content
    CharacterVector content = readLines(temp_path);
    
    if (content.size() == 0) {
      stop("Download succeeded but file is empty");
    }
    
    return content;
    
  } catch(std::exception& e) {
    // Sanitized error - don't expose URL
    std::string msg = "Function '";
    msg += function_name;
    msg += "' not found. Please check the function name.";
    stop(msg);
  }
  
  return CharacterVector::create();
}

//' Securely fetch problems list (URL hidden)
//' @keywords internal
// [[Rcpp::export(".cpp_fetch_problems_list")]]
CharacterVector cpp_fetch_problems_list() {
  
  std::string base_url = get_github_base();
  std::string url = base_url + "/functions/_problems.R";
  
  Function download_file("download.file");
  Function readLines("readLines");
  Function tempfile("tempfile");
  
  CharacterVector temp = tempfile();
  std::string temp_path = as<std::string>(temp);
  
  try {
    download_file(url, temp_path, 
                 Named("mode", "w"), 
                 Named("quiet", true));
    
    CharacterVector content = readLines(temp_path);
    return content;
    
  } catch(std::exception& e) {
    // Return empty if problems list doesn't exist - no error exposure
    return CharacterVector::create();
  }
}

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
