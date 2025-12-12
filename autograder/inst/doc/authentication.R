## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# # Check current mode (internal function)
# autograder:::.cpp_get_auth_info()
# #> $mode
# #> [1] "secure"
# #> $has_token
# #> [1] TRUE
# #> $url_length
# #> [1] 128
# #> $token_length
# #> [1] 96

## -----------------------------------------------------------------------------
# library(autograder)
# 
# # One-command configuration and build
# autograder_configure(
#   url = \"https://api.github.com/repos/YourOrg/PrivateRepo/contents\",
#   token = \"github_pat_xxxxxxxxxxxxxxxxxxxx\",
#   install = TRUE  # Install after building
# )

## -----------------------------------------------------------------------------
# autograder_setup_wizard()
# # Guides you through each step with validation

## -----------------------------------------------------------------------------
# # Verify token works before building
# autograder_test_token(
#   url = \"https://api.github.com/repos/YourOrg/PrivateRepo/contents\",
#   token = \"github_pat_xxxxxxxxxxxxxxxxxxxx\"
# )
# #> ✓ Token is valid
# #> ✓ Repository accessible

## -----------------------------------------------------------------------------
# # Option A: Use the built-in configure function
# autograder_configure(
#   url = \"https://api.github.com/repos/YourOrg/PrivateRepo/contents\",
#   token = \"github_pat_xxxxxxxxxxxxxxxxxxxx\",
#   install = TRUE
# )
# 
# # Option B: Manual process using the helper script
# source("tools/encrypt_url_helper.R")
# 
# # Verify token works
# test_token()
# #> ✓ Token is valid
# #> ✓ Repository accessible
# #> ✓ Functions folder found
# 
# # Configure secure mode
# setup_secure_mode()
# #> ✓ Secure mode configured
# #> ✓ Token encrypted and embedded
# #> ✓ Package ready to build
# 
# # Build the package
# build_package()

## -----------------------------------------------------------------------------
# install.packages("autograder_0.4.0.tar.gz", repos = NULL, type = "source")
# library(autograder)
# autograder("problem_name")

## -----------------------------------------------------------------------------
# library(autograder)
# 
# # Get authentication info (internal function)
# info <- autograder:::.cpp_get_auth_info()
# info$mode      # Always "secure"
# info$has_token # TRUE if token configured
# 
# # Get auth mode directly
# autograder:::.cpp_get_auth_mode()
# #> [1] "secure"

## -----------------------------------------------------------------------------
# # Get current authentication info
# info <- autograder:::.cpp_get_auth_info()
# # Returns: list with $mode, $has_token, $url_length, $token_length
# 
# # Get auth mode directly
# autograder:::.cpp_get_auth_mode()
# # Returns: "secure"

