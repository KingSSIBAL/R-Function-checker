# ============================================================================
# AUTOGRADER ENCRYPTION HELPER v0.4.0
# ============================================================================
#
# SETUP:
#   1. Copy .env.example to .env
#   2. Fill in your REPO_URL and GITHUB_TOKEN
#   3. source("tools/encrypt_url_helper.R")
#
# ============================================================================
# AVAILABLE COMMANDS
# ============================================================================
#
# test_token()
#   - Tests if your GitHub token can access the private repository
#   - Run this BEFORE encrypting to verify credentials work
#   - Shows HTTP status and helpful error messages if it fails
#
# encrypt_and_update()
#   - Encrypts your URL and token from .env file
#   - Updates the C++ config file in autograder/src/crypto/
#   - After running, rebuild with: devtools::install("autograder")
#
# setup_secure_mode(url, token)
#   - Quick setup for PRIVATE repository
#   - Example:
#       setup_secure_mode(
#         url = "https://api.github.com/repos/YOU/private-repo/contents",
#         token = "github_pat_xxxxx"
#       )
#
# show_help()
#   - Shows current configuration status and available commands
#
# ============================================================================

# Load modules
.dir <- if (basename(getwd()) == "tools") getwd() else file.path(getwd(), "tools")
source(file.path(.dir, "lib", "env_loader.R"))
source(file.path(.dir, "lib", "crypto.R"))
source(file.path(.dir, "lib", "token_test.R"))
source(file.path(.dir, "lib", "generator.R"))
source(file.path(.dir, "lib", "main.R"))

# Show available commands on load
if (interactive()) show_help()
