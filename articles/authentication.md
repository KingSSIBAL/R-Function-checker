# Authentication and Private Repositories

## Overview

The autograder supports two authentication modes:

| Mode       | Use Case                          | Security Level |
|------------|-----------------------------------|----------------|
| **Legacy** | Open courses, public repositories | Low            |
| **Secure** | Exams, private repositories       | High           |

## Authentication Modes

### Legacy Mode (Default)

Uses public GitHub raw URLs without authentication. Test cases are
publicly accessible but still protected through:

- Hidden test cases (details not shown to students)
- Encrypted URL transmission
- Reference implementation obfuscation

``` r
# Check current mode (internal function)
autograder:::.cpp_get_auth_info()
#> $mode
#> [1] "legacy"
#> $has_token
#> [1] FALSE
```

### Secure Mode

Uses GitHub API with personal access token for private repository
access:

- Test cases stored in private repository
- Token embedded (encrypted) in package
- Full repository access control

``` r
autograder:::.cpp_get_auth_info()
#> $mode
#> [1] "secure"
#> $has_token
#> [1] TRUE
```

## Setting Up Secure Mode

### Step 1: Create a Private Repository

1.  Create a new private repository on GitHub

2.  Add your test case files:

        private-repo/
        ├── functions/
        │   ├── _problems.R
        │   └── your_functions.R
        └── data/
            └── your_data.csv

### Step 2: Generate a GitHub Token

1.  Go to GitHub → Settings → Developer Settings → Personal Access
    Tokens
2.  Click **Generate new token (fine-grained)**
3.  Configure:
    - **Name**: `autograder-course-name`
    - **Expiration**: End of semester
    - **Repository access**: Only select repositories → your private
      repo
    - **Permissions**: Contents → Read-only
4.  Copy the generated token

### Step 3: Configure the Autograder

Create a `.env` file in the `tools/` directory:

``` env
# tools/.env
AUTH_MODE=secure
REPO_URL=https://api.github.com/repos/YourOrg/PrivateRepo/contents
GITHUB_TOKEN=github_pat_xxxxxxxxxxxxxxxxxxxx
```

### Step 4: Build the Package

``` r
source("tools/encrypt_url_helper.R")

# Verify token works
test_token()
#> ✓ Token is valid
#> ✓ Repository accessible
#> ✓ Functions folder found

# Configure secure mode
setup_secure_mode()
#> ✓ Secure mode configured
#> ✓ Token encrypted and embedded
#> ✓ Package ready to build

# Build the package
build_package()
```

### Step 5: Distribute to Students

Distribute the built package (`.tar.gz` or `.zip`). Students install
normally:

``` r
install.packages("autograder_0.4.0.tar.gz", repos = NULL, type = "source")
library(autograder)
autograder("problem_name")
```

The token is embedded and encrypted—students don’t need to configure
anything.

## Security Considerations

### Token Security

- Token is **encrypted** using AES-256 with a derived key
- Each package build generates **unique encryption**
- Token is **never exposed** in plain text
- Students **cannot extract** the original token

### Best Practices

1.  **Use fine-grained tokens** with minimal permissions
2.  **Set expiration dates** aligned with your course
3.  **Rotate tokens** each semester
4.  **Scope to specific repository** only
5.  **Monitor repository access** for suspicious activity

### What Students Can See

| Component                   | Visible to Students? |
|-----------------------------|----------------------|
| Test inputs (visible tests) | ✓ Yes                |
| Test inputs (hidden tests)  | ✗ No                 |
| Expected outputs (visible)  | ✓ Yes                |
| Expected outputs (hidden)   | ✗ No                 |
| Reference implementation    | ✗ No                 |
| GitHub token                | ✗ No                 |
| Repository URL              | ✗ No (encrypted)     |

## Checking Authentication Status

### From R

``` r
library(autograder)

# Get authentication info (internal function)
info <- autograder:::.cpp_get_auth_info()
info$mode      # "legacy" or "secure"
info$has_token # TRUE if token configured

# Get auth mode directly
autograder:::.cpp_get_auth_mode()
#> [1] "secure"
```

### From C++

The package exposes C++ functions for authentication:

``` cpp
// In C++ extensions (internal)
std::string mode = get_auth_mode();     // "legacy" or "secure"
std::string token = get_github_token(); // encrypted token
```

## Troubleshooting

### “Token validation failed”

    Error: GitHub token validation failed

**Solutions:** - Verify token hasn’t expired - Check token has
`Contents: Read` permission - Confirm repository name is correct -
Ensure repository is accessible with the token

### “Repository not found”

    Error: Repository not accessible

**Solutions:** - Verify the REPO_URL format: - For API:
`https://api.github.com/repos/OWNER/REPO/contents` - For raw:
`https://raw.githubusercontent.com/OWNER/REPO/BRANCH/repo` - Check
repository exists and is accessible

### “Function not found” (but exists in repo)

    Error: Function 'problem_name' not found

**Solutions:** - Ensure functions are in `functions/` subfolder - Verify
`_problems.R` lists the function - Check file permissions in repository

## Switching Between Modes

### Switch to Legacy Mode

``` r
source("tools/encrypt_url_helper.R")
setup_legacy_mode()
build_package()
```

### Switch to Secure Mode

``` r
source("tools/encrypt_url_helper.R")

# Create .env file first, then:
setup_secure_mode()
build_package()
```

## Example .env File

``` env
# ============================================================================
# AUTOGRADER CONFIGURATION
# ============================================================================

# Authentication mode: "secure" or "legacy"
AUTH_MODE=secure

# Repository URL
# Secure mode:  https://api.github.com/repos/OWNER/REPO/contents
# Legacy mode:  https://raw.githubusercontent.com/OWNER/REPO/BRANCH/repo
REPO_URL=https://api.github.com/repos/MyOrg/stat101-private/contents

# GitHub Personal Access Token (required for secure mode)
GITHUB_TOKEN=github_pat_11ABCDEFG_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# Optional: Custom encryption key factors (comma-separated)
# Leave empty to use defaults
KEY_FACTORS=
```

## API Reference

### R Functions (Internal)

``` r
# Get current authentication info
info <- autograder:::.cpp_get_auth_info()
# Returns: list with $mode and $has_token

# Get auth mode directly
autograder:::.cpp_get_auth_mode()
# Returns: "legacy" or "secure"
```

### Environment Variables

| Variable       | Required    | Description               |
|----------------|-------------|---------------------------|
| `AUTH_MODE`    | Yes         | `secure` or `legacy`      |
| `REPO_URL`     | Yes         | Full URL to repository    |
| `GITHUB_TOKEN` | Secure only | Personal access token     |
| `KEY_FACTORS`  | No          | Custom encryption factors |
