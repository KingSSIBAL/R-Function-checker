# Instructor Tools

This directory contains utilities for instructors to configure and customize the autograder.

## 📁 Structure

```
tools/
├── README.md              # This file
├── encrypt_url_helper.R   # Main setup script
├── .env                   # Credentials (gitignored, create locally)
├── .env.example           # Template for .env file
└── lib/                   # Modular helper functions
    ├── env_loader.R       # .env file parser
    ├── crypto.R           # Encryption utilities
    ├── token_test.R       # GitHub token testing
    ├── generator.R        # Config file generator
    └── main.R             # Main configuration functions
```

## 🚀 Quick Start

### Option 1: Public Repository (Legacy Mode)

For open courses where test cases can be public:

```r
source("tools/encrypt_url_helper.R")
setup_legacy_mode()
```

### Option 2: Private Repository (Secure Mode)

For exams or when test cases must be protected:

1. **Create a GitHub Personal Access Token:**
   - Go to GitHub → Settings → Developer Settings → Personal Access Tokens
   - Create a fine-grained token with `Contents: Read-only` permission
   - Scope it to your private repository

2. **Create `.env` file:**
```env
AUTH_MODE=secure
BASE_URL=https://raw.githubusercontent.com/YourOrg/PrivateRepo/main
GITHUB_TOKEN=github_pat_xxxxxxxxxxxx
```

3. **Run setup:**
```r
source("tools/encrypt_url_helper.R")
test_token()       # Verify token works
setup_secure_mode()
```

4. **Build and distribute:**
```bash
R CMD build autograder
R CMD INSTALL autograder_*.tar.gz
```

## 📋 Available Functions

### Main Functions

| Function | Description |
|----------|-------------|
| `setup_legacy_mode()` | Configure for public repository |
| `setup_secure_mode()` | Configure for private repository with token |
| `test_token()` | Verify GitHub token works |
| `encrypt_and_update()` | Manually encrypt URL and update config |

### Helper Functions

| Function | File | Description |
|----------|------|-------------|
| `load_env()` | `lib/env_loader.R` | Load credentials from .env |
| `encrypt_url()` | `lib/crypto.R` | Encrypt repository URL |
| `test_github_token()` | `lib/token_test.R` | Test token against API |
| `generate_config()` | `lib/generator.R` | Generate C++ config file |

## 🔐 Security

### .env File

The `.env` file contains sensitive credentials and is **gitignored** by default.

```env
# Authentication mode: "legacy" or "secure"
AUTH_MODE=secure

# Base URL for raw file access
BASE_URL=https://raw.githubusercontent.com/YourOrg/Repo/main

# GitHub Personal Access Token (for secure mode)
GITHUB_TOKEN=github_pat_xxxxxxxxxxxx
```

### Token Requirements

For private repository access, create a **fine-grained** personal access token:

1. Go to: GitHub → Settings → Developer Settings → Personal Access Tokens → Fine-grained tokens
2. Set expiration (recommend: 1 semester/year)
3. Repository access: Select your private repo
4. Permissions: Contents → Read-only
5. Generate and copy token

### Token Security Tips

- ✅ Use fine-grained tokens (not classic)
- ✅ Limit scope to specific repository
- ✅ Set reasonable expiration
- ✅ Keep `.env` in gitignore
- ❌ Never commit tokens to git
- ❌ Never share tokens with students

## 📝 Repository Structure

Your test case repository should have this structure:

```
your-repo/
├── functions/
│   ├── _problems.R          # List of available problems
│   ├── factorial.R          # Test cases
│   ├── fibonacci.R          # Test cases
│   ├── analyze_data.R       # Test cases with data files
│   └── your_function.R      # Your custom test cases
└── data/
    ├── sample.csv           # Data files for tests
    ├── data.xlsx            # Excel files
    └── data.rds             # R data objects
```

## 🔧 Troubleshooting

### Token Test Fails

```r
test_token()
# Error: 401 Unauthorized
```

**Solutions:**
- Verify token hasn't expired
- Check repository access permissions
- Ensure `Contents: Read-only` permission is set

### URL Not Found (404)

**Solutions:**
- Verify BASE_URL format: `https://raw.githubusercontent.com/OWNER/REPO/BRANCH`
- Check repository exists and is accessible
- Verify file path structure matches expected layout

## 📦 Workflow

```
┌─────────────────┐
│  Create .env    │
│  (credentials)  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  test_token()   │
│  (verify works) │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ setup_*_mode()  │
│ (generate conf) │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  R CMD build    │
│  (create pkg)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Distribute to  │
│    students     │
└─────────────────┘
```

## 📞 Support

- [Instructor Guide](../docs/instructor-guide.md)
- [Issues](https://github.com/KingSSIBAL/R-Function-checker/issues)
- Contact: rcagub@up.edu.ph
