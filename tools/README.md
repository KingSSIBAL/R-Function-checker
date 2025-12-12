# Instructor Tools

Utilities for configuring the autograder with private repositories.

## Structure

```
tools/
├── encrypt_url_helper.R   # Main setup script
├── release.R              # Release automation
├── .env                   # Credentials (gitignored)
└── lib/                   # Helper modules
```

## Quick Start

```r
library(autograder)
autograder_configure(
  url = "https://api.github.com/repos/YourOrg/YourCourse/contents",
  token = "github_pat_xxxx",
  install = TRUE
)
```

Or manually:

```r
source("tools/encrypt_url_helper.R")
test_token()
setup_secure_mode()
```

## Release

```r
source("tools/release.R")
prepare_release("0.5.0")
```
