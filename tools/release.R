# =============================================================================
# AUTOGRADER - RELEASE AUTOMATION SCRIPT
# =============================================================================
#
# File: tools/release.R
# Purpose: Automate the release process for new package versions
#
# Usage:
#   source("tools/release.R")
#   prepare_release("0.5.0")
#
# =============================================================================

#' Prepare a new release
#'
#' @description
#' Automates the release preparation process including:
#' - Version bump in DESCRIPTION
#' - NEWS.md update
#' - Documentation regeneration
#' - Full test suite run
#' - Package build and check
#'
#' @param version New version string (e.g., "0.5.0")
#' @param dry_run If TRUE, show what would be done without making changes
#'
#' @return Invisible TRUE on success
#'
#' @examples
#' \dontrun{
#' prepare_release("0.5.0")
#' prepare_release("0.5.0", dry_run = TRUE)
#' }
prepare_release <- function(version, dry_run = FALSE) {
  # Validate version format
  if (!grepl("^\\d+\\.\\d+\\.\\d+$", version)) {
    stop("Version must be in format X.Y.Z (e.g., 0.5.0)")
  }
  
  pkg_dir <- "autograder"
  
  if (!dir.exists(pkg_dir)) {
    stop("Must be run from repository root (R-Function-checker/)")
  }
  
  cat("=== Preparing release", version, "===\n\n")
  
  # Step 1: Update DESCRIPTION version
  cat("1. Updating DESCRIPTION version...\n")
  desc_file <- file.path(pkg_dir, "DESCRIPTION")
  desc <- readLines(desc_file)
  desc <- gsub("^Version:.*$", paste0("Version: ", version), desc)
  
  if (!dry_run) {
    writeLines(desc, desc_file)
    cat("   Updated to version", version, "\n")
  } else {
    cat("   [DRY RUN] Would update to version", version, "\n")
  }
  
  # Step 2: Update encrypted_config.h with new version
  cat("2. Updating encrypted_config.h key factors...\n")
  config_file <- file.path(pkg_dir, "src", "encrypted_config.h")
  if (file.exists(config_file)) {
    config <- readLines(config_file)
    config <- gsub('v[0-9]+\\.[0-9]+\\.[0-9]+', paste0("v", version), config)
    
    if (!dry_run) {
      writeLines(config, config_file)
      cat("   Updated key factors\n")
    } else {
      cat("   [DRY RUN] Would update key factors\n")
    }
  }
  
  # Step 3: Check NEWS.md has entry for this version
  cat("3. Checking NEWS.md...\n")
  news_file <- file.path(pkg_dir, "NEWS.md")
  if (file.exists(news_file)) {
    news <- readLines(news_file)
    if (!any(grepl(version, news))) {
      warning("NEWS.md does not contain entry for version ", version)
      cat("   WARNING: Add release notes to NEWS.md\n")
    } else {
      cat("   Found entry for version", version, "\n")
    }
  }
  
  # Step 4: Regenerate documentation
  cat("4. Regenerating documentation...\n")
  if (!dry_run) {
    tryCatch({
      devtools::document(pkg_dir)
      cat("   Documentation updated\n")
    }, error = function(e) {
      warning("Documentation generation failed: ", e$message)
    })
  } else {
    cat("   [DRY RUN] Would regenerate documentation\n")
  }
  
  # Step 5: Run tests
  cat("5. Running test suite...\n")
  if (!dry_run) {
    tryCatch({
      result <- devtools::test(pkg_dir)
      if (any(result$failed > 0)) {
        stop("Tests failed. Fix before releasing.")
      }
      cat("   All tests passed\n")
    }, error = function(e) {
      stop("Test suite failed: ", e$message)
    })
  } else {
    cat("   [DRY RUN] Would run tests\n")
  }
  
  # Step 6: Build package
  cat("6. Building package...\n")
  if (!dry_run) {
    tryCatch({
      devtools::build(pkg_dir)
      cat("   Package built successfully\n")
    }, error = function(e) {
      stop("Build failed: ", e$message)
    })
  } else {
    cat("   [DRY RUN] Would build package\n")
  }
  
  # Step 7: R CMD check
  cat("7. Running R CMD check...\n")
  if (!dry_run) {
    tryCatch({
      result <- devtools::check(pkg_dir, cran = TRUE)
      if (length(result$errors) > 0 || length(result$warnings) > 0) {
        warning("R CMD check had warnings/errors. Review before releasing.")
      }
      cat("   R CMD check completed\n")
    }, error = function(e) {
      warning("R CMD check failed: ", e$message)
    })
  } else {
    cat("   [DRY RUN] Would run R CMD check\n")
  }
  
  cat("\n=== Release preparation complete ===\n")
  cat("\nNext steps:\n")
  cat("1. Review changes in git diff\n")
  cat("2. Commit with message: 'Release v", version, "'\n", sep = "")
  cat("3. Create git tag: git tag -a v", version, " -m 'Version ", version, "'\n", sep = "")
  cat("4. Push: git push origin main --tags\n")
  
  invisible(TRUE)
}

#' Update NEWS.md with new version entry
#'
#' @param version Version string
#' @param changes List of changes by category
#'
#' @examples
#' \dontrun{
#' update_news("0.5.0", list(
#'   breaking = c("Removed deprecated grade() function"),
#'   features = c("Added OpenMP parallel processing"),
#'   fixes = c("Fixed memory leak in comparison module")
#' ))
#' }
update_news <- function(version, changes) {
  news_file <- "autograder/NEWS.md"
  
  # Build new entry
  entry <- paste0("# autograder ", version, "\n\n")
  
  if (!is.null(changes$breaking) && length(changes$breaking) > 0) {
    entry <- paste0(entry, "## Breaking Changes\n\n")
    for (change in changes$breaking) {
      entry <- paste0(entry, "- ", change, "\n")
    }
    entry <- paste0(entry, "\n")
  }
  
  if (!is.null(changes$features) && length(changes$features) > 0) {
    entry <- paste0(entry, "## New Features\n\n")
    for (change in changes$features) {
      entry <- paste0(entry, "- ", change, "\n")
    }
    entry <- paste0(entry, "\n")
  }
  
  if (!is.null(changes$fixes) && length(changes$fixes) > 0) {
    entry <- paste0(entry, "## Bug Fixes\n\n")
    for (change in changes$fixes) {
      entry <- paste0(entry, "- ", change, "\n")
    }
    entry <- paste0(entry, "\n")
  }
  
  # Prepend to NEWS.md
  if (file.exists(news_file)) {
    existing <- readLines(news_file)
    writeLines(c(entry, existing), news_file)
  } else {
    writeLines(entry, news_file)
  }
  
  cat("Updated NEWS.md with entry for version", version, "\n")
  invisible(TRUE)
}
