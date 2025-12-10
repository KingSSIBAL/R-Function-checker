# ============================================================================
# ENVIRONMENT FILE LOADER
# ============================================================================

load_env_file <- function(env_path = NULL) {
    if (is.null(env_path)) {
        possible_paths <- c(
            file.path(getwd(), ".env"),
            file.path(getwd(), "tools", ".env"),
            file.path(dirname(getwd()), ".env"),
            file.path(dirname(getwd()), "tools", ".env")
        )
        
        for (p in possible_paths) {
            if (file.exists(p)) {
                env_path <- p
                break
            }
        }
    }
    
    if (is.null(env_path) || !file.exists(env_path)) {
        return(list())
    }
    
    lines <- readLines(env_path, warn = FALSE)
    env_vars <- list()
    
    for (line in lines) {
        line <- trimws(line)
        if (nchar(line) == 0 || startsWith(line, "#")) next
        
        if (grepl("=", line)) {
            parts <- strsplit(line, "=", fixed = TRUE)[[1]]
            key <- trimws(parts[1])
            value <- trimws(paste(parts[-1], collapse = "="))
            value <- gsub('^["\']|["\']$', '', value)
            env_vars[[key]] <- value
        }
    }
    
    env_vars
}
