## nolint: commented_code_linter.
library(here)

# Set up test environment
setup_test_env <- function() {
  # Create required directories if they don't exist
  dirs <- c(
    file.path(here::here(), "R"),
    file.path(here::here(), "inst", "extdata")
  )
  
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  
  # Check if required files exist and create them if needed
  required_files <- c(
    file.path(here::here(), "R", "csv_mapping.R"),
    file.path(here::here(), "R", "data_classes.R"),
    file.path(here::here(), "R", "validation_classes.R"),
    file.path(here::here(), "R", "validation_rules.R")
  )
  
  for (file in required_files) {
    if (!file.exists(file)) {
      file.create(file)
    }
  }
}

# Run setup when helper is loaded
setup_test_env()
