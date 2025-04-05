# project_specific_rprofile.R

# Set CRAN mirror (example)
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Load renv
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# Function to check and update renv status
# Documentation: https://rstudio.github.io/renv/articles/renv.html
# Example: To manually check the status of the environment, run `renv::status()` in the R console.
check_and_update_renv <- function() {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cran.rstudio.com/")
  }
  library(renv)
  
  # Check the status of renv
  status <- renv::status()
  if (!status$synchronized) {
    message("renv is out of sync. Restoring the environment...")
    renv::restore(prompt = FALSE)  # Restores the environment to match the renv.lock file
    message("Environment restored.")
  } else {
    message("renv is synchronized.")
  }
}

# Call the function to check and update renv
check_and_update_renv()

# Function to run the Shiny app
# Example: To run the Shiny app, call `run_shiny_app()` in the R console.
run_shiny_app <- function() {
  source("app_shiny.R")
}

# Function to print CLI usage
# Example: To see CLI usage, call `print_cli_usage()` in the R console.
print_cli_usage <- function() {
  cat("Usage: Rscript app_cli.R --file <filepath> [options]\n")
  cat("Use Rscript app_cli.R --help for more information.\n")
}
