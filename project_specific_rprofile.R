# project_specific_rprofile.R

# Set CRAN mirror (example)
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Load renv
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# Function to run the Shiny app
run_shiny_app <- function() {
  source("app_shiny.R")
}

# Function to print CLI usage
print_cli_usage <- function() {
  cat("Usage: Rscript app_cli.R --file <filepath> [options]\n")
  cat("Use Rscript app_cli.R --help for more information.\n")
}
