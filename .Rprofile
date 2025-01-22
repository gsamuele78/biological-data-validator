# .Rprofile

# Check if the project-specific .Rprofile exists
if (file.exists("project_specific_rprofile.R")) {
  # Source the project-specific .Rprofile
  source("project_specific_rprofile.R")

  # Function to check if a variable is already defined
  is_defined <- function(variable_name) {
    exists(variable_name, envir = .GlobalEnv)
  }

  # Get the content of the project-specific .Rprofile
  project_rprofile_content <- readLines("project_specific_rprofile.R")

  # Iterate over each line in the project-specific .Rprofile
  for (line in project_rprofile_content) {
    # Check if the line defines a variable (simple check for assignment)
    if (grepl("<-", line)) {
      # Extract the variable name
      var_name <- trimws(strsplit(line, "<-")[[1]][1])

      # Check if the variable is already defined
      if (!is_defined(var_name)) {
        # If not defined, evaluate the line (execute the definition)
        eval(parse(text = line), envir = .GlobalEnv)
      }
    }
  }

  # Print a message indicating that the project-specific .Rprofile was processed
  message("Loaded project-specific R settings from project_specific_rprofile.R")
}
