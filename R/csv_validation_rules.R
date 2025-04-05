#' R/csv_validation_rules.R # nolint: commented_code_linter.
#' Common CSV filename patterns
#' This file defines validation rules specific to CSV files, including filename validation,
#' file existence checks, and file structure validation.

# Documentation:
# - R6 Classes: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
# - readr: https://readr.tidyverse.org/

#' Define common filename patterns for CSV files
CSVFilenamePatterns <- list(
  plot_pattern = "^Plot_Template_INFI(\\d{4})\\.csv$",  # Pattern for main plot files
  species_pattern = "^Species_Template_INFI(\\d{4})\\.csv$"  # Pattern for species files
)

#' @title CSVFilenameValidationRule
#' @description Validates the naming conventions of CSV files.
#' Example:
#' ```
#' rule <- CSVFilenameValidationRule$new()
#' errors <- rule$check(data_source)
#' print(errors)
#' ```
CSVFilenameValidationRule <- R6Class("CSVFilenameValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Check the naming conventions of the main and species CSV files.
    #' @param data_source A DataSource object containing the file paths.
    #' @return A data frame of validation errors, if any.
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects

      # Check if filepath is valid
      if (is.null(data_source$filepath) || data_source$filepath == "") {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "Generic", error = "Invalid Filepath",
          message = "Filepath is missing or empty."
        ))
        return(do.call(rbind, lapply(errors, function(e) e$to_dataframe_row())))
      }

      main_file <- basename(data_source$filepath)
      plot_pattern <- CSVFilenamePatterns$plot_pattern

      # Validate main file format
      if (!grepl(paste0("^", plot_pattern, "$"), main_file, ignore.case = TRUE)) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "Generic", error = "Filename Violation",
          message = paste(
            "Invalid main file name format. Expected: Plot_Template_INFIYYYY.csv, Got:", 
            main_file
          )
        ))
      }

      # Extract year from main file
      year <- gsub(plot_pattern, "\\1", main_file)

      # Derive species file name
      expected_species_file <- paste0("Species_Template_INFI", year, ".csv")
      dir_path <- dirname(data_source$filepath)
      if (dir_path == ".") {
        dir_path <- getwd()  # Use the current working directory
      }
      species_path <- file.path(dir_path, expected_species_file)

      # Validate species file format
      if (!file.exists(species_path)) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "Generic", error = "Missing File",
          message = paste(
            "Species file not found or invalid format. Expected:", 
            expected_species_file
          )
        ))
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    }
  )
)

#' @title CSVFileValidationRule
#' @description Validates the existence of required CSV files.
#' Example:
#' ```
#' rule <- CSVFileValidationRule$new()
#' errors <- rule$check(data_source)
#' print(errors)
#' ```
CSVFileValidationRule <- R6Class("CSVFileValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Check the existence of the main and species CSV files.
    #' @param data_source A DataSource object containing the file paths.
    #' @return A data frame of validation errors, if any.
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects

      main_file <- basename(data_source$filepath)
      species_file <- basename(gsub(
        CSVFilenamePatterns$plot_pattern,
        "Species_Template_INFI\\1.csv",
        main_file,
        ignore.case = TRUE
      ))

      # Check if both files exist
      species_path <- file.path(dirname(data_source$filepath), species_file)

      if (!file.exists(species_path)) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "Generic", error = "Missing File",
          message = paste("Missing species CSV file:", species_path)
        ))
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    }
  )
)

#' @title CSVFileStructureValidationRule
#' @description Validates the structure and encoding of CSV files.
#' Example:
#' ```
#' rule <- CSVFileStructureValidationRule$new()
#' errors <- rule$check(data_source)
#' print(errors)
#' ```
CSVFileStructureValidationRule <- R6Class("CSVFileStructureValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Check the structure and encoding of the main and species CSV files.
    #' @param data_source A DataSource object containing the file paths.
    #' @return A data frame of validation errors, if any.
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects

      # Only run this validation for CSV data sources
      if (data_source$file_type != "csv") {
        return(do.call(rbind, lapply(errors, function(e) e$to_dataframe_row())))
      }

      # Get filenames using patterns
      main_file <- basename(data_source$filepath)
      species_file <- basename(gsub(
        CSVFilenamePatterns$plot_pattern,
        "Species_Template_INFI\\1.csv",
        main_file,
        ignore.case = TRUE
      ))

      # Check if both files exist with correct structure
      species_path <- file.path(dirname(data_source$filepath), species_file)

      if (!file.exists(species_path)) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "File Type Violation", error = "Missing File",
          message = paste("Species data file not found:", species_path)
        ))
      }

      # Check if the file is in UTF-8 encoding using readr's guess_encoding
      tryCatch({
        file_encoding <- readr::guess_encoding(data_source$filepath)
        if (nrow(file_encoding) > 0) {
          top_encoding <- file_encoding$encoding[1]
          if (tolower(top_encoding) != "utf-8" && tolower(top_encoding) != "ascii") {
            errors <- append(errors, ValidationError$new(
              source = "FileSystem", row = NA, column = NA,
              error_code = 1, type = "Generic", error = "Encoding Issue",
              message = paste0(
                "CSV file is not in UTF-8 encoding. Detected: ", 
                top_encoding
              )
            ))
          }
        }
      }, error = function(e) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "Generic", error = "Encoding Detection Error",
          message = paste("Could not determine file encoding:", e$message)
        ))
      })

      # Check if the CSV uses ',' as column separator and '.' for decimals
      csv_content <- readLines(data_source$filepath, n = 1)
      if (!grepl(",", csv_content)) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "File Type Violation", error = "Invalid Separator",
          message = "CSV file does not use ',' as column separator."
        ))
      }

      sample_data <- read.csv(data_source$filepath, nrows = 10, sep = ",", dec = ".")
      if (any(is.na(as.numeric(gsub(",", "", unlist(sample_data), fixed = TRUE))))) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "File Type Violation", error = "Invalid Decimal Format",
          message = "CSV file does not use '.' as decimal separator."
        ))
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    }
  )
)
