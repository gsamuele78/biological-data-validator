#' Common CSV filename patterns
CSVFilenamePatterns <- list(
  plot_pattern = "^Plot_Template_INFI(\\d{4})\\.csv$",
  species_pattern = "^Species_Template_INFI(\\d{4})\\.csv$"
)

#' CSV filename validation rule
CSVFilenameValidationRule <- R6Class("CSVFilenameValidationRule",
  inherit = ValidationRule,
  public = list(
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects
      
      main_file <- basename(data_source$filepath)
      plot_pattern <- CSVFilenamePatterns$plot_pattern
      
      # Validate main file format
      if (!grepl(paste0("^", plot_pattern, "$"), main_file, ignore.case = TRUE)) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "Generic", error = "Filename Violation",
          message = paste("Invalid main file name format. Expected: Plot_Template_INFIYYYY.csv, Got:", main_file)
        ))
      }
      
      # Extract year from main file
      year <- gsub(plot_pattern, "\\1", main_file)
      
      # Derive species file name
      expected_species_file <- paste0("Species_Template_INFI", year, ".csv")
      species_path <- file.path(dirname(data_source$filepath), expected_species_file)
      
      # Validate species file format
      if (!file.exists(species_path)) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "Generic", error = "Missing File",
          message = paste("Species file not found or invalid format. Expected:", expected_species_file)
        ))
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    }
  )
)

#' CSV-specific validation rule
CSVFileValidationRule <- R6Class("CSVFileValidationRule",
  inherit = ValidationRule,
  public = list(
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

#' CSV-specific structure validation rule
CSVFileStructureValidationRule <- R6Class("CSVFileStructureValidationRule",
  inherit = ValidationRule,
  public = list(
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
      
      # Check if the file is in UTF-8 encoding
      if (!isTRUE(all.equal(fileEncoding(data_source$filepath), "UTF-8"))) {
        errors <- append(errors, ValidationError$new(
          source = "FileSystem", row = NA, column = NA,
          error_code = 1, type = "Generic", error = "Encoding Issue",
          message = "CSV file is not in UTF-8 encoding."
        ))
      }

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
