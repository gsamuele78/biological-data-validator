library(R6)

# Source the ValidationError class
source("R/validation_error.R")

#' Base class for validation rules
ValidationRule <- R6Class("ValidationRule",
  public = list(
    check = function(data_source) {
      stop("Subclasses must implement the 'check' method.")
    },
    get_error_level = function() {
      "Error"  # Default error level
    }
  )
)

#' Data type validation rule
DataTypeValidationRule <- R6Class("DataTypeValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Check the data source for data type errors
    #' @param data_source A DataSource object
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects

      # Sheet 1 validation
      for (i in seq_along(data_source$sheet1_data)) {
        data_row <- data_source$sheet1_data[[i]]

        if (!is.character(data_row$Plot.code)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Plot.code",
            error_code = 1, type = "Generic", error = "Data Type Violation - Plot.code",
            message = "Plot.code should be alphanumeric."
          ))
        }
        if (!is.numeric(data_row$SU) || data_row$SU < 1 || data_row$SU > 4) {
          error_type <- if (!is.numeric(data_row$SU)) "Data Type Violation - SU" else "Out of range - SU"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "SU",
            error_code = 1, type = "Generic", error = error_type,
            message = "SU should be a number between 1 and 4."
          ))
        }
        if (!inherits(data_row$Sample.date, "Date") || 
            !grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(data_row$Sample.date))) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Sample.date",
            error_code = 1, type = "Generic", error = "Data Type Violation - Sample.date",
            message = "Sample.date should be a valid date in the format YYYY-MM-DD."
          ))
        }
        if (!is.character(data_row$Detector)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Detector",
            error_code = 1, type = "Generic", error = "Data Type Violation - Detector",
            message = "Detector should be a string."
          ))
        }
        if (!is.numeric(data_row$X)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "X",
            error_code = 1, type = "Generic", error = "Data Type Violation - X",
            message = "X should be numeric (longitude)."
          ))
        }
        if (!is.numeric(data_row$Y)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Y",
            error_code = 1, type = "Generic", error = "Data Type Violation - Y",
            message = "Y should be numeric (latitude)."
          ))
        }
        if (!is.character(data_row$Region)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Region",
            error_code = 1, type = "Generic", error = "Data Type Violation - Region",
            message = "Region should be a string."
          ))
        }
        if (!is.numeric(data_row$Elevation) || data_row$Elevation < 0 || data_row$Elevation > 6000) {
          error_type <- if (!is.numeric(data_row$Elevation)) "Data Type Violation - Elevation" else "Out of range - Elevation"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Elevation",
            error_code = 1, type = "Generic", error = error_type,
            message = "Elevation should be numeric (0-6000 meters)."
          ))
        }
        if (!is.numeric(data_row$Aspect) || data_row$Aspect < 0 || data_row$Aspect > 360) {
          error_type <- if (!is.numeric(data_row$Aspect)) "Data Type Violation - Aspect" else "Out of range - Aspect"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Aspect",
            error_code = 1, type = "Generic", error = error_type,
            message = "Aspect should be numeric (0-360 degrees)."
          ))
        }
        if (!is.numeric(data_row$Slope) || data_row$Slope < 0 || data_row$Slope > 90) {
          error_type <- if (!is.numeric(data_row$Slope)) "Data Type Violation - Slope" else "Out of range - Slope"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Slope",
            error_code = 1, type = "Generic", error = error_type,
            message = "Slope should be numeric (0-90 degrees)."
          ))
        }
        if (!is.numeric(data_row$Cop.tot) || data_row$Cop.tot < 0 || data_row$Cop.tot > 100) {
          error_type <- if (!is.numeric(data_row$Cop.tot)) "Data Type Violation - Cop.tot" else "Out of range - Cop.tot"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Cop.tot",
            error_code = 1, type = "Generic", error = error_type,
            message = "Cop.tot should be numeric (0-100%)."
          ))
        }
        if (!is.numeric(data_row$Tree.cov) || data_row$Tree.cov < 0 || data_row$Tree.cov > 100) {
          error_type <- if (!is.numeric(data_row$Tree.cov)) "Data Type Violation - Tree.cov" else "Out of range - Tree.cov"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Tree.cov",
            error_code = 1, type = "Generic", error = error_type,
            message = "Tree.cov should be numeric (0-100%)."
          ))
        }
        if (!is.numeric(data_row$Shrub.cov) || data_row$Shrub.cov < 0 || data_row$Shrub.cov > 100) {
          error_type <- if (!is.numeric(data_row$Shrub.cov)) "Data Type Violation - Shrub.cov" else "Out of range - Shrub.cov"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Shrub.cov",
            error_code = 1, type = "Generic", error = error_type,
            message = "Shrub.cov should be numeric (0-100%)."
          ))
        }
        if (!is.numeric(data_row$Herb.cov) || data_row$Herb.cov < 0 || data_row$Herb.cov > 100) {
          error_type <- if (!is.numeric(data_row$Herb.cov)) "Data Type Violation - Herb.cov" else "Out of range - Herb.cov"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Herb.cov",
            error_code = 1, type = "Generic", error = error_type,
            message = "Herb.cov should be numeric (0-100%)."
          ))
        }
        if (!is.numeric(data_row$Brioph.cov) || data_row$Brioph.cov < 0 || data_row$Brioph.cov > 100) {
          error_type <- if (!is.numeric(data_row$Brioph.cov)) "Data Type Violation - Brioph.cov" else "Out of range - Brioph.cov"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Brioph.cov",
            error_code = 1, type = "Generic", error = error_type,
            message = "Brioph.cov should be numeric (0-100%)."
          ))
        }
        if (!is.numeric(data_row$Bare.soil.cov) || data_row$Bare.soil.cov < 0 || data_row$Bare.soil.cov > 100) {
          error_type <- if (!is.numeric(data_row$Bare.soil.cov)) "Data Type Violation - Bare.soil.cov" else "Out of range - Bare.soil.cov"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Bare.soil.cov",
            error_code = 1, type = "Generic", error = error_type,
            message = "Bare.soil.cov should be numeric (0-100%)."
          ))
        }
        if (!is.numeric(data_row$Litter.cov) || data_row$Litter.cov < 0 || data_row$Litter.cov > 100) {
          error_type <- if (!is.numeric(data_row$Litter.cov)) "Data Type Violation - Litter.cov" else "Out of range - Litter.cov"
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "Litter.cov",
            error_code = 1, type = "Generic", error = error_type,
            message = "Litter.cov should be numeric (0-100%)."
          ))
        }
        if (!is.character(data_row$notes)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = i, column = "notes",
            error_code = 1, type = "Generic", error = "Data Type Violation - notes",
            message = "notes should be a string."
          ))
        }
      }

      # Sheet 2 validation
      for (i in seq_along(data_source$sheet2_data)) {
        data_row <- data_source$sheet2_data[[i]]

        if (!is.character(data_row$Plot.code)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet2", row = i, column = "Plot.code",
            error_code = 1, type = "Generic", error = "Data Type Violation - Plot.code",
            message = "Plot.code should be alphanumeric."
          ))
        }
        if (!is.numeric(data_row$Subplot) || data_row$Subplot < 1 || data_row$Subplot > 4) {
          error_type <- if (!is.numeric(data_row$Subplot)) "Data Type Violation - Subplot" else "Out of range - Subplot"
          errors <- append(errors, ValidationError$new(
            source = "Sheet2", row = i, column = "Subplot",
            error_code = 1, type = "Generic", error = error_type,
            message = "Subplot should be a number between 1 and 4."
          ))
        }
        if (!is.character(data_row$Layer) || !data_row$Layer %in% c("T", "S", "H")) {
          error_type <- if (!is.character(data_row$Layer)) "Data Type Violation - Layer" else "Out of range - Layer"
          errors <- append(errors, ValidationError$new(
            source = "Sheet2", row = i, column = "Layer",
            error_code = 1, type = "Generic", error = error_type,
            message = "Layer should be one of: T, S, H."
          ))
        }
        if (!is.character(data_row$Species)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet2", row = i, column = "Species",
            error_code = 1, type = "Generic", error = "Data Type Violation - Species",
            message = "Species should be a string."
          ))
        }
        if (!is.numeric(data_row$cover) || data_row$cover < 0 || data_row$cover > 100) {
          error_type <- if (!is.numeric(data_row$cover)) "Data Type Violation - cover" else "Out of range - cover"
          errors <- append(errors, ValidationError$new(
            source = "Sheet2", row = i, column = "cover",
            error_code = 1, type = "Generic", error = error_type,
            message = "cover should be numeric (0-100%)."
          ))
        }
        if (!is.character(data_row$Notes)) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet2", row = i, column = "Notes",
            error_code = 1, type = "Generic", error = "Data Type Violation - Notes",
            message = "Notes should be a string."
          ))
        }
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    },
    #' @description
    #' Get the error level for this rule
    get_error_level = function() {
      "Warning"  # Data type issues are often warnings
    }
  )
)

#' Maximum rows validation rule
MaxRowsValidationRule <- R6Class("MaxRowsValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Check the data source for maximum rows errors
    #' @param data_source A DataSource object
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects

      # Rule 1: Maximum 4 rows with same Plot.code and SU values 1,2,3,4
      plot_counts <- table(sapply(data_source$sheet1_data, function(x) x$Plot.code))
      if (any(plot_counts > 4)) {
        for (plot in names(plot_counts[plot_counts > 4])) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = NA, column = "Plot.code",
            error_code = 1, type = "Generic", error = "Max Rows Violation",
            message = paste("More than 4 rows with Plot.code:", plot)
          ))
        }
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    }
  )
)

#' Unique SU values validation rule
UniqueSUValidationRule <- R6Class("UniqueSUValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Check the data source for unique SU errors
    #' @param data_source A DataSource object
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects

      for (plot in unique(sapply(data_source$sheet1_data, function(x) x$Plot.code))) {
        su_values <- sapply(data_source$sheet1_data[sapply(data_source$sheet1_data, 
                                                           function(x) x$Plot.code == plot)], 
                            function(x) x$SU)
        if (length(su_values) != length(unique(su_values))) {
          errors <- append(errors, ValidationError$new(
            source = "Sheet1", row = NA, column = "SU",
            error_code = 1, type = "Generic", error = "Unique SU Violation",
            message = paste("Duplicate SU values found for Plot.code:", plot)
          ))
        }
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    }
  )
)

#' Notes validation rule
NotesValidationRule <- R6Class("NotesValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Check the data source for missing notes errors
    #' @param data_source A DataSource object
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects
      
      sheet1_data_list <- data_source$sheet1_data
      sheet2_data_list <- data_source$sheet2_data
      
      for (i in seq_along(sheet1_data_list)) {
        plot_code <- sheet1_data_list[[i]]$Plot.code
        su_value <- sheet1_data_list[[i]]$SU
          
        # Find corresponding rows in Sheet2
        matching_rows_sheet2 <- which(sapply(sheet2_data_list, function(x) x$Plot.code) == plot_code & 
                                        sapply(sheet2_data_list, function(x) x$Subplot) == su_value)
          
        if (length(matching_rows_sheet2) == 0) {
          # Check if Note in Sheet1 contains text
          if (is.null(sheet1_data_list[[i]]$notes) || is.na(sheet1_data_list[[i]]$notes) || 
                sheet1_data_list[[i]]$notes == "") {
            errors <- append(errors, ValidationError$new(
              source = "Sheet2", row = NA, column = "Subplot",
              error_code = 1, type = "Generic", error = "Missing Notes Violation",
              message = paste("Missing data in Sheet2 for Plot.code:", 
                              plot_code, "and SU:", su_value, 
                              "without a corresponding note in Sheet1.")
            ))
          }
        }
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    }
  )
)

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
      species_pattern <- CSVFilenamePatterns$species_pattern
      
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

#' Not null validation rule
NotNullValidationRule <- R6Class("NotNullValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Check the data source for not null violations
    #' @param data_source A DataSource object
    check = function(data_source) {
      errors <- list()  # Use a list to collect ValidationError objects
      
      # Sheet 1 validation
      for (i in seq_along(data_source$sheet1_data)) {
        data_row <- data_source$sheet1_data[[i]]
        for (field in names(data_row)) {
          if (field != "notes" && (is.null(data_row[[field]]) || is.na(data_row[[field]]) || data_row[[field]] == "")) {
            errors <- append(errors, ValidationError$new(
              source = "Sheet1", row = i, column = field,
              error_code = 1, type = "Generic", error = paste("Not Null Violation -", field),
              message = paste(field, "should not be empty.")
            ))
          }
        }
      }
      
      # Sheet 2 validation
      for (i in seq_along(data_source$sheet2_data)) {
        data_row <- data_source$sheet2_data[[i]]
        for (field in names(data_row)) {
          if (field != "Notes" && (is.null(data_row[[field]]) || is.na(data_row[[field]]) || data_row[[field]] == "")) {
            errors <- append(errors, ValidationError$new(
              source = "Sheet2", row = i, column = field,
              error_code = 1, type = "Generic", error = paste("Not Null Violation -", field),
              message = paste(field, "should not be empty.")
            ))
          }
        }
      }

      # Convert ValidationError objects to a data frame
      do.call(rbind, lapply(errors, function(e) e$to_dataframe_row()))
    },
    #' @description
    #' Get the error level for this rule
    get_error_level = function() {
      "Error"  # Not null violations are critical errors
    }
  )
)


