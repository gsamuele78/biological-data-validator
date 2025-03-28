library(R6)

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
      errors <- data.frame(Source = character(), Row = integer(), Column = character(), 
                           Message = character(), stringsAsFactors = FALSE)

      # Sheet 1 validation
      for (i in seq_along(data_source$sheet1_data)) {
        data_row <- data_source$sheet1_data[[i]]

        if (!is.character(data_row$Plot.code)) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Plot.code", 
                                             Message = "Plot.code should be alphanumeric."))
        }
        if (!is.numeric(data_row$SU) || data_row$SU < 1 || data_row$SU > 4) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "SU", 
                                             Message = "SU should be a number between 1 and 4."))
        }
        if (!inherits(data_row$Sample.date, "Date") || 
            !grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(data_row$Sample.date))) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Sample.date", 
                                             Message = "Sample.date should be a valid date in the format YYYY-MM-DD."))
        }
        if (!is.character(data_row$Detector)) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Detector", 
                                             Message = "Detector should be a string."))
        }
        if (!is.numeric(data_row$X)) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "X", 
                                             Message = "X should be numeric (longitude)."))
        }
        if (!is.numeric(data_row$Y)) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Y", 
                                             Message = "Y should be numeric (latitude)."))
        }
        if (!is.character(data_row$Region)) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Region", 
                                             Message = "Region should be a string."))
        }
        if (!is.numeric(data_row$Elevation)) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Elevation", 
                                             Message = "Elevation should be numeric."))
        }
        if (!is.numeric(data_row$Aspect) || data_row$Aspect < 0 || data_row$Aspect > 360) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Aspect", 
                                             Message = "Aspect should be numeric (0-360 degrees)."))
        }
        if (!is.numeric(data_row$Slope) || data_row$Slope < 0 || data_row$Slope > 90) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Slope", 
                                             Message = "Slope should be numeric (0-90 degrees)."))
        }
        if (!is.numeric(data_row$Cop.tot) || data_row$Cop.tot < 0 || data_row$Cop.tot > 100) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Cop.tot", 
                                             Message = "Cop.tot should be numeric (0-100%)."))
        }
        if (!is.numeric(data_row$Tree.cov) || data_row$Tree.cov < 0 || data_row$Tree.cov > 100) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Tree.cov", 
                                             Message = "Tree.cov should be numeric (0-100%)."))
        }
        if (!is.numeric(data_row$Shrub.cov) || data_row$Shrub.cov < 0 || data_row$Shrub.cov > 100) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Shrub.cov", 
                                             Message = "Shrub.cov should be numeric (0-100%)."))
        }
        if (!is.numeric(data_row$Herb.cov) || data_row$Herb.cov < 0 || data_row$Herb.cov > 100) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Herb.cov", 
                                             Message = "Herb.cov should be numeric (0-100%)."))
        }
        if (!is.numeric(data_row$Brioph.cov) || data_row$Brioph.cov < 0 || data_row$Brioph.cov > 100) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Brioph.cov", 
                                             Message = "Brioph.cov should be numeric (0-100%)."))
        }
        if (!is.numeric(data_row$Bare.soil.cov) || data_row$Bare.soil.cov < 0 || data_row$Bare.soil.cov > 100) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Bare.soil.cov", 
                                             Message = "Bare.soil.cov should be numeric (0-100%)."))
        }
        if (!is.numeric(data_row$Litter.cov) || data_row$Litter.cov < 0 || data_row$Litter.cov > 100) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "Litter.cov", 
                                             Message = "Litter.cov should be numeric (0-100%)."))
        }
        if (!is.character(data_row$notes)) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = i, 
                                             Column = "notes", 
                                             Message = "notes should be a string."))
        }
      }

      # Sheet 2 validation
      for (i in seq_along(data_source$sheet2_data)) {
        data_row <- data_source$sheet2_data[[i]]

        if (!is.character(data_row$Plot.code)) {
          errors <- rbind(errors, data.frame(Source = "Sheet2", Row = i, 
                                             Column = "Plot.code", 
                                             Message = "Plot.code should be alphanumeric."))
        }
        # ... (Add validation for other columns in Sheet2 with specific row numbers)
      }

      return(errors)
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
      errors <- data.frame(Source = character(), Row = integer(), Column = character(), 
                           Message = character(), stringsAsFactors = FALSE)

      # Rule 1: Maximum 4 rows with same Plot.code and SU values 1,2,3,4
      plot_counts <- table(sapply(data_source$sheet1_data, function(x) x$Plot.code))
      if (any(plot_counts > 4)) {
        for (plot in names(plot_counts[plot_counts > 4])) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = NA, 
                                             Column = "Plot.code", 
                                             Message = paste("More than 4 rows with Plot.code:", plot)))
        }
      }

      return(errors)
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
      errors <- data.frame(Source = character(), Row = integer(), Column = character(), 
                           Message = character(), stringsAsFactors = FALSE)

      for (plot in unique(sapply(data_source$sheet1_data, function(x) x$Plot.code))) {
        su_values <- sapply(data_source$sheet1_data[sapply(data_source$sheet1_data, 
                                                           function(x) x$Plot.code == plot)], 
                            function(x) x$SU)
        if (length(su_values) != length(unique(su_values))) {
          errors <- rbind(errors, data.frame(Source = "Sheet1", Row = NA, 
                                             Column = "SU", 
                                             Message = paste("Duplicate SU values found for Plot.code:", plot)))
        }
      }

      return(errors)
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
      errors <- data.frame(Source = character(), Row = integer(), Column = character(), 
                           Message = character(), stringsAsFactors = FALSE)
      
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
            errors <- rbind(errors, data.frame(Source = "Sheet2", Row = NA, 
                                               Column = "Subplot", 
                                               Message = paste("Missing data in Sheet2 for Plot.code:", 
                                                               plot_code, "and SU:", su_value, 
                                                               "without a corresponding note in Sheet1.")))
          }
        }
      }
      
      return(errors)
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
      errors <- data.frame(Source = character(), Row = integer(), Column = character(), 
                          Message = character(), stringsAsFactors = FALSE)
      
      main_file <- basename(data_source$filepath)
      plot_pattern <- CSVFilenamePatterns$plot_pattern
      species_pattern <- CSVFilenamePatterns$species_pattern
      
      # Validate main file format
      if (!grepl(paste0("^", plot_pattern, "$"), main_file, ignore.case = TRUE)) {
        errors <- rbind(errors, data.frame(
          Source = "FileSystem", Row = NA, Column = NA,
          Message = paste("Invalid main file name format. Expected: Plot_Template_INFIYYYY.csv, Got:", main_file)
        ))
      }
      
      # Extract year from main file
      year <- gsub(plot_pattern, "\\1", main_file)
      
      # Derive species file name
      expected_species_file <- paste0("Species_Template_INFI", year, ".csv")
      species_path <- file.path(dirname(data_source$filepath), expected_species_file)
      
      # Validate species file format
      if (!file.exists(species_path)) {
        errors <- rbind(errors, data.frame(
          Source = "FileSystem", Row = NA, Column = NA,
          Message = paste("Species file not found or invalid format. Expected:", expected_species_file)
        ))
      }
      
      return(errors)
    }
  )
)

#' CSV-specific validation rule
CSVFileValidationRule <- R6Class("CSVFileValidationRule",
  inherit = ValidationRule,
  public = list(
    check = function(data_source) {
      errors <- data.frame(Source = character(), Row = integer(), Column = character(), 
                          Message = character(), stringsAsFactors = FALSE)
      
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
        errors <- rbind(errors, data.frame(
          Source = "FileSystem", Row = NA, Column = NA,
          Message = paste("Missing species CSV file:", species_path),
          stringsAsFactors = FALSE
        ))
      }
      
      return(errors)
    }
  )
)

#' CSV-specific structure validation rule
CSVFileStructureValidationRule <- R6Class("CSVFileStructureValidationRule",
  inherit = ValidationRule,
  public = list(
    check = function(data_source) {
      errors <- data.frame(Source = character(), Row = integer(), Column = character(), 
                          Message = character(), stringsAsFactors = FALSE)
      
      # Only run this validation for CSV data sources
      if (data_source$file_type != "csv") {
        return(errors)
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
        errors <- rbind(errors, data.frame(
          Source = "FileSystem", Row = NA, Column = NA,
          Message = paste("Species data file not found:", species_path),
          stringsAsFactors = FALSE
        ))
      }
      
      # Check if the file is in UTF-8 encoding
      if (!isTRUE(all.equal(fileEncoding(data_source$filepath), "UTF-8"))) {
        errors <- rbind(errors, data.frame(
          Source = "FileSystem", Row = NA, Column = NA,
          Message = "CSV file is not in UTF-8 encoding."
        ))
      }

      # Check if the CSV uses ',' as column separator and '.' for decimals
      csv_content <- readLines(data_source$filepath, n = 1)
      if (!grepl(",", csv_content)) {
        errors <- rbind(errors, data.frame(
          Source = "FileSystem", Row = NA, Column = NA,
          Message = "CSV file does not use ',' as column separator."
        ))
      }
      
      sample_data <- read.csv(data_source$filepath, nrows = 10, sep = ",", dec = ".")
      if (any(is.na(as.numeric(gsub(",", "", unlist(sample_data), fixed = TRUE))))) {
        errors <- rbind(errors, data.frame(
          Source = "FileSystem", Row = NA, Column = NA,
          Message = "CSV file does not use '.' as decimal separator."
        ))
      }

      return(errors)
    }
  )
)


