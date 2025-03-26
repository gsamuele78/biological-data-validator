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
  # ...existing code from validation_classes.R for DataTypeValidationRule...
#' Data type validation rule
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
        # ... (Add validation for other columns in Sheet1 with specific row numbers)
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
  # ...existing code from validation_classes.R for MaxRowsValidationRule...
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
  # ...existing code from validation_classes.R for UniqueSUValidationRule...
#' Unique SU values validation rule
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
  # ...existing code from validation_classes.R for NotesValidationRule...
#' Notes validation rule (checks if notes are present when SU rows are empty in Sheet 2)
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

#' CSV-specific validation rule
CSVFileStructureValidationRule <- R6Class("CSVFileStructureValidationRule",
  inherit = ValidationRule,
  # ...existing code from validation_classes.R for CSVFileStructureValidationRule...
#' CSV-specific validation rule
  public = list(
    #' @description
    #' Check if the CSV files have the correct structure
    #' @param data_source A DataSource object
    check = function(data_source) {
      errors <- data.frame(Source = character(), Row = integer(), Column = character(), 
                           Message = character(), stringsAsFactors = FALSE)
      
      # Only run this validation for CSV data sources
      if (data_source$file_type != "csv") {
        return(errors)
      }
      
      # Check if both files exist
      base_name <- tools::file_path_sans_ext(data_source$filepath)
      species_path <- paste0(base_name, "_species.csv")
      
      if (!file.exists(species_path)) {
        errors <- rbind(errors, data.frame(Source = "FileSystem", Row = NA, 
                                        Column = NA, 
                                        Message = paste("Species data file not found:", species_path)))
      }
      
      return(errors)
    }
  )
)

#' CSV-specific validation rule
CSVFileValidationRule <- R6Class("CSVFileValidationRule",
  inherit = ValidationRule,
  # ...existing code from validation_classes.R for CSVFileValidationRule...
  public = list(
    check = function(data_source) {
      errors <- self$create_empty_errors()
      
      # Check if both CSV files exist
      base_name <- tools::file_path_sans_ext(data_source$filepath)
      species_path <- paste0(base_name, "_species.csv")
      
      if (!file.exists(species_path)) {
        errors <- rbind(errors, 
          self$create_error(
            "FileSystem", NA, NA, 
            paste("Missing species CSV file:", species_path)
          )
        )
      }
      
      return(errors)
    }
  )
)


