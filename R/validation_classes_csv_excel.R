# R/validation_classes.R
library(R6)

#' Base class for validation rules
ValidationRule <- R6Class("ValidationRule",
  public = list(
    #' @description
    #' Check the data source for errors
    #' @param data_source A DataSource object
    check = function(data_source) {
      stop("Subclasses must implement the 'check' method.")
    },
    #' @description
    #' Get the error level for this rule
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

#' Notes validation rule (checks if notes are present when SU rows are empty in Sheet 2)
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

#' CSV-specific validation rule
CSVFileStructureValidationRule <- R6Class("CSVFileStructureValidationRule",
  inherit = ValidationRule,
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

#' Validator class that applies all validation rules
Validator <- R6Class("Validator",
  public = list(
    #' @field rules List of validation rules
    rules = list(),
    #' @field path_generator PathGenerator object
    path_generator = NULL,

    #' @description
    #' Create a new Validator object
    #' @param path_generator A PathGenerator object
    initialize = function(path_generator) {
      self$path_generator <- path_generator
      self$add_rule(DataTypeValidationRule$new())
      self$add_rule(MaxRowsValidationRule$new())
      self$add_rule(UniqueSUValidationRule$new())
      self$add_rule(NotesValidationRule$new())
      self$add_rule(CSVFileStructureValidationRule$new())
    },
    
    #' @description
    #' Add a validation rule to the validator
    #' @param rule A ValidationRule object
    add_rule = function(rule) {
      self$rules <- c(self$rules, rule)
    },

    #' @description
    #' Validate the data source using all added rules
    #' @param data_source A DataSource object
    validate = function(data_source) {
      all_errors <- data.frame(Source = character(), 
                               Row = integer(), 
                               Column = character(), 
                               Message = character(), 
                               Level = character(), # Add error level
                               stringsAsFactors = FALSE)

      for (rule in self$rules) {
        errors <- rule$check(data_source)
        
        # Add error level to the errors dataframe
        if (nrow(errors) > 0) {
          errors$Level <- rule$get_error_level()
        }
        
        all_errors <- rbind(all_errors, errors)
      }
      
      # Add file type information to the validation results
      if (nrow(all_errors) > 0) {
        all_errors$FileType <- data_source$file_type
      }

      return(all_errors)
    },
    
    #' @description
    #' Generate a validation report and save it to a file
    #' @param data_source A DataSource object
    #' @param output_path Path to save the validation report (optional)
    #' @return A data frame containing validation errors
    generate_validation_report = function(data_source, output_path = NULL) {
      # Run validation
      validation_results <- self$validate(data_source)
      
      # If output path is provided, save the report
      if (!is.null(output_path)) {
        # Determine the format based on file extension
        file_ext <- tolower(tools::file_ext(output_path))
        
        if (file_ext == "csv") {
          write.csv(validation_results, file = output_path, row.names = FALSE)
        } else if (file_ext %in% c("xlsx", "xls")) {
          # Create a workbook and write the results
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Validation Results")
          openxlsx::writeData(wb, "Validation Results", validation_results)
          openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
        } else {
          warning("Unsupported output format. Report will not be saved.")
        }
      }
      
      return(validation_results)
    }
  )
)