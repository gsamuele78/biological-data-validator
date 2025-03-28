# R/validation_classes.R
library(R6)

# Source validation rules
source("R/validation_rules.R")

#' @title Validator
#' @description Main class that applies validation rules
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
      # Add default validation rules
      self$add_default_rules()
    },
    
    #' @description
    #' Add default validation rules
    add_default_rules = function() {
      self$add_rule(CSVFilenameValidationRule$new())
      self$add_rule(CSVFileStructureValidationRule$new())
      self$add_rule(NotNullValidationRule$new()) # Register the new validation rule
      self$add_rule(DataTypeValidationRule$new())
      self$add_rule(MaxRowsValidationRule$new())
      self$add_rule(UniqueSUValidationRule$new())
      self$add_rule(NotesValidationRule$new())
      
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

#' Not Null Validation Rule
#' Ensures that all fields (except notes) in sheet1_data and sheet2_data are not empty
NotNullValidationRule <- R6Class("NotNullValidationRule",
  inherit = ValidationRule,
  public = list(
    # ...existing code from NotNullValidationRule in validation_rules.R...
  )
)