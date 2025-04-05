# R/validation_classes.R # nolint: commented_code_linter.
library(R6)

#' Source validation rules
source("R/validation_rules.R")  # Contains base and specific validation rules
source("R/csv_validation_rules.R")  # Contains CSV-specific validation rules
source("R/validation_error.R")  # Contains the ValidationError class

#' @title Validator
#' @description Main class that applies validation rules to a data source.
#' This class manages a list of validation rules and applies them to a given data source.
#' It also generates validation reports and saves them to a file if needed.
#'
#' Documentation: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
#' Example: To create a Validator object and validate a data source:
#' ```
#' path_generator <- PathGenerator$new(base_path = "output/")
#' validator <- Validator$new(path_generator)
#' data_source <- DataSource$new("data/Plot_Template_INFI2023.csv")
#' errors <- validator$validate(data_source)
#' validator$generate_validation_report(data_source, "validation_report.csv")
#' ```
Validator <- R6Class("Validator",
  public = list(
    #' @field rules List of validation rules
    #' A list of validation rules (objects of classes inheriting from ValidationRule).
    rules = list(),
    
    #' @field path_generator PathGenerator object
    #' An instance of the PathGenerator class used for generating file paths.
    path_generator = NULL,

    #' @description
    #' Create a new Validator object.
    #' @param path_generator A PathGenerator object for managing file paths.
    #' Example: `validator <- Validator$new(path_generator)`
    initialize = function(path_generator) {
      # Validate the path_generator parameter
      if (!is.null(path_generator) && !inherits(path_generator, "PathGenerator") && !is.function(path_generator)) {
        stop("path_generator must be a PathGenerator object, a reactive function, or NULL")
      }
      
      self$path_generator <- path_generator
      # Add default validation rules
      self$add_default_rules()
    },
    
    #' @description
    #' Add default validation rules to the Validator.
    #' These rules include checks for file structure, data types, and specific constraints.
    #' Example: `validator$add_default_rules()`
    add_default_rules = function() {
      self$add_rule(CSVFilenameValidationRule$new())        # Validates file naming conventions
      self$add_rule(CSVFileStructureValidationRule$new())   # Validates file structure
      self$add_rule(CSVFileValidationRule$new())            # Validates file content
      self$add_rule(DataTypeValidationRule$new())           # Validates data types
      self$add_rule(NotNullValidationRule$new())            # Ensures required fields are not null
      self$add_rule(DuplicateRowValidationRule$new())       # Checks for duplicate rows
      self$add_rule(MaxRowsValidationRule$new())            # Ensures maximum row constraints
      self$add_rule(UniqueSUValidationRule$new())           # Ensures unique sampling unit (SU) values
      self$add_rule(NotesValidationRule$new())              # Checks for missing notes
      
      # Domain-specific validation rules
      self$add_rule(SpeciesExistenceValidationRule$new())   # Validates species existence
      self$add_rule(DetectorExistenceValidationRule$new())  # Validates detector existence
      self$add_rule(RegionExistenceValidationRule$new())    # Validates region existence
      self$add_rule(CodiceExistenceValidationRule$new())    # Validates codice existence
    },
    
    #' @description
    #' Add a validation rule to the Validator.
    #' @param rule A ValidationRule object to add to the list of rules.
    #' Example: `validator$add_rule(DataTypeValidationRule$new())`
    add_rule = function(rule) {
      self$rules <- c(self$rules, rule)
    },

    #' @description
    #' Validate the data source using all added rules.
    #' @param data_source A DataSource object containing the data to validate.
    #' @return A data frame containing validation errors.
    #' Example: `errors <- validator$validate(data_source)`
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
    #' Generate a validation report and save it to a file.
    #' @param data_source A DataSource object containing the data to validate.
    #' @param output_path Path to save the validation report (optional).
    #' @return A data frame containing validation errors.
    #' Example: `validator$generate_validation_report(data_source, "report.csv")`
    generate_validation_report = function(data_source, output_path = NULL) {
      # Run validation
      validation_results <- self$validate(data_source)
      
      # If output path is provided, save the report
      if (!is.null(output_path)) {
        # Ensure the file extension is CSV
        file_ext <- tolower(tools::file_ext(output_path))
        if (file_ext != "csv") {
          stop("Only CSV format is supported for the validation report.")
        }
        
        # Save the report as a CSV file
        write.csv(validation_results, file = output_path, row.names = FALSE)
      }
      
      return(validation_results)
    }
  )
)




