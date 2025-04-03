#' R/validation_classes.R # nolint: commented_code_linter.
library(R6)

#' Source validation rules
source("R/validation_rules.R")
source("R/csv_validation_rules.R")  # Source the new CSV validation rules file

#' Source the ValidationError class
source("R/validation_error.R")

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
      # Enhanced error handling for path_generator parameter
      if (!is.null(path_generator) && !inherits(path_generator, "PathGenerator") && !is.function(path_generator)) {
        stop("path_generator must be a PathGenerator object, a reactive function, or NULL")
      }
      
      self$path_generator <- path_generator
      # Add default validation rules
      self$add_default_rules()
    },
    
    #' @description
    #' Add default validation rules
    add_default_rules = function() {
      self$add_rule(CSVFilenameValidationRule$new())        # filename validation
      self$add_rule(CSVFileStructureValidationRule$new())   # file structure validation
      self$add_rule(CSVFileValidationRule$new())            # file content validation  
      self$add_rule(DataTypeValidationRule$new())           # data type validation
      self$add_rule(NotNullValidationRule$new())            # not null validation
      self$add_rule(DuplicateRowValidationRule$new())       # duplicate row validation
      self$add_rule(MaxRowsValidationRule$new())            # max rows validation
      self$add_rule(UniqueSUValidationRule$new())           # unique SU validation
      self$add_rule(NotesValidationRule$new())              # notes validation
      
      self$add_rule(SpeciesExistenceValidationRule$new())   # species existence validation
      self$add_rule(DetectorExistenceValidationRule$new())  # detector existence validation
      self$add_rule(RegionExistenceValidationRule$new())    # region existence validation
      self$add_rule(CodiceExistenceValidationRule$new())    # codice existence validation
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




