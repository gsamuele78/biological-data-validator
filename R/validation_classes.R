library(R6)

#' Base class for validation rules
#' @description A base class that provides common functionality for all validation rules
ValidationRule <- R6Class("ValidationRule",
  private = list(
    #' @field error_level Default error level for this rule
    error_level = "Error"
  ),
  public = list(
    #' @description
    #' Create a new ValidationRule object
    #' @param error_level Character string indicating the severity of rule violations
    initialize = function(error_level = "Error") {
      private$error_level <- error_level
    },
    
    #' @description
    #' Check the Excel data for errors
    #' @param excel_data An ExcelData object
    #' @return A data frame containing validation errors
    check = function(excel_data) {
      stop("Subclasses must implement the 'check' method.")
    },
    
    #' @description
    #' Get the error level for this rule
    #' @return A character string indicating the error level
    get_error_level = function() {
      return(private$error_level)
    },
    
    #' @description
    #' Helper method to create an error entry
    #' @param sheet Character string indicating which sheet the error is in
    #' @param row Integer indicating the row number of the error
    #' @param column Character string indicating the column name of the error
    #' @param message Character string describing the error
    #' @return A data frame row with error information
    create_error = function(sheet, row, column, message) {
      data.frame(
        Sheet = sheet,
        Row = row,
        Column = column,
        Message = message,
        stringsAsFactors = FALSE
      )
    },
    
    #' @description
    #' Create an empty errors data frame with the standard structure
    #' @return An empty data frame for storing validation errors
    create_empty_errors = function() {
      data.frame(
        Sheet = character(),
        Row = integer(),
        Column = character(),
        Message = character(),
        stringsAsFactors = FALSE
      )
    }
  )
)

#' Data type validation rule
#' @description Rule for validating data types in Excel sheets
DataTypeValidationRule <- R6Class("DataTypeValidationRule",
  inherit = ValidationRule,
  private = list(
    #' @field min_su Minimum allowed SU value
    min_su = 1,
    #' @field max_su Maximum allowed SU value
    max_su = 4
  ),
  public = list(
    #' @description
    #' Create a new DataTypeValidationRule object
    #' @param min_su Minimum allowed SU value
    #' @param max_su Maximum allowed SU value
    #' @param error_level Character string indicating the severity of rule violations
    initialize = function(min_su = 1, max_su = 4, error_level = "Warning") {
      super$initialize(error_level)
      private$min_su <- min_su
      private$max_su <- max_su
    },
    
    #' @description
    #' Check the Excel data for data type errors
    #' @param excel_data An ExcelData object
    #' @return A data frame containing any validation errors found
    check = function(excel_data) {
      errors <- self$create_empty_errors()

      # Sheet 1 validation
      for (i in seq_along(excel_data$sheet1_data)) {
        data_row <- excel_data$sheet1_data[[i]]
        
        # Row offset to match Excel row numbers (assuming 1 header row + data starts at row 2)
        excel_row <- i + 1
        
        # Check Plot.code is character/string
        if (!is.null(data_row$Plot.code) && !is.character(data_row$Plot.code)) {
          errors <- rbind(errors, self$create_error(
            "Sheet1", excel_row, "Plot.code", "Plot.code should be alphanumeric."
          ))
        }
        
        # Check SU is numeric and within range
        if (is.null(data_row$SU) || !is.numeric(data_row$SU) || data_row$SU < private$min_su || data_row$SU > private$max_su) {
          errors <- rbind(errors, self$create_error(
            "Sheet1", excel_row, "SU", sprintf("SU should be a number between %d and %d.", private$min_su, private$max_su)
          ))
        }
        
        # Additional column validations can be added here following the same pattern
      }

      # Sheet 2 validation
      for (i in seq_along(excel_data$sheet2_data)) {
        data_row <- excel_data$sheet2_data[[i]]
        
        # Row offset for Excel row numbers
        excel_row <- i + 1
        
        if (!is.null(data_row$Plot.code) && !is.character(data_row$Plot.code)) {
          errors <- rbind(errors, self$create_error(
            "Sheet2", excel_row, "Plot.code", "Plot.code should be alphanumeric."
          ))
        }
        
        # Additional Sheet2 column validations can be added here
      }

      return(errors)
    }
  )
)

#' Maximum rows validation rule
#' @description Rule for validating maximum number of rows with the same Plot.code
MaxRowsValidationRule <- R6Class("MaxRowsValidationRule",
  inherit = ValidationRule,
  private = list(
    #' @field max_rows Maximum number of rows allowed per Plot.code
    max_rows = 4
  ),
  public = list(
    #' @description
    #' Create a new MaxRowsValidationRule object
    #' @param max_rows Maximum number of rows allowed per Plot.code
    #' @param error_level Character string indicating the severity of rule violations
    initialize = function(max_rows = 4, error_level = "Error") {
      super$initialize(error_level)
      private$max_rows <- max_rows
    },
    
    #' @description
    #' Check the Excel data for maximum rows errors
    #' @param excel_data An ExcelData object
    #' @return A data frame containing any validation errors found
    check = function(excel_data) {
      errors <- self$create_empty_errors()

      # Extract all Plot.code values
      plot_codes <- sapply(excel_data$sheet1_data, function(x) x$Plot.code)
      
      # Count occurrences of each Plot.code
      plot_counts <- table(plot_codes)
      
      # Find Plot.codes with too many rows
      excess_plots <- names(plot_counts[plot_counts > private$max_rows])
      
      if (length(excess_plots) > 0) {
        for (plot in excess_plots) {
          # Find row indices for this Plot.code
          plot_rows <- which(plot_codes == plot)
          
          # Add Excel row offset
          excel_rows <- paste(plot_rows + 1, collapse = ", ")
          
          errors <- rbind(errors, self$create_error(
            "Sheet1", 
            NA, # We're listing all rows in the message instead
            "Plot.code", 
            sprintf("More than %d rows with Plot.code: %s (rows: %s)", 
                   private$max_rows, plot, excel_rows)
          ))
        }
      }

      return(errors)
    }
  )
)

#' Unique SU values validation rule
#' @description Rule for validating that SU values are unique within each Plot.code
UniqueSUValidationRule <- R6Class("UniqueSUValidationRule",
  inherit = ValidationRule,
  private = list(
    #' @field min_su Minimum allowed SU value
    min_su = 1,
    #' @field max_su Maximum allowed SU value
    max_su = 4
  ),
  public = list(
    #' @description
    #' Create a new UniqueSUValidationRule object
    #' @param min_su Minimum allowed SU value
    #' @param max_su Maximum allowed SU value
    #' @param error_level Character string indicating the severity of rule violations
    initialize = function(min_su = 1, max_su = 4, error_level = "Error") {
      super$initialize(error_level)
      private$min_su <- min_su
      private$max_su <- max_su
    },
    
    #' @description
    #' Check the Excel data for unique SU errors
    #' @param excel_data An ExcelData object
    #' @return A data frame containing any validation errors found
    check = function(excel_data) {
      errors <- self$create_empty_errors()
      
      # Group data by Plot.code
      plot_codes <- unique(sapply(excel_data$sheet1_data, function(x) x$Plot.code))
      
      for (plot in plot_codes) {
        # Find rows with this plot code (indices in the data structure)
        plot_indices <- which(sapply(excel_data$sheet1_data, function(x) x$Plot.code == plot))
        
        if (length(plot_indices) > 0) {
          # Extract SU values for this plot
          su_values <- sapply(excel_data$sheet1_data[plot_indices], function(x) x$SU)
          
          # Find duplicates
          dup_su_values <- su_values[duplicated(su_values)]
          
          if (length(dup_su_values) > 0) {
            for (dup_su in unique(dup_su_values)) {
              # Find indices of the duplicated values
              dup_indices <- plot_indices[which(su_values == dup_su)]
              
              # Add Excel row offset
              excel_rows <- paste(dup_indices + 1, collapse = ", ")
              
              errors <- rbind(errors, self$create_error(
                "Sheet1", 
                NA, # We're listing all rows in the message
                "SU", 
                sprintf("Duplicate SU value %d found for Plot.code: %s (rows: %s)", 
                       dup_su, plot, excel_rows)
              ))
            }
          }
        }
      }
      
      return(errors)
    }
  )
)

#' Notes validation rule
#' @description Rule for checking if notes are present when SU rows are empty in Sheet 2
NotesValidationRule <- R6Class("NotesValidationRule",
  inherit = ValidationRule,
  public = list(
    #' @description
    #' Create a new NotesValidationRule object
    #' @param error_level Character string indicating the severity of rule violations
    initialize = function(error_level = "Warning") {
      super$initialize(error_level)
    },
    
    #' @description
    #' Check the Excel data for missing notes errors
    #' @param excel_data An ExcelData object
    #' @return A data frame containing any validation errors found
    check = function(excel_data) {
      errors <- self$create_empty_errors()
      
      sheet1_data_list <- excel_data$sheet1_data
      sheet2_data_list <- excel_data$sheet2_data
      
      for (i in seq_along(sheet1_data_list)) {
        # Skip if the row data is NULL
        if (is.null(sheet1_data_list[[i]])) {
          next
        }
        
        plot_code <- sheet1_data_list[[i]]$Plot.code
        su_value <- sheet1_data_list[[i]]$SU
        
        # Skip if either value is missing
        if (is.null(plot_code) || is.null(su_value)) {
          next()
        }
          
        # Find corresponding rows in Sheet2
        matching_rows_sheet2 <- which(
          sapply(sheet2_data_list, function(x) {
            !is.null(x) && !is.null(x$Plot.code) && !is.null(x$Subplot) && 
            x$Plot.code == plot_code && x$Subplot == su_value
          })
        )
          
        if (length(matching_rows_sheet2) == 0) {
          # Check if Note in Sheet1 contains text
          notes_empty <- TRUE
          
          if (!is.null(sheet1_data_list[[i]]$notes)) {
            if (!is.na(sheet1_data_list[[i]]$notes) && sheet1_data_list[[i]]$notes != "") {
              notes_empty <- FALSE
            }
          }
          
          if (notes_empty) {
            # Excel row number with offset
            excel_row <- i + 1
            
            errors <- rbind(errors, self$create_error(
              "Sheet1", 
              excel_row, 
              "notes", 
              sprintf("Missing data in Sheet2 for Plot.code: %s and SU: %d without a corresponding note in Sheet1.", 
                     plot_code, su_value)
            ))
          }
        }
      }
      
      return(errors)
    }
  )
)

#' Validator class that applies all validation rules
#' @description A class that orchestrates the application of all validation rules
Validator <- R6Class("Validator",
  public = list(
    #' @field rules List of validation rules
    rules = list(),
    
    #' @field path_generator PathGenerator object
    path_generator = NULL,

    #' @description
    #' Create a new Validator object
    #' @param path_generator A PathGenerator object
    #' @return A new Validator object
    initialize = function(path_generator) {
      self$path_generator <- path_generator
      self$add_rule(DataTypeValidationRule$new())
      self$add_rule(MaxRowsValidationRule$new())
      self$add_rule(UniqueSUValidationRule$new())
      self$add_rule(NotesValidationRule$new())
    },
    
    #' @description
    #' Add a validation rule to the validator
    #' @param rule A ValidationRule object
    #' @return The validator object (for method chaining)
    add_rule = function(rule) {
      self$rules <- c(self$rules, list(rule))
      invisible(self)
    },

    #' @description
    #' Validate the Excel data using all added rules
    #' @param excel_data An ExcelData object
    #' @return A data frame with all validation errors found
    validate = function(excel_data) {
      all_errors <- data.frame(
        Sheet = character(), 
        Row = integer(), 
        Column = character(), 
        Message = character(), 
        Level = character(),
        stringsAsFactors = FALSE
      )

      for (rule in self$rules) {
        # Check for NULL or invalid rule
        if (is.null(rule) || !("check" %in% names(rule))) {
          next()
        }
        
        # Apply the validation rule
        errors <- rule$check(excel_data)
        
        # Add error level to the errors dataframe
        if (nrow(errors) > 0) {
          errors$Level <- rule$get_error_level()
          all_errors <- rbind(all_errors, errors)
        }
      }

      return(all_errors)
    }
  )
)
