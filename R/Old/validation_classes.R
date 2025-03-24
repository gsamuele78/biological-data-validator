library(R6)

#' Base class for validation rules
#' @description A base class providing common functionality for validation rules
ValidationRule <- R6Class(
  "ValidationRule",
  private = list(
    error_level = "Error"
  ),
  public = list(
    initialize = function(error_level = "Error") {
      private$error_level <- error_level
    },
    
    check = function(excel_data) {
      stop("Subclasses must implement the 'check' method.")
    },
    
    get_error_level = function() {
      return(private$error_level)
    },
    
    create_error = function(sheet, row, column, message) {
      generic_message <- sprintf(
        "Generic - Error - row (%s) - column (%s) - %s",
        ifelse(is.na(row), "NA", row), column, message
      )
      data.frame(
        Sheet = sheet,
        Row = row,
        Column = column,
        Message = generic_message,
        stringsAsFactors = FALSE
      )
    },
    
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

#' @title DataTypeValidationRule
#' @description Rule for validating data types
DataTypeValidationRule <- R6Class(
  "DataTypeValidationRule",
  inherit = ValidationRule,
  private = list(
    min_su = 1,
    max_su = 4
  ),
  public = list(
    initialize = function(min_su = 1, max_su = 4, error_level = "Warning") {
      super$initialize(error_level)
      private$min_su <- min_su
      private$max_su <- max_su
    },
    
    check = function(excel_data) {
      errors <- self$create_empty_errors()
      
      for (i in seq_along(excel_data$sheet1_data)) {
        data_row <- excel_data$sheet1_data[[i]]
        excel_row <- i + 1
        
        if (!is.null(data_row$Plot.code) && !is.character(data_row$Plot.code)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Plot.code",
              "Plot.code should be alphanumeric."
            )
          )
        }
        
        if (is.null(data_row$SU) || !is.numeric(data_row$SU) ||
            data_row$SU < private$min_su || data_row$SU > private$max_su) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "SU",
              sprintf(
                "SU should be a number between %d and %d.",
                private$min_su,
                private$max_su
              )
            )
          )
        }
        
        if (!inherits(data_row$Sample.date, "Date")) {
          tryCatch({
            parsed_date <- as.Date(data_row$Sample.date, format = "%m/%d/%Y")
            if (!is.na(parsed_date)) {
              errors <- rbind(
                errors,
                self$create_error(
                  "Sheet1",
                  excel_row,
                  "Sample.date",
                  paste(
                    "Sample.date should be in MM/DD/YYYY format",
                    "(e.g., 01/16/2024)."
                  )
                )
              )
            } else {
              errors <- rbind(
                errors,
                self$create_error(
                  "Sheet1",
                  excel_row,
                  "Sample.date",
                  "Sample.date is not a valid date. Please use MM/DD/YYYY format."
                )
              )
            }
          }, error = function(e) {
            errors <- rbind(
              errors,
              self$create_error(
                "Sheet1",
                excel_row,
                "Sample.date",
                "Sample.date is not a valid date. Please use MM/DD/YYYY format."
              )
            )
          })
        }
        
        if (!is.null(data_row$Detector) && !is.character(data_row$Detector)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Detector",
              "Detector should be alphanumeric."
            )
          )
        }
        
        if (!is.null(data_row$X) && is.numeric(data_row$X)) {
          x_str <- as.character(data_row$X)
          decimal_places <- nchar(strsplit(x_str, "\\.")[[1]][2])
          if (decimal_places != 15) {
            errors <- rbind(
              errors,
              self$create_error(
                "Sheet1",
                excel_row,
                "X",
                paste(
                  "X should be numeric with 15 decimal places and conform",
                  "to EPSG:32632."
                )
              )
            )
          }
        } else {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "X",
              paste(
                "X should be numeric with 15 decimal places and conform",
                "to EPSG:32632."
              )
            )
          )
        }
        
        if (!is.null(data_row$Y) && is.numeric(data_row$Y)) {
          y_str <- as.character(data_row$Y)
          decimal_places <- nchar(strsplit(y_str, "\\.")[[1]][2])
          if (decimal_places != 15) {
            errors <- rbind(
              errors,
              self$create_error(
                "Sheet1",
                excel_row,
                "Y",
                paste(
                  "Y should be numeric with 15 decimal places and conform",
                  "to EPSG:32632."
                )
              )
            )
          }
        } else {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Y",
              paste(
                "Y should be numeric with 15 decimal places and conform",
                "to EPSG:32632."
              )
            )
          )
        }
        
        if (!is.null(data_row$Region) && !is.character(data_row$Region)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Region",
              "Region should be alphanumeric."
            )
          )
        }
        
        if (!is.null(data_row$Elevation) && !is.numeric(data_row$Elevation)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Elevation",
              "Elevation should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Aspect) && !is.numeric(data_row$Aspect)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Aspect",
              "Aspect should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Slope) && !is.numeric(data_row$Slope)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Slope",
              "Slope should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Cop.tot) && !is.numeric(data_row$Cop.tot)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Cop.tot",
              "Cop.tot should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Litter.cov) && !is.numeric(data_row$Litter.cov)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Litter.cov",
              "Litter.cov should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Bare.soil.cov) &&
            !is.numeric(data_row$Bare.soil.cov)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Bare.soil.cov",
              "Bare.soil.cov should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Tree.cov) && !is.numeric(data_row$Tree.cov)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Tree.cov",
              "Tree.cov should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Tree.h) && !is.numeric(data_row$Tree.h)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Tree.h",
              "Tree.h should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Shrub.cov) && !is.numeric(data_row$Shrub.cov)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Shrub.cov",
              "Shrub.cov should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Shrub.h) && !is.numeric(data_row$Shrub.h)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Shrub.h",
              "Shrub.h should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Herb.cov) && !is.numeric(data_row$Herb.cov)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Herb.cov",
              "Herb.cov should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Herb.h) && !is.numeric(data_row$Herb.h)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Herb.h",
              "Herb.h should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Brioph.cov) && !is.numeric(data_row$Brioph.cov)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "Brioph.cov",
              "Brioph.cov should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$notes) && !is.character(data_row$notes)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet1",
              excel_row,
              "notes",
              "notes should be alphanumeric."
            )
          )
        }
      }
      
      for (i in seq_along(excel_data$sheet2_data)) {
        data_row <- excel_data$sheet2_data[[i]]
        excel_row <- i + 1
        
        if (!is.null(data_row$Plot.code) && !is.character(data_row$Plot.code)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet2",
              excel_row,
              "Plot.code",
              "Plot.code should be alphanumeric."
            )
          )
        }
        
        if (is.null(data_row$Subplot) || !is.numeric(data_row$Subplot) ||
            data_row$Subplot < private$min_su ||
            data_row$Subplot > private$max_su) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet2",
              excel_row,
              "Subplot",
              sprintf(
                "Subplot should be a number between %d and %d.",
                private$min_su,
                private$max_su
              )
            )
          )
        }
        
        if (!is.null(data_row$Species) && !is.character(data_row$Species)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet2",
              excel_row,
              "Species",
              "Species should be alphanumeric."
            )
          )
        }
        
        if (!is.null(data_row$species_abb) &&
            !is.character(data_row$species_abb)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet2",
              excel_row,
              "species_abb",
              "species_abb should be alphanumeric."
            )
          )
        }
        
        if (!is.null(data_row$cover) && !is.numeric(data_row$cover)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet2",
              excel_row,
              "cover",
              "cover should be numeric."
            )
          )
        }
        
        if (!is.null(data_row$Layer) && !is.character(data_row$Layer)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet2",
              excel_row,
              "Layer",
              "Layer should be alphanumeric."
            )
          )
        }
        
        if (!is.null(data_row$Notes) && !is.character(data_row$Notes)) {
          errors <- rbind(
            errors,
            self$create_error(
              "Sheet2",
              excel_row,
              "Notes",
              "Notes should be alphanumeric."
            )
          )
        }
      }
      
      return(errors)
    }
  )
)

#' @title MaxRowsValidationRule 
#' @description Rule for validating maximum rows per Plot.code
MaxRowsValidationRule <- R6Class(
  "MaxRowsValidationRule",
  inherit = ValidationRule,
  private = list(
    max_rows = 4
  ),
  public = list(
    initialize = function(max_rows = 4, error_level = "Error") {
      super$initialize(error_level)
      private$max_rows <- max_rows
    },
    
    check = function(excel_data) {
      errors <- self$create_empty_errors()
      
      plot_codes <- sapply(excel_data$sheet1_data, function(x) x$Plot.code)
      plot_counts <- table(plot_codes)
      excess_plots <- names(plot_counts[plot_counts > private$max_rows])
      
      if (length(excess_plots) > 0) {
        for (plot in excess_plots) {
          plot_rows <- which(plot_codes == plot)
          excel_rows <- paste(plot_rows + 1, collapse = ", ")
          
          msg <- sprintf(
            "More than %d rows with Plot.code: %s (rows: %s)",
            private$max_rows, plot, excel_rows
          )
          
          errors <- rbind(
            errors,
            self$create_error("Sheet1", NA, "Plot.code", msg)
          )
        }
      }
      
      return(errors)
    }
  )
)

#' @title UniqueSUValidationRule
#' @description Rule for validating unique SU values within Plot.code
UniqueSUValidationRule <- R6Class(
  "UniqueSUValidationRule",
  inherit = ValidationRule,
  private = list(
    min_su = 1,
    max_su = 4
  ),
  public = list(
    initialize = function(min_su = 1, max_su = 4, error_level = "Error") {
      super$initialize(error_level)
      private$min_su <- min_su
      private$max_su <- max_su
    },
    
    check = function(excel_data) {
      errors <- self$create_empty_errors()
      
      plot_codes <- unique(
        sapply(excel_data$sheet1_data, function(x) x$Plot.code)
      )
      
      for (plot in plot_codes) {
        plot_indices <- which(
          sapply(excel_data$sheet1_data, function(x) x$Plot.code == plot)
        )
        
        if (length(plot_indices) > 0) {
          su_values <- sapply(
            excel_data$sheet1_data[plot_indices],
            function(x) x$SU
          )
          
          dup_su_values <- su_values[duplicated(su_values)]
          
          if (length(dup_su_values) > 0) {
            for (dup_su in unique(dup_su_values)) {
              dup_indices <- plot_indices[which(su_values == dup_su)]
              excel_rows <- paste(dup_indices + 1, collapse = ", ")
              
              msg <- sprintf(
                "Duplicate SU value %d found for Plot.code: %s (rows: %s)",
                dup_su, plot, excel_rows
              )
              
              errors <- rbind(
                errors,
                self$create_error("Sheet1", NA, "SU", msg)
              )
            }
          }
        }
      }
      
      return(errors)
    }
  )
)

#' @title NotesValidationRule 
#' @description Rule for checking notes presence when SU rows are empty
NotesValidationRule <- R6Class(
  "NotesValidationRule",
  inherit = ValidationRule,
  public = list(
    initialize = function(error_level = "Warning") {
      super$initialize(error_level)
    },
    
    check = function(excel_data) {
      errors <- self$create_empty_errors()
      
      sheet1_data_list <- excel_data$sheet1_data
      sheet2_data_list <- excel_data$sheet2_data
      
      for (i in seq_along(sheet1_data_list)) {
        if (is.null(sheet1_data_list[[i]])) {
          next
        }
        
        plot_code <- sheet1_data_list[[i]]$Plot.code
        su_value <- sheet1_data_list[[i]]$SU
        
        if (is.null(plot_code) || is.null(su_value)) {
          next()
        }
        
        matching_rows_sheet2 <- which(
          sapply(
            sheet2_data_list,
            function(x) {
              !is.null(x) && !is.null(x$Plot.code) && !is.null(x$Subplot) &&
                x$Plot.code == plot_code && x$Subplot == su_value
            }
          )
        )
        
        if (length(matching_rows_sheet2) == 0) {
          notes_empty <- TRUE
          
          if (!is.null(sheet1_data_list[[i]]$notes)) {
            if (!is.na(sheet1_data_list[[i]]$notes) &&
                sheet1_data_list[[i]]$notes != "") {
              notes_empty <- FALSE
            }
          }
          
          if (notes_empty) {
            excel_row <- i + 1
            
            errors <- rbind(
              errors,
              self$create_error(
                "Sheet1",
                excel_row,
                "notes",
                sprintf(
                  "Missing data in Sheet2 for Plot.code: %s and SU: %d without a corresponding note in Sheet1.",
                  plot_code,
                  su_value
                )
              )
            )
          }
        }
      }
      
      return(errors)
    }
  )
)

#' @title Validator
#' @description Main class that applies all validation rules
Validator <- R6Class(
  "Validator",
  public = list(
    rules = list(),
    path_generator = NULL,
    
    initialize = function(path_generator) {
      self$path_generator <- path_generator
      self$add_rule(DataTypeValidationRule$new())
      self$add_rule(MaxRowsValidationRule$new())
      self$add_rule(UniqueSUValidationRule$new())
      self$add_rule(NotesValidationRule$new())
    },
    
    add_rule = function(rule) {
      self$rules <- c(self$rules, list(rule))
      invisible(self)
    },
    
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
        if (is.null(rule) || !("check" %in% names(rule))) next
        
        errors <- rule$check(excel_data)
        if (nrow(errors) > 0) {
          errors$Level <- rule$get_error_level()
          all_errors <- rbind(all_errors, errors)
        }
      }
      
      return(all_errors)
    }
  )
)
