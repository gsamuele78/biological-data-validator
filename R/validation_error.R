# R/validation_error.R # nolint: commented_code_linter.
library(R6)

#' @title ValidationError
#' @description Represents a validation error encountered during data validation.
#' This class is used to encapsulate details about validation errors, such as the source of the error,
#' the row and column where it occurred, and a descriptive message.
#'
#' Documentation: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
#' Example: To create a ValidationError object:
#' ```
#' error <- ValidationError$new(
#'   source = "Sheet1",
#'   row = 1,
#'   column = "Plot.code",
#'   error_code = 1,
#'   type = "Generic",
#'   error = "Invalid Data",
#'   message = "Plot.code cannot be empty."
#' )
#' print(error$to_dataframe_row())
#' ```
ValidationError <- R6Class("ValidationError",
  public = list(
    #' @field source The source of the error (e.g., "Sheet1", "Sheet2").
    source = NULL,
    #' @field row The row number where the error occurred.
    row = NULL,
    #' @field column The column name where the error occurred.
    column = NULL,
    #' @field error_code A numeric code representing the type of error.
    error_code = NULL,
    #' @field type The type of error (e.g., "Generic", "Data Type Violation").
    type = NULL,
    #' @field error A short description of the error.
    error = NULL,
    #' @field message A detailed message describing the error.
    message = NULL,

    #' @description
    #' Create a new ValidationError object.
    #' @param source The source of the error (e.g., "Sheet1", "Sheet2").
    #' @param row The row number where the error occurred.
    #' @param column The column name where the error occurred.
    #' @param error_code A numeric code representing the type of error.
    #' @param type The type of error (e.g., "Generic", "Data Type Violation").
    #' @param error A short description of the error.
    #' @param message A detailed message describing the error.
    initialize = function(source, row, column, error_code, type, error, message) {
      self$source <- source
      self$row <- row
      self$column <- column
      self$error_code <- error_code
      self$type <- type
      self$error <- error
      self$message <- message
    },

    #' @description
    #' Convert the ValidationError object to a data frame row.
    #' @return A data frame with a single row containing the error details.
    #' Example:
    #' ```
    #' error <- ValidationError$new(
    #'   source = "Sheet1",
    #'   row = 1,
    #'   column = "Plot.code",
    #'   error_code = 1,
    #'   type = "Generic",
    #'   error = "Invalid Data",
    #'   message = "Plot.code cannot be empty."
    #' )
    #' df <- error$to_dataframe_row()
    #' print(df)
    #' ```
    to_dataframe_row = function() {
      data.frame(
        Source = self$source,
        Row = self$row,
        Column = self$column,
        ErrorCode = self$error_code,
        Type = self$type,
        Error = self$error,
        Message = self$message,
        stringsAsFactors = FALSE
      )
    }
  )
)
