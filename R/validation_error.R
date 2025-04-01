#' R/validation_error.R
library(R6)

ValidationError <- R6Class("ValidationError",
  public = list(
    Source = NULL,
    Row = NULL,
    Column = NULL,
    `Error-code` = NULL,
    Type = NULL,
    Error = NULL,
    Message = NULL,
    
    #' @description
    #' Initialize a new ValidationError object
    #' @param source The source of the error (e.g., "Sheet1")
    #' @param row The row number where the error occurred
    #' @param column The column name where the error occurred
    #' @param error_code The error code
    #' @param type The type of the error
    #' @param error The specific error description
    #' @param message The detailed error message
    initialize = function(source, row, column, error_code, type, error, message) {
      self$Source <- source
      self$Row <- row
      self$Column <- column
      self$`Error-code` <- error_code
      self$Type <- type
      self$Error <- error
      self$Message <- message
    },
    
    #' @description
    #' Convert the ValidationError object to a data frame row
    to_dataframe_row = function() {
      data.frame(
        Source = self$Source,
        Row = self$Row,
        Column = self$Column,
        `Error-code` = self$`Error-code`,
        Type = self$Type,
        Error = self$Error,
        Message = self$Message,
        stringsAsFactors = FALSE
      )
    }
  )
)
