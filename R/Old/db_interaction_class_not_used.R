#' Database Interaction Class
#'
#' @description Handles database operations
#' @export
DBInteraction <- R6::R6Class("DBInteraction",
  public = list(
    #' @field connection Database connection
    connection = NULL,
    
    #' @description Initialize database connection
    #' @param dbname Name of the database
    initialize = function(dbname = ":memory:") {
      self$connection <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    },
    
    #' @description Close database connection
    finalize = function() {
      if (!is.null(self$connection)) {
        DBI::dbDisconnect(self$connection)
      }
    }
  )
)
