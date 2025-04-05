#' R/db_interaction_class.R # nolint: commented_code_linter.
library(R6)
library(DBI)
library(RSQLite)

# Purpose:
# This file defines the `DatabaseHandler` class, which is responsible for interacting with the SQLite database.
# It provides methods to create tables, add, update, delete, and query records, and manage associated images.

# Documentation:
# - R6 Classes: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
# - DBI: https://cran.r-project.org/web/packages/DBI/index.html
# - RSQLite: https://cran.r-project.org/web/packages/RSQLite/index.html

#' @title DatabaseHandler
#' @description Class for interacting with the SQLite database.
#' Example:
#' ```
#' db_handler <- DatabaseHandler$new("validation_history.db")
#' db_handler$add_plot_data("file.csv", "Plot1", "2023-05-10", "DetectorA", "RegionX", "Valid", "report.html")
#' ```
DatabaseHandler <- R6Class(
  "DatabaseHandler",
  public = list(
    #' @field db Database connection object
    db = NULL,

    #' @description
    #' Create a new DatabaseHandler object and connect to the database.
    #' @param db_path Path to the SQLite database file.
    initialize = function(db_path = "validation_history.db") {
      self$db <- dbConnect(RSQLite::SQLite(), db_path)
      self$create_tables()
    },

    #' @description
    #' Create the necessary database tables if they don't exist.
    create_tables = function() {
      dbExecute(
        self$db,
        "CREATE TABLE IF NOT EXISTS plot_data (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          filepath TEXT,
          plot_code TEXT,
          sample_date TEXT,
          detector TEXT,
          region TEXT,
          validation_status TEXT,
          report_path TEXT,
          timestamp TEXT DEFAULT CURRENT_TIMESTAMP
        )"
      )

      dbExecute(
        self$db,
        "CREATE TABLE IF NOT EXISTS images (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          plot_data_id INTEGER,
          image_path TEXT,
          FOREIGN KEY (plot_data_id) REFERENCES plot_data(id)
        )"
      )
    },

    #' @description
    #' Add plot data to the database.
    #' @param filepath Path to the data file.
    #' @param plot_code Plot code.
    #' @param sample_date Sample date.
    #' @param detector Detector.
    #' @param region Region.
    #' @param validation_status Validation status.
    #' @param report_path Path to the validation report.
    add_plot_data = function(filepath, plot_code, sample_date, detector, region, validation_status, report_path) {
      dbExecute(
        self$db,
        "INSERT INTO plot_data (filepath, plot_code, sample_date, detector, region, validation_status, report_path) VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(filepath, plot_code, sample_date, detector, region, validation_status, report_path)
      )
      return(dbGetQuery(self$db, "SELECT last_insert_rowid()")[1, 1])
    },

    #' @description
    #' Add image data to the database.
    #' @param plot_data_id ID of the associated plot data.
    #' @param image_path Path to the image file.
    add_image_data = function(plot_data_id, image_path) {
      dbExecute(
        self$db,
        "INSERT INTO images (plot_data_id, image_path) VALUES (?, ?)",
        params = list(plot_data_id, image_path)
      )
    },

    #' @description
    #' Get plot history from the database with optional filtering.
    #' @param plot_code Plot code to filter by (optional).
    #' @param from_date Start date for filtering (optional).
    #' @param to_date End date for filtering (optional).
    get_plot_history = function(plot_code = NULL, from_date = NULL, to_date = NULL) {
      query <- "SELECT * FROM plot_data WHERE 1=1"
      params <- list()

      if (!is.null(plot_code)) {
        query <- paste(query, "AND plot_code LIKE ?")
        params <- c(params, paste0("%", plot_code, "%"))
      }

      if (!is.null(from_date)) {
        query <- paste(query, "AND sample_date >= ?")
        params <- c(params, as.character(from_date))
      }

      if (!is.null(to_date)) {
        query <- paste(query, "AND sample_date <= ?")
        params <- c(params, as.character(to_date))
      }

      query <- paste(query, "ORDER BY timestamp DESC")
      dbGetQuery(self$db, query, params = params)
    },

    #' @description
    #' Get images associated with a plot data record.
    #' @param plot_data_id ID of the plot data record.
    get_images = function(plot_data_id) {
      dbGetQuery(
        self$db,
        "SELECT * FROM images WHERE plot_data_id = ?",
        params = list(plot_data_id)
      )
    },

    #' @description
    #' Update a plot data record in the database.
    #' @param id ID of the record to update.
    #' @param filepath New filepath.
    #' @param plot_code New plot code.
    #' @param sample_date New sample date.
    #' @param detector New detector.
    #' @param region New region.
    #' @param validation_status New validation status.
    #' @param report_path New report path.
    update_plot_data = function(id, filepath, plot_code, sample_date, detector, region, validation_status, report_path) {
      dbExecute(
        self$db,
        "UPDATE plot_data SET filepath = ?, plot_code = ?, sample_date = ?, detector = ?, region = ?, validation_status = ?, report_path = ? WHERE id = ?",
        params = list(filepath, plot_code, sample_date, detector, region, validation_status, report_path, id)
      )
    },

    #' @description
    #' Delete a plot data record and associated images from the database.
    #' @param id ID of the record to delete.
    delete_plot_data = function(id) {
      dbExecute(
        self$db,
        "DELETE FROM images WHERE plot_data_id = ?",
        params = list(id)
      )
      dbExecute(
        self$db,
        "DELETE FROM plot_data WHERE id = ?",
        params = list(id)
      )
    },

    #' @description
    #' Close the database connection.
    #' @return TRUE if successful.
    close = function() {
      if (!is.null(self$db)) {
        dbDisconnect(self$db)
        self$db <- NULL
        return(TRUE)
      }
      return(FALSE)
    },

    #' @description
    #' Finalize method to ensure database connection is closed when object is garbage collected.
    finalize = function() {
      tryCatch({
        self$close()
        message("Database connection closed successfully.")
      }, error = function(e) {
        warning(paste("Error closing database connection:", e$message))
      })
    }
  )
)

#' @title Validate and Store Data
#' @description Function to validate ecological data and store the results in the database.
#' Example:
#' ```
#' validate_and_store_data("data.xlsx", "validation_history.db", validation_function, c("image1.png", "image2.png"))
#' ```
#' @param filepath Path to the data file (Excel or CSV).
#' @param db_path Path to the SQLite database file.
#' @param validation_function A function that takes a DataSource object and returns a list with 
#'        validation_status and report_path.
#' @param image_paths Optional vector of image file paths associated with the plot data.
#' @return The ID of the newly created database record.
#' @export
validate_and_store_data <- function(filepath, db_path = "validation_history.db", 
                                   validation_function = NULL, image_paths = NULL) {
  # Create DataSource object
  data_source <- DataSource$new(filepath)

  # Create DatabaseHandler object
  db_handler <- DatabaseHandler$new(db_path)

  # Use tryCatch to ensure database connection is closed
  tryCatch({
    # Perform validation if a validation function is provided
    if (!is.null(validation_function)) {
      validation_result <- validation_function(data_source)
      validation_status <- validation_result$validation_status
      report_path <- validation_result$report_path
    } else {
      # Default to "pending" if no validation function is provided
      validation_status <- "pending"
      report_path <- NULL
    }

    # Process data source and add to database
    plot_data_id <- db_handler$process_data_source(
      data_source = data_source,
      validation_status = validation_status,
      report_path = report_path,
      image_paths = image_paths
    )

    return(plot_data_id)
  }, finally = {
    # Always close the database connection
    db_handler$close()
  })
}

#' @title Get Plot History with Images
#' @description Function to retrieve plot history along with associated images.
#' Example:
#' ```
#' get_plot_history_with_images("validation_history.db", "Plot1", "2023-01-01", "2023-12-31")
#' ```
#' @param db_path Path to the SQLite database file.
#' @param plot_code Optional plot code to filter by.
#' @param from_date Optional start date for filtering.
#' @param to_date Optional end date for filtering.
#' @return A list containing plot data and associated images.
#' @export
get_plot_history_with_images <- function(db_path = "validation_history.db", 
                                        plot_code = NULL, from_date = NULL, to_date = NULL) {
  # Create DatabaseHandler object
  db_handler <- DatabaseHandler$new(db_path)

  # Use tryCatch to ensure database connection is closed
  tryCatch({
    # Get plot history with images
    history <- db_handler$get_plot_history_with_images(plot_code, from_date, to_date)

    return(history)
  }, finally = {
    # Always close the database connection
    db_handler$close()
  })
}