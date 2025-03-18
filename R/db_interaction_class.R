# R/db_interaction_class.R
library(R6)
library(DBI)
library(RSQLite)

#' DatabaseHandler class for interacting with the SQLite database
DatabaseHandler <- R6Class("DatabaseHandler",
  public = list(
    #' @field db Database connection object
    db = NULL,

    #' @description
    #' Create a new DatabaseHandler object and connect to the database
    #' @param db_path Path to the SQLite database file
    initialize = function(db_path = "validation_history.db") {
      self$db <- dbConnect(RSQLite::SQLite(), db_path)
      self$create_tables()
    },

    #' @description
    #' Create the necessary database tables if they don't exist
    create_tables = function() {
      dbExecute(self$db, "CREATE TABLE IF NOT EXISTS plot_data (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                filepath TEXT,
                plot_code TEXT,
                sample_date TEXT,
                detector TEXT,
                region TEXT,
                validation_status TEXT,
                report_path TEXT,
                timestamp TEXT DEFAULT CURRENT_TIMESTAMP
              )")

      dbExecute(self$db, "CREATE TABLE IF NOT EXISTS images (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                plot_data_id INTEGER,
                image_path TEXT,
                FOREIGN KEY (plot_data_id) REFERENCES plot_data(id)
              )")
    },

    #' @description
    #' Add plot data to the database
    #' @param filepath Path to the Excel file
    #' @param plot_code Plot code
    #' @param sample_date Sample date
    #' @param detector Detector
    #' @param region Region
    #' @param validation_status Validation status
    #' @param report_path Path to the report
    add_plot_data = function(filepath, plot_code, sample_date, detector, region, validation_status, report_path) {
      dbExecute(self$db, "INSERT INTO plot_data (filepath, plot_code, sample_date, detector, region, validation_status, report_path) VALUES (?, ?, ?, ?, ?, ?, ?)",
                params = list(filepath, plot_code, sample_date, detector, region, validation_status, report_path))
      return(dbGetQuery(self$db, "SELECT last_insert_rowid()")[1, 1])
    },

    #' @description
    #' Add image data to the database
    #' @param plot_data_id ID of the associated plot data
    #' @param image_path Path to the image file
    add_image_data = function(plot_data_id, image_path) {
      dbExecute(self$db, "INSERT INTO images (plot_data_id, image_path) VALUES (?, ?)",
                params = list(plot_data_id, image_path))
    },

    #' @description
    #' Get plot history from the database with optional filtering
    #' @param plot_code Plot code to filter by (optional)
    #' @param from_date Start date for filtering (optional)
    #' @param to_date End date for filtering (optional)
    get_plot_history = function(plot_code = NULL, from_date = NULL, to_date = NULL) {
      query <- "SELECT * FROM plot_data WHERE 1=1"
      params <- list()
        
      if (!is.null(plot_code) && plot_code != "") {
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
        
      if (length(params) > 0) {
        dbGetQuery(self$db, query, params = params)
      } else {
        dbGetQuery(self$db, query)
      }
    },
    
    #' @description
    #' Update a plot data record in the database
    #' @param id ID of the record to update
    #' @param filepath New filepath
    #' @param plot_code New plot code
    #' @param sample_date New sample date
    #' @param detector New detector
    #' @param region New region
    #' @param validation_status New validation status
    #' @param report_path New report path
    update_plot_data = function(id, filepath, plot_code, sample_date, detector, region, validation_status, report_path) {
      dbExecute(self$db, "UPDATE plot_data SET filepath = ?, plot_code = ?, sample_date = ?, detector = ?, region = ?, validation_status = ?, report_path = ? WHERE id = ?",
                params = list(filepath, plot_code, sample_date, detector, region, validation_status, report_path, id))
    },

    #' @description
    #' Delete a plot data record and associated images from the database
    #' @param id ID of the record to delete
    delete_plot_data = function(id) {
      dbExecute(self$db, "DELETE FROM plot_data WHERE id = ?", params = list(id))
      dbExecute(self$db, "DELETE FROM images WHERE plot_data_id = ?", params = list(id)) # Delete associated images
    }
  )
)
