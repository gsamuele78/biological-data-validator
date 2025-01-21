# R/db_interaction_class.R (Continued from the previous response)

library(R6)
library(DBI)
library(RSQLite)

# Database interaction
DatabaseHandler <- R6Class("DatabaseHandler",
  public = list(
    db = NULL,

    initialize = function(db_path = "validation_history.db") {
      self$db <- dbConnect(RSQLite::SQLite(), db_path)
      self$create_tables()
    },

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

    add_plot_data = function(filepath, plot_code, sample_date, detector, region, validation_status, report_path) {
      dbExecute(self$db, "INSERT INTO plot_data (filepath, plot_code, sample_date, detector, region, validation_status, report_path) VALUES (?, ?, ?, ?, ?, ?, ?)",
                params = list(filepath, plot_code, sample_date, detector, region, validation_status, report_path))
      return(dbGetQuery(self$db, "SELECT last_insert_rowid()")[1, 1])
    },

    add_image_data = function(plot_data_id, image_path) {
      dbExecute(self$db, "INSERT INTO images (plot_data_id, image_path) VALUES (?, ?)",
                params = list(plot_data_id, image_path))
    },

    # Add filtering to get_plot_history
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

    # Update a record
    update_plot_data = function(id, filepath, plot_code, sample_date, detector, region, validation_status, report_path) {
      dbExecute(self$db, "UPDATE plot_data SET filepath = ?, plot_code = ?, sample_date = ?, detector = ?, region = ?, validation_status = ?, report_path = ? WHERE id = ?",
                params = list(filepath, plot_code, sample_date, detector, region, validation_status, report_path, id))
    },

    # Delete a record
    delete_plot_data = function(id) {
      dbExecute(self$db, "DELETE FROM plot_data WHERE id = ?", params = list(id))
      dbExecute(self$db, "DELETE FROM images WHERE plot_data_id = ?", params = list(id)) # Delete associated images
    }
  )
)
