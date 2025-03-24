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
    #' Get images associated with a plot data record
    #' @param plot_data_id ID of the plot data record
    get_images = function(plot_data_id) {
      dbGetQuery(self$db, "SELECT * FROM images WHERE plot_data_id = ?", 
                 params = list(plot_data_id))
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
      dbExecute(self$db, "DELETE FROM images WHERE plot_data_id = ?", params = list(id)) # Delete associated images first
      dbExecute(self$db, "DELETE FROM plot_data WHERE id = ?", params = list(id))
    },
    
    #' @description
    #' Process and store data from a DataSource object
    #' @param data_source A DataSource object containing the ecological data
    #' @param validation_status The validation status of the data (e.g., "valid", "invalid", "pending")
    #' @param report_path Optional path to a validation report file
    #' @param image_paths Optional vector of image file paths associated with the plot data
    #' @return The ID of the newly created database record
    process_data_source = function(data_source, validation_status, report_path = NULL, image_paths = NULL) {
      # Extract the first sheet1 data record for metadata
      if (length(data_source$sheet1_data) == 0) {
        stop("No data found in the DataSource object")
      }
      
      # Get the first record for metadata
      first_record <- data_source$sheet1_data[[1]]
      
      # Extract key metadata fields
      filepath <- data_source$filepath
      plot_code <- first_record$Plot.code
      sample_date <- as.character(first_record$Sample.date)
      detector <- first_record$Detector
      region <- first_record$Region
      
      # Add plot data to database
      plot_data_id <- self$add_plot_data(
        filepath = filepath,
        plot_code = plot_code,
        sample_date = sample_date,
        detector = detector,
        region = region,
        validation_status = validation_status,
        report_path = report_path
      )
      
      # Add associated images if provided
      if (!is.null(image_paths) && length(image_paths) > 0) {
        for (image_path in image_paths) {
          if (file.exists(image_path)) {
            self$add_image_data(plot_data_id, image_path)
          } else {
            warning(paste("Image file not found:", image_path))
          }
        }
      }
      
      # Return the ID of the newly created record
      return(plot_data_id)
    },
    
    #' @description
    #' Get plot history with associated images
    #' @param plot_code Optional plot code to filter by
    #' @param from_date Optional start date for filtering
    #' @param to_date Optional end date for filtering
    #' @return A list containing plot data and associated images
    get_plot_history_with_images = function(plot_code = NULL, from_date = NULL, to_date = NULL) {
      # Get plot history
      plot_history <- self$get_plot_history(plot_code, from_date, to_date)
      
      # If there's no history, return empty list
      if (nrow(plot_history) == 0) {
        return(list(plot_data = data.frame(), images = data.frame()))
      }
      
      # Get associated images for each plot data record
      image_data <- data.frame(plot_data_id = integer(), image_path = character(), stringsAsFactors = FALSE)
      
      for (id in plot_history$id) {
        images <- self$get_images(id)
        
        if (nrow(images) > 0) {
          image_data <- rbind(image_data, images)
        }
      }
      
      return(list(plot_data = plot_history, images = image_data))
    },
    
    #' @description
    #' Close the database connection
    #' @return TRUE if successful
    close = function() {
      if (!is.null(self$db)) {
        dbDisconnect(self$db)
        self$db <- NULL
        return(TRUE)
      }
      return(FALSE)
    }
  )
)

#' @title Validate and Store Data
#' @description Function to validate ecological data and store the results in the database
#' @param filepath Path to the data file (Excel or CSV)
#' @param db_path Path to the SQLite database file
#' @param validation_function A function that takes a DataSource object and returns a list with 
#'        validation_status and report_path
#' @param image_paths Optional vector of image file paths associated with the plot data
#' @return The ID of the newly created database record
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
#' @description Function to retrieve plot history along with associated images
#' @param db_path Path to the SQLite database file
#' @param plot_code Optional plot code to filter by
#' @param from_date Optional start date for filtering
#' @param to_date Optional end date for filtering
#' @return A list containing plot data and associated images
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