# R/utils.R

# Utility functions for logging and other common tasks

#' @description
#' Set up logging for the application
#' @param log_level The logging level (e.g., "INFO", "DEBUG", "ERROR", "TRACE")
#' @param log_file Path to the log file
setup_logging <- function(log_level = "INFO", log_file = "validation.log") {
  # Check if logger package is installed, if not, install it
  if (!requireNamespace("logger", quietly = TRUE)) {
    install.packages("logger")
  }

  # Load the logger package
  library(logger)

  # Convert the log level string to an actual log level object
  log_level_obj <- switch(toupper(log_level),
                          "TRACE" = logger::TRACE,
                          "DEBUG" = logger::DEBUG,
                          "INFO" = logger::INFO,
                          "WARN" = logger::WARN,
                          "ERROR" = logger::ERROR,
                          "FATAL" = logger::FATAL,
                          logger::INFO) # Default to INFO

  # Configure logger
  logger::log_threshold(log_level_obj)
  logger::log_appender(logger::appender_file(log_file))

  # Log message to indicate that logging has started
  logger::log_info("Logging started with level {log_level}...")
}

#' @description
#' Log an INFO level message
#' @param msg The message to log
log_info <- function(msg) {
  # Check if logger package is installed
  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("The 'logger' package is required for logging. Please install it.")
  }
  
  # Log the message
  logger::log_info(msg)
}

#' @description
#' Handle image uploads and saving (used in Shiny app)
#' @param image_files List of uploaded image files
#' @param base_path Base path for saving images
handle_image_uploads <- function(image_files, base_path) {
    for (file in image_files) {
        # Assuming image_files is a data frame with 'datapath' and 'name' columns (from Shiny)
        new_path <- file.path(base_path, file$name) 
        file.copy(file$datapath, new_path)
    }
}

# Import necessary libraries (if not already imported)
library(logger)
library(readxl)

#' Load data from a CSV file.
#'
#' This function loads data from a CSV file into a data frame. It handles potential errors
#' during file reading and logs the outcome using the logger package.
#'
#' @param file_path The path to the CSV file.
#' @param description A brief description of the data being loaded. Used for logging.
#' @return A data frame containing the data from the CSV file, or NULL if an error occurred.
#' @examples
#' \dontrun{
#'   data <- load_csv_data("path/to/your/data.csv", "Customer data")
#'   if (!is.null(data)) {
#'     print(head(data))
#'   }
#' }
load_csv_data <- function(file_path, description = "CSV data") {
  log_info(paste("Loading", description, "from CSV file:", file_path))
  tryCatch({
    data <- read.csv(file_path)
    log_success(paste("Successfully loaded", description, "from CSV file:", file_path))
    return(data)
  }, error = function(e) {
    log_error(paste("Failed to load", description, "from CSV file:", file_path, "Error:", e$message))
    return(NULL)
  })
}


#' Load data from an Excel file (xlsx or xls).
#'
#' This function loads data from an Excel file (either .xlsx or .xls) into a data frame.
#' It uses the `readxl` package and handles potential errors during file reading.
#' The outcome is logged using the `logger` package.
#'
#' @param file_path The path to the Excel file.
#' @param description A brief description of the data being loaded. Used for logging.
#' @return A data frame containing the data from the Excel file.  Stops execution if an error occurs.
#' @examples
#' \dontrun{
#'   excel_data <- load_excel_data("path/to/your/data.xlsx", "Product catalog")
#'   print(head(excel_data))
#' }
load_excel_data <- function(file_path, description = "Excel data") {
  log_info(paste("Loading", description, "from Excel file:", file_path))

  # Check if file exists
  if (!file.exists(file_path)) {
    log_error(paste("File does not exist:", file_path))
    stop(paste("File does not exist:", file_path))
  }

  # Check if file has a valid extension
  if (!grepl("\\.(xlsx|xls)$", file_path)) {
    log_error(paste("Invalid file extension for:", file_path))
    stop(paste("Invalid file extension for:", file_path))
  }

  # Load Excel data using readxl package
  tryCatch(
    expr = {
      data <- readxl::read_excel(file_path) # Qualify with readxl:: to avoid potential conflicts
      log_success(paste("Successfully loaded", description, "from Excel file:", file_path))
      return(data)
    },
    error = function(e) {
      log_error(paste("Failed to load", description, "from Excel file:", file_path, "\nError:", e$message))
      stop(paste("Failed to load Excel file:", file_path, "\nError:", e$message)) # Stop execution on error
    }
  )
}


#' Load a list of valid values from a text file.
#'
#' This function reads a text file, where each line represents a valid value,
#' and returns a vector of these values. The function handles potential errors
#' during file reading and logs the outcome using the logger package.
#'
#' @param file_path The path to the text file containing valid values.
#' @param description A brief description of the data being loaded. Used for logging.
#' @return A character vector containing the valid values, or NULL if an error occurred.
#' @examples
#' \dontrun{
#'   valid_values <- load_valid_values("path/to/valid_values.txt", "Valid species names")
#'   print(head(valid_values))
#' }
load_valid_values <- function(file_path, description = "Valid values") {
  log_info(paste("Loading", description, "from text file:", file_path))
  tryCatch({
    valid_values <- readLines(file_path)
    log_success(paste("Successfully loaded", description, "from text file:", file_path))
    return(valid_values)
  }, error = function(e) {
    log_error(paste("Failed to load", description, "from text file:", file_path, "Error:", e$message))
    return(NULL)
  })
}
