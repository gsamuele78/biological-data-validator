# R/utils.R

# Utility functions for logging and data loading

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
  #library(logger)

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
#' Log a success message
#' @param msg The message to log
log_success <- function(msg) {
  # Check if logger package is installed
  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("The 'logger' package is required for logging. Please install it.")
  }
  
  # Log the message
  logger::log_success(msg)
}

#' @description
#' Log an error message
#' @param msg The message to log
log_error <- function(msg) {
  # Check if logger package is installed
  if (!requireNamespace("logger", quietly = TRUE)) {
    stop("The 'logger' package is required for logging. Please install it.")
  }
  
  # Log the message
  logger::log_error(msg)
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

# Import necessary libraries
library(logger)
library(readxl)
library(readr)
library(tools)

#' Auto-detect file type based on extension
#'
#' This function determines the file type based on the file extension.
#'
#' @param file_path The path to the file.
#' @return A string indicating the file type: "excel", "csv", or NULL if unsupported.
#' @examples
#' \dontrun{
#'   file_type <- detect_file_type("data.xlsx")
#'   print(file_type)  # "excel"
#' }
detect_file_type <- function(file_path) {
  file_ext <- tolower(tools::file_ext(file_path))
  
  if (file_ext %in% c("xlsx", "xls")) {
    return("excel")
  } else if (file_ext == "csv") {
    return("csv")
  } else {
    return(NULL)
  }
}

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
  logger::log_info(paste("Loading", description, "from CSV file:", file_path))
  
  # Check if file exists
  if (!file.exists(file_path)) {
    logger::log_error(paste("File does not exist:", file_path))
    return(NULL)
  }
  
  tryCatch({
    data <- readr::read_csv(file_path, col_types = readr::cols())
    logger::log_success(paste("Successfully loaded", description, "from CSV file:", file_path))
    return(as.data.frame(data))
  }, error = function(e) {
    logger::log_error(paste("Failed to load", description, "from CSV file:", file_path, "Error:", e$message))
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
#' @param sheet Sheet name or index to read (default is 1).
#' @param description A brief description of the data being loaded. Used for logging.
#' @return A data frame containing the data from the Excel file, or NULL if an error occurred.
#' @examples
#' \dontrun{
#'   excel_data <- load_excel_data("path/to/your/data.xlsx", 1, "Product catalog")
#'   print(head(excel_data))
#' }
load_excel_data <- function(file_path, sheet = 1, description = "Excel data") {
  logger::log_info(paste("Loading", description, "from Excel file:", file_path, "sheet:", sheet))

  # Check if file exists
  if (!file.exists(file_path)) {
    logger::log_error(paste("File does not exist:", file_path))
    return(NULL)
  }

  # Check if file has a valid extension
  if (!grepl("\\.(xlsx|xls)$", file_path)) {
    logger::log_error(paste("Invalid file extension for:", file_path))
    return(NULL)
  }

  # Load Excel data using readxl package
  tryCatch({
    data <- readxl::read_excel(file_path, sheet = sheet)
    logger::log_success(paste("Successfully loaded", description, "from Excel file:", file_path, "sheet:", sheet))
    return(as.data.frame(data))
  }, error = function(e) {
    logger::log_error(paste("Failed to load", description, "from Excel file:", file_path, "sheet:", sheet, "\nError:", e$message))
    return(NULL)
  })
}

#' Load data from a file (auto-detect file type).
#'
#' This function automatically detects the file type (CSV or Excel) and loads the data accordingly.
#' It handles potential errors during file reading and logs the outcome.
#'
#' @param file_path The path to the data file.
#' @param description A brief description of the data being loaded. Used for logging.
#' @param sheet Sheet name or index to read (for Excel files, default is 1).
#' @return A data frame containing the data from the file, or NULL if an error occurred.
#' @examples
#' \dontrun{
#'   data <- load_data("path/to/your/data.xlsx", "Product catalog")
#'   print(head(data))
#' }
load_data <- function(file_path, description = "data", sheet = 1) {
  file_type <- detect_file_type(file_path)
  
  if (is.null(file_type)) {
    logger::log_error(paste("Unsupported file type for:", file_path))
    return(NULL)
  }
  
  if (file_type == "excel") {
    return(load_excel_data(file_path, sheet, description))
  } else if (file_type == "csv") {
    return(load_csv_data(file_path, description))
  }
}

#' Create a DataSource object from a file.
#'
#' This function creates a DataSource object from a file, automatically detecting the file type.
#'
#' @param file_path The path to the data file.
#' @param file_type Optional file type override ("excel" or "csv"). If NULL, it will be auto-detected.
#' @return A DataSource object, or NULL if an error occurred.
#' @examples
#' \dontrun{
#'   data_source <- create_data_source("path/to/your/data.xlsx")
#'   if (!is.null(data_source)) {
#'     print(length(data_source$sheet1_data))
#'   }
#' }
create_data_source <- function(file_path, file_type = NULL) {
  logger::log_info(paste("Creating DataSource from file:", file_path))
  
  tryCatch({
    # Create DataSource object
    data_source <- DataSource$new(file_path, file_type)
    logger::log_success(paste("Successfully created DataSource from file:", file_path))
    return(data_source)
  }, error = function(e) {
    logger::log_error(paste("Failed to create DataSource from file:", file_path, "Error:", e$message))
    return(NULL)
  })
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
  logger::log_info(paste("Loading", description, "from text file:", file_path))
  
  # Check if file exists
  if (!file.exists(file_path)) {
    logger::log_error(paste("File does not exist:", file_path))
    return(NULL)
  }
  
  tryCatch({
    valid_values <- readLines(file_path)
    logger::log_success(paste("Successfully loaded", description, "from text file:", file_path))
    return(valid_values)
  }, error = function(e) {
    logger::log_error(paste("Failed to load", description, "from text file:", file_path, "Error:", e$message))
    return(NULL)
  })
}

#' Validate a data file and generate a report.
#'
#' This function validates a data file using the Validator class and generates a validation report.
#'
#' @param file_path The path to the data file to validate.
#' @param output_path Optional path to save the validation report.
#' @param validator A Validator object. If NULL, a new one will be created.
#' @return A data frame containing the validation results.
#' @examples
#' \dontrun{
#'   results <- validate_data_file("path/to/your/data.xlsx", "validation_report.csv")
#'   print(nrow(results))
#' }
validate_data_file <- function(file_path, output_path = NULL, validator = NULL) {
  logger::log_info(paste("Validating data file:", file_path))
  
  # Create a DataSource object
  data_source <- create_data_source(file_path)
  
  if (is.null(data_source)) {
    logger::log_error("Failed to create DataSource object")
    return(NULL)
  }
  
  # Create a validator if not provided
  if (is.null(validator)) {
    validator <- Validator$new(NULL)  # Assuming PathGenerator is optional
  }
  
  # Validate the data
  validation_results <- validator$generate_validation_report(data_source, output_path)
  
  # Log validation results
  num_errors <- nrow(validation_results)
  if (num_errors > 0) {
    logger::log_info(paste("Validation complete. Found", num_errors, "issues."))
  } else {
    logger::log_success("Validation complete. No issues found.")
  }
  
  return(validation_results)
}