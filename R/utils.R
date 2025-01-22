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
