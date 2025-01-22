# R/utils.R

# Utility functions for logging and other common tasks

#' @description
#' Set up logging for the application
#' @param log_level The logging level (e.g., "INFO", "DEBUG", "ERROR")
#' @param log_file Path to the log file
setup_logging <- function(log_level = "INFO", log_file = "validation.log") {
  # Configure logger
  logger::log_threshold(log_level)
  logger::log_appender(logger::appender_file(log_file))

  # Log message to indicate that logging has started
  log_info("Logging started...")
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
