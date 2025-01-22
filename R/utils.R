# R/utils.R

# Utility functions for logging and other common tasks

setup_logging <- function(log_level = "INFO", log_file = "validation.log") {
  # Configure logger
  logger::log_threshold(log_level)
  logger::log_appender(logger::appender_file(log_file))

  # Log message to indicate that logging has started
  log_info("Logging started...")
}
