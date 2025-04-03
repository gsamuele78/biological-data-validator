#' R/utils.R # nolint: commented_code_linter.

#' Utility functions for logging and data loading

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
library(readr)
library(tools)

#' Load data from CSV files.
#'
#' This function loads data from a CSV file pair (main and species) into data frames.
#'
#' @param file_path The path to the main CSV file.
#' @param description A brief description of the data being loaded.
#' @return A list containing two data frames (main and species), or NULL if error.
load_csv_data <- function(file_path, description = "CSV data") {
  logger::log_info(sprintf("Loading %s from: %s", description, file_path))
  
  # Check if main file exists
  if (!file.exists(file_path)) {
    logger::log_error(sprintf("Main CSV file not found: %s", file_path))
    return(NULL)
  }
  
  # Get species file path
  species_path <- paste0(tools::file_path_sans_ext(file_path), "_species.csv")
  if (!file.exists(species_path)) {
    logger::log_error(sprintf("Species CSV file not found: %s", species_path))
    return(NULL)
  }
  
  tryCatch({
    # Load both CSV files
    main_data <- readr::read_csv(file_path, col_types = readr::cols())
    species_data <- readr::read_csv(species_path, col_types = readr::cols())
    
    # Load field mappings
    source("R/csv_mapping.R")
    
    # Map CSV field names to internal names
    for (internal_name in names(SHEET1_CSV_MAPPING)) {
      csv_name <- SHEET1_CSV_MAPPING[[internal_name]]
      if (csv_name %in% names(main_data)) {
        names(main_data)[names(main_data) == csv_name] <- internal_name
      }
    }
    
    for (internal_name in names(SHEET2_CSV_MAPPING)) {
      csv_name <- SHEET2_CSV_MAPPING[[internal_name]]
      if (csv_name %in% names(species_data)) {
        names(species_data)[names(species_data) == csv_name] <- internal_name
      }
    }
    
    logger::log_success(sprintf("Successfully loaded %s", description))
    return(list(
      main_data = as.data.frame(main_data),
      species_data = as.data.frame(species_data)
    ))
  }, error = function(e) {
    logger::log_error(sprintf("Failed to load %s: %s", description, e$message))
    return(NULL)
  })
}

#' Create a DataSource object from CSV files.
#'
#' This function creates a DataSource object from a pair of CSV files.
#'
#' @param file_path The path to the main CSV file.
#' @return A DataSource object, or NULL if an error occurred.
create_data_source <- function(file_path) {
  logger::log_info(sprintf("Creating DataSource from: %s", file_path))
  
  tryCatch({
    data_source <- DataSource$new(file_path, "csv")
    logger::log_success("Successfully created DataSource")
    return(data_source)
  }, error = function(e) {
    logger::log_error(sprintf("Failed to create DataSource: %s", e$message))
    return(NULL)
  })
}

# Remove Excel-specific functions
load_excel_data <- NULL

#' Validate CSV data files and generate a report.
#'
#' This function validates CSV data files using the Validator class.
#'
#' @param file_path The path to the main CSV file.
#' @param output_path Optional path to save the validation report.
#' @param validator A Validator object. If NULL, a new one will be created.
#' @return A data frame containing the validation results.
validate_csv_files <- function(file_path, output_path = NULL, validator = NULL) {
  logger::log_info(sprintf("Validating CSV files: %s", file_path))
  
  data_source <- create_data_source(file_path)
  if (is.null(data_source)) {
    return(NULL)
  }
  
  if (is.null(validator)) {
    validator <- Validator$new(NULL)
  }
  
  validation_results <- validator$generate_validation_report(data_source, output_path)
  
  num_errors <- nrow(validation_results)
  if (num_errors > 0) {
    logger::log_info(sprintf("Found %d validation issues", num_errors))
  } else {
    logger::log_success("No validation issues found")
  }
  
  return(validation_results)
}

#' @description
#' Load the reference species list from the predefined path
#' @return A vector of species names
load_reference_species_list <- function() {
  reference_file <- file.path("inst", "extdata", "lists", "Lista_Riferimento_Species_Nomenclature.csv")
  if (!file.exists(reference_file)) {
    stop("Reference species list not found at: ", reference_file)
  }
  
  reference_species <- read.csv(reference_file, stringsAsFactors = FALSE)$species
  return(reference_species)
}

#' @description
#' Load the reference detector list for the current year
#' @return A vector of detector names
load_reference_detector_list <- function() {
  current_year <- format(Sys.Date(), "%Y")
  reference_file <- file.path("inst", "extdata", "lists", paste0("Lista_Riferimento_Rilevatori", current_year, ".csv"))
  if (!file.exists(reference_file)) {
    stop("Reference detector list not found at: ", reference_file)
  }
  
  reference_detectors <- read.csv(reference_file, stringsAsFactors = FALSE)$detector
  return(reference_detectors)
}

#' @description
#' Load the reference region list from the predefined path
#' @return A vector of region names
load_reference_region_list <- function() {
  reference_file <- file.path("inst", "extdata", "lists", "Lista_Riferimento_Regioni.csv")
  if (!file.exists(reference_file)) {
    stop("Reference region list not found at: ", reference_file)
  }
  
  reference_regions <- read.csv(reference_file, stringsAsFactors = FALSE)$region
  return(reference_regions)
}

#' @description
#' Load the reference codice list from the predefined path
#' @return A vector of codice values
load_reference_codice_list <- function() {
  reference_file <- file.path("inst", "extdata", "lists", "Lista_Riferimento_Codice.csv")
  if (!file.exists(reference_file) || reference_file == "") {
    stop("Reference codice list not found at: ", reference_file)
  }
  
  reference_codice <- read.csv(reference_file, stringsAsFactors = FALSE)$codice
  return(reference_codice)
}