#' R/path_generation_class.R # nolint: commented_code_linter.
library(R6)
library(lubridate)

# Purpose:
# This file defines the `PathGenerator` class, which is responsible for creating organized directory
# structures and file paths based on plot data. It supports both Excel and CSV file formats.

# Documentation:
# - R6 Classes: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
# - lubridate: https://lubridate.tidyverse.org/

#' @title PathGenerator
#' @description Class for creating directory paths and file paths based on plot data.
#' Example:
#' ```
#' path_gen <- PathGenerator$new(base_path = "output/")
#' dir_path <- path_gen$generate("Plot1", "2023-05-10", "DetectorA", "RegionX")
#' file_path <- path_gen$generate_file_path(sheet1_data, file_type = "csv")
#' ```
PathGenerator <- R6Class(
  "PathGenerator",
  public = list(
    #' @field base_path Base path for generating directories
    base_path = NULL,
    
    #' @field path_format Format for directory structure (default: "date/region/detector/plot")
    path_format = NULL,
    
    #' @description
    #' Create a new PathGenerator object.
    #' @param base_path The base path for directory structure.
    #' @param path_format Format for directory structure (options:
    #'   "date/region/detector/plot", "region/date/detector/plot",
    #'   "detector/region/date/plot").
    #' Example:
    #' ```
    #' path_gen <- PathGenerator$new(base_path = "output/")
    #' ```
    initialize = function(base_path = getwd(),
                          path_format = "region/detector/date/plot") {
      if (!is.character(base_path) || length(base_path) != 1 || base_path == "") {
        stop("Base path must be a single non-empty character string")
      }
      
      self$base_path <- normalizePath(base_path, mustWork = FALSE)
      
      # Validate path format
      valid_formats <- c(
        "date/region/detector/plot", "region/date/detector/plot",
        "detector/region/date/plot"
      )
      if (!path_format %in% valid_formats) {
        warning("Invalid path format. Using default: 'date/region/detector/plot'")
        self$path_format <- "date/region/detector/plot"
      } else {
        self$path_format <- path_format
      }
    },
    
    #' @description
    #' Generate a directory path based on plot code, sample date, detector, and region.
    #' @param plot_code Plot code.
    #' @param sample_date Sample date (Date object or character string in YYYY-MM-DD format).
    #' @param detector Detector.
    #' @param region Region.
    #' @return Full path to the created directory.
    #' Example:
    #' ```
    #' dir_path <- path_gen$generate("Plot1", "2023-05-10", "DetectorA", "RegionX")
    #' ```
    generate = function(plot_code, sample_date, detector, region) {
      # Input validation
      if (missing(plot_code) || is.null(plot_code) || plot_code == "") {
        stop("Plot code is required")
      }
      
      if (missing(sample_date) || is.null(sample_date)) {
        stop("Sample date is required")
      }
      
      if (missing(detector) || is.null(detector) || detector == "") {
        stop("Detector is required")
      }
      
      if (missing(region) || is.null(region) || region == "") {
        stop("Region is required")
      }
      
      # Parse date if it's a string
      if (is.character(sample_date)) {
        tryCatch({
          sample_date <- as.Date(sample_date)
        }, error = function(e) {
          stop("Invalid date format. Please use YYYY-MM-DD format.")
        })
      }
      
      # Format date components
      year <- year(sample_date)
      month <- sprintf("%02d", month(sample_date))
      day <- sprintf("%02d", day(sample_date))
      date_folder <- file.path(year, month, day)
      
      # Create path based on selected format
      path <- switch(
        self$path_format,
        "date/region/detector/plot" = file.path(
          self$base_path, date_folder, region, detector, plot_code
        ),
        "region/date/detector/plot" = file.path(
          self$base_path, region, date_folder, detector, plot_code
        ),
        "detector/region/date/plot" = file.path(
          self$base_path, detector, region, date_folder, plot_code
        ),
        file.path(
          self$base_path, date_folder, region, detector, plot_code
        ) # Default fallback
      )
      
      # Create directory if it doesn't exist
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
      
      return(path)
    },
    
    #' @description
    #' Generate a path for a specific file type based on plot data.
    #' @param sheet1_data Sheet1Data object.
    #' @param file_type Type of file to create path for (excel or csv).
    #' @param file_name Optional custom file name (without extension).
    #' @return Full path to the file (including extension).
    #' Example:
    #' ```
    #' file_path <- path_gen$generate_file_path(sheet1_data, file_type = "csv")
    #' ```
    generate_file_path = function(sheet1_data, file_type = "excel",
                                  file_name = NULL) {
      if (!inherits(sheet1_data, "Sheet1Data")) {
        stop("sheet1_data must be a Sheet1Data object")
      }
      
      # Generate the directory path
      dir_path <- self$generate(
        plot_code = sheet1_data$Plot.code,
        sample_date = sheet1_data$Sample.date,
        detector = sheet1_data$Detector,
        region = sheet1_data$Region
      )
      
      # Create file name if not provided
      if (is.null(file_name)) {
        file_name <- paste0(
          "data_",
          sheet1_data$Plot.code,
          "_",
          format(sheet1_data$Sample.date, "%Y%m%d")
        )
      }
      
      # Add appropriate extension
      file_extension <- switch(
        tolower(file_type),
        "excel" = ".xlsx",
        "csv" = ".csv",
        ".xlsx" # Default fallback
      )
      
      file_path <- file.path(dir_path, paste0(file_name, file_extension))
      
      return(file_path)
    },
    
    #' @description
    #' Generate complementary species file path for CSV format.
    #' @param main_file_path Path to the main CSV file.
    #' @return Full path to the species file.
    #' Example:
    #' ```
    #' species_path <- path_gen$generate_species_file_path("data/Plot_Template_INFI2023.csv")
    #' ```
    generate_species_file_path = function(main_file_path) {
      if (!grepl("\\.csv$", main_file_path, ignore.case = TRUE)) {
        stop("Main file path must be a CSV file")
      }
      
      base_name <- tools::file_path_sans_ext(main_file_path)
      species_path <- paste0(base_name, "_species.csv")
      
      return(species_path)
    },
    
    #' @description
    #' Generate file paths for a CSV dataset.
    #' @param sheet1_data Sheet1Data object.
    #' @param create_dirs Whether to create directories.
    #' @return A list containing main_path and species_path.
    #' Example:
    #' ```
    #' paths <- path_gen$generate_csv_paths(sheet1_data)
    #' ```
    generate_csv_paths = function(sheet1_data, create_dirs = TRUE) {
      # Generate base directory path
      dir_path <- self$generate(
        plot_code = sheet1_data$Plot.code,
        sample_date = sheet1_data$Sample.date,
        detector = sheet1_data$Detector,
        region = sheet1_data$Region
      )
      
      # Create base filename
      base_name <- paste0(
        "data_",
        sheet1_data$Plot.code,
        "_",
        format(sheet1_data$Sample.date, "%Y%m%d")
      )
      
      # Generate paths
      main_path <- file.path(dir_path, paste0(base_name, ".csv"))
      species_path <- file.path(dir_path, paste0(base_name, "_species.csv"))
      
      # Create directories if requested
      if (create_dirs && !dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
      
      return(list(
        main_path = main_path,
        species_path = species_path
      ))
    }
  )
)