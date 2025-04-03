# nolint: commented_code_linter. #' R/data_classes.R 
library(R6)
library(openxlsx)
library(readxl)
library(readr)
library(tools)

#' Load CSV mapping configuration
source("R/csv_mapping.R")

#' @import R6
NULL

#' Data Validator Class
#'
#' @description Base class for data validation
#' @export
DataValidator <- R6::R6Class(
  "DataValidator",
  public = list(
    #' @field data The data to validate
    data = NULL,

    #' @description Initialize a new data validator
    #' @param data Data to validate
    initialize = function(data = NULL) {
      self$data <- data
    },

    #' @description Validate the data
    validate = function() {
      stop("Method validate() must be implemented by subclass")
    }
  )
)

##############################################
# Sheet1Data Class
##############################################

#' @title Sheet1Data Class
#' @description Represents a single row in Sheet 1 of the data file.
#'
#' This class stores environmental and site information extracted from the first
#' sheet.
#'
#' @param data_row A named list or one-row data frame containing the following
#'   fields:
#' \itemize{
#'   \item \strong{Plot.code}: Plot code (alphanumeric string)
#'   \item \strong{SU}: Sampling Unit (numeric, expected range 1-4)
#'   \item \strong{Sample.date}: Sample date (in a recognizable date format)
#'   \item \strong{Detector}: Detector name (string)
#'   \item \strong{X}: Longitude (numeric, WGS84)
#'   \item \strong{Y}: Latitude (numeric, WGS84)
#'   \item \strong{Region}: Region name (string)
#'   \item \strong{Elevation}: Elevation in meters (numeric)
#'   \item \strong{Aspect}: Sun exposition in degrees (numeric)
#'   \item \strong{Slope}: Soil inclination in degrees (numeric)
#'   \item \strong{Cop.tot}: Total cover percentage (numeric, 0-100)
#'   \item \strong{Tree.cov}: Tree cover percentage (numeric, 0-100)
#'   \item \strong{Shrub.cov}: Shrub cover percentage (numeric, 0-100)
#'   \item \strong{Herb.cov}: Herb cover percentage (numeric, 0-100)
#'   \item \strong{Brioph.cov}: Bryophyte cover percentage (numeric, 0-100)
#'   \item \strong{Bare.soil.cov}: Bare soil cover percentage (numeric, 0-100)
#'   \item \strong{Litter.cov}: Litter cover percentage (numeric, 0-100)
#'   \item \strong{notes}: Additional notes (string)
#' }
#'
#' @return A new instance of \code{Sheet1Data}.
#'
#' @examples
#' row <- list(Plot.code = "A1", SU = 1, Sample.date = "2023-05-10",
#'             Detector = "ABC", X = 12.34, Y = 56.78, Region = "North",
#'             Elevation = 100, Aspect = 180, Slope = 15, Cop.tot = 90,
#'             Tree.cov = 70, Shrub.cov = 30, Herb.cov = 40,
#'             Brioph.cov = 5, Bare.soil.cov = 10, Litter.cov = 20,
#'             notes = "Sample note")
#' obj <- Sheet1Data$new(row)
Sheet1Data <- R6Class(
  "Sheet1Data",
  public = list(
    #' @field Plot.code Plot code (alphanumeric string)
    Plot.code = NULL,
    #' @field SU Sampling Unit (numeric, range 1-4)
    SU = NULL,
    #' @field Sample.date Sample date (Date object)
    Sample.date = NULL,
    #' @field Detector Detector name (string)
    Detector = NULL,
    #' @field X Longitude (numeric, WGS84)
    X = NULL,
    #' @field Y Latitude (numeric, WGS84)
    Y = NULL,
    #' @field Region Region name (string)
    Region = NULL,
    #' @field Elevation Elevation in meters (numeric)
    Elevation = NULL,
    #' @field Aspect Sun exposition in degrees (numeric)
    Aspect = NULL,
    #' @field Slope Soil inclination in degrees (numeric)
    Slope = NULL,
    #' @field Cop.tot Total cover percentage (numeric, 0-100)
    Cop.tot = NULL,
    #' @field Tree.cov Tree cover percentage (numeric, 0-100)
    Tree.cov = NULL,
    #' @field Shrub.cov Shrub cover percentage (numeric, 0-100)
    Shrub.cov = NULL,
    #' @field Herb.cov Herb cover percentage (numeric, 0-100)
    Herb.cov = NULL,
    #' @field Brioph.cov Bryophyte cover percentage (numeric, 0-100)
    Brioph.cov = NULL,
    #' @field Bare.soil.cov Bare soil cover percentage (numeric, 0-100)
    Bare.soil.cov = NULL,
    #' @field Litter.cov Litter cover percentage (numeric, 0-100)
    Litter.cov = NULL,
    #' @field notes Additional notes (string)
    notes = NULL,

    initialize = function(data_row) {
      self$Plot.code <- as.character(data_row[["Plot.code"]])
      self$SU <- as.numeric(data_row[["SU"]])
      self$Sample.date <- as.Date(data_row[["Sample.date"]])
      self$Detector <- as.character(data_row[["Detector"]])
      self$X <- as.numeric(data_row[["X"]])
      self$Y <- as.numeric(data_row[["Y"]])
      self$Region <- as.character(data_row[["Region"]])
      self$Elevation <- as.numeric(data_row[["Elevation"]])
      self$Aspect <- as.numeric(data_row[["Aspect"]])
      self$Slope <- as.numeric(data_row[["Slope"]])
      self$Cop.tot <- as.numeric(data_row[["Cop.tot"]])
      self$Tree.cov <- as.numeric(data_row[["Tree.cov"]])
      self$Shrub.cov <- as.numeric(data_row[["Shrub.cov"]])
      self$Herb.cov <- as.numeric(data_row[["Herb.cov"]])
      self$Brioph.cov <- as.numeric(data_row[["Brioph.cov"]])
      self$Bare.soil.cov <- as.numeric(data_row[["Bare.soil.cov"]])
      self$Litter.cov <- as.numeric(data_row[["Litter.cov"]])
      self$notes <- as.character(data_row[["notes"]])
    },

    #' @description Convert the Sheet1Data object to a data frame.
    #' @return A data frame containing a single row with the object's data.
    #' @examples
    #' df <- obj$to_data_frame()
    #' print(df)
    to_data_frame = function() {
      data.frame(
        Plot.code = self$Plot.code,
        SU = self$SU,
        Sample.date = as.character(self$Sample.date),
        Detector = self$Detector,
        X = self$X,
        Y = self$Y,
        Region = self$Region,
        Elevation = self$Elevation,
        Aspect = self$Aspect,
        Slope = self$Slope,
        Cop.tot = self$Cop.tot,
        Tree.cov = self$Tree.cov,
        Shrub.cov = self$Shrub.cov,
        Herb.cov = self$Herb.cov,
        Brioph.cov = self$Brioph.cov,
        Bare.soil.cov = self$Bare.soil.cov,
        Litter.cov = self$Litter.cov,
        notes = self$notes,
        stringsAsFactors = FALSE
      )
    }
  )
)

##############################################
# Sheet2Data Class
##############################################

#' @title Sheet2Data Class
#' @description Represents a single row in Sheet 2 of the data file.
#'
#' This class stores species and cover information extracted from the second
#' sheet.
#'
#' @param data_row A named list or one-row data frame containing the following
#'   fields:
#' \itemize{
#'   \item \strong{Plot.code}: Plot code (alphanumeric string)
#'   \item \strong{Subplot}: Subplot number (numeric, expected range 1-4)
#'   \item \strong{Layer}: Layer (string)
#'   \item \strong{Species}: Species name (string)
#'   \item \strong{cover}: Cover percentage (numeric, 0-100)
#'   \item \strong{Notes}: Notes (string)
#' }
#'
#' @return A new instance of \code{Sheet2Data}.
#'
#' @examples
#' row2 <- list(Plot.code = "A1", Subplot = 2, Layer = "Tree",
#'              Species = "Quercus ilex", cover = 80,
#'              Notes = "Healthy specimen")
#' obj2 <- Sheet2Data$new(row2)
Sheet2Data <- R6Class(
  "Sheet2Data",
  public = list(
    #' @field Plot.code Plot code (alphanumeric string)
    Plot.code = NULL,
    #' @field Subplot Subplot number (numeric, range 1-4)
    Subplot = NULL,
    #' @field Layer Layer (string)
    Layer = NULL,
    #' @field Species Species name (string)
    Species = NULL,
    #' @field cover Cover percentage (numeric, 0-100)
    cover = NULL,
    #' @field Notes Notes (string)
    Notes = NULL,

    initialize = function(data_row) {
      self$Plot.code <- as.character(data_row[["Plot.code"]])
      self$Subplot <- as.numeric(data_row[["Subplot"]])
      self$Layer <- as.character(data_row[["Layer"]])
      self$Species <- as.character(data_row[["Species"]])
      self$cover <- as.numeric(data_row[["cover"]])
      self$Notes <- as.character(data_row[["Notes"]])
    },

    #' @description Convert the Sheet2Data object to a data frame.
    #' @return A data frame containing a single row with the object's data.
    #' @examples
    #' df2 <- obj2$to_data_frame()
    #' head(df2)
    to_data_frame = function() {
      data.frame(
        Plot.code = self$Plot.code,
        Subplot = self$Subplot,
        Layer = self$Layer,
        Species = self$Species,
        cover = self$cover,
        Notes = self$Notes,
        stringsAsFactors = FALSE
      )
    }
  )
)

##############################################
# DataSource Class
##############################################

#' @title DataSource Class
#' @description Represents and handles loading and exporting CSV data files.
DataSource <- R6Class(
  "DataSource",
  public = list(
    #' @field sheet1_data A list of Sheet1Data objects.
    sheet1_data = list(),
    #' @field sheet2_data A list of Sheet2Data objects.
    sheet2_data = list(),
    #' @field filepath Path to the data file.
    filepath = NULL,
    #' @field file_type Type of the data file (always "csv").
    file_type = "csv",
    #' @field species_filepath Path to the species data file.
    species_filepath = NULL,

    initialize = function(filepath) {
      self$filepath <- filepath
      self$validate_filenames()
      self$load_data()
    },

    validate_filenames = function() {
      main_file <- basename(self$filepath)
      plot_pattern <- CSVFilenamePatterns$plot_pattern
      species_pattern <- CSVFilenamePatterns$species_pattern

      # Validate main file format
      if (!grepl(paste0("^", plot_pattern, "$"), main_file, ignore.case = TRUE)) {
        stop(
          paste(
            "Invalid main file name format. Expected: Plot_Template_INFIYYYY.csv, Got:",
            main_file
          )
        )
      }

      # Extract year from main file
      year <- gsub(plot_pattern, "\\1", main_file)

      # Derive species file name
      expected_species_file <- paste0("Species_Template_INFI", year, ".csv")
      species_path <- file.path(dirname(self$filepath), expected_species_file)

      # Validate species file format
      if (!file.exists(species_path)) {
        stop(
          paste(
            "Species file not found or invalid format. Expected:",
            expected_species_file
          )
        )
      }

      self$species_filepath <- species_path
    },

    load_data = function() {
      # Check if main file exists
      if (!file.exists(self$filepath)) {
        stop(paste("Main CSV file not found:", self$filepath))
      }

      # Read CSV files
      sheet1 <- tryCatch(
        readr::read_csv(self$filepath, col_types = readr::cols()),
        error = function(e) stop("Error reading main CSV file:", e$message)
      )

      sheet2 <- tryCatch(
        readr::read_csv(self$species_filepath, col_types = readr::cols()),
        error = function(e) stop("Error reading species CSV file:", e$message)
      )

      # Ensure mappings exist before using them
      if (!exists("SHEET1_CSV_MAPPING") || !exists("SHEET2_CSV_MAPPING")) {
        stop("CSV mappings (SHEET1_CSV_MAPPING or SHEET2_CSV_MAPPING) are not defined.")
      }

      # Map CSV field names to internal names using csv_mapping.R
      for (internal_name in names(SHEET1_CSV_MAPPING)) {
        csv_name <- SHEET1_CSV_MAPPING[[internal_name]]
        if (csv_name %in% names(sheet1)) {
          names(sheet1)[names(sheet1) == csv_name] <- internal_name
        }
      }

      for (internal_name in names(SHEET2_CSV_MAPPING)) {
        csv_name <- SHEET2_CSV_MAPPING[[internal_name]]
        if (csv_name %in% names(sheet2)) {
          names(sheet2)[names(sheet2) == csv_name] <- internal_name
        }
      }

      # Create data objects
      for (i in seq_len(nrow(sheet1))) {
        self$sheet1_data[[i]] <- Sheet1Data$new(sheet1[i, ])
      }

      for (i in seq_len(nrow(sheet2))) {
        self$sheet2_data[[i]] <- Sheet2Data$new(sheet2[i, ])
      }
    },

    export_data = function(output_path) {
      # Convert data to data frames
      sheet1_df <- do.call(
        rbind,
        lapply(self$sheet1_data, function(x) x$to_data_frame())
      )
      sheet2_df <- do.call(
        rbind,
        lapply(self$sheet2_data, function(x) x$to_data_frame())
      )

      # Map internal names back to CSV names
      for (internal_name in names(SHEET1_CSV_MAPPING)) {
        csv_name <- SHEET1_CSV_MAPPING[[internal_name]]
        names(sheet1_df)[names(sheet1_df) == internal_name] <- csv_name
      }

      for (internal_name in names(SHEET2_CSV_MAPPING)) {
        csv_name <- SHEET2_CSV_MAPPING[[internal_name]]
        names(sheet2_df)[names(sheet2_df) == internal_name] <- csv_name
      }

      # Write CSV files
      species_path <- paste0(tools::file_path_sans_ext(output_path), "_species.csv")
      readr::write_csv(sheet1_df, output_path)
      readr::write_csv(sheet2_df, species_path)
    }
  )
)