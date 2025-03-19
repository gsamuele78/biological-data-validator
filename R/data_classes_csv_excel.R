# R/data_classes_csv_excel.R
library(R6)
library(openxlsx)
library(readxl)
library(readr)

##############################################
# Sheet1Data Class
##############################################

#' @title Sheet1Data Class
#' @description Represents a single row in Sheet 1 of the data file.
#'
#' This class stores environmental and site information extracted from the first sheet.
#'
#' @param data_row A named list or one-row data frame containing the following fields:
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
#'   \item \strong{Litter.cov}: Litter cover percentage (numeric, 0-100)
#'   \item \strong{Bare.soil.cov}: Bare soil cover percentage (numeric, 0-100)
#'   \item \strong{Tree.cov}: Tree cover percentage (numeric, 0-100)
#'   \item \strong{Tree.h}: Tree height in meters (numeric)
#'   \item \strong{Shrub.cov}: Shrub cover percentage (numeric, 0-100)
#'   \item \strong{Shrub.h}: Shrub height in meters (numeric)
#'   \item \strong{Herb.cov}: Herb cover percentage (numeric, 0-100)
#'   \item \strong{Herb.h}: Herb height in meters (numeric)
#'   \item \strong{Brioph.cov}: Bryophyte cover percentage (numeric, 0-100)
#'   \item \strong{notes}: Additional notes (string)
#' }
#'
#' @return A new instance of \code{Sheet1Data}.
#'
#' @examples
#' row <- list(Plot.code = "A1", SU = 1, Sample.date = "2023-05-10",
#'             Detector = "ABC", X = 12.34, Y = 56.78, Region = "North",
#'             Elevation = 100, Aspect = 180, Slope = 15, Cop.tot = 90,
#'             Litter.cov = 20, Bare.soil.cov = 10, Tree.cov = 70, Tree.h = 12,
#'             Shrub.cov = 30, Shrub.h = 3, Herb.cov = 40, Herb.h = 1,
#'             Brioph.cov = 5, notes = "Sample note")
#' obj <- Sheet1Data$new(row)
Sheet1Data <- R6Class("Sheet1Data",
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
    #' @field Litter.cov Litter cover percentage (numeric, 0-100)
    Litter.cov = NULL,
    #' @field Bare.soil.cov Bare soil cover percentage (numeric, 0-100)
    Bare.soil.cov = NULL,
    #' @field Tree.cov Tree cover percentage (numeric, 0-100)
    Tree.cov = NULL,
    #' @field Tree.h Tree height (numeric, in meters)
    Tree.h = NULL,
    #' @field Shrub.cov Shrub cover percentage (numeric, 0-100)
    Shrub.cov = NULL,
    #' @field Shrub.h Shrub height (numeric, in meters)
    Shrub.h = NULL,
    #' @field Herb.cov Herb cover percentage (numeric, 0-100)
    Herb.cov = NULL,
    #' @field Herb.h Herb height (numeric, in meters)
    Herb.h = NULL,
    #' @field Brioph.cov Bryophyte cover percentage (numeric, 0-100)
    Brioph.cov = NULL,
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
      self$Litter.cov <- as.numeric(data_row[["Litter.cov"]])
      self$Bare.soil.cov <- as.numeric(data_row[["Bare.soil.cov"]])
      self$Tree.cov <- as.numeric(data_row[["Tree.cov"]])
      self$Tree.h <- as.numeric(data_row[["Tree.h"]])
      self$Shrub.cov <- as.numeric(data_row[["Shrub.cov"]])
      self$Shrub.h <- as.numeric(data_row[["Shrub.h"]])
      self$Herb.cov <- as.numeric(data_row[["Herb.cov"]])
      self$Herb.h <- as.numeric(data_row[["Herb.h"]])
      self$Brioph.cov <- as.numeric(data_row[["Brioph.cov"]])
      self$notes <- as.character(data_row[["notes"]])
    },
    #' @description Convert the Sheet1Data object to a data frame.
    #' @return A data frame containing a single row with the object's data.
    #' @examples
    #' df <- obj$to_data_frame()\nprint(df)
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
        Litter.cov = self$Litter.cov,
        Bare.soil.cov = self$Bare.soil.cov,
        Tree.cov = self$Tree.cov,
        Tree.h = self$Tree.h,
        Shrub.cov = self$Shrub.cov,
        Shrub.h = self$Shrub.h,
        Herb.cov = self$Herb.cov,
        Herb.h = self$Herb.h,
        Brioph.cov = self$Brioph.cov,
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
#' This class stores species and cover information extracted from the second sheet.
#'
#' @param data_row A named list or one-row data frame containing the following fields:
#' \itemize{
#'   \item \strong{Plot.code}: Plot code (alphanumeric string)
#'   \item \strong{Subplot}: Subplot number (numeric, expected range 1-4)
#'   \item \strong{Species}: Species name (string)
#'   \item \strong{species_abb}: Species abbreviation (string)
#'   \item \strong{cover}: Cover percentage (numeric, 0-100)
#'   \item \strong{Layer}: Layer (string)
#'   \item \strong{Notes}: Notes (string)
#' }
#'
#' @return A new instance of \code{Sheet2Data}.
#'
#' @examples
#' row2 <- list(Plot.code = "A1", Subplot = 2, Species = "Sp1",
#'              species_abb = "S1", cover = 80, Layer = "Understory", Notes = "Healthy")
#' obj2 <- Sheet2Data$new(row2)
Sheet2Data <- R6Class("Sheet2Data",
  public = list(
    #' @field Plot.code Plot code (alphanumeric string)
    Plot.code = NULL,
    #' @field Subplot Subplot number (numeric, range 1-4)
    Subplot = NULL,
    #' @field Species Species name (string)
    Species = NULL,
    #' @field species_abb Species abbreviation (string)
    species_abb = NULL,
    #' @field cover Cover percentage (numeric, 0-100)
    cover = NULL,
    #' @field Layer Layer (string)
    Layer = NULL,
    #' @field Notes Notes (string)
    Notes = NULL,
    
    initialize = function(data_row) {
      self$Plot.code <- as.character(data_row[["Plot.code"]])
      self$Subplot <- as.numeric(data_row[["Subplot"]])
      self$Species <- as.character(data_row[["Species"]])
      self$species_abb <- as.character(data_row[["species_abb"]])
      self$cover <- as.numeric(data_row[["cover"]])
      self$Layer <- as.character(data_row[["Layer"]])
      self$Notes <- as.character(data_row[["Notes"]])
    },
    #' @description Convert the Sheet2Data object to a data frame.
    #' @return A data frame containing a single row with the object's data.
    #' @examples
    #' df2 <- obj2$to_data_frame()\nhead(df2)
    to_data_frame = function() {
      data.frame(
        Plot.code = self$Plot.code,
        Subplot = self$Subplot,
        Species = self$Species,
        species_abb = self$species_abb,
        cover = self$cover,
        Layer = self$Layer,
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
#' @description Represents and handles loading and exporting data from CSV or Excel files.
#'
#' This class provides methods to load data from a file (either Excel or CSV) and export the data to Excel or CSV.
#'
#' @param filepath A string specifying the path to the data file.
#' @param file_type Optional. Specify \"excel\" or \"csv\". If NULL, the file type is auto-detected based on the file extension.
#'
#' @return A new instance of \code{DataSource} with the loaded data stored in \code{sheet1_data} and \code{sheet2_data}.
#'
#' @examples
#' # For an Excel file:
#' ds_excel <- DataSource$new("data.xlsx")
#' # For a CSV file (auto-detected):
#' ds_csv <- DataSource$new("data.csv")
DataSource <- R6Class("DataSource",
  public = list(
    #' @field sheet1_data A list of Sheet1Data objects.
    sheet1_data = list(),
    #' @field sheet2_data A list of Sheet2Data objects.
    sheet2_data = list(),
    #' @field filepath Path to the data file.
    filepath = NULL,
    #' @field file_type Type of the data file (\"excel\" or \"csv\").
    file_type = NULL,
    
    initialize = function(filepath, file_type = NULL) {
      self$filepath <- filepath
      
      # Auto-detect file type if not specified
      if (is.null(file_type)) {
        file_ext <- tolower(tools::file_ext(filepath))
        if (file_ext %in% c("xlsx", "xls")) {
          self$file_type <- "excel"
        } else if (file_ext == "csv") {
          self$file_type <- "csv"
        } else {
          stop("Unsupported file type. Please specify 'excel' or 'csv'.")
        }
      } else {
        self$file_type <- tolower(file_type)
        if (!self$file_type %in% c("excel", "csv")) {
          stop("Unsupported file type. Please specify 'excel' or 'csv'.")
        }
      }
      
      self$load_data()
    },
    
    #' @description Loads data from the file specified by \code{filepath}.
    #' @details This method calls the appropriate data loading function based on the file type.
    #' @return No return value. The data is loaded into \code{sheet1_data} and \code{sheet2_data}.
    #' @examples
    #' ds <- DataSource$new("data.xlsx")\n# Data is automatically loaded into ds$sheet1_data and ds$sheet2_data.
    load_data = function() {
      if (self$file_type == "excel") {
        self$load_excel_data()
      } else if (self$file_type == "csv") {
        self$load_csv_data()
      }
    },
    
    #' @description Loads data from an Excel file.
    #' @details Reads the first two sheets of the Excel file and creates \code{Sheet1Data} and \code{Sheet2Data} objects for each row.
    #' @return No return value. Populates \code{sheet1_data} and \code{sheet2_data}.
    #' @examples
    #' ds <- DataSource$new("data.xlsx")\n# ds$load_excel_data() is called automatically.
    load_excel_data = function() {
      sheet1 <- openxlsx::read.xlsx(self$filepath, sheet = 1)
      sheet2 <- openxlsx::read.xlsx(self$filepath, sheet = 2)
      
      for (i in seq_len(nrow(sheet1))) {
        self$sheet1_data[[i]] <- Sheet1Data$new(sheet1[i, ])
      }
      
      for (i in seq_len(nrow(sheet2))) {
        self$sheet2_data[[i]] <- Sheet2Data$new(sheet2[i, ])
      }
    },
    
    #' @description Loads data from CSV files.
    #' @details For CSV data, this method expects two files: one main CSV file for Sheet1 data and a second CSV file for Sheet2 data. The second file is assumed to have a \"_species.csv\" suffix appended to the base name of the main file.
    #' @return No return value. Populates \code{sheet1_data} and \code{sheet2_data}.
    #' @examples
    #' ds <- DataSource$new("data.csv")\n# ds$load_csv_data() is called automatically.
    load_csv_data = function() {
      # For CSV, we expect two files: main_file.csv and main_file_species.csv
      base_name <- tools::file_path_sans_ext(self$filepath)
      
      # Load Sheet1 data (main file)
      sheet1_path <- self$filepath
      if (!file.exists(sheet1_path)) {
        stop("Sheet1 CSV file not found: ", sheet1_path)
      }
      sheet1 <- readr::read_csv(sheet1_path, col_types = readr::cols())
      
      # Load Sheet2 data (species file)
      sheet2_path <- paste0(base_name, "_species.csv")
      if (!file.exists(sheet2_path)) {
        warning("Sheet2 CSV file not found: ", sheet2_path)
        sheet2 <- data.frame()
      } else {
        sheet2 <- readr::read_csv(sheet2_path, col_types = readr::cols())
      }
      
      # Convert to data frames to ensure consistent behavior
      sheet1 <- as.data.frame(sheet1)
      sheet2 <- as.data.frame(sheet2)
      
      # Create Sheet1Data objects
      for (i in seq_len(nrow(sheet1))) {
        self$sheet1_data[[i]] <- Sheet1Data$new(sheet1[i, ])
      }
      
      # Create Sheet2Data objects
      for (i in seq_len(nrow(sheet2))) {
        self$sheet2_data[[i]] <- Sheet2Data$new(sheet2[i, ])
      }
    },
    
    #' @description Exports the loaded data to an Excel file.
    #' @param output_path A string specifying the path where the Excel file will be saved.
    #' @return Returns TRUE if the export is successful.
    #' @examples
    #' ds <- DataSource$new("data.xlsx")\n# Export to Excel file:\n# ds$export_to_excel(\"output.xlsx\")\nresult <- ds$export_to_excel(\"output.xlsx\")\nprint(result)
    export_to_excel = function(output_path) {
      # Convert data to data frames
      sheet1_df <- do.call(rbind, lapply(self$sheet1_data, function(x) x$to_data_frame()))
      sheet2_df <- do.call(rbind, lapply(self$sheet2_data, function(x) x$to_data_frame()))
      
      # Create a new workbook
      wb <- openxlsx::createWorkbook()
      
      # Add sheets
      openxlsx::addWorksheet(wb, "Sheet1")
      openxlsx::addWorksheet(wb, "Sheet2")
      
      # Write data to sheets
      openxlsx::writeData(wb, "Sheet1", sheet1_df)
      openxlsx::writeData(wb, "Sheet2", sheet2_df)
      
      # Save workbook
      openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
      
      return(TRUE)
    },
    
    #' @description Exports the loaded data to CSV files.
    #' @param output_path A string specifying the path to save the main CSV file.
    #' @return Returns TRUE if the export is successful.
    #' @details This method writes the main CSV file for Sheet1 data and a separate CSV file for Sheet2 data with a \"_species.csv\" suffix appended to the base name.
    #' @examples
    #' ds <- DataSource$new("data.csv")\n# Export to CSV files:\n# ds$export_to_csv(\"output.csv\")\nresult <- ds$export_to_csv(\"output.csv\")\nprint(result)
    export_to_csv = function(output_path) {
      # Convert data to data frames
      sheet1_df <- do.call(rbind, lapply(self$sheet1_data, function(x) x$to_data_frame()))
      sheet2_df <- do.call(rbind, lapply(self$sheet2_data, function(x) x$to_data_frame()))
      
      # Get base name for species file
      base_name <- tools::file_path_sans_ext(output_path)
      species_path <- paste0(base_name, "_species.csv")
      
      # Write data to CSV files
      readr::write_csv(sheet1_df, output_path)
      readr::write_csv(sheet2_df, species_path)
      
      return(TRUE)
    }
  )
)