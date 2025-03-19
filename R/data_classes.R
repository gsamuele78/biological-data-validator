#' @title Data Classes for Biological Data Validation
#' @description
#' This file contains R6 classes for representing and processing rows of data from an Excel file
#' used in biological environment field studies. It includes classes for:
#' \itemize{
#'   \item \code{Sheet1Data}: Represents a single row from Sheet 1.
#'   \item \code{Sheet2Data}: Represents a single row from Sheet 2.
#'   \item \code{ExcelData}: Loads and holds data from both sheets.
#' }
#'
#' @details
#' Each class uses R6 for object-oriented programming and includes methods to convert the stored data into a data frame.
#'
#' @examples
#' # Example for Sheet1Data:
#' row1 <- data.frame(
#'   Plot.code = "P001",
#'   SU = 1,
#'   Sample.date = "2025-03-15",
#'   Detector = "Detector1",
#'   X = 12.345,
#'   Y = 67.890,
#'   Region = "North",
#'   Elevation = 250,
#'   Aspect = 180,
#'   Slope = 15,
#'   Cop.tot = 80,
#'   Litter.cov = 20,
#'   Bare.soil.cov = 10,
#'   Tree.cov = 60,
#'   Tree.h = 10,
#'   Shrub.cov = 30,
#'   Shrub.h = 2,
#'   Herb.cov = 40,
#'   Herb.h = 0.5,
#'   Brioph.cov = 5,
#'   notes = "Sample note",
#'   stringsAsFactors = FALSE
#' )
#' sheet1_obj <- Sheet1Data$new(row1)
#' df1 <- sheet1_obj$to_data_frame()
#' print(df1)
#'
#' # Example for Sheet2Data:
#' row2 <- data.frame(
#'   Plot.code = "P001",
#'   Subplot = 2,
#'   Species = "SpeciesA",
#'   species_abb = "SpA",
#'   cover = 75,
#'   Layer = "Understory",
#'   Notes = "Healthy specimen",
#'   stringsAsFactors = FALSE
#' )
#' sheet2_obj <- Sheet2Data$new(row2)
#' df2 <- sheet2_obj$to_data_frame()
#' print(df2)
#'
#' # Example for ExcelData:
#' # Assuming an Excel file \"data.xlsx\" with two sheets matching the required structure:
#' # excel_data <- ExcelData$new("data.xlsx")
#' # Access the loaded data:
#' # head(excel_data$sheet1_data[[1]]$to_data_frame())
#'
#' @import R6 openxlsx readxl
NULL

##############################################
# Sheet1Data Class
##############################################

#' @title Sheet1Data Class
#' @description
#' Represents a single row from Sheet 1 of the Excel file.
#'
#' @details
#' The expected input is a named vector or one-row data frame containing the following columns:
#' \itemize{
#'   \item \code{Plot.code}: Plot code (alphanumeric string)
#'   \item \code{SU}: Sampling Unit (numeric, expected range 1-4)
#'   \item \code{Sample.date}: Sample date (date in a recognizable format)
#'   \item \code{Detector}: Detector name (string)
#'   \item \code{X}: Longitude (numeric, WGS84)
#'   \item \code{Y}: Latitude (numeric, WGS84)
#'   \item \code{Region}: Region name (string)
#'   \item \code{Elevation}: Elevation in meters (numeric)
#'   \item \code{Aspect}: Sun exposition in degrees (numeric)
#'   \item \code{Slope}: Soil inclination in degrees (numeric)
#'   \item \code{Cop.tot}: Total cover percentage (numeric, 0-100)
#'   \item \code{Litter.cov}: Litter cover percentage (numeric, 0-100)
#'   \item \code{Bare.soil.cov}: Bare soil cover percentage (numeric, 0-100)
#'   \item \code{Tree.cov}: Tree cover percentage (numeric, 0-100)
#'   \item \code{Tree.h}: Tree height in meters (numeric)
#'   \item \code{Shrub.cov}: Shrub cover percentage (numeric, 0-100)
#'   \item \code{Shrub.h}: Shrub height in meters (numeric)
#'   \item \code{Herb.cov}: Herb cover percentage (numeric, 0-100)
#'   \item \code{Herb.h}: Herb height in meters (numeric)
#'   \item \code{Brioph.cov}: Bryophyte cover percentage (numeric, 0-100)
#'   \item \code{notes}: Additional notes (string)
#' }
#'
#' @return A new instance of \code{Sheet1Data}.
#'
#' @examples
#' row1 <- data.frame(
#'   Plot.code = "P001",
#'   SU = 1,
#'   Sample.date = "2025-03-15",
#'   Detector = "Detector1",
#'   X = 12.345,
#'   Y = 67.890,
#'   Region = "North",
#'   Elevation = 250,
#'   Aspect = 180,
#'   Slope = 15,
#'   Cop.tot = 80,
#'   Litter.cov = 20,
#'   Bare.soil.cov = 10,
#'   Tree.cov = 60,
#'   Tree.h = 10,
#'   Shrub.cov = 30,
#'   Shrub.h = 2,
#'   Herb.cov = 40,
#'   Herb.h = 0.5,
#'   Brioph.cov = 5,
#'   notes = "Sample note",
#'   stringsAsFactors = FALSE
#' )
#' sheet1_obj <- Sheet1Data$new(row1)
Sheet1Data <- R6Class("Sheet1Data",
  public = list(
    #' @field Plot.code Plot code (alphanumeric string)
    Plot.code = NULL,
    #' @field SU Sampling Unit (numeric, expected range 1-4)
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
    #' @field Tree.h Tree height in meters (numeric)
    Tree.h = NULL,
    #' @field Shrub.cov Shrub cover percentage (numeric, 0-100)
    Shrub.cov = NULL,
    #' @field Shrub.h Shrub height in meters (numeric)
    Shrub.h = NULL,
    #' @field Herb.cov Herb cover percentage (numeric, 0-100)
    Herb.cov = NULL,
    #' @field Herb.h Herb height in meters (numeric)
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
    #' @return A \code{data.frame} containing the data from this object.
    #' @examples
    #' df <- sheet1_obj$to_data_frame()\n# View structure of the data frame:\n# str(df)
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
#' @description Represents a single row from Sheet 2 of the Excel file.
#'
#' @details
#' The expected input is a named vector or one-row data frame containing the following columns:
#' \itemize{
#'   \item \code{Plot.code}: Plot code (alphanumeric string)
#'   \item \code{Subplot}: Subplot number (numeric, expected range 1-4)
#'   \item \code{Species}: Species name (string)
#'   \item \code{species_abb}: Species abbreviation (string)
#'   \item \code{cover}: Cover percentage (numeric, 0-100)
#'   \item \code{Layer}: Layer (string)
#'   \item \code{Notes}: Notes (string)
#' }
#'
#' @return A new instance of \code{Sheet2Data}.
#'
#' @examples
#' row2 <- data.frame(
#'   Plot.code = "P001",
#'   Subplot = 2,
#'   Species = "SpeciesA",
#'   species_abb = "SpA",
#'   cover = 75,
#'   Layer = "Understory",
#'   Notes = "Healthy specimen",
#'   stringsAsFactors = FALSE
#' )
#' sheet2_obj <- Sheet2Data$new(row2)
Sheet2Data <- R6Class("Sheet2Data",
  public = list(
    #' @field Plot.code Plot code (alphanumeric string)
    Plot.code = NULL,
    #' @field Subplot Subplot number (numeric, expected range 1-4)
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
    #' @return A \code{data.frame} containing the data from this object.
    #' @examples
    #' df2 <- sheet2_obj$to_data_frame()\n# Display the first few rows:\n# head(df2)
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
# ExcelData Class
##############################################

#' @title ExcelData Class
#' @description Represents and loads data from an Excel file that contains two sheets.
#'
#' @details
#' The Excel file is expected to have at least two sheets:
#' \itemize{
#'   \item Sheet 1: Contains columns matching the expectations of \code{Sheet1Data}.
#'   \item Sheet 2: Contains columns matching the expectations of \code{Sheet2Data}.
#' }
#'
#' @return A new instance of \code{ExcelData} with two lists:
#' \itemize{
#'   \item \code{sheet1_data}: A list of \code{Sheet1Data} objects.
#'   \item \code{sheet2_data}: A list of \code{Sheet2Data} objects.
#' }
#'
#' @examples
#' # Given an Excel file \"data.xlsx\" with the appropriate structure:
#' excel_data <- ExcelData$new("data.xlsx")
#' # Access the data from Sheet 1:
#' df1 <- excel_data$sheet1_data[[1]]$to_data_frame()
#' print(df1)
ExcelData <- R6Class("ExcelData",
  public = list(
    #' @field sheet1_data List of \code{Sheet1Data} objects.
    sheet1_data = list(),
    #' @field sheet2_data List of \code{Sheet2Data} objects.
    sheet2_data = list(),
    #' @field filepath Path to the Excel file.
    filepath = NULL,
    
    #' @description Initialize a new \code{ExcelData} object and load data from the specified Excel file.
    #'
    #' @param filepath A string representing the path to the Excel file.
    #' @return A new \code{ExcelData} object with data loaded from both sheets.
    #' @examples
    #' excel_data <- ExcelData$new("data.xlsx")
    initialize = function(filepath) {
      self$filepath <- filepath
      self$load_data()
    },
    
    #' @description Load data from the Excel file specified in \code{filepath}.
    #' @details This method reads the first two sheets of the Excel file using the \code{openxlsx} package
    #' and creates lists of \code{Sheet1Data} and \code{Sheet2Data} objects.
    #' @return Invisibly returns \code{NULL} (its primary effect is populating the object fields).
    #' @examples
    #' excel_data <- ExcelData$new("data.xlsx")
    #' head(excel_data$sheet1_data[[1]]$to_data_frame())
    load_data = function() {
      sheet1 <- openxlsx::read.xlsx(self$filepath, sheet = 1)
      sheet2 <- openxlsx::read.xlsx(self$filepath, sheet = 2)
      
      for (i in seq_len(nrow(sheet1))) {  
        self$sheet1_data[[i]] <- Sheet1Data$new(sheet1[i, ])
      }
      for (i in seq_len(nrow(sheet2))) {  
        self$sheet2_data[[i]] <- Sheet2Data$new(sheet2[i, ])
      }
    }
  )
)
