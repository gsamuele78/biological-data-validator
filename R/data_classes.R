# R/data_classes.R
library(R6)
library(openxlsx)
library(readxl)

#' Class to represent a single row in Sheet 1 of the Excel file
Sheet1Data <- R6Class("Sheet1Data",
  public = list(
    #' @field Plot.code Plot code (alphanumeric string)
    Plot.code = NULL,
    #' @field SU Sampling Unit (number in range 1-4)
    SU = NULL,
    #' @field Sample.date Sample date (Date object)
    Sample.date = NULL,
    #' @field Detector Detector name (string)
    Detector = NULL,
    #' @field X Longitude (WGS84)
    X = NULL,
    #' @field Y Latitude (WGS84)
    Y = NULL,
    #' @field Region Region name (string)
    Region = NULL,
    #' @field Elevation Elevation in meters (numeric)
    Elevation = NULL,
    #' @field Aspect Sun exposition in degrees (numeric)
    Aspect = NULL,
    #' @field Slope Soil inclination in degrees (numeric)
    Slope = NULL,
    #' @field Cop.tot Total cover percentage (0-100)
    Cop.tot = NULL,
    #' @field Litter.cov Litter cover percentage (0-100)
    Litter.cov = NULL,
    #' @field Bare.soil.cov Bare soil cover percentage (0-100)
    Bare.soil.cov = NULL,
    #' @field Tree.cov Tree cover percentage (0-100)
    Tree.cov = NULL,
    #' @field Tree.h Tree height in meters (numeric)
    Tree.h = NULL,
    #' @field Shrub.cov Shrub cover percentage (0-100)
    Shrub.cov = NULL,
    #' @field Shrub.h Shrub height in meters (numeric)
    Shrub.h = NULL,
    #' @field Herb.cov Herb cover percentage (0-100)
    Herb.cov = NULL,
    #' @field Herb.h Herb height in meters (numeric)
    Herb.h = NULL,
    #' @field Brioph.cov Bryophyte cover percentage (0-100)
    Brioph.cov = NULL,
    #' @field notes Notes (string)
    notes = NULL,

    #' @description
    #' Create a new Sheet1Data object
    #' @param data_row A row from Sheet 1 of the Excel file
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
    }
    #' @description
    #' Convert the Sheet1Data object to a data frame
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

#' Class to represent a single row in Sheet 2 of the Excel file
Sheet2Data <- R6Class("Sheet2Data",
  public = list(
    #' @field Plot.code Plot code (alphanumeric string)
    Plot.code = NULL,
    #' @field Subplot Subplot number (number in range 1-4)
    Subplot = NULL,
    #' @field Species Species name (string)
    Species = NULL,
    #' @field species_abb Species abbreviation (string)
    species_abb = NULL,
    #' @field cover Cover percentage (0-100)
    cover = NULL,
    #' @field Layer Layer (string)
    Layer = NULL,
    #' @field Notes Notes (string)
    Notes = NULL,

    #' @description
    #' Create a new Sheet2Data object
    #' @param data_row A row from Sheet 2 of the Excel file
    initialize = function(data_row) {
      self$Plot.code <- as.character(data_row[["Plot.code"]])
      self$Subplot <- as.numeric(data_row[["Subplot"]])
      self$Species <- as.character(data_row[["Species"]])
      self$species_abb <- as.character(data_row[["species_abb"]])
      self$cover <- as.numeric(data_row[["cover"]])
      self$Layer <- as.character(data_row[["Layer"]])
      self$Notes <- as.character(data_row[["Notes"]])
    }
    #' @description
    #' Convert the Sheet2Data object to a data frame
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

#' Class to represent the Excel data (both sheets)
ExcelData <- R6Class("ExcelData",
  public = list(
    #' @field sheet1_data List of Sheet1Data objects
    sheet1_data = list(),
    #' @field sheet2_data List of Sheet2Data objects
    sheet2_data = list(),
    #' @field filepath Path to the Excel file
    filepath = NULL,

    #' @description
    #' Create a new ExcelData object and load data from the Excel file
    #' @param filepath Path to the Excel file
    initialize = function(filepath) {
      self$filepath <- filepath
      self$load_data()
    },

    #' @description
    #' Load data from the Excel file
    load_data = function() {
      sheet1 <- openxlsx::read.xlsx(self$filepath, sheet = 1)
      sheet2 <- openxlsx::read.xlsx(self$filepath, sheet = 2)

      for (i in 1:nrow(sheet1)) {
        self$sheet1_data[[i]] <- Sheet1Data$new(sheet1[i, ])
      }

      for (i in 1:nrow(sheet2)) {
        self$sheet2_data[[i]] <- Sheet2Data$new(sheet2[i, ])
      }
    }
  )
)
