# R/data_classes.R

library(R6)
library(openxlsx)

# Class to represent a single row in Sheet 1
Sheet1Data <- R6Class("Sheet1Data",
  public = list(
    Plot.code = NULL,
    SU = NULL,
    Sample.date = NULL,
    Detector = NULL,
    X = NULL,
    Y = NULL,
    Region = NULL,
    Elevation = NULL,
    Aspect = NULL,
    Slope = NULL,
    Cop.tot = NULL,
    Litter.cov = NULL,
    Bare.soil.cov = NULL,
    Tree.cov = NULL,
    Tree.h = NULL,
    Shrub.cov = NULL,
    Shrub.h = NULL,
    Herb.cov = NULL,
    Herb.h = NULL,
    Brioph.cov = NULL,
    notes = NULL,
    
    initialize = function(data_row) {
      self$Plot.code <- data_row[["Plot.code"]]
      self$SU <- data_row[["SU"]]
      self$Sample.date <- data_row[["Sample.date"]]
      self$Detector <- data_row[["Detector"]]
      self$X <- data_row[["X"]]
      self$Y <- data_row[["Y"]]
      self$Region <- data_row[["Region"]]
      self$Elevation <- data_row[["Elevation"]]
      self$Aspect <- data_row[["Aspect"]]
      self$Slope <- data_row[["Slope"]]
      self$Cop.tot <- data_row[["Cop.tot"]]
      self$Litter.cov <- data_row[["Litter.cov"]]
      self$Bare.soil.cov <- data_row[["Bare.soil.cov"]]
      self$Tree.cov <- data_row[["Tree.cov"]]
      self$Tree.h <- data_row[["Tree.h"]]
      self$Shrub.cov <- data_row[["Shrub.cov"]]
      self$Shrub.h <- data_row[["Shrub.h"]]
      self$Herb.cov <- data_row[["Herb.cov"]]
      self$Herb.h <- data_row[["Herb.h"]]
      self$Brioph.cov <- data_row[["Brioph.cov"]]
      self$notes <- data_row[["notes"]]
    }
  )
)

# Class to represent a single row in Sheet 2
Sheet2Data <- R6Class("Sheet2Data",
  public = list(
    Plot.code = NULL,
    Subplot = NULL,
    Species = NULL,
    species_abb = NULL,
    cover = NULL,
    Layer = NULL,
    Notes = NULL,
    
    initialize = function(data_row) {
      self$Plot.code <- data_row[["Plot.code"]]
      self$Subplot <- data_row[["Subplot"]]
      self$Species <- data_row[["Species"]]
      self$species_abb <- data_row[["species_abb"]]
      self$cover <- data_row[["cover"]]
      self$Layer <- data_row[["Layer"]]
      self$Notes <- data_row[["Notes"]]
    }
  )
)

# Class to represent the Excel data (both sheets)
ExcelData <- R6Class("ExcelData",
  public = list(
    sheet1_data = list(),
    sheet2_data = list(),
    filepath = NULL,

    initialize = function(filepath) {
      self$filepath <- filepath
      self$load_data()
    },

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
