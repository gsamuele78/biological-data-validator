# R/path_generation_class.R
library(R6)
library(lubridate)

#' PathGenerator class for creating directory paths based on data values
PathGenerator <- R6Class("PathGenerator",
  public = list(
    #' @field base_path The base path for generating directories
    base_path = NULL,
    
    #' @description
    #' Create a new PathGenerator object
    #' @param base_path The base path
    initialize = function(base_path = "/default/path") {
      self$base_path <- base_path
    },
    
    #' @description
    #' Generate a directory path based on plot code, sample date, detector, and region
    #' @param plot_code Plot code
    #' @param sample_date Sample date
    #' @param detector Detector
    #' @param region Region
    generate = function(plot_code, sample_date, detector, region) {
      year <- year(sample_date)
      month <- sprintf("%02d", month(sample_date))
      day <- sprintf("%02d", day(sample_date))
      
      path <- file.path(self$base_path, year, month, day, region, detector, plot_code)
      
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
      
      return(path)
    }
  )
)
