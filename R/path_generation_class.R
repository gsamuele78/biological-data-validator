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
    #' @param region Region
    generate = function(plot_code, region) {
      
      path <- file.path(self$base_path, region, plot_code)
      
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
      
      return(path)
    }
  )
)
