# R/path_generation_class.R

library(R6)
library(lubridate)

PathGenerator <- R6Class("PathGenerator",
  public = list(
    base_path = NULL,
    
    initialize = function(base_path = "/default/path") {
      self$base_path <- base_path
    },
    
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
