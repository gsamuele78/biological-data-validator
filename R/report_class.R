# R/report_class.R

library(R6)
library(rmarkdown)

Report <- R6Class("Report",
  public = list(
    filepath = NULL,
    errors = NULL,
    sheet1 = NULL,
    sheet2 = NULL,

    initialize = function(filepath, errors, sheet1, sheet2) {
      self$filepath <- filepath
      self$errors <- errors
      self$sheet1 <- sheet1
      self$sheet2 <- sheet2
    },

    generate = function(output_path) {
      rmarkdown::render("report.Rmd",
                        output_file = file.path(output_path, "report-validation.html"),
                        params = list(
                          filepath = self$filepath,
                          errors = self$errors,
                          sheet1 = self$sheet1,
                          sheet2 = self$sheet2
                        ))
    }
  )
)
