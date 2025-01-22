# R/report_class.R
library(R6)
library(rmarkdown)

#' Report class for generating validation reports
Report <- R6Class("Report",
  public = list(
    #' @field filepath Path to the Excel file
    filepath = NULL,
    #' @field errors Validation errors
    errors = NULL,
    #' @field sheet1 Sheet 1 data
    sheet1 = NULL,
    #' @field sheet2 Sheet 2 data
    sheet2 = NULL,

    #' @description
    #' Create a new Report object
    #' @param filepath Path to the Excel file
    #' @param errors Data frame of validation errors
    #' @param sheet1 Sheet 1 data
    #' @param sheet2 Sheet 2 data
    initialize = function(filepath, errors, sheet1, sheet2) {
      self$filepath <- filepath
      self$errors <- errors
      self$sheet1 <- sheet1
      self$sheet2 <- sheet2
    },

    #' @description
    #' Generate the HTML report
    #' @param output_path Directory where the report should be saved
    #' @param project_root The root directory of the project
    generate = function(output_path, project_root) {
      rmarkdown::render(file.path(project_root, "report.Rmd"),
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
