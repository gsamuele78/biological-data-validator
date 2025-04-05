#' R/report_class.R # nolint: commented_code_linter.
library(R6)
library(rmarkdown)
library(logger)

# Purpose:
# This file defines the `Report` class, which is responsible for generating validation reports
# from CSV data. The reports are generated in HTML format using R Markdown templates and can
# also export validation errors to CSV files.

# Documentation:
# - R6 Classes: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
# - rmarkdown: https://rmarkdown.rstudio.com/

#' @title Report
#' @description Class for generating validation reports from CSV data.
#' Example:
#' ```
#' data_source <- DataSource$new("data/Plot_Template_INFI2023.csv")
#' errors <- validator$validate(data_source)
#' report <- Report$new(data_source, errors)
#' report$generate("output/", "project_root/")
#' ```
Report <- R6Class("Report",
  public = list(
    #' @field data_source DataSource object containing the processed CSV data
    data_source = NULL,
    #' @field errors Validation errors
    errors = NULL,

    #' @description
    #' Create a new Report object.
    #' @param data_source DataSource object containing the CSV data.
    #' @param errors Data frame of validation errors.
    initialize = function(data_source, errors) {
      if (data_source$file_type != "csv") {
        stop("Data source must be CSV format")
      }
      self$data_source <- data_source
      self$errors <- errors
    },

    #' @description
    #' Generate the HTML report.
    #' @param output_path Directory where the report should be saved.
    #' @param project_root The root directory of the project.
    #' Example:
    #' ```
    #' report$generate("output/", "project_root/")
    #' ```
    generate = function(output_path, project_root) {
      logger::log_info(paste("Generating validation report for:", self$data_source$filepath))
      
      # Create data frames from CSV data
      sheet1_df <- do.call(rbind, lapply(self$data_source$sheet1_data, function(x) x$to_data_frame()))
      sheet2_df <- do.call(rbind, lapply(self$data_source$sheet2_data, function(x) x$to_data_frame()))
      
      # Get CSV field mappings and rename columns
      source(file.path(project_root, "R", "csv_mapping.R"))
      
      # Map internal names back to CSV names for display
      for (internal_name in names(SHEET1_CSV_MAPPING)) {
        csv_name <- SHEET1_CSV_MAPPING[[internal_name]]
        names(sheet1_df)[names(sheet1_df) == internal_name] <- csv_name
      }
      
      for (internal_name in names(SHEET2_CSV_MAPPING)) {
        csv_name <- SHEET2_CSV_MAPPING[[internal_name]]
        names(sheet2_df)[names(sheet2_df) == internal_name] <- csv_name
      }
      
      # Generate report
      tryCatch({
        rmarkdown::render(
          file.path(project_root, "report.Rmd"),
          output_file = file.path(output_path, "report-validation.html"),
          params = list(
            filepath = self$data_source$filepath,
            errors = self$errors,
            sheet1 = sheet1_df,
            sheet2 = sheet2_df,
            file_type = "csv"
          )
        )
        logger::log_success(sprintf(
          "Successfully generated validation report at: %s",
          file.path(output_path, "report-validation.html")
        ))
        return(TRUE)
      }, error = function(e) {
        logger::log_error(sprintf("Failed to generate report: %s", e$message))
        return(FALSE)
      })
    },
    
    #' @description
    #' Export validation errors to a CSV file.
    #' @param output_path Path to save the errors CSV file.
    #' @return TRUE if successful, FALSE otherwise.
    #' Example:
    #' ```
    #' report$export_errors_to_csv("output/errors.csv")
    #' ```
    export_errors_to_csv = function(output_path) {
      logger::log_info(sprintf("Exporting validation errors to: %s", output_path))
      
      if (!is.null(self$errors) && nrow(self$errors) > 0) {
        tryCatch({
          readr::write_csv(self$errors, output_path)
          logger::log_success("Successfully exported validation errors")
          return(TRUE)
        }, error = function(e) {
          logger::log_error(sprintf("Failed to export errors: %s", e$message))
          return(FALSE)
        })
      } else {
        logger::log_info("No validation errors to export")
        return(FALSE)
      }
    }
  )
)