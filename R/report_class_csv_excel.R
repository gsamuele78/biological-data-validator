# R/report_class.R # nolint: commented_code_linter.
library(R6)
library(rmarkdown)
#library(logger)

#' Report class for generating validation reports
Report <- R6Class("Report",
  public = list(
    #' @field data_source DataSource object containing the processed data
    data_source = NULL,
    #' @field errors Validation errors
    errors = NULL,

    #' @description
    #' Create a new Report object
    #' @param data_source DataSource object containing the processed data
    #' @param errors Data frame of validation errors
    initialize = function(data_source, errors) {
      self$data_source <- data_source
      self$errors <- errors
    },

    #' @description
    #' Generate the HTML report
    #' @param output_path Directory where the report should be saved
    #' @param project_root The root directory of the project
    generate = function(output_path, project_root) {
      # Create sheet1_df and sheet2_df data frames from data_source
      sheet1_df <- do.call(rbind, lapply(self$data_source$sheet1_data, function(x) x$to_data_frame()))
      sheet2_df <- do.call(rbind, lapply(self$data_source$sheet2_data, function(x) x$to_data_frame()))
      
      logger::log_info(paste("Generating validation report for:", self$data_source$filepath))
      
      tryCatch({
        rmarkdown::render(file.path(project_root, "report.Rmd"),
                          output_file = file.path(output_path, "report-validation.html"),
                          params = list(
                            filepath = self$data_source$filepath,
                            errors = self$errors,
                            sheet1 = sheet1_df,
                            sheet2 = sheet2_df,
                            file_type = self$data_source$file_type
                          ))
        logger::log_success(paste("Successfully generated validation report at:", 
                                  file.path(output_path, "report-validation.html")))
        return(TRUE)
      }, error = function(e) {
        logger::log_error(paste("Failed to generate validation report. Error:", e$message))
        return(FALSE) # nolint: return_linter.
      })
    },
    
    #' @description
    #' Export errors to CSV file
    #' @param output_path Path to save the errors CSV file
    #' @return TRUE if successful
    export_errors_to_csv = function(output_path) {
      logger::log_info(paste("Exporting validation errors to CSV:", output_path))
      
      tryCatch({
        if (!is.null(self$errors) && nrow(self$errors) > 0) {
          readr::write_csv(self$errors, output_path)
          logger::log_success(paste("Successfully exported validation errors to:", output_path))
          return(TRUE)
        } else {
          logger::log_info("No errors to export.")
          return(FALSE)
        }
      }, error = function(e) {
        logger::log_error(paste("Failed to export validation errors. Error:", e$message))
        return(FALSE) # nolint: return_linter.
      })
    }
  )
)