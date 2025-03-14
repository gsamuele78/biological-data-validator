#!/usr/bin/env Rscript
# app_cli.R (Main CLI Script)

# Install and load necessary packages using renv
if (!require("renv")) install.packages("renv")
renv::restore()

library(optparse)
library(logger)

# Source R functions and classes
source("R/data_classes.R")
source("R/validation_classes.R")
source("R/report_class.R")
source("R/path_generation_class.R")
source("R/email_class.R")
source("R/db_interaction_class.R")
source("R/utils.R")  # For logging and other utilities

# --- Command Line Options ---

option_list <- list(
  make_option(c("-f", "--file"), type = "character", default = NULL,
              help = "Path to the Excel data file", metavar = "character"),
  make_option(c("-i", "--images"), type = "character", default = NULL,
              help = "Comma-separated paths to image files (optional)", metavar = "character"),
  make_option(c("-b", "--base_path"), type = "character", default = getwd(),
              help = "Base path for saving data and reports", metavar = "character"),
  make_option(c("-e", "--email"), type = "character", default = NULL,
              help = "Email address to send the report to (optional)", metavar = "character"),
  make_option(c("-d", "--database"), type = "character", default = "validation_history.db",
              help = "Path to the SQLite database file", metavar = "character"),
  make_option(c("-s", "--search"), action = "store_true", default = FALSE,
              help = "Perform a search on the database records"),
  make_option(c("-p", "--plot_code"), type = "character", default = NULL,
              help = "Plot code to search for (use with --search)", metavar = "character"),
  make_option(c("--from_date"), type = "character", default = NULL,
              help = "Start date for search (YYYY-MM-DD, use with --search)", metavar = "character"),
  make_option(c("--to_date"), type = "character", default = NULL,
              help = "End date for search (YYYY-MM-DD, use with --search)", metavar = "character"),
  make_option(c("-u", "--update"), action = "store_true", default = FALSE,
              help = "Update a database record"),
  make_option(c("--update_id"), type = "integer", default = NULL,
              help = "ID of the record to update (use with --update)", metavar = "integer"),
  make_option(c("--delete"), action = "store_true", default = FALSE,
              help = "Delete a database record"),
  make_option(c("--delete_id"), type = "integer", default = NULL,
              help = "ID of the record to delete (use with --delete)", metavar = "integer")
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# --- Main Execution ---

main <- function(opt) {
    # Start logging
    setup_logging() # Function defined in R/utils.R

    # Initialize DatabaseHandler
    db_handler <- DatabaseHandler$new(opt$database)

    # Find project root
    project_root <- rprojroot::find_root(rprojroot::has_file(".Rprofile"))

    if (opt$search) {
        # --- Search Records ---
        log_info("Performing database search...")
        records <- db_handler$get_plot_history(
            plot_code = opt$plot_code,
            from_date = if (!is.null(opt$from_date)) as.Date(opt$from_date) else NULL,
            to_date = if (!is.null(opt$to_date)) as.Date(opt$to_date) else NULL
        )
        
        if (nrow(records) > 0) {
            print(records)
            log_info("Found {nrow(records)} matching records.")
        } else {
            log_info("No matching records found.")
        }

    } else if (opt$update && !is.null(opt$update_id)) {
        # --- Update Record ---
        log_info("Updating record with ID: {opt$update_id}...")
        # You'll need to add logic here to get the updated values
        # For example, prompt the user or use additional command-line options
        # ...
        
        # Example update (replace with actual updated values)
        db_handler$update_plot_data(
            id = opt$update_id,
            filepath = "updated_filepath",
            plot_code = "updated_plot_code",
            sample_date = as.character(Sys.Date()),
            detector = "updated_detector",
            region = "updated_region",
            validation_status = "updated_status",
            report_path = "updated_report_path"
        )
        log_info("Record updated successfully.")

    } else if (opt$delete && !is.null(opt$delete_id)) {
        # --- Delete Record ---
        log_info("Deleting record with ID: {opt$delete_id}...")
        db_handler$delete_plot_data(opt$delete_id)
        log_info("Record deleted successfully.")

    } else if (!is.null(opt$file)) {
        # --- Validate Data ---
        log_info("Starting data validation...")
        log_info("Loading data from: {opt$file}")

        # Initialize PathGenerator with the base path
        path_generator <- PathGenerator$new(opt$base_path)

        # Initialize Validator with the PathGenerator instance
        validator <- Validator$new(path_generator)

        # Load Excel data
        excel_data <- ExcelData$new(opt$file)

        # Validate data
        errors <- validator$validate(excel_data)

        # Generate report path
        sample_row <- excel_data$sheet1_data[[1]]
        data_path <- path_generator$generate(
          sample_row$Plot.code,
          sample_row$Region
        )
        report_path <- file.path(data_path, "report-validation.html")

        # Generate report using the Report class
        report <- Report$new(opt$file, errors, excel_data$sheet1_data, excel_data$sheet2_data)
        #report$generate(data_path)
        report$generate(data_path, project_root)
        
        
        if (nrow(errors) > 0) {
            log_error("Data validation failed with {nrow(errors)} errors.")
            print(errors)  # Print errors to console
        } else {
            log_info("Data validation successful.")
        
            # Add data to database
            plot_data_id <- db_handler$add_plot_data(
                opt$file, 
                sample_row$Plot.code, 
                as.character(sample_row$Sample.date), 
                sample_row$Detector, 
                sample_row$Region, 
                "Success", 
                report_path
            )
            
            # If images were provided, add them to the database as well
            if (!is.null(opt$images)) {
                image_paths <- unlist(strsplit(opt$images, ","))
                for (image_path in image_paths) {
                    db_handler$add_image_data(plot_data_id, image_path)
                }
            }
        }

        # Send email (if provided)
        if (!is.null(opt$email)) {
          email_sender <- EmailSender$new()
          email_sender$send(report_path, opt$email)
          log_info("Report sent to: {opt$email}")
        }
    } else {
        # --- Invalid Usage ---
        log_error("Invalid usage. Use --help for more information.")
    }
}

# Run main function if script is executed
if (!interactive()) {
  main(opt)
}
