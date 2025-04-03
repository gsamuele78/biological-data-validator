# nolint: commented_code_linter.
library(testthat)
library(rprojroot)
library(readr)
source("../../R/report_class.R")
source("../../R/csv_mapping.R")
source("../../R/data_classes.R")

context("CSV Report Generation")

# Helper function to create test CSV files
create_test_csv_files <- function() {
  temp_dir <- tempdir()
  main_file <- file.path(temp_dir, "test.csv")
  species_file <- file.path(temp_dir, "test_species.csv")
  
  # Create test data with CSV field names
  main_data <- data.frame(
    plot_code = "Test1",
    su = 1,
    sample_date = "2023-01-15",
    detector = "Det1",
    longitude = 12.123456789012345,
    latitude = 45.123456789012345,
    region = "North",
    elevation_m = 100,
    stringsAsFactors = FALSE
  )
  
  species_data <- data.frame(
    plot_code = "Test1",
    subplot = 1,
    species_name = "Species1",
    species_code = "SP1",
    species_cover = 80,
    vegetation_layer = "Herb",
    species_notes = "Test",
    stringsAsFactors = FALSE
  )
  
  write.csv(main_data, main_file, row.names = FALSE)
  write.csv(species_data, species_file, row.names = FALSE)
  
  list(
    main_path = main_file,
    species_path = species_file
  )
}

test_that("Report generates from CSV data", {
  # Create test files
  csv_files <- create_test_csv_files()
  
  # Create test data source
  data_source <- DataSource$new(csv_files$main_path)
  
  # Create test errors
  errors <- data.frame(
    Sheet = c("Main", "Species"),
    Row = c(1, 1),
    Column = c("plot_code", "species_name"),
    Message = c("Test error 1", "Test error 2"),
    Level = c("Error", "Warning"),
    stringsAsFactors = FALSE
  )
  
  # Create and test report
  report <- Report$new(data_source, errors)
  
  # Create temp directory for report output
  report_dir <- file.path(tempdir(), "report_test")
  dir.create(report_dir, showWarnings = FALSE)
  
  # Generate report
  project_root <- find_root(has_file(".Rprofile"))
  result <- report$generate(report_dir, project_root)
  
  # Verify report generation
  expect_true(result)
  expect_true(file.exists(file.path(report_dir, "report-validation.html")))
  
  # Test error export
  error_file <- file.path(report_dir, "validation_errors.csv")
  expect_true(report$export_errors_to_csv(error_file))
  expect_true(file.exists(error_file))
  
  # Clean up
  unlink(c(csv_files$main_path, csv_files$species_path, report_dir), recursive = TRUE)
})

test_that("Report handles missing data gracefully", {
  # Create test files with minimal data
  csv_files <- create_test_csv_files()
  data_source <- DataSource$new(csv_files$main_path)
  
  # Create report with no errors
  report <- Report$new(data_source, data.frame())
  
  # Test report generation
  report_dir <- file.path(tempdir(), "empty_report_test")
  dir.create(report_dir, showWarnings = FALSE)
  
  project_root <- find_root(has_file(".Rprofile"))
  result <- report$generate(report_dir, project_root)
  
  expect_true(result)
  expect_true(file.exists(file.path(report_dir, "report-validation.html")))
  
  # Clean up
  unlink(c(csv_files$main_path, csv_files$species_path, report_dir), recursive = TRUE)
})
