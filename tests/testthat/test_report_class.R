# tests/testthat/test_report_class.R
library(testthat)
source("../../R/report_class.R")
source("../../R/data_classes.R") 

context("Report Generation")

test_that("Report generates an HTML file", {
  # Create dummy data for the report
  dummy_errors <- data.frame(
    Sheet = c("Sheet1", "Sheet2"),
    Row = c(1, 2),
    Column = c("Plot.code", "Species"),
    Message = c("Invalid plot code", "Invalid species name"),
    Level = c("Error", "Warning")
  )
  dummy_sheet1_data <- list(Sheet1Data$new(list(Plot.code = "Plot1", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X = 1, Y = 1, Elevation = 1, Aspect = 1, Slope = 1, Cop.tot = 1, Litter.cov = 1, Bare.soil.cov = 1, Tree.cov = 1, Tree.h = 1, Shrub.cov = 1, Shrub.h = 1, Herb.cov = 1, Herb.h = 1, Brioph.cov = 1, notes = "Note 1")))
  dummy_sheet2_data <- list(Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")))
  
  # Create a Report object
  report <- Report$new(
    filepath = "dummy_filepath.xlsx",
    errors = dummy_errors,
    sheet1 = dummy_sheet1_data,
    sheet2 = dummy_sheet2_data
  )
  
  # Generate the report in a temporary directory
  temp_report_dir <- file.path(tempdir(), "report_test")
  dir.create(temp_report_dir, showWarnings = FALSE)
  report_file <- file.path(temp_report_dir, "report-validation.html")
  
  report$generate(temp_report_dir)
  
  # Check if the report file exists
  expect_true(file.exists(report_file))
  
  # Clean up the temporary directory (optional)
  # unlink(temp_report_dir, recursive = TRUE)
})
