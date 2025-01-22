# tests/testthat/test_validation_classes.R
library(testthat)
source("../../R/data_classes.R")
source("../../R/validation_classes.R")
source("../../R/path_generation_class.R")

context("Validation Classes")

# Create dummy ExcelData objects for testing
valid_excel_data <- ExcelData$new("dummy_path")
valid_excel_data$sheet1_data <- list(
  Sheet1Data$new(list(Plot.code = "Plot1", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 1")),
  Sheet1Data$new(list(Plot.code = "Plot1", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 2")),
  Sheet1Data$new(list(Plot.code = "Plot1", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 3")),
  Sheet1Data$new(list(Plot.code = "Plot1", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 4"))
)
valid_excel_data$sheet2_data <- list(
  Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
  Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
  Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C")),
  Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 4, Species = "Species D", species_abb = "Sp. D", cover = 80, Layer = "Moss", Notes = "Note D"))
)

invalid_excel_data <- ExcelData$new("dummy_path")
invalid_excel_data$sheet1_data <- list(
  Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 1")),
  Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 2")), # Duplicate SU
  Sheet1Data$new(list(Plot.code = "Plot2", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 3")),
  Sheet1Data$new(list(Plot.code = "Plot2", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 4"))
)
invalid_excel_data$sheet2_data <- list(
  Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
  Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
  Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C"))
  # Missing Subplot 4
)

# Initialize Validator for testing
path_generator <- PathGenerator$new(base_path = tempdir())
validator <- Validator$new(path_generator)

test_that("DataTypeValidationRule identifies correct errors", {
  errors <- DataTypeValidationRule$new()$check(invalid_excel_data)
  expect_true(nrow(errors) > 0) # Expecting errors
  expect_equal(errors$Message[1], "Duplicate SU values found for Plot.code: Plot2") # Example error message
})

test_that("MaxRowsValidationRule identifies correct errors", {
    # Test with more than 4 rows for a plot code
    invalid_excel_data_max_rows <- ExcelData$new("dummy_path")
    invalid_excel_data_max_rows$sheet1_data <- list(
        Sheet1Data$new(list(Plot.code = "Plot3", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X = 1, Y = 1, Elevation = 1, Aspect = 1, Slope = 1, Cop.tot = 1, Litter.cov = 1, Bare.soil.cov = 1, Tree.cov = 1, Tree.h = 1, Shrub.cov = 1, Shrub.h = 1, Herb.cov = 1, Herb.h = 1, Brioph.cov = 1, notes = "Note 1")),
        Sheet1Data$new(list(Plot.code = "Plot3", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X = 1, Y = 1, Elevation = 1, Aspect = 1, Slope = 1, Cop.tot = 1, Litter.cov = 1, Bare.soil.cov = 1, Tree.cov = 1, Tree.h = 1, Shrub.cov = 1, Shrub.h = 1, Herb.cov = 1, Herb.h = 1, Brioph.cov = 1, notes = "Note 2")),
        Sheet1Data$new(list(Plot.code = "Plot3", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X = 1, Y = 1, Elevation = 1, Aspect = 1, Slope = 1, Cop.tot = 1, Litter.cov = 1, Bare.soil.cov = 1, Tree.cov = 1, Tree.h = 1, Shrub.cov = 1, Shrub.h = 1, Herb.cov = 1, Herb.h = 1, Brioph.cov = 1, notes = "Note 3")),
        Sheet1Data$new(list(Plot.code = "Plot3", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X = 1, Y = 1, Elevation = 1, Aspect = 1, Slope = 1, Cop.tot = 1, Litter.cov = 1, Bare.soil.cov = 1, Tree.cov = 1, Tree.h = 1, Shrub.cov = 1, Shrub.h = 1, Herb.cov = 1, Herb.h = 1, Brioph.cov = 1, notes = "Note 4")),
        Sheet1Data$new(list(Plot.code = "Plot3", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X = 1, Y = 1, Elevation = 1, Aspect = 1, Slope = 1, Cop.tot = 1, Litter.cov = 1, Bare.soil.cov = 1, Tree.cov = 1, Tree.h = 1, Shrub.cov = 1, Shrub.h = 1, Herb.cov = 1, Herb.h = 1, Brioph.cov = 1, notes = "Note 5")) # Extra row
    )
    
    errors <- MaxRowsValidationRule$new()$check(invalid_excel_data_max_rows)
    expect_true(nrow(errors) > 0)
    expect_equal(errors$Message[1], "More than 4 rows with Plot.code: Plot3")
})

test_that("UniqueSUValidationRule identifies correct errors", {
  errors <- UniqueSUValidationRule$new()$check(invalid_excel_data)
  expect_true(nrow(errors) > 0)
  expect_equal(errors$Message[1], "Duplicate SU values found for Plot.code: Plot2")
})

test_that("NotesValidationRule identifies correct errors", {
  errors <- NotesValidationRule$new()$check(invalid_excel_data)
  expect_true(nrow(errors) > 0)
  expect_equal(errors$Message[1], "Missing data in Sheet2 for Plot.code: Plot2 and SU: 4 without a corresponding note in Sheet1.")
})

test_that("Validator applies all rules correctly", {
  errors <- validator$validate(invalid_excel_data)
  expect_true(nrow(errors) > 0)
  
  # Check for specific error messages to ensure all rules are applied
  expect_true(any(grepl("Duplicate SU values found for Plot.code: Plot2", errors$Message)))
  expect_true(any(grepl("Missing data in Sheet2 for Plot.code: Plot2 and SU: 4 without a corresponding note in Sheet1.", errors$Message)))
})
