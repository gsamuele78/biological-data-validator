# tests/test_validation.R

library(testthat)
library(R6)

# Source the necessary R files (make sure paths are correct)
source("../R/data_classes.R")
source("../R/validation_classes.R")
source("../R/path_generation_class.R")

# --- Test Setup ---
# Initialize a path generator (can be used by multiple tests)
path_generator <- PathGenerator$new("/test/path")  # Use a dummy path for testing

# --- Test Cases ---

# Test DataTypeValidationRule
test_that("DataTypeValidationRule identifies incorrect data types", {
  validator <- Validator$new(path_generator) # Create a validator instance

  # Create an ExcelData object with invalid data types
  invalid_sheet1_data <- list(
    Sheet1Data$new(list(Plot.code = 123, SU = "A", Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1")) # Invalid types
  )
  invalid_sheet2_data <- list(
    Sheet2Data$new(list(Plot.code = 456, Subplot = "B", Species = "Species A", species_abb = "Sp. A", cover = "50", Layer = "Tree", Notes = "Note A")) # Invalid types
  )
  invalid_excel_data <- ExcelData$new("dummy_path")
  invalid_excel_data$sheet1_data <- invalid_sheet1_data
  invalid_excel_data$sheet2_data <- invalid_sheet2_data

  # Run validation
  errors <- validator$validate(invalid_excel_data)

  # Check if errors are found for the specific issues
  expect_true(any(grepl("Plot.code should be alphanumeric", errors$Message)))
  expect_true(any(grepl("SU should be a number between 1 and 4", errors$Message)))
})

# Test MaxRowsValidationRule
test_that("MaxRowsValidationRule identifies plots with too many rows", {
  validator <- Validator$new(path_generator)

  # Create an ExcelData object with too many rows for a plot
  invalid_sheet1_data <- list(
    Sheet1Data$new(list(Plot.code = "PlotA", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1")),
    Sheet1Data$new(list(Plot.code = "PlotA", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2")),
    Sheet1Data$new(list(Plot.code = "PlotA", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3")),
    Sheet1Data$new(list(Plot.code = "PlotA", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4")),
    Sheet1Data$new(list(Plot.code = "PlotA", SU = 5, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 5")) # Extra row
  )
  invalid_excel_data <- ExcelData$new("dummy_path")
  invalid_excel_data$sheet1_data <- invalid_sheet1_data
  invalid_excel_data$sheet2_data <- list() # Empty sheet2

  # Run validation
  errors <- validator$validate(invalid_excel_data)

  # Check if the error is found
  expect_true(any(grepl("More than 4 rows with Plot.code: PlotA", errors$Message)))
})

# Test UniqueSUValidationRule
test_that("UniqueSUValidationRule identifies plots with duplicate SU values", {
  validator <- Validator$new(path_generator)

  # Create an ExcelData object with duplicate SU values
  invalid_sheet1_data <- list(
    Sheet1Data$new(list(Plot.code = "PlotB", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1")),
    Sheet1Data$new(list(Plot.code = "PlotB", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2")),
    Sheet1Data$new(list(Plot.code = "PlotB", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3")), # Duplicate SU
    Sheet1Data$new(list(Plot.code = "PlotB", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4"))
  )
  invalid_excel_data <- ExcelData$new("dummy_path")
  invalid_excel_data$sheet1_data <- invalid_sheet1_data
  invalid_excel_data$sheet2_data <- list() # Empty sheet2

  # Run validation
  errors <- validator$validate(invalid_excel_data)

  # Check if the error is found
  expect_true(any(grepl("Duplicate SU values found for Plot.code: PlotB", errors$Message)))
})

# Test NotesValidationRule
test_that("NotesValidationRule identifies missing Sheet2 data without notes in Sheet1", {
  validator <- Validator$new(path_generator)

  # Create an ExcelData object with missing Sheet2 data and no note in Sheet1
  invalid_sheet1_data <- list(
    Sheet1Data$new(list(Plot.code = "PlotC", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "")), # No note
    Sheet1Data$new(list(Plot.code = "PlotC", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2")),
    Sheet1Data$new(list(Plot.code = "PlotC", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3")),
    Sheet1Data$new(list(Plot.code = "PlotC", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4"))
  )
  invalid_excel_data <- ExcelData$new("dummy_path")
  invalid_excel_data$sheet1_data <- invalid_sheet1_data
  invalid_excel_data$sheet2_data <- list(
    Sheet2Data$new(list(Plot.code = "PlotC", Subplot = 2, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
    Sheet2Data$new(list(Plot.code = "PlotC", Subplot = 3, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
    Sheet2Data$new(list(Plot.code = "PlotC", Subplot = 4, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C"))
  ) # Missing Subplot 1

  # Run validation
  errors <- validator$validate(invalid_excel_data)
  # Check if the error is found
  expect_true(any(grepl("Missing data in Sheet2 for Plot.code: PlotC and SU: 1 without a corresponding note in Sheet1", errors$Message)))
})

# --- Test Documentation ---

# You can add a section to your `doc/developer_guide.md` about testing:

## Testing

#The project uses `testthat` for unit testing. Tests are located in the `tests/` directory.

#To run the tests:

#1. Make sure you have the `testthat` package installed:
#    ```R
#    install.packages("testthat")
#    ```
#2. In RStudio, open the `tests/test_validation.R` file.
#3. Click the "Run Tests" button in the top-right corner of the editor, or use the keyboard shortcut (usually Ctrl+Shift+T).

#Each test case checks a specific validation rule or function. When adding new features or modifying existing ones, it's recommended to write corresponding unit tests to ensure code correctness.
