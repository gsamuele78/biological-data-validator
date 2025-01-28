# tests/testthat/test_validation_classes.R
library(testthat)
library(openxlsx) # Make sure to include this for Excel operations
source("../../R/data_classes.R")
source("../../R/validation_classes.R")
source("../../R/path_generation_class.R")

context("Validation Classes")

# --- Helper Function to Create Dummy Excel Data ---
create_dummy_excel_data <- function(mode = "generate", file_path = NULL) {
  if (mode == "generate") {
    # Generate dummy data using R
    sheet1_data <- list(
      Sheet1Data$new(list(Plot.code = "Plot1", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 1")),
      Sheet1Data$new(list(Plot.code = "Plot1", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 2")),
      Sheet1Data$new(list(Plot.code = "Plot1", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 3")),
      Sheet1Data$new(list(Plot.code = "Plot1", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 4"))
    )
    sheet2_data <- list(
      Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
      Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
      Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C")),
      Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 4, Species = "Species D", species_abb = "Sp. D", cover = 80, Layer = "Moss", Notes = "Note D"))
    )
    
    invalid_sheet1_data <- list(
      Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 1")),
      Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 2")), # Duplicate SU
      Sheet1Data$new(list(Plot.code = "Plot2", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 3")),
      Sheet1Data$new(list(Plot.code = "Plot2", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1, notes = "Note 4"))
    )
    invalid_sheet2_data <- list(
      Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
      Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
      Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C"))
      # Missing Subplot 4
    )
    
    # Create a new workbook
    wb <- createWorkbook()
    
    # Add Sheet1 and write data
    addWorksheet(wb, "Sheet1")
    sheet1_df <- do.call(rbind, lapply(sheet1_data, function(x) data.frame(x$data)))
    writeData(wb, "Sheet1", sheet1_df)
    
    # Add Sheet2 and write data
    addWorksheet(wb, "Sheet2")
    sheet2_df <- do.call(rbind, lapply(sheet2_data, function(x) data.frame(x$data)))
    writeData(wb, "Sheet2", sheet2_df)
    
    # Add Invalid Sheet1
    addWorksheet(wb, "InvalidSheet1")
    invalid_sheet1_df <- do.call(rbind, lapply(invalid_sheet1_data, function(x) data.frame(x$data)))
    writeData(wb, "InvalidSheet1", invalid_sheet1_df)
    
    # Add Invalid Sheet2
    addWorksheet(wb, "InvalidSheet2")
    invalid_sheet2_df <- do.call(rbind, lapply(invalid_sheet2_data, function(x) data.frame(x$data)))
    writeData(wb, "InvalidSheet2", invalid_sheet2_df)
    
    # Save the workbook
    dummy_file_path <- tempfile(pattern = "dummy_data", fileext = ".xlsx")
    saveWorkbook(wb, dummy_file_path, overwrite = TRUE)
    
    # Return ExcelData object
    excel_data <- ExcelData$new(dummy_file_path)
    
    # Read data from Sheet1
    sheet1_data <- lapply(1:nrow(sheet1_df), function(i) {
      Sheet1Data$new(as.list(sheet1_df[i, ]))
    })
    excel_data$sheet1_data <- sheet1_data
    
    # Read data from Sheet2
    sheet2_data <- lapply(1:nrow(sheet2_df), function(i) {
      Sheet2Data$new(as.list(sheet2_df[i, ]))
    })
    excel_data$sheet2_data <- sheet2_data

     # Read data from InvalidSheet1
     invalid_sheet1_data <- lapply(1:nrow(invalid_sheet1_df), function(i) {
      Sheet1Data$new(as.list(invalid_sheet1_df[i, ]))
    })
    excel_data$invalid_sheet1_data <- invalid_sheet1_data
    
    # Read data from InvalidSheet2
    invalid_sheet2_data <- lapply(1:nrow(invalid_sheet2_df), function(i) {
      Sheet2Data$new(as.list(invalid_sheet2_df[i, ]))
    })
    excel_data$invalid_sheet2_data <- invalid_sheet2_data
    
    return(excel_data)
    
  } else if (mode == "load" && !is.null(file_path)) {
    # Load data from the specified file
    excel_data <- ExcelData$new(file_path)
    
    # Dynamically read all sheets and assign data to corresponding lists
    sheet_names <- getSheetNames(file_path)
    for (sheet_name in sheet_names) {
      sheet_data <- read.xlsx(file_path, sheet = sheet_name)
      
      # Assuming a convention for naming sheet data lists in ExcelData
      if (grepl("Sheet1", sheet_name)) {
        data_list_name <- "sheet1_data"
        data_class <- Sheet1Data
      } else if (grepl("Sheet2", sheet_name)) {
        data_list_name <- "sheet2_data"
        data_class <- Sheet2Data
      } else if (grepl("InvalidSheet1", sheet_name)) {
        data_list_name <- "invalid_sheet1_data"
        data_class <- Sheet1Data
      } else if (grepl("InvalidSheet2", sheet_name)) {
        data_list_name <- "invalid_sheet2_data"
        data_class <- Sheet2Data
      }
      else{
        next # Skip sheets that don't match the naming convention
      }
      
      # Convert data frame rows to list of SheetData objects
      data_list <- lapply(1:nrow(sheet_data), function(i) {
        data_class$new(as.list(sheet_data[i, ]))
      })
      
      # Assign the list to the corresponding property in ExcelData
      excel_data[[data_list_name]] <- data_list
    }
    
    return(excel_data)
  } else {
    stop("Invalid mode or missing file path.")
  }
}

# --- Example Usage and Test Setup ---

# 1. Generate dummy data using R
valid_excel_data <- create_dummy_excel_data(mode = "generate")
invalid_excel_data <- valid_excel_data
# 2. Load data from a file (replace with your actual file paths)
path_generator <- PathGenerator$new(base_path = tempdir())
valid_excel_data_from_file <- create_dummy_excel_data(mode = "load", file_path = "../../data/sample_data_valid.xlsx")
invalid_excel_data_from_file <- create_dummy_excel_data(mode = "load", file_path = "../../data/sample_data_non_valid.xlsx")

# --- Initialize Validator for testing ---
validator <- Validator$new(path_generator)

# --- Test Cases ---

test_that("DataTypeValidationRule identifies correct errors", {
  errors <- DataTypeValidationRule$new()$check(invalid_excel_data)
  expect_true(nrow(errors) > 0) # Expecting errors
  expect_equal(errors$Message[1], "Duplicate SU values found for Plot.code: Plot2") # Example error message
  
  errors_file <- DataTypeValidationRule$new()$check(invalid_excel_data_from_file)
  expect_true(nrow(errors_file) > 0)
  expect_equal(errors_file$Message[1], "Duplicate SU values found for Plot.code: Plot2")
})

test_that("MaxRowsValidationRule identifies correct errors", {
  # Test with more than 4 rows for a plot code
  invalid_excel_data_max_rows <- create_dummy_excel_data(mode = "generate")
  invalid_excel_data_max_rows$sheet1_data <- append(invalid_excel_data_max_rows$sheet1_data, 
                                                    list(Sheet1Data$new(list(Plot.code = "Plot1", SU = 5, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", X = 1, Y = 1, Elevation = 1, Aspect = 1, Slope = 1, Cop.tot = 1, Litter.cov = 1, Bare.soil.cov = 1, Tree.cov = 1, Tree.h = 1, Shrub.cov = 1, Shrub.h = 1, Herb.cov = 1, Herb.h = 1, Brioph.cov = 1, notes = "Note 5"))))
  
  errors <- MaxRowsValidationRule$new()$check(invalid_excel_data_max_rows)
  expect_true(nrow(errors) > 0)
  expect_equal(errors$Message[1], "More than 4 rows with Plot.code: Plot1")
  
  invalid_excel_data_max_rows_from_file <- create_dummy_excel_data(mode = "load", file_path = "../../data/sample_data_non_valid.xlsx")
  errors_file <- MaxRowsValidationRule$new()$check(invalid_excel_data_max_rows_from_file)
  expect_true(nrow(errors_file) > 0)
  expect_equal(errors_file$Message[1], "More than 4 rows with Plot.code: Plot2")
})

test_that("UniqueSUValidationRule identifies correct errors", {
  errors <- UniqueSUValidationRule$new()$check(invalid_excel_data)
  expect_true(nrow(errors) > 0)
  expect_equal(errors$Message[1], "Duplicate SU values found for Plot.code: Plot2")
  
  errors_file <- UniqueSUValidationRule$new()$check(invalid_excel_data_from_file)
  expect_true(nrow(errors_file) > 0)
  expect_equal(errors_file$Message[1], "Duplicate SU values found for Plot.code: Plot2")
})

test_that("NotesValidationRule identifies correct errors", {
  errors <- NotesValidationRule$new()$check(invalid_excel_data)
  expect_true(nrow(errors) > 0)
  expect_equal(errors$Message[1], "Missing data in InvalidSheet2 for Plot.code: Plot2 and SU: 4 without a corresponding note in InvalidSheet1.")
  
  errors_file <- NotesValidationRule$new()$check(invalid_excel_data_from_file)
  expect_true(nrow(errors_file) > 0)
  expect_equal(errors_file$Message[1], "Missing data in InvalidSheet2 for Plot.code: Plot2 and SU: 4 without a corresponding note in InvalidSheet1.")
})

test_that("Validator applies all rules correctly", {
  errors <- validator$validate(invalid_excel_data)
  expect_true(nrow(errors) > 0)
  
  # Check for specific error messages to ensure all rules are applied
  expect_true(any(grepl("Duplicate SU values found for Plot.code: Plot2", errors$Message)))
  expect_true(any(grepl("Missing data in InvalidSheet2 for Plot.code: Plot2 and SU: 4 without a corresponding note in InvalidSheet1.", errors$Message)))
  
  errors_file <- validator$validate(invalid_excel_data_from_file)
  expect_true(nrow(errors_file) > 0)
  expect_true(any(grepl("Duplicate SU values found for Plot.code: Plot2", errors_file$Message)))
  expect_true(any(grepl("Missing data in InvalidSheet2 for Plot.code: Plot2 and SU: 4 without a corresponding note in InvalidSheet1.", errors_file$Message)))
})
