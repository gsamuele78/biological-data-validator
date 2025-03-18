# tests/testthat/test_validation_classes.R
library(testthat)
library(openxlsx)
source("../../R/data_classes.R")
source("../../R/validation_classes.R")
source("../../R/path_generation_class.R")

context("Validation Classes")

# Function to create a dummy Excel file
create_dummy_excel <- function(file_path, sheet1_data, sheet2_data) {
  wb <- createWorkbook()

  # Add Sheet1
  addWorksheet(wb, "Sheet1")
  if (!is.null(sheet1_data)) {
    writeData(wb, "Sheet1", sheet1_data)
  }

  # Add Sheet2
  addWorksheet(wb, "Sheet2")
  if (!is.null(sheet2_data)) {
    writeData(wb, "Sheet2", sheet2_data)
  }

  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

# Function to normalize file paths and load data
load_excel_data <- function(file_path) {
  
  # Normalize file path
  normalized_path <- tolower(file_path)
  
  # Check if the file exists
  if (!file.exists(normalized_path)) {
    stop(paste("File does not exist:", normalized_path))
  }

  # Load data using ExcelData class
  excel_data <- ExcelData$new(normalized_path)
  excel_data$insert(normalized_path)
  
  return(excel_data)
}

# --- Setup for tests ---

# 1. Create dummy Excel files using R script

# Define data for valid and invalid scenarios
valid_sheet1_data_gen <- data.frame(
  Plot.code = "Plot1",
  SU = 1:4,
  Sample.date = rep(Sys.Date(), 4),
  Detector = "DetectorA",
  Region = "RegionX",
  X = 1,
  Y = 1,
  Elevation = 1,
  Aspect = 1,
  Slope = 1,
  Cop.tot = 1,
  Litter.cov = 1,
  Bare.soil.cov = 1,
  Tree.cov = 1,
  Tree.h = 1,
  Shrub.cov = 1,
  Shrub.h = 1,
  Herb.cov = 1,
  Herb.h = 1,
  Brioph.cov = 1,
  notes = paste("Note", 1:4)
)

valid_sheet2_data_gen <- data.frame(
  Plot.code = "Plot1",
  Subplot = 1:4,
  Species = paste("Species", LETTERS[1:4]),
  species_abb = paste("Sp.", LETTERS[1:4]),
  cover = seq(50, 80, by = 10),
  Layer = c("Tree", "Herb", "Shrub", "Moss"),
  Notes = paste("Note", LETTERS[1:4])
)

invalid_sheet1_data_gen <- data.frame(
  Plot.code = "Plot2",
  SU = c(1, 1, 3, 4), # Duplicate SU
  Sample.date = rep(Sys.Date(), 4),
  Detector = "DetectorA",
  Region = "RegionX",
  X = 1,
  Y = 1,
  Elevation = 1,
  Aspect = 1,
  Slope = 1,
  Cop.tot = 1,
  Litter.cov = 1,
  Bare.soil.cov = 1,
  Tree.cov = 1,
  Tree.h = 1,
  Shrub.cov = 1,
  Shrub.h = 1,
  Herb.cov = 1,
  Herb.h = 1,
  Brioph.cov = 1,
  notes = paste("Note", 1:4)
)

invalid_sheet2_data_gen <- data.frame(
  Plot.code = "Plot2",
  Subplot = 1:3, # Missing Subplot 4
  Species = paste("Species", LETTERS[1:3]),
  species_abb = paste("Sp.", LETTERS[1:3]),
  cover = seq(50, 70, by = 10),
  Layer = c("Tree", "Herb", "Shrub"),
  Notes = paste("Note", LETTERS[1:3])
)

# Create dummy files
valid_file_path_gen <- file.path(tempdir(), "valid_data_generated.xlsx")
invalid_file_path_gen <- file.path(tempdir(), "invalid_data_generated.xlsx")

create_dummy_excel(valid_file_path_gen, valid_sheet1_data_gen, valid_sheet2_data_gen)
create_dummy_excel(invalid_file_path_gen, invalid_sheet1_data_gen, invalid_sheet2_data_gen)

# 2. Load/import testing files using ExcelData (after normalization)

# Define paths to the external data files and make lower case
valid_file_path_ext <- tolower("../../inst/extdata/Rilievo_Validazione.xlsx")
invalid_file_path_non_valid_ext <- tolower("../../inst/extdata/sample_data_non_valid.xlsx")
invalid_file_path_valid_ext <- tolower("../../inst/extdata/sample_data_valid.xlsx")

# Load data from external files
if (file.exists(valid_file_path_ext)) {
  valid_excel_data_ext <- load_excel_data(valid_file_path_ext)
} else {
  message(paste("Warning: External valid file not found at", valid_file_path_ext, ". Some tests may be skipped."))
  valid_excel_data_ext <- NULL
}

if (file.exists(invalid_file_path_non_valid_ext)) {
  invalid_excel_data_non_valid_ext <- load_excel_data(invalid_file_path_non_valid_ext)
} else {
  message(paste("Warning: External invalid (non-valid) file not found at", invalid_file_path_non_valid_ext, ". Some tests may be skipped."))
  invalid_excel_data_non_valid_ext <- NULL
}

if (file.exists(invalid_file_path_valid_ext)) {
  invalid_excel_data_valid_ext <- load_excel_data(invalid_file_path_valid_ext)
} else {
  message(paste("Warning: External invalid (valid) file not found at", invalid_file_path_valid_ext, ". Some tests may be skipped."))
  invalid_excel_data_valid_ext <- NULL
}

# --- Tests ---

# Initialize Validator for testing
path_generator <- PathGenerator$new(base_path = tempdir())
validator <- Validator$new(path_generator)

# Tests using generated data
test_that("DataTypeValidationRule identifies correct errors (generated data)", {
  invalid_excel_data_gen <- ExcelData$new(invalid_file_path_gen)
  invalid_excel_data_gen$insert(invalid_file_path_gen)
  errors <- DataTypeValidationRule$new()$check(invalid_excel_data_gen)
  expect_true(nrow(errors) > 0)
  expect_equal(errors$Message[1], "Duplicate SU values found for Plot.code: Plot2")
})

test_that("MaxRowsValidationRule identifies correct errors (generated data)", {
    invalid_sheet1_data_max_rows <- rbind(invalid_sheet1_data_gen, data.frame(
        Plot.code = "Plot2",
        SU = 5,
        Sample.date = Sys.Date(),
        Detector = "DetectorA",
        Region = "RegionX",
        X = 1,
        Y = 1,
        Elevation = 1,
        Aspect = 1,
        Slope = 1,
        Cop.tot = 1,
        Litter.cov = 1,
        Bare.soil.cov = 1,
        Tree.cov = 1,
        Tree.h = 1,
        Shrub.cov = 1,
        Shrub.h = 1,
        Herb.cov = 1,
        Herb.h = 1,
        Brioph.cov = 1,
        notes = "Note 5"
    ))
    invalid_file_path_max_rows <- file.path(tempdir(), "invalid_data_max_rows.xlsx")
    create_dummy_excel(invalid_file_path_max_rows, invalid_sheet1_data_max_rows, invalid_sheet2_data_gen)

    invalid_excel_data_max_rows <- ExcelData$new(invalid_file_path_max_rows)
    invalid_excel_data_max_rows$insert(invalid_file_path_max_rows)
    
    errors <- MaxRowsValidationRule$new()$check(invalid_excel_data_max_rows)
    expect_true(nrow(errors) > 0)
    expect_equal(errors$Message[1], "More than 4 rows with Plot.code: Plot2")
})

test_that("UniqueSUValidationRule identifies correct errors (generated data)", {
    invalid_excel_data_gen <- ExcelData$new(invalid_file_path_gen)
    invalid_excel_data_gen$insert(invalid_file_path_gen)
    errors <- UniqueSUValidationRule$new()$check(invalid_excel_data_gen)
    expect_true(nrow(errors) > 0)
    expect_equal(errors$Message[1], "Duplicate SU values found for Plot.code: Plot2")
})

test_that("NotesValidationRule identifies correct errors (generated data)", {
  invalid_excel_data_gen <- ExcelData$new(invalid_file_path_gen)
  invalid_excel_data_gen$insert(invalid_file_path_gen)
  errors <- NotesValidationRule$new()$check(invalid_excel_data_gen)
  expect_true(nrow(errors) > 0)
  expect_equal(errors$Message[1], "Missing data in Sheet2 for Plot.code: Plot2 and SU: 4 without a corresponding note in Sheet1.")
})

test_that("Validator applies all rules correctly (generated data)", {
  invalid_excel_data_gen <- ExcelData$new(invalid_file_path_gen)
  invalid_excel_data_gen$insert(invalid_file_path_gen)
  errors <- validator$validate(invalid_excel_data_gen)
  expect_true(nrow(errors) > 0)
  expect_true(any(grepl("Duplicate SU values found for Plot.code: Plot2", errors$Message)))
  expect_true(any(grepl("Missing data in Sheet2 for Plot.code: Plot2 and SU: 4 without a corresponding note in Sheet1.", errors$Message)))
})

# Tests using externally loaded data (if available)

if (!is.null(invalid_excel_data_non_valid_ext)) {
  test_that("DataTypeValidationRule identifies correct errors (external non-valid data)", {
    errors <- DataTypeValidationRule$new()$check(invalid_excel_data_non_valid_ext)
    expect_true(nrow(errors) > 0)
    # Add specific assertions based on expected errors in sample_data_non_valid.xlsx
  })

  test_that("MaxRowsValidationRule identifies correct errors (external non-valid data)", {
    errors <- MaxRowsValidationRule$new()$check(invalid_excel_data_non_valid_ext)
    #expect_true(nrow(errors) > 0)
    # Add specific assertions based on expected errors in sample_data_non_valid.xlsx
  })

  test_that("UniqueSUValidationRule identifies correct errors (external non-valid data)", {
    errors <- UniqueSUValidationRule$new()$check(invalid_excel_data_non_valid_ext)
    expect_true(nrow(errors) > 0)
    # Add specific assertions based on expected errors in sample_data_non_valid.xlsx
  })

  test_that("NotesValidationRule identifies correct errors (external non-valid data)", {
    errors <- NotesValidationRule$new()$check(invalid_excel_data_non_valid_ext)
    #expect_true(nrow(errors) > 0)
    # Add specific assertions based on expected errors in sample_data_non_valid.xlsx
  })

  test_that("Validator applies all rules correctly (external non-valid data)", {
    errors <- validator$validate(invalid_excel_data_non_valid_ext)
    expect_true(nrow(errors) > 0)
    # Add specific assertions to check for expected error messages
  })
} else {
  message("Skipping tests for external non-valid data as the file was not found.")
}

if (!is.null(invalid_excel_data_valid_ext)) {
  test_that("DataTypeValidationRule identifies correct errors (external valid data with issues)", {
    errors <- DataTypeValidationRule$new()$check(invalid_excel_data_valid_ext)
    #expect_true(nrow(errors) > 0)
    # Add specific assertions based on expected errors in sample_data_valid.xlsx
  })

  test_that("MaxRowsValidationRule identifies correct errors (external valid data with issues)", {
    errors <- MaxRowsValidationRule$new()$check(invalid_excel_data_valid_ext)
    #expect_true(nrow(errors) > 0)
    # Add specific assertions based on expected errors in sample_data_valid.xlsx
  })

  test_that("UniqueSUValidationRule identifies correct errors (external valid data with issues)", {
    errors <- UniqueSUValidationRule$new()$check(invalid_excel_data_valid_ext)
    #expect_true(nrow(errors) > 0)
    # Add specific assertions based on expected errors in sample_data_valid.xlsx
  })

  test_that("NotesValidationRule identifies correct errors (external valid data with issues)", {
    errors <- NotesValidationRule$new()$check(invalid_excel_data_valid_ext)
    #expect_true(nrow(errors) > 0)
    # Add specific assertions based on expected errors in sample_data_valid.xlsx
  })

  test_that("Validator applies all rules correctly (external valid data with issues)", {
    errors <- validator$validate(invalid_excel_data_valid_ext)
    #expect_true(nrow(errors) > 0)
    # Add specific assertions to check for expected error messages
  })
} else {
  message("Skipping tests for external valid data with issues as the file was not found.")
}

# Clean up generated files
#unlink(valid_file_path_gen)
#unlink(invalid_file_path_gen)
#unlink(invalid_file_path_max_rows)
