# tests/testthat/test_validation_classes.R # nolint: commented_code_linter.
#' @title Test suite for validation classes
#' @description Tests for the validation classes in the biological data validator package
#' @author Updated version based on original code

library(testthat)
library(openxlsx)

# Fix source paths to use package-relative paths
source(file.path(here::here(), "R", "data_classes.R"))
source(file.path(here::here(), "R", "report_class.R"))
source(file.path(here::here(), "R", "path_generation_class.R"))
source(file.path(here::here(), "R", "validation_classes.R"))
source(file.path(here::here(), "R", "utils.R"))
source(file.path(here::here(), "R", "csv_mapping.R"))

context("Validation Classes")

#' Create a dummy Excel file for testing
#'
#' @param file_path Path where the Excel file will be saved
#' @param sheet1_data Data frame for Sheet1 (or NULL)
#' @param sheet2_data Data frame for Sheet2 (or NULL)
#' @return The file path (invisibly)
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

  invisible(file_path)
}

#' Create a dummy CSV file for testing
#'
#' @param file_path Path where the CSV file will be saved
#' @param sheet1_data Data frame for Sheet1 (or NULL)
#' @param sheet2_data Data frame for Sheet2 (or NULL)
#' @return The file path (invisibly)
create_dummy_csv <- function(file_path, sheet1_data, sheet2_data) {
  if (!is.null(sheet1_data) && !is.null(sheet2_data)) {
    write.csv(sheet1_data, file_path, row.names = FALSE)
    write.csv(sheet2_data, gsub("\\.csv", "_sheet2\\.csv", file_path), row.names = FALSE) # Save sheet2 to a separate file
  } else {
    stop("Both sheet1_data and sheet2_data must be provided for CSV files.")
  }
  invisible(file_path)
}

##' Load Excel data from file
##'
##' @param file_path Path to the Excel file
##' @param description Optional description for the data
##' @return ExcelData object with loaded data
#load_excel_data <- function(file_path, description = NULL) {
#  # Normalize file path
#  normalized_path <- tolower(file_path)
#
#  # Check if the file exists
#  if (!file.exists(normalized_path)) {
#    stop(paste("File does not exist:", normalized_path))
#  }
#
#  # Load data using ExcelData class
#  excel_data <- ExcelData$new(normalized_path)
#  excel_data$insert(normalized_path)
#
#  return(excel_data)
#}

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
  notes = c("Note 1", "Note 2", "Note 3", "") # Empty note for the last row
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

# Create temporary directory if it doesn't exist
if (!dir.exists(tempdir())) {
  dir.create(tempdir(), recursive = TRUE)
}

# Create dummy files
#valid_file_path_gen <- file.path(tempdir(), "valid_data_generated.xlsx")
#invalid_file_path_gen <- file.path(tempdir(), "invalid_data_generated.xlsx")

#create_dummy_excel(valid_file_path_gen, valid_sheet1_data_gen, valid_sheet2_data_gen)
#create_dummy_excel(invalid_file_path_gen, invalid_sheet1_data_gen, invalid_sheet2_data_gen)

# Create dummy CSV files
valid_file_path_csv <- file.path(tempdir(), "valid_data_generated.csv")
invalid_file_path_csv <- file.path(tempdir(), "invalid_data_generated.csv")

create_dummy_csv(valid_file_path_csv, valid_sheet1_data_gen, valid_sheet2_data_gen)
create_dummy_csv(invalid_file_path_csv, invalid_sheet1_data_gen, invalid_sheet2_data_gen)

## 2. Load/import testing files using ExcelData (after normalization)
#
## Define paths to the external data files and make lower case
##rel_path_valid_file_xlsx <- tolower("../../inst/extdata/Rilievo_Validazione.xlsx")
#rel_path_invalid_file_sample <- tolower("../../inst/extdata/sample_data_no_valid.xlsx")
#rel_path_valid_file_sample <- tolower("../../inst/extdata/sample_data_valid.xlsx")
#
## Try to load data from external files if they exist
#valid_excel_data_real <- tryCatch({
#  if (file.exists(rel_path_valid_file_xlsx)) {
#    load_excel_data(rel_path_valid_file_xlsx, "Rilievo Validazione")
#  } else {
#    message(paste(
#      "Warning: External valid file not found at",
#      rel_path_valid_file_xlsx,
#      ". Some tests may be skipped."
#    ))
#    NULL
#  }
#}, error = function(e) {
#  message(paste("Error loading file:", e$message))
#  NULL
#})
#
#invalid_excel_data_sample <- tryCatch({
#  if (file.exists(rel_path_invalid_file_sample)) {
#    load_excel_data(rel_path_invalid_file_sample, "Sample Data (non-valid)")
#  } else {
#    message(paste(
#      "Warning: External invalid (non-valid) file not found at",
#      rel_path_invalid_file_sample,
#      ". Some tests may be skipped."
#    ))
#    NULL
#  }
#}, error = function(e) {
#  message(paste("Error loading file:", e$message))
#  NULL
#})
#
#valid_excel_data_sample <- tryCatch({
#  if (file.exists(rel_path_valid_file_sample)) {
#    load_excel_data(rel_path_valid_file_sample, "Sample Data (valid)")
#  } else {
#    message(paste(
#      "Warning: External invalid (valid) file not found at",
#      rel_path_valid_file_sample,
#      ". Some tests may be skipped."
#    ))
#    NULL
#  }
#}, error = function(e) {
#  message(paste("Error loading file:", e$message))
#  NULL
#})

# --- Test Setup ---

# Initialize Validator for testing
path_generator <- PathGenerator$new(base_path = tempdir())
validator <- Validator$new(path_generator)

# --- Tests ---

# --- Comment out Excel-based tests ---
# Commenting out tests that use Excel files
# test_that("DataTypeValidationRule identifies correct errors (generated data)", { ... })
# test_that("MaxRowsValidationRule identifies correct errors (generated data)", { ... })
# test_that("UniqueSUValidationRule identifies correct errors (generated data)", { ... })
# test_that("NotesValidationRule identifies correct errors (generated data)", { ... })
# test_that("Validator applies all rules correctly (generated data)", { ... })

# --- Add CSV-based tests ---
#library(testthat)
#library(R6)
#source("../../R/validation_classes.R")
#source("../../R/csv_mapping.R")

context("Validation Classes - CSV")

# Helper function to create test CSV files
create_test_csvs <- function(main_data, species_data) {
  temp_dir <- tempdir()
  main_file <- file.path(temp_dir, "test.csv")
  species_file <- file.path(temp_dir, "test_species.csv")
  
  write.csv(main_data, main_file, row.names = FALSE)
  write.csv(species_data, species_file, row.names = FALSE)
  
  return(main_file)
}

test_that("DataTypeValidationRule works with CSV data", {
  # Arrange
  main_data <- data.frame(
    plot_code = "Test1",
    su = 1:4,
    sample_date = rep("2023-01-15", 4),
    detector = "Det1",
    longitude = rep(12.123456789012345, 4),
    latitude = rep(45.123456789012345, 4),
    region = "North"
  )
  
  species_data <- data.frame(
    plot_code = "Test1",
    subplot = 1:4,
    species_name = paste("Species", 1:4),
    species_code = paste0("SP", 1:4),
    species_cover = seq(50, 80, 10),
    vegetation_layer = c("Tree", "Shrub", "Herb", "Moss"),
    species_notes = paste("Note", 1:4)
  )
  
  csv_file <- create_test_csvs(main_data, species_data)
  data_source <- DataSource$new(csv_file)
  
  # Act
  rule <- DataTypeValidationRule$new()
  errors <- rule$check(data_source)
  
  # Assert
  expect_equal(nrow(errors), 0)
  unlink(c(csv_file, sub("\\.csv$", "_species.csv", csv_file)))
})

test_that("MaxRowsValidationRule works with CSV data", {
  # Arrange
  main_data <- data.frame(
    plot_code = c(rep("Test1", 5)),
    su = c(1, 2, 3, 4, 5),
    sample_date = rep("2023-01-15", 5),
    detector = "Det1",
    longitude = rep(12.123456789012345, 5),
    latitude = rep(45.123456789012345, 5),
    region = "North"
  )
  
  species_data <- data.frame(
    plot_code = "Test1",
    subplot = 1:4,
    species_name = paste("Species", 1:4),
    species_code = paste0("SP", 1:4),
    species_cover = seq(50, 80, 10),
    vegetation_layer = c("Tree", "Shrub", "Herb", "Moss"),
    species_notes = paste("Note", 1:4)
  )
  
  csv_file <- create_test_csvs(main_data, species_data)
  data_source <- DataSource$new(csv_file)
  
  # Act
  rule <- MaxRowsValidationRule$new()
  errors <- rule$check(data_source)
  
  # Assert
  expect_true(nrow(errors) > 0)
  expect_true(any(grepl("More than 4 rows with Plot.code", errors$Message)))
  unlink(c(csv_file, sub("\\.csv$", "_species.csv", csv_file)))
})

test_that("UniqueSUValidationRule works with CSV data", {
  # Arrange
  main_data <- data.frame(
    plot_code = "Test1",
    su = c(1, 1, 3, 4), # Duplicate SU
    sample_date = rep("2023-01-15", 4),
    detector = "Det1",
    longitude = rep(12.123456789012345, 4),
    latitude = rep(45.123456789012345, 4),
    region = "North"
  )
  
  species_data <- data.frame(
    plot_code = "Test1",
    subplot = 1:4,
    species_name = paste("Species", 1:4),
    species_code = paste0("SP", 1:4),
    species_cover = seq(50, 80, 10),
    vegetation_layer = c("Tree", "Shrub", "Herb", "Moss"),
    species_notes = paste("Note", 1:4)
  )
  
  csv_file <- create_test_csvs(main_data, species_data)
  data_source <- DataSource$new(csv_file)
  
  # Act
  rule <- UniqueSUValidationRule$new()
  errors <- rule$check(data_source)
  
  # Assert
  expect_true(nrow(errors) > 0)
  expect_true(any(grepl("Duplicate SU values found for Plot.code", errors$Message)))
  unlink(c(csv_file, sub("\\.csv$", "_species.csv", csv_file)))
})

test_that("Validator applies all rules correctly (CSV data)", {
  # Arrange
  main_data <- data.frame(
    plot_code = c("Test1", "Test1", "Test1", "Test1", "Test1"),
    su = c(1, 2, 3, 4, 5), # Exceeds max rows
    sample_date = rep("2023-01-15", 5),
    detector = "Det1",
    longitude = rep(12.123456789012345, 5),
    latitude = rep(45.123456789012345, 5),
    region = "North"
  )
  
  species_data <- data.frame(
    plot_code = "Test1",
    subplot = 1:4,
    species_name = paste("Species", 1:4),
    species_code = paste0("SP", 1:4),
    species_cover = seq(50, 80, 10),
    vegetation_layer = c("Tree", "Shrub", "Herb", "Moss"),
    species_notes = paste("Note", 1:4)
  )
  
  csv_file <- create_test_csvs(main_data, species_data)
  data_source <- DataSource$new(csv_file)
  
  # Act
  validator <- Validator$new(NULL)
  errors <- validator$validate(data_source)
  
  # Assert
  expect_true(nrow(errors) > 0)
  expect_true(any(errors$Level == "Error"))
  expect_true(any(errors$Level == "Warning"))
  unlink(c(csv_file, sub("\\.csv$", "_species.csv", csv_file)))
})

# Clean up generated files at the end of tests
on.exit({
  # List of files to remove
  files_to_remove <- c(
    valid_file_path_gen,
    invalid_file_path_gen,
    valid_file_path_csv,
    invalid_file_path_csv,
    file.path(tempdir(), "invalid_data_max_rows.xlsx"),
    file.path(tempdir(), "empty_data.xlsx"),
    invalid_file_path_max_rows_csv
  )

  # Remove each file if it exists
  for (file in files_to_remove) {
    tryCatch({
      if (file.exists(file)) {
        unlink(file)
      }
    }, error = function(e) {
      message(paste("Warning: Could not remove file", file, "-", e$message))
    })
  }
})

#library(testthat)
#library(R6)
#source("../../R/validation_classes.R")
#source("../../R/csv_mapping.R")

context("CSV Validation Rules")

# Helper function to create test CSV files
create_test_csvs <- function(main_data, species_data) {
  temp_dir <- tempdir()
  main_file <- file.path(temp_dir, "test.csv")
  species_file <- file.path(temp_dir, "test_species.csv")
  
  write.csv(main_data, main_file, row.names = FALSE)
  write.csv(species_data, species_file, row.names = FALSE)
  
  return(main_file)
}

test_that("DataTypeValidationRule works with CSV data", {
  # Create test data with CSV field names
  main_data <- data.frame(
    plot_code = "Test1",
    su = 1:4,
    sample_date = rep("2023-01-15", 4),
    detector = "Det1",
    longitude = rep(12.123456789012345, 4),
    latitude = rep(45.123456789012345, 4),
    region = "North"
  )
  
  species_data <- data.frame(
    plot_code = "Test1",
    subplot = 1:4,
    species_name = paste("Species", 1:4),
    species_code = paste0("SP", 1:4),
    species_cover = seq(50, 80, 10),
    vegetation_layer = c("Tree", "Shrub", "Herb", "Moss"),
    species_notes = paste("Note", 1:4)
  )
  
  csv_file <- create_test_csvs(main_data, species_data)
  data_source <- DataSource$new(csv_file)
  
  rule <- DataTypeValidationRule$new()
  errors <- rule$check(data_source)
  
  expect_equal(nrow(errors), 0)
  unlink(c(csv_file, sub("\\.csv$", "_species.csv", csv_file)))
})

# ...Add similar tests for other validation rules...