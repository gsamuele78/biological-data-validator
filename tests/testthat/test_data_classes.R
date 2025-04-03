# tests/testthat/test_data_classes.R # nolint: commented_code_linter.
library(testthat)
library(R6)
source("../../R/data_classes.R")
source("../../R/csv_mapping.R")

context("CSV Data Classes")

test_that("DataSource loads CSV files correctly", {
  # Create test files
  temp_dir <- tempdir()
  main_file <- file.path(temp_dir, "test.csv")
  species_file <- file.path(temp_dir, "test_species.csv")
  
  # Test data using CSV field names
  main_data <- data.frame(
    plot_code = "Test1",
    su = 1,
    sample_date = "2023-01-01",
    detector = "Det1",
    longitude = 12.123456789012345,
    latitude = 45.123456789012345,
    region = "North",
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
  
  ds <- DataSource$new(main_file)
  expect_equal(ds$file_type, "csv")
  expect_true(length(ds$sheet1_data) > 0)
  expect_true(length(ds$sheet2_data) > 0)
  
  unlink(c(main_file, species_file))
})

test_that("DataSource handles file encoding detection correctly", {
  # Create test files
  temp_dir <- tempdir()
  main_file <- file.path(temp_dir, "test.csv")
  species_file <- file.path(temp_dir, "test_species.csv")
  
  # Test data using CSV field names
  main_data <- data.frame(
    plot_code = "Test1",
    su = 1,
    sample_date = "2023-01-01",
    detector = "Det1",
    longitude = 12.123456789012345,
    latitude = 45.123456789012345,
    region = "North",
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
  
  # Write files with UTF-8 encoding
  write.csv(main_data, main_file, row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(species_data, species_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Test that encoding detection works
  ds <- DataSource$new(main_file)
  expect_equal(ds$file_type, "csv")
  expect_true(length(ds$sheet1_data) > 0)
  expect_true(length(ds$sheet2_data) > 0)
  
  unlink(c(main_file, species_file))
})

test_that("DataSource export_data method works correctly", {
  # Create test files
  temp_dir <- tempdir()
  main_file <- file.path(temp_dir, "test.csv")
  species_file <- file.path(temp_dir, "test_species.csv")
  
  # Test data using CSV field names
  main_data <- data.frame(
    plot_code = "Test1",
    su = 1,
    sample_date = "2023-01-01",
    detector = "Det1",
    longitude = 12.123456789012345,
    latitude = 45.123456789012345,
    region = "North",
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
  
  # Load data
  ds <- DataSource$new(main_file)
  
  # Test export function
  output_dir <- file.path(temp_dir, "output")
  output_file <- file.path(output_dir, "exported.csv")
  
  # Export data
  result <- ds$export_data(output_file)
  
  # Check that files were created
  expect_true(file.exists(result$main_path))
  expect_true(file.exists(result$species_path))
  
  # Clean up
  unlink(c(main_file, species_file, result$main_path, result$species_path), recursive = TRUE)
  if(dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
})

test_that("DataSource handles invalid filepath correctly", {
  # Test with invalid filepath
  expect_error(DataSource$new(NULL), "Invalid filepath")
  expect_error(DataSource$new(NA), "Invalid filepath")
  expect_error(DataSource$new(c("file1.csv", "file2.csv")), "Invalid filepath")
})

# Additional tests for error handling
