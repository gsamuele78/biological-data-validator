library(testthat)
library(R6)

# Load required files
source("../../R/csv_mapping.R")
source("../../R/data_classes.R")
source("../../R/validation_classes.R")

context("CSV Data Validation")

test_that("CSV file pair loading works", {
  # Create temporary CSV files for testing
  temp_dir <- tempdir()
  main_csv <- file.path(temp_dir, "test_data.csv")
  species_csv <- file.path(temp_dir, "test_data_species.csv")
  
  # Create test data
  main_data <- data.frame(
    plot_code = "A1",
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
    plot_code = "A1",
    subplot = 1,
    species_name = "Species1",
    species_code = "SP1",
    species_cover = 75,
    vegetation_layer = "Herb",
    species_notes = "Test",
    stringsAsFactors = FALSE
  )
  
  # Write test files
  write.csv(main_data, main_csv, row.names = FALSE)
  write.csv(species_data, species_csv, row.names = FALSE)
  
  # Test data source creation
  data_source <- DataSource$new(main_csv)
  
  expect_equal(data_source$file_type, "csv")
  expect_equal(length(data_source$sheet1_data), 1)
  expect_equal(length(data_source$sheet2_data), 1)
  
  # Test field mappings
  expect_equal(data_source$sheet1_data[[1]]$Plot.code, "A1")
  expect_equal(data_source$sheet1_data[[1]]$SU, 1)
  
  # Clean up
  unlink(c(main_csv, species_csv))
})

test_that("CSV validation detects missing species file", {
  # Create temporary CSV file without species file
  temp_dir <- tempdir()
  main_csv <- file.path(temp_dir, "test_data.csv")
  
  # Create test data
  main_data <- data.frame(
    plot_code = "A1",
    su = 1,
    sample_date = "2023-01-15",
    stringsAsFactors = FALSE
  )
  
  # Write test file
  write.csv(main_data, main_csv, row.names = FALSE)
  
  # Create validator
  validator <- Validator$new(NULL)
  
  # Test validation
  expect_error(DataSource$new(main_csv), "Species CSV file not found")
  
  # Clean up
  unlink(main_csv)
})

test_that("CSV field mapping works correctly", {
  # Test data with CSV field names
  csv_data <- list(
    plot_code = "A1",
    su = 1,
    sample_date = "2023-01-15",
    detector = "Det1",
    longitude = 12.123456789012345,
    latitude = 45.123456789012345
  )
  
  # Convert to internal names
  internal_names <- list()
  for (internal_name in names(SHEET1_CSV_MAPPING)) {
    csv_name <- SHEET1_CSV_MAPPING[[internal_name]]
    if (csv_name %in% names(csv_data)) {
      internal_names[[internal_name]] <- csv_data[[csv_name]]
    }
  }
  
  expect_equal(internal_names$Plot.code, "A1")
  expect_equal(internal_names$X, 12.123456789012345)
  expect_equal(internal_names$Y, 45.123456789012345)
})
