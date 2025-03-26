# tests/testthat/test_path_generation_class.R
library(testthat)
library(lubridate)
source("../../R/path_generation.R")
source("../../R/csv_mapping.R")

context("CSV Path Generation")

test_that("PathGenerator generates correct CSV paths", {
  # Setup
  path_generator <- PathGenerator$new(base_path = tempdir())
  sample_date <- as.Date("2023-05-10")
  
  # Test base directory path
  expected_base_path <- file.path(
    tempdir(), "2023", "05", "10", "RegionA", "DetectorB", "PlotX"
  )
  
  generated_path <- path_generator$generate(
    "PlotX", sample_date, "DetectorB", "RegionA"
  )
  
  expect_equal(generated_path, expected_base_path)
  expect_true(dir.exists(generated_path))
})

test_that("PathGenerator generates correct CSV file paths", {
  # Setup test data
  test_data <- list(
    Plot.code = "Test1",
    Sample.date = as.Date("2023-05-10"),
    Detector = "Det1",
    Region = "North"
  )
  sheet1_data <- structure(test_data, class = "Sheet1Data")
  
  path_generator <- PathGenerator$new(tempdir())
  
  # Test CSV paths generation
  paths <- path_generator$generate_csv_paths(sheet1_data)
  
  # Extract expected base components
  base_dir <- file.path(
    tempdir(), "2023", "05", "10", "North", "Det1", "Test1"
  )
  expected_main <- file.path(base_dir, "data_Test1_20230510.csv")
  expected_species <- file.path(base_dir, "data_Test1_20230510_species.csv")
  
  # Verify paths
  expect_equal(paths$main_path, expected_main)
  expect_equal(paths$species_path, expected_species)
  expect_true(dir.exists(dirname(paths$main_path)))
})

test_that("PathGenerator handles different path formats", {
  test_formats <- c(
    "date/region/detector/plot",
    "region/date/detector/plot",
    "detector/region/date/plot"
  )
  
  for (format in test_formats) {
    path_generator <- PathGenerator$new(
      base_path = tempdir(),
      path_format = format
    )
    
    paths <- path_generator$generate_csv_paths(
      structure(list(
        Plot.code = "Test1",
        Sample.date = as.Date("2023-05-10"),
        Detector = "Det1",
        Region = "North"
      ), class = "Sheet1Data")
    )
    
    expect_true(!is.null(paths$main_path))
    expect_true(!is.null(paths$species_path))
    expect_true(dir.exists(dirname(paths$main_path)))
  }
})

# Clean up test directories
on.exit({
  unlink(file.path(tempdir(), c("2023", "North", "Det1")), recursive = TRUE)
})
