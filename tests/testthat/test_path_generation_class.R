# tests/testthat/test_path_generation_class.R
library(testthat)
library(lubridate)
source("../../R/path_generation_class.R")

context("Path Generation")

test_that("PathGenerator generates correct paths", {
  path_generator <- PathGenerator$new(base_path = tempdir()) # Use tempdir() for testing
  
  sample_date <- as.Date("2023-05-10")
  expected_path <- file.path(tempdir(), "2023", "05", "10", "RegionA", "DetectorB", "PlotX")
  
  generated_path <- path_generator$generate("PlotX", sample_date, "DetectorB", "RegionA")
  
  expect_equal(generated_path, expected_path)
  expect_true(dir.exists(generated_path)) # Check if directory was created
})
