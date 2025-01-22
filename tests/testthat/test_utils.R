# tests/testthat/test_utils.R
library(testthat)
source("../../R/utils.R")

context("Utility Functions")

test_that("setup_logging creates a log file", {
  log_file_path <- file.path(tempdir(), "test_log.log")
  setup_logging(log_level = "DEBUG", log_file = log_file_path) # Use DEBUG level for testing
  
  log_info("This is a test log message.")
  
  expect_true(file.exists(log_file_path))
  
  # Read the log file and check its content (optional)
  log_content <- readLines(log_file_path)
  expect_true(any(grepl("This is a test log message.", log_content)))
  
  # Clean up the log file (optional)
  # file.remove(log_file_path)
})
