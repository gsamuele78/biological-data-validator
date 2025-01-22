# tests/testthat/test_db_interaction_class.R
library(testthat)
source("../../R/db_interaction_class.R")

context("Database Interactions")

test_that("DatabaseHandler adds and retrieves plot data", {
  # Use an in-memory database for testing
  db_handler <- DatabaseHandler$new(":memory:") 
  
  # Add plot data
  plot_data_id <- db_handler$add_plot_data(
    filepath = "test_filepath.xlsx",
    plot_code = "TestPlot",
    sample_date = "2023-01-15",
    detector = "TestDetector",
    region = "TestRegion",
    validation_status = "Success",
    report_path = "test_report_path.html"
  )
  
  # Retrieve plot data
  retrieved_data <- db_handler$get_plot_history(plot_code = "TestPlot")
  
  # Assertions
  expect_equal(nrow(retrieved_data), 1)
  expect_equal(retrieved_data$filepath[1], "test_filepath.xlsx")
  expect_equal(retrieved_data$plot_code[1], "TestPlot")
  expect_equal(retrieved_data$sample_date[1], "2023-01-15")
  expect_equal(retrieved_data$detector[1], "TestDetector")
  expect_equal(retrieved_data$region[1], "TestRegion")
  expect_equal(retrieved_data$validation_status[1], "Success")
  expect_equal(retrieved_data$report_path[1], "test_report_path.html")
  
  # Clean up (not necessary for in-memory database, but good practice)
  # dbDisconnect(db_handler$db)
})

# Add more tests for other DatabaseHandler methods (add_image_data, update_plot_data, delete_plot_data) as needed.
