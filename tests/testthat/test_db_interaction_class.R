# tests/testthat/test_db_interaction_class.R
library(testthat)
source("../../R/db_interaction_class.R")
source("../../R/csv_mapping.R")

context("Database Interactions with CSV Data")

# Helper function to create test CSV files
create_test_csv_files <- function(temp_dir) {
  main_file <- file.path(temp_dir, "test_data.csv")
  species_file <- file.path(temp_dir, "test_data_species.csv")
  
  # Create test data using CSV field names
  main_data <- data.frame(
    plot_code = "TestPlot",
    su = 1,
    sample_date = "2023-01-15",
    detector = "TestDetector",
    region = "TestRegion",
    stringsAsFactors = FALSE
  )
  
  species_data <- data.frame(
    plot_code = "TestPlot",
    subplot = 1,
    species_name = "Species1",
    species_code = "SP1",
    species_cover = 80,
    vegetation_layer = "Tree",
    species_notes = "Test note",
    stringsAsFactors = FALSE
  )
  
  write.csv(main_data, main_file, row.names = FALSE)
  write.csv(species_data, species_file, row.names = FALSE)
  
  return(main_file)
}

test_that("DatabaseHandler adds and retrieves CSV data", {
  # Use an in-memory database for testing
  db_handler <- DatabaseHandler$new(":memory:")
  
  # Create temporary CSV files
  temp_dir <- tempdir()
  csv_path <- create_test_csv_files(temp_dir)
  
  # Add plot data
  plot_data_id <- db_handler$add_plot_data(
    filepath = csv_path,
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
  expect_equal(retrieved_data$filepath[1], csv_path)
  expect_equal(retrieved_data$plot_code[1], "TestPlot")
  expect_equal(retrieved_data$sample_date[1], "2023-01-15")
  expect_equal(retrieved_data$detector[1], "TestDetector")
  expect_equal(retrieved_data$region[1], "TestRegion")
  expect_equal(retrieved_data$validation_status[1], "Success")
  
  # Clean up
  unlink(c(csv_path, sub("\\.csv$", "_species.csv", csv_path)))
})

test_that("DatabaseHandler processes DataSource object correctly", {
  db_handler <- DatabaseHandler$new(":memory:")
  temp_dir <- tempdir()
  csv_path <- create_test_csv_files(temp_dir)
  
  # Create DataSource
  data_source <- DataSource$new(csv_path)
  
  # Process data source
  plot_data_id <- db_handler$process_data_source(
    data_source = data_source,
    validation_status = "valid",
    report_path = "report.html"
  )
  
  # Verify data was stored correctly
  history <- db_handler$get_plot_history_with_images()
  expect_equal(nrow(history$plot_data), 1)
  expect_equal(history$plot_data$plot_code[1], "TestPlot")
  
  # Clean up
  unlink(c(csv_path, sub("\\.csv$", "_species.csv", csv_path)))
})

test_that("DatabaseHandler handles image data with CSV files", {
  db_handler <- DatabaseHandler$new(":memory:")
  temp_dir <- tempdir()
  csv_path <- create_test_csv_files(temp_dir)
  
  # Create test image paths
  image_paths <- c(
    file.path(temp_dir, "image1.jpg"),
    file.path(temp_dir, "image2.jpg")
  )
  
  # Create dummy image files
  file.create(image_paths)
  
  # Add data with images
  plot_data_id <- db_handler$add_plot_data(
    filepath = csv_path,
    plot_code = "TestPlot",
    sample_date = "2023-01-15",
    detector = "TestDetector",
    region = "TestRegion",
    validation_status = "Success",
    report_path = "test_report_path.html"
  )
  
  # Add image data
  for (image_path in image_paths) {
    db_handler$add_image_data(plot_data_id, image_path)
  }
  
  # Retrieve and verify image data
  history <- db_handler$get_plot_history_with_images()
  expect_equal(nrow(history$images), 2)
  expect_true(all(image_paths %in% history$images$image_path))
  
  # Clean up
  unlink(c(csv_path, sub("\\.csv$", "_species.csv", csv_path), image_paths))
})
