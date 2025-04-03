# tests/testthat/test_file_encoding.R # nolint: commented_code_linter.
library(testthat)
library(R6)

# Load required files
source("../../R/data_classes.R")
source("../../R/utils.R")
source("../../R/csv_mapping.R")

context("File Encoding Detection")

test_that("File encoding detection works correctly with different encodings", {
  # Create test files with different encodings
  temp_dir <- tempdir()
  
  # UTF-8 file
  utf8_file <- file.path(temp_dir, "utf8_test.csv")
  utf8_species_file <- file.path(temp_dir, "utf8_test_species.csv")
  
  # Test data with some non-ASCII characters
  utf8_data <- data.frame(
    plot_code = "TestPlot",
    su = 1,
    sample_date = "2023-01-01",
    detector = "Tèst Détector", # non-ASCII characters
    region = "Régión", # non-ASCII characters
    stringsAsFactors = FALSE
  )
  
  utf8_species_data <- data.frame(
    plot_code = "TestPlot",
    subplot = 1,
    species_name = "Spécies Tèst", # non-ASCII characters
    species_code = "SP1",
    species_cover = 80,
    vegetation_layer = "Tree",
    stringsAsFactors = FALSE
  )
  
  # Write files with UTF-8 encoding
  write.csv(utf8_data, utf8_file, row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(utf8_species_data, utf8_species_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Test loading with encoding detection
  tryCatch({
    # Test the load_csv_data function directly
    result <- load_csv_data(utf8_file, utf8_species_file)
    
    # Check that data was loaded correctly
    expect_true(!is.null(result$main_data))
    expect_true(!is.null(result$species_data))
    
    # Check that non-ASCII characters were preserved
    expect_true(any(grepl("Tèst Détector", as.character(result$main_data$detector))))
    expect_true(any(grepl("Régión", as.character(result$main_data$region))))
    expect_true(any(grepl("Spécies Tèst", as.character(result$species_data$species_name))))
    
    # Test through DataSource class
    ds <- DataSource$new(utf8_file)
    expect_equal(ds$file_type, "csv")
    expect_true(length(ds$sheet1_data) > 0)
    expect_true(length(ds$sheet2_data) > 0)
    
    # Check that non-ASCII characters were preserved
    expect_true(any(grepl("Tèst", ds$sheet1_data[[1]]$Detector)))
    expect_true(any(grepl("Régión", ds$sheet1_data[[1]]$Region)))
    expect_true(any(grepl("Spécies", ds$sheet2_data[[1]]$Species)))
    
  }, error = function(e) {
    fail(paste("Error in file encoding test:", e$message))
  })
  
  # Clean up
  unlink(c(utf8_file, utf8_species_file))
})

test_that("File encoding detection handles invalid files gracefully", {
  # Create an empty file
  temp_dir <- tempdir()
  empty_file <- file.path(temp_dir, "empty.csv")
  file.create(empty_file)
  
  # Try to detect encoding of empty file
  tryCatch({
    encoding <- readr::guess_encoding(empty_file)
    expect_true(is.data.frame(encoding))
    # Empty file might return empty data frame or UTF-8 as default
    if (nrow(encoding) > 0) {
      expect_equal(encoding$encoding[1], "UTF-8")
    }
  }, error = function(e) {
    # Some implementations might throw an error for empty files
    expect_true(grepl("empty", e$message, ignore.case = TRUE) || 
                grepl("no data", e$message, ignore.case = TRUE))
  })
  
  # Clean up
  unlink(empty_file)
})
