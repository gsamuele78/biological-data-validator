# examples/examples.R

# Load necessary libraries and source R files (assuming renv is used)
if (!require("renv")) install.packages("renv")
renv::restore()

library(logger)

source("R/data_classes.R")
source("R/validation_classes.R")
source("R/report_class.R")
source("R/path_generation_class.R")
source("R/email_class.R")
source("R/db_interaction_class.R")
source("R/utils.R")

# --- Example Data ---

# Create example valid and invalid Excel data (you can also load from data/sample_data.xlsx)
valid_sheet1_data <- list(
    Sheet1Data$new(list(Plot.code = "Plot1", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1)),
    Sheet1Data$new(list(Plot.code = "Plot1", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1)),
    Sheet1Data$new(list(Plot.code = "Plot1", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1)),
    Sheet1Data$new(list(Plot.code = "Plot1", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1))
)

valid_sheet2_data <- list(
    Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
    Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
    Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C")),
    Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 4, Species = "Species D", species_abb = "Sp. D", cover = 80, Layer = "Moss", Notes = "Note D"))
)

invalid_sheet1_data <- list(
    Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1)),
    Sheet1Data$new(list(Plot.code = "Plot2", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1)), # Duplicate SU
    Sheet1Data$new(list(Plot.code = "Plot2", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1)),
    Sheet1Data$new(list(Plot.code = "Plot2", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1))
)

invalid_sheet2_data <- list(
    Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
    Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
    Sheet2Data$new(list(Plot.code = "Plot2", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C"))
    # Missing Subplot 4
)

# --- Example Usage ---

# 1. Validate Data
log_info("Example 1: Validating Data")

# Create dummy ExcelData objects
valid_excel_data <- ExcelData$new("dummy_path")  # Provide a dummy path
valid_excel_data$sheet1_data <- valid_sheet1_data
valid_excel_data$sheet2_data <- valid_sheet2_data

invalid_excel_data <- ExcelData$new("dummy_path")  # Provide a dummy path
invalid_excel_data$sheet1_data <- invalid_sheet1_data
invalid_excel_data$sheet2_data <- invalid_sheet2_data

# Initialize Validator (you can use a temporary path for testing)
example_path_generator <- PathGenerator$new(base_path = tempdir()) 
example_validator <- Validator$new(example_path_generator)

# Perform validation
valid_errors <- example_validator$validate(valid_excel_data)
invalid_errors <- example_validator$validate(invalid_excel_data)

# Print results
cat("\nValidation Results (Valid Data):\n")
if (nrow(valid_errors) == 0) {
  cat("No errors found.\n")
} else {
  print(valid_errors)
}

cat("\nValidation Results (Invalid Data):\n")
if (nrow(invalid_errors) == 0) {
  cat("No errors found.\n")
} else {
  print(invalid_errors)
}

# 2. Generate Report
log_info("Example 2: Generating Report")

# Create a Report object (using invalid data to show errors in the report)
example_report <- Report$new(
  filepath = "dummy_path", # Replace with an actual file path if you have a sample Excel file
  errors = invalid_errors, 
  sheet1 = invalid_sheet1_data, 
  sheet2 = invalid_sheet2_data
)

# Generate the report in a temporary directory
report_output_path <- file.path(tempdir(), "example_report")
dir.create(report_output_path, recursive = TRUE, showWarnings = FALSE)
example_report$generate(report_output_path)
log_info("Report generated at: {report_output_path}")

# 3. Database Interaction
log_info("Example 3: Database Interaction")

# Create a temporary database for testing
db_path <- file.path(tempdir(), "example_db.sqlite")
db_handler <- DatabaseHandler$new(db_path)

# Add plot data
plot_data_id <- db_handler$add_plot_data(
  filepath = "dummy_path/sample_data.xlsx", 
  plot_code = "Plot1", 
  sample_date = as.character(Sys.Date()), 
  detector = "DetectorX", 
  region = "RegionY", 
  validation_status = "Success", 
  report_path = "dummy_path/report.html"
)
log_info("Added plot data with ID: {plot_data_id}")

# Add image data (optional)
# db_handler$add_image_data(plot_data_id, "dummy_path/image1.jpg")
# db_handler$add_image_data(plot_data_id, "dummy_path/image2.png")

# Get plot history
plot_history <- db_handler$get_plot_history()
cat("\nPlot History:\n")
print(plot_history)

# Search for records
search_results <- db_handler$get_plot_history(plot_code = "Plot1")
cat("\nSearch Results (Plot Code = 'Plot1'):\n")
print(search_results)

# Update a record (replace with actual values)
db_handler$update_plot_data(
    id = plot_data_id,
    filepath = "dummy_path/updated_sample_data.xlsx",
    plot_code = "Plot1_updated",
    sample_date = as.character(Sys.Date()),
    detector = "DetectorZ",
    region = "RegionA",
    validation_status = "Failed",
    report_path = "dummy_path/updated_report.html"
)
log_info("Updated plot data with ID: {plot_data_id}")

# Delete a record
# db_handler$delete_plot_data(plot_data_id)
# log_info("Deleted plot data with ID: {plot_data_id}")
