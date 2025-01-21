# Example of using the validation functions with valid data

# Assuming you are in the project's root directory
source("R/data_classes.R")
source("R/validation_classes.R")
source("R/path_generation_class.R")
source("R/report_class.R") # If you want to generate a report

# Create a sample ExcelData object (replace with your actual data loading)
valid_sheet1_data <- list(
  Sheet1Data$new(list(Plot.code = "Plot1", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1")),
  Sheet1Data$new(list(Plot.code = "Plot1", SU = 2, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 2")),
  Sheet1Data$new(list(Plot.code = "Plot1", SU = 3, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 3")),
  Sheet1Data$new(list(Plot.code = "Plot1", SU = 4, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 4"))
)

valid_sheet2_data <- list(
  Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 1, Species = "Species A", species_abb = "Sp. A", cover = 50, Layer = "Tree", Notes = "Note A")),
  Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 2, Species = "Species B", species_abb = "Sp. B", cover = 60, Layer = "Herb", Notes = "Note B")),
  Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 3, Species = "Species C", species_abb = "Sp. C", cover = 70, Layer = "Shrub", Notes = "Note C")),
  Sheet2Data$new(list(Plot.code = "Plot1", Subplot = 4, Species = "Species D", species_abb = "Sp. D", cover = 80, Layer = "Moss", Notes = "Note D"))
)

valid_excel_data <- ExcelData$new("dummy_path")
valid_excel_data$sheet1_data <- valid_sheet1_data
valid_excel_data$sheet2_data <- valid_sheet2_data

# Initialize PathGenerator and Validator
path_generator <- PathGenerator$new("/example/path")
validator <- Validator$new(path_generator)

# Perform validation
errors <- validator$validate(valid_excel_data)

# Print validation results
if (nrow(errors) > 0) {
  print("Validation Errors Found:")
  print(errors)
} else {
  print("Data is valid!")
}

# You can generate a report (optional):
# report <- Report$new("dummy_filepath", errors, valid_sheet1_data, valid_sheet2_data)
# report$generate("/path/to/output")
