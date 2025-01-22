# Usage Examples

This document provides examples of how to use the Biological Environment Data Validation Tool. The examples are based on the code in the `examples/examples.R` file.

## Prerequisites

-   You should have R installed and the project dependencies set up using `renv`. See the [Installation](README.md#installation) section in the main `README.md` for instructions.

## Running the Examples

1. Open R or RStudio in the project directory.
2. Execute the following command in the R console:

    ```R
    source("examples/examples.R")
    ```

This will run all the examples in the `examples/examples.R` file.

## Example 1: Validating Data

This example demonstrates how to validate data using both valid and invalid example data.

### Valid Data

```R
# Create example valid data
valid_sheet1_data <- list(
    Sheet1Data$new(list(Plot.code = "Plot1", SU = 1, Sample.date = Sys.Date(), Detector = "DetectorA", Region = "RegionX", notes = "Note 1", X=1,Y=1,Elevation=1,Aspect=1,Slope=1,Cop.tot=1,Litter.cov=1,Bare.soil.cov=1,Tree.cov=1,Tree.h=1,Shrub.cov=1,Shrub.h=1,Herb.cov=1,Herb.h=1,Brioph.cov=1)),
    # ... (more Sheet1Data objects for Plot1)
)

valid_sheet2_data <- list(
    Sheet2Data$new(list(Plot.code = "Plot1", Sub
