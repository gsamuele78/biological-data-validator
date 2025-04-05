#' R/csv_mapping.R # nolint: commented_code_linter.
#' CSV field mapping configuration
#' Defines the mappings between CSV columns and internal field names.

# Purpose:
# This file defines mappings between the column names in the CSV files and the internal field names used in the application.
# These mappings are used to standardize the data structure and ensure compatibility with the application's logic.

# Documentation:
# - R6 Classes: https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html

#' Mapping for Sheet1 CSV fields
#' This mapping is used for the main data sheet (Sheet1).
SHEET1_CSV_MAPPING <- list(
  Plot.code = "codice",
  SU = "plot",
  Sample.date = "sample_date",
  Detector = "detector",
  Region = "region",
  Elevation = "elevation",                    # Elevation in meters
  Aspect = "aspect",                          # Aspect in degrees
  Slope = "slope",                            # Slope in degrees
  Cop.tot = "tot_cov",                        # Total cover percentage
  Tree.cov = "tree_cov",                      # Tree cover percentage
  Shrub.cov = "shrub_cov",                    # Shrub cover percentage
  Herb.cov = "herb_cov",                      # Herbaceous cover percentage
  Brioph.cov = "brioph_cov",                  # Bryophyte cover percentage
  Bare.soil.cov = "bare_soil_cov",            # Bare soil cover percentage
  Litter.cov = "litter_cov",                  # Litter cover percentage
  notes = "notes"
)

#' Mapping for Sheet2 CSV fields
#' This mapping is used for the species data sheet (Sheet2).
SHEET2_CSV_MAPPING <- list(
  Plot.code = "codice",
  Subplot = "plot",                           # Subplot number
  Layer = "layer",                            # Vegetation layer (T-S-H)
  Species = "species(genus_species)",         # Species name (Genus + species)
  cover = "cover_perc",                       # Cover percentage
  Notes = "notes"
)

#' CSV Mapping Class
#'
#' @description Handles CSV data mapping and validation rules.
#' Example:
#' ```
#' csv_mapping <- CSVMapping$new()
#' csv_mapping$add_rule("Plot.code", ValidationRule$new())
#' ```
CSVMapping <- R6::R6Class("CSVMapping",
  public = list(
    #' @field mapping_rules List of mapping rules
    mapping_rules = NULL,
    
    #' @description Initialize a new CSV mapping instance.
    #' @return A new `CSVMapping` object.
    initialize = function() {
      self$mapping_rules <- list()
    },
    
    #' @description Add a mapping rule.
    #' @param column_name Name of the column.
    #' @param validation_rule Rule to validate the column.
    add_rule = function(column_name, validation_rule) {
      self$mapping_rules[[column_name]] <- validation_rule
    }
  )
)

