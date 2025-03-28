#' CSV field mapping configuration
#' Defines the mappings between CSV columns and internal field names

#' Mapping for Sheet1 CSV fields
SHEET1_CSV_MAPPING <- list(
  Plot.code = "plot_code",
  SU = "su",
  Sample.date = "sample_date",
  Detector = "detector",
  X = "longitude",
  Y = "latitude",
  Region = "region",
  Elevation = "elevation_m",
  Aspect = "aspect_deg",
  Slope = "slope_deg",
  Cop.tot = "cover_total",
  Litter.cov = "litter_cover",
  Bare.soil.cov = "bare_soil_cover",
  Tree.cov = "tree_cover",
  #Tree.h = "tree_height",
  Shrub.cov = "shrub_cover",
  #Shrub.h = "shrub_height",
  Herb.cov = "herb_cover",
  #Herb.h = "herb_height",
  Brioph.cov = "bryophyte_cover",
  notes = "notes"
)

#' Mapping for Sheet2 CSV fields
SHEET2_CSV_MAPPING <- list(
  Plot.code = "plot_code",
  Subplot = "subplot",
  Species = "species_name",
  species_abb = "species_code",
  cover = "species_cover",
  Layer = "vegetation_layer",
  Notes = "species_notes"
)

#' CSV Mapping Class
#'
#' @description Handles CSV data mapping and validation rules
#' @export
CSVMapping <- R6::R6Class("CSVMapping",
  public = list(
    #' @field mapping_rules List of mapping rules
    mapping_rules = NULL,
    
    #' @description Initialize a new CSV mapping instance
    #' @return A new `CSVMapping` object
    initialize = function() {
      self$mapping_rules <- list()
    },
    
    #' @description Add a mapping rule
    #' @param column_name Name of the column
    #' @param validation_rule Rule to validate the column
    add_rule = function(column_name, validation_rule) {
      self$mapping_rules[[column_name]] <- validation_rule
    }
  )
)
