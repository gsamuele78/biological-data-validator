#' CSV field mapping configuration
#' Defines the mappings between CSV columns and internal field names

#' Mapping for Sheet1 CSV fields
SHEET1_CSV_MAPPING <- list(
  Plot.code = "Plot.code",
  SU = "SU",
  Sample.date = "Sample.date",
  Detector = "Detector",
  X = "X",                                    # "longitude"
  Y = "Y",                                    # "latitude"
  Region = "Region",
  Elevation = "Elevation",                    # "elevation_m"
  Aspect = "Aspect",                          # "aspect_deg"
  Slope = "Slope",                            # "slope_deg"
  Cop.tot = "Tot.cov",                        # "cover_total"
  Tree.cov = "Tree.cov",                      # "tree_cover"
  Shrub.cov = "Shrub.cov",                    # "shrub_cover"
  Herb.cov = "Herb.cov",                      # "herbaceous_cover"
  Brioph.cov = "Brioph.cov",                  # "bryophyte_cover"
  Bare.soil.cov = "Bare.soil.cov",            # "bare_soil_cover"
  Litter.cov = "Litter.cov",                  # "litter_cover"
  notes = "Notes"
)

#' Mapping for Sheet2 CSV fields
SHEET2_CSV_MAPPING <- list(
  Plot.code = "Plot.code",
  Subplot = "SU",                             # "subplot_number"
  Layer = "Layer",                            # "vegetation_layer T-S-H"
  Species = "species (Genus + species)",      # "species (Genus + species)"
  cover = "Cover(perc.)",                     # "species_cover Cover(perc.)"
  Notes = "Notes"
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
