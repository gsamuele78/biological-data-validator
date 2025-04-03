#' R/csv_mapping.R # nolint: commented_code_linter.
#' CSV field mapping configuration
#' Defines the mappings between CSV columns and internal field names

#' Mapping for Sheet1 CSV fields
SHEET1_CSV_MAPPING <- list(
  Plot.code = "codice",
  SU = "plot",
  Sample.date = "sample_date",
  Detector = "detector",
  #X = "x",                                    # "longitude"
  #Y = "y",                                    # "latitude"
  Region = "region",
  Elevation = "elevation",                    # "elevation_m"
  Aspect = "aspect",                          # "aspect_deg"
  Slope = "slope",                            # "slope_deg"
  Cop.tot = "tot_cov",                        # "cover_total"
  Tree.cov = "tree_cov",                      # "tree_cover"
  Shrub.cov = "shrub_cov",                    # "shrub_cover"
  Herb.cov = "herb_cov",                      # "herbaceous_cover"
  Brioph.cov = "brioph_cov",                  # "bryophyte_cover"
  Bare.soil.cov = "bare_soil_cov",            # "bare_soil_cover"
  Litter.cov = "litter_cov",                  # "litter_cover"
  notes = "notes"
)

#' Mapping for Sheet2 CSV fields
SHEET2_CSV_MAPPING <- list(
  Plot.code = "codice",
  Subplot = "plot",                             # "subplot_number"
  Layer = "layer",                            # "vegetation_layer T-S-H"
  Species = "species(genus_species)",      # "species (Genus + species)"
  cover = "cover_perc",                     # "species_cover Cover(perc.)"
  Notes = "notes"
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

