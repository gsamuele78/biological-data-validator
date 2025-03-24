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
  Tree.h = "tree_height",
  Shrub.cov = "shrub_cover",
  Shrub.h = "shrub_height",
  Herb.cov = "herb_cover",
  Herb.h = "herb_height",
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
