# =============================================================================
# 3 Prepare feature weight information
# =============================================================================

library(dplyr)

# -----------------------------------------------------------------------------
# PARAMETERS - edit these
# -----------------------------------------------------------------------------

species_dir      <- "X:/Projects/bird_flu_surveillance/species/"
raster_ext       <- "\\.tif$"

weights_csv_path <- "input_data/h5birdflu_species_weights.csv"

# Column names in weights_csv_path for each weighting scenario - edit to match
weight_cols <- c(
  "weight_natrisk_linear",
  "weight_natrisk_exp",
  "weight_suscept_linear",
  "weight_epbc_linear"
)

default_weight <- 1   # used when a species is missing a value for a scenario
output_csv     <- "input_data/species_weight_scenarios.csv"

# -----------------------------------------------------------------------------
# BUILD SPECIES KEY FROM RASTERS
# -----------------------------------------------------------------------------

raster_paths <- list.files(species_dir, pattern = raster_ext, full.names = TRUE)
if (length(raster_paths) == 0) {
  stop("No rasters matching '", raster_ext, "' found under: ", species_dir)
}

feat <- data.frame(
  species  = tools::file_path_sans_ext(basename(raster_paths)),
  filename = normalizePath(raster_paths, winslash = "/", mustWork = FALSE),
  stringsAsFactors = FALSE
)

cat(sprintf("Found %d feature rasters in %s\n", nrow(feat), species_dir))

# -----------------------------------------------------------------------------
# BUILD MATCHING SPECIES KEY IN WEIGHTS TABLE, JOIN, FILL DEFAULTS
# -----------------------------------------------------------------------------

weights <- read.csv(weights_csv_path, stringsAsFactors = FALSE) %>%
  mutate(
    species = paste0(LISTED_TAXON_ID, "_", SCIENTIFIC_NAME),
    species = gsub("/", "-", species, fixed = TRUE),
    species = gsub(" ", "_", species, fixed = TRUE)
  ) %>%
  select(species, all_of(weight_cols))

feat <- left_join(feat, weights, by = "species")

for (col in weight_cols) {
  n_missing <- sum(is.na(feat[[col]]))
  if (n_missing > 0) {
    cat(sprintf("%d species missing '%s' - using default_weight = %s\n",
                n_missing, col, default_weight))
  }
  feat[[col]][is.na(feat[[col]])] <- default_weight
}

# -----------------------------------------------------------------------------
# WRITE OUTPUT
# -----------------------------------------------------------------------------

write.csv(feat[, c("filename", weight_cols)], output_csv, row.names = FALSE)
cat("Wrote:", output_csv, "\n")
