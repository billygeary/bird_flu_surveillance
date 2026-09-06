# =============================================================================
# build_zonation_run.R
#
# Builds the core input files for a Zonation 5 run from a folder of
# species/feature rasters:
#   - a feature list .txt file (weight + filename columns, one row per
#     species)
#   - a .z5 settings file pointing to that feature list
#
# NOTE: the .z5 settings file below only sets the feature list path. Confirm
# the exact settings key name against your Zonation 5 manual/example
# setups - I've used a placeholder key ("feature list file") that you should
# check/adjust to match what your install expects.
#
# Requires: terra
# =============================================================================

library(terra)

# -----------------------------------------------------------------------------
# 1. PARAMETERS - edit these for your run
# -----------------------------------------------------------------------------

# Folder containing feature rasters - flat, one file per species.
species_dir   <- "X:/Projects/bird_flu_surveillance/species/"
raster_ext    <- "\\.tif$"          # regex matching your raster files

# --- Species weights (individual) -------------------------------------------
# Weight is set per species. Two ways to supply it, tried in this order:
#   1. species_weights_csv, if it exists - columns: species, weight
#   2. species_weights_inline below - a named vector you fill in directly in
#      this script, keyed by the raster filename (without extension)
# Any species matched by neither gets default_weight.
species_weights_csv <- read.csv("input_data/h5birdflu_species_weights.csv")   # set to NULL to skip
species_weights_csv = 
  species_weights_csv %>% 
  mutate(species = paste0(LISTED_TAXON_ID, "_", SCIENTIFIC_NAME)) %>%
  mutate(species = gsub("/", "-", species)) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  select(species, weight)
default_weight <- 1   # used when a species has no weight from either source

output_dir         <- "weighted_zonation_run"
run_name            <- "h5n1_priority_weighted"
feature_list_file  <- file.path(output_dir, paste0(run_name, "_features.txt"))
settings_file      <- file.path(output_dir, paste0(run_name, ".z5"))

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 2. FIND FEATURE RASTERS
# -----------------------------------------------------------------------------

raster_paths <- list.files(species_dir, pattern = raster_ext,
                           recursive = FALSE, full.names = TRUE)

if (length(raster_paths) == 0) {
  stop("No rasters matching '", raster_ext, "' found under: ", species_dir)
}

species_name <- tools::file_path_sans_ext(basename(raster_paths))

feat <- data.frame(
  species  = species_name,
  filename = normalizePath(raster_paths, winslash = "/", mustWork = FALSE),
  stringsAsFactors = FALSE
)

cat(sprintf("Found %d feature rasters in %s\n", nrow(feat), species_dir))

# -----------------------------------------------------------------------------
# 3. SANITY CHECK: CRS / EXTENT / RESOLUTION CONSISTENCY
# -----------------------------------------------------------------------------
# Zonation requires all inputs on a common grid. This does not reproject or
# resample for you - it flags mismatches so you can fix them upstream
# (e.g. with a shared terra::rast() template and resample()/project()).

ref <- rast(raster_paths[1])
mismatches <- character(0)

for (i in seq_along(raster_paths)) {
  r <- rast(raster_paths[i])
  same_crs <- crs(r) == crs(ref)
  same_ext <- ext(r) == ext(ref)
  same_res <- all(res(r) == res(ref))
  if (!(same_crs && same_ext && same_res)) {
    mismatches <- c(mismatches, species_name[i])
  }
}

if (length(mismatches) > 0) {
  warning(
    "The following features do not match the reference grid (CRS/extent/",
    "resolution) and will likely cause Zonation to error or misalign:\n  - ",
    paste(mismatches, collapse = "\n  - "),
    "\nResample/reproject these onto a common template raster before running."
  )
} else {
  cat("All feature rasters share a common CRS, extent, and resolution.\n")
}

# -----------------------------------------------------------------------------
# 4. ATTACH WEIGHTS (PER SPECIES)
# -----------------------------------------------------------------------------

weight_lookup <- numeric(0)
if (!is.null(species_weights_csv)) {
  w <- species_weights_csv
  stopifnot(all(c("species", "weight") %in% names(w)))
  csv_lookup <- setNames(w$weight, w$species)
  weight_lookup[names(csv_lookup)] <- csv_lookup   # CSV overrides inline on overlap
}

feat = left_join(feat, w)
n_default <- sum(is.na(feat$weight))
unassigned_species <- feat$species[is.na(feat$weight)]
feat$weight[is.na(feat$weight)] <- default_weight

if (n_default > 0) {
  cat(sprintf(
    "%d of %d species have no individual weight set - using default_weight = %s:\n  - %s\n",
    n_default, nrow(feat), default_weight,
    paste(unassigned_species, collapse = "\n  - ")
  ))
}

# -----------------------------------------------------------------------------
# 5. WRITE FEATURE LIST .csv FILE
# -----------------------------------------------------------------------------
# Header row + one row per feature: weight, filename.

feature_table <- feat[, c("weight", "filename")]
write.table(feature_table, feature_list_file, sep = "\t",
            row.names = FALSE, col.names = TRUE, quote = FALSE)
cat("Wrote feature list:", feature_list_file, "\n")

# -----------------------------------------------------------------------------
# 6. WRITE .z5 SETTINGS FILE
# -----------------------------------------------------------------------------
# Minimal settings file pointing to the feature list. CONFIRM the key name
# below ("feature list file") against your Zonation 5 manual/example
# setups - this is a placeholder and may need adjusting to match your
# install's exact syntax.

settings_lines <- c(
  sprintf("feature list file = %s",
          normalizePath(feature_list_file, winslash = "/", mustWork = FALSE))
)

writeLines(settings_lines, settings_file)
cat("Wrote settings file:", settings_file, "\n")

# -----------------------------------------------------------------------------
# 7. SUMMARY
# -----------------------------------------------------------------------------

cat("\n--- Run summary ---\n")
cat(sprintf("Features: %d\n", nrow(feat)))
cat(sprintf("Weights:  range %s - %s (default %s applied to %d species)\n",
            min(feat$weight), max(feat$weight), default_weight, n_default))
cat("\nGenerated files:\n")
cat(" -", feature_list_file, "\n")
cat(" -", settings_file, "\n")
cat("\nNext step, e.g.:\n")
cat(sprintf('  z5 --mode=ABF "%s" <output_folder>\n', settings_file))

# -----------------------------------------------------------------------------
# 7. Run Zonation
# -----------------------------------------------------------------------------

# =============================================================================
# Calls the Zonation 5 CLI (z5) directly from R via system2(), so the whole
# build -> run pipeline (e.g. build_zonation_run.R -> this script) can stay
# in one place without opening the Zonation GUI.
# =============================================================================

# -----------------------------------------------------------------------------
# PARAMETERS - edit these
# -----------------------------------------------------------------------------

z5_path        <- "C:/ProgramData/Microsoft/Windows/Start Menu/Programs/Zonation 5"   # path to the z5/zonation5 executable, or just "z5"
# if it's on your PATH - check with Sys.which("z5")
settings_file  <- "zonation_run/h5n1_priority.z5"
output_dir     <- "zonation_run/output"
mode           <- "CAZ"  # e.g. "CAZ", "ABF" - confirm valid modes for your
# Zonation 5 version/manual
extra_args     <- character(0)  # any additional CLI flags, e.g. c("-w")

# -----------------------------------------------------------------------------
# RUN
# -----------------------------------------------------------------------------

if (Sys.which(z5_path) == "" && !file.exists(z5_path)) {
  stop("Can't find the Zonation 5 executable at '", z5_path, "'. ",
       "Set z5_path to the full path, e.g. ",
       "'C:/Program Files (x86)/Zonation5/z5.exe' on Windows.")
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

args <- c(extra_args, paste0("--mode=", mode), settings_file, output_dir)

cat("Running:\n  ", z5_path, paste(args, collapse = " "), "\n\n")

start_time <- Sys.time()
result <- system2(z5_path, args = args, stdout = TRUE, stderr = TRUE)
elapsed <- difftime(Sys.time(), start_time, units = "mins")

cat(result, sep = "\n")
cat(sprintf("\nFinished in %.1f minutes.\n", as.numeric(elapsed)))

status <- attr(result, "status")
if (!is.null(status) && status != 0) {
  warning("z5 exited with a non-zero status (", status, ") - check the output above.")
} else {
  cat("Run completed. Outputs written to:", output_dir, "\n")
}

