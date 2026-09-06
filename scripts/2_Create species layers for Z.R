# 3 - Calculate overlap of MNES and Native Vegetation Data
library(sf)
library(terra)
library(tidyverse)

# Read in the data
snes_path = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/epbc_cumulative_impacts/data/SNES_Public.gdb"
vector_layers <- terra::vector_layers(snes_path)
snes = read_sf(snes_path, "SNES_Public")
snes_df = snes %>% st_drop_geometry() %>%
  filter(TAXON_GROUP %in% c("birds", "mammals")) %>%
  select("LISTED_TAXON_ID","CURRENT_ID", "MAP_TAXON_ID","SCIENTIFIC_NAME",  
         "EPBC_NAMES","VERNACULAR_NAME","THREATENED_STATUS") %>% distinct()

write.csv(snes_df, "input_data/snes_species.csv")

# Filter to the species on the gov list. 
input_species_list = read.csv("input_data/h5birdflu_species_weights.csv")

snes_filtered = snes %>% 
  filter(LISTED_TAXON_ID %in% input_species_list$LISTED_TAXON_ID) %>%
  filter(PRESENCE_CATEGORY == "Species or species habitat likely to occur")

# ============================================================
# Analysis mask
# ============================================================
aus_shp = read_sf("input_data/Australia Outline.shp")
raster_template <- rast(vect(aus_shp), res = 0.02)
aus_mask = rasterize(aus_shp, raster_template, touches = TRUE)

writeRaster(aus_mask, "input_data/australia_mask.tif", overwrite=TRUE)

raster_dir    <- "X:/Projects/bird_flu_surveillance/species/"
species_ranges <- vect(snes_filtered)

# ============================================================
# LOOP: species x year
# ============================================================
snes_filtered$species_id = 1:nrow(snes_filtered)
species_ids = snes_filtered$species_id

for (sp in seq_along(species_ids)) {
  
  message(sprintf("Species %s (%d of %d)", species_ids[sp], sp, length(species_ids)))
  
  sp_vect <- tryCatch(
    species_ranges[sp, ],
    error = function(e) {
      write_csv(
        data.frame(species_id = species_ids[sp], year = NA, 
                   stage = "load_polygon", error = conditionMessage(e)),
        error_path, append = file.exists(error_path), col_names = !file.exists(error_path)
      )
      NULL
    }
  )
  
  if (is.null(sp_vect)) next
  
  sp_rast = rasterize(sp_vect, aus_mask)
  sp_rast = mask(sp_rast, aus_mask)
  filename =  paste0(sp_vect$LISTED_TAXON_ID, "_", sp_vect$SCIENTIFIC_NAME)
  filename = gsub("/", "-", filename)
  filename = gsub(" ", "_", filename)
  names(sp_rast) <- filename
  
  writeRaster(sp_rast, filename = paste0(raster_dir, filename, ".tif"), overwrite=TRUE)
  
  rm(sp_vect,sp_rast)
  gc()
}
