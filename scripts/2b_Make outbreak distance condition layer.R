# =============================================================================
# 2b - Make distance from outbreak condition layer
# =============================================================================

library(terra)
library(sf)
library(dplyr)

# Read in the data
h5n1_detections = read.csv("input_data/h5n1_detections_FAO_03092026.csv")
h5n1_detections = h5n1_detections %>% filter(Country == "Australia") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("EPSG:4283"))

aus_mask = rast("input_data/australia_mask.tif")

###############################################
#### 1 - Distance from current outbreak ####
###############################################

# Reproject to an equal-area, metres-based CRS before any distance-based
# calculation - unprojected degrees don't give consistent distances across
# Australia's latitude range
aus_albers <- "EPSG:9473"   # GDA2020 / Australian Albers

aus_mask_proj <- terra::project(aus_mask, y=terra::crs(aus_albers))
h5n1_detections_proj <- terra::project(vect(h5n1_detections), y=terra::crs(aus_albers))

# Rasterize onto a metric grid at a resolution you choose deliberately
# (pick something sensible in metres, not inherited from the 0.02 dd grid -
# e.g. ~2km cells is roughly what 0.02 dd approximates at mid-latitudes)
detections_rast <- terra::rasterize(h5n1_detections_proj, 
                                    aus_mask_proj,
                                    fun = "max", background = 0)


# For every cell, the true straight-line distance (in metres, since
# aus_mask_proj is projected) to the nearest H5N1 detection point
dist_to_detection <- terra::distance(aus_mask_proj, h5n1_detections_proj) %>%
  terra::mask(aus_mask_proj)

# Make distance decay function to that exact distance
h <- 6.5
bandwidth_m <- 10^h              # bandwidth in metres, e.g. ~3160 km at h=6.5

sigma_m <- bandwidth_m / 3
decay <- exp(-dist_to_detection / sigma_m)

decay_wgs84 <- terra::project(decay, aus_mask, method = "bilinear")

# Rescale from 0-1 for condition layer
decay_rescaled <- spatialEco::raster.transformation(decay_wgs84, 
                                                    trans = "norm", smin = 0.0, smax = 1)


decay_rescaled[decay_rescaled < 0.1] <- 0.1

plot(decay_rescaled)
points(h5n1_detections)
# Save output
writeRaster(decay_rescaled, "input_data/outbreak_distance_cond.tif", overwrite=TRUE)

###############################################
#### 2 - Kernel density of current outbreak ####
###############################################
# Now a kernal density decay function
decay_sum <- terra::rast(aus_mask_proj)
values(decay_sum) <- 0

for (i in seq_len(nrow(h5n1_detections_proj))) {
  d <- terra::distance(aus_mask_proj, h5n1_detections_proj[i, ])
  decay_sum <- decay_sum + exp(-d / sigma_m)
}

decay_sum <- terra::mask(decay_sum, aus_mask_proj)

# Reproject/resample back to match your 0.02 dd analysis grid for use as a
# condition layer alongside your species rasters
kde_wgs84 <- terra::project(decay_sum, aus_mask, method = "bilinear")

# Rescale from 0-1 for condition layer
outbreak_density_rescaled <- spatialEco::raster.transformation(kde_wgs84, 
                                                                trans = "norm", smin = 0, smax = 1)
outbreak_density_rescaled[outbreak_density_rescaled < 0.1] <- 0.1

plot(outbreak_density_rescaled)
points(h5n1_detections)
# Save output

writeRaster(outbreak_density_rescaled, "input_data/outbreak_density_cond.tif", overwrite=TRUE)

###############################################
#### 3 - Kernel distance from future outbreak ####
###############################################
new_outbreak = data.frame(longitude = 132.578741, latitude = -12.654039)
new_outbreak = st_as_sf(new_outbreak, coords = c("longitude", "latitude"), crs = st_crs("EPSG:4283"))
new_outbreak <- terra::project(vect(new_outbreak), y=terra::crs(aus_albers))
future_outbreak <- rbind(h5n1_detections_proj, new_outbreak)

# For every cell, the true straight-line distance (in metres, since
# aus_mask_proj is projected) to the nearest H5N1 detection point
future_dist_to_detection <- terra::distance(aus_mask_proj, future_outbreak) %>%
  terra::mask(aus_mask_proj)

# Make distance decay function to that exact distance
h <- 6.5
bandwidth_m <- 10^h              # bandwidth in metres, e.g. ~3160 km at h=6.5

sigma_m <- bandwidth_m / 3
future_decay <- exp(-future_dist_to_detection / sigma_m)

future_decay_wgs84 <- terra::project(future_decay, aus_mask, method = "bilinear")

# Rescale from 0-1 for condition layer
future_decay_rescaled <- spatialEco::raster.transformation(future_decay_wgs84, 
                                                    trans = "norm", smin = 0.0, smax = 1)

future_decay_rescaled

plot(future_decay_rescaled)
points(future_outbreak)
# Save output
writeRaster(future_decay_rescaled, "input_data/future_outbreak_distance_cond.tif", overwrite=TRUE)

###############################################
#### 4 - Kernel density of future outbreak ####
###############################################
# Now a kernal density decay function
future_decay_sum <- terra::rast(aus_mask_proj)
values(future_decay_sum) <- 0

for (i in seq_len(nrow(future_outbreak))) {
  d <- terra::distance(aus_mask_proj, future_outbreak[i, ])
  future_decay_sum <- future_decay_sum + exp(-d / sigma_m)
}

future_decay_sum <- terra::mask(future_decay_sum, aus_mask_proj)

# Reproject/resample back to match the 0.02 dd analysis grid for use as a
# condition layer alongside species rasters
future_kde_wgs84 <- terra::project(future_decay_sum, aus_mask, method = "bilinear")

# Rescale from 0-1 for condition layer
future_outbreak_density_rescaled <- spatialEco::raster.transformation(future_kde_wgs84, 
                                                               trans = "norm", smin = 0, smax = 1)

plot(future_outbreak_density_rescaled)
points(future_outbreak)
# Save output

future_outbreak_density_rescaled[future_outbreak_density_rescaled < 0.1] <- 0.1


writeRaster(future_outbreak_density_rescaled, "input_data/future_outbreak_density_cond.tif", overwrite=TRUE)


##############################################
#### Summary Plot of Relationship ####
library(tidyverse)
dist_to_detection_gda94 <- terra::project(dist_to_detection, aus_mask, method = "bilinear")


comparison_stack = c(dist_to_detection_gda94, decay_rescaled, outbreak_density_rescaled)

comparison_df <- terra::spatSample(comparison_stack, size = 10000,
                                   method = "random", na.rm = TRUE,
                                   as.df = TRUE) 
names(comparison_df) <- c("distance_m", "distance_decay", "density_decay")

comparison_df = comparison_df %>%
  mutate(distance_km = distance_m / 1000) %>%
  select(-distance_m) %>%
  pivot_longer(cols = c(distance_decay, density_decay),
               names_to = "surface_type", values_to = "condition_value")

ggplot(comparison_df, aes(x = distance_km, y = condition_value, colour = surface_type)) +
  geom_point(alpha = 0.3, size = 0.5, shape = 16) +
  geom_smooth(se = FALSE, method = "loess", span = 0.2, linewidth = 1) +
  scale_colour_manual(
    name   = "Condition surface",
    values = c(density_decay = "#1b9e77", distance_decay = "#d95f02"),
    labels = c(density_decay = "Kernel density (accumulated)",
               distance_decay = "Nearest-detection distance decay")
  ) +
  labs(
    x = "Distance to nearest detection (km)",
    y = "Condition value (0-1)"
  ) +
  theme_classic() + facet_wrap(~surface_type) +
  theme(legend.position = "bottom", strip.text = element_blank())
