# Make distance from outbreak layer
library(terra)
library(sf)
library(dplyr)

# Read in the data
h5n1_detections = read.csv("input_data/h5n1_detections_FAO_03092026.csv")
h5n1_detections = h5n1_detections %>% filter(Country == "Australia") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("EPSG:4283"))

aus_mask = rast("input_data/australia_mask.tif")

# Make kernel density 
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
                                    fun = "count", background = 0)

h <- 6.5
bandwidth_m <- 10^h              # bandwidth in metres, e.g. ~3160 km at h=6.5
sigma_m <- bandwidth_m / 3        # 3-sigma rule: ~99.7% of kernel mass within bandwidth

w <- terra::focalMat(detections_rast, sigma_m, "Gauss")

kde <- terra::focal(detections_rast,
                    w = w,
                    fun = "sum",
                    na.rm = TRUE,
                    fillvalue = 0) %>%
  terra::mask(aus_mask_proj)

# Reproject/resample back to match your 0.02 dd analysis grid for use as a
# condition layer alongside your species rasters
kde_wgs84 <- terra::project(kde, aus_mask, method = "bilinear")

# Rescale from 0-1 for condition layer
outbreak_distance_rescaled <- spatialEco::raster.transformation(kde_wgs84, trans = "norm", smin = 0.5, smax = 1)

plot(outbreak_distance_rescaled)
points(h5n1_detections)
# Save output
writeRaster(outbreak_distance_rescaled, "input_data/outbreak_distance_cond.tif", overwrite=TRUE)
