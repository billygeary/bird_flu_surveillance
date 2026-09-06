## Prepare Travel Cost Layer
library(terra)

aus_mask = rast("input_data/australia_mask.tif")

travel_time = rast("input_data/travel_time_to_cities_12.tif")

# Clip to mask
travel_time = project(travel_time, aus_mask)
travel_time_aus = resample(travel_time, aus_mask, method = "bilinear")
travel_time_aus = mask(travel_time_aus, aus_mask)

# Convert to cost
# $120 per hour for personnel cost, insurance, fuel per Southwell papers
travel_cost_aus = travel_time_aus * 120

# Save
writeRaster(travel_cost_aus, "input_data/travel_cost.tif")
