
## Make some plots
library(tidyterra)
library(ggplot2)
library(patchwork)

aus = vect("input_data/Australia Outline.shp")

s1 = rast(paste0(scen_dir, "/output/rankmap.tif"))
s2 = rast(paste0(scen_dir_s2, "/output/rankmap.tif"))
s3 = rast(paste0(scen_dir_s3, "/output/rankmap.tif"))

names(s1)<- "z_weighted"
names(s2)<- "z_weighted_cost"
names(s3)<- "z_weighted_cost_distoutbreak"

# Initial plots
s2_plot = ggplot() +
  geom_spatraster(data = s2) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void()


s3_plot = ggplot() +
  geom_spatraster(data = s3) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void()

s2_plot + s3_plot + plot_annotation(tag_levels = 'a') + 
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

# Change in priority due to Outbreak Locations
change = s3-s2

ggplot() +
  geom_spatraster(data = change) + 
  scale_fill_gradient2(
    low      = "darkred",
    mid      = "white",
    high     = "darkblue",
    midpoint = 0,
    na.value = NA,
    name     = "Change in\npriority"
  ) + geom_spatvector(data = aus, fill = NA) + 
  geom_spatvector(data=vect(h5n1_detections), colour = "yellow") +
  theme_void() + theme(legend.position='bottom')

# Input figure for supp info
cost_path = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/travel_cost.tif"
outbreaks = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/outbreak_distance_cond.tif"

travel = rast(cost_path)
dist = rast(outbreaks)

names(travel) <- "travel_cost"
names(dist) <- "distance_from_outbreak"

travel_plot = ggplot() +
  geom_spatraster(data = travel) + 
  geom_spatvector(data = aus, fill = NA) +
  scale_fill_whitebox_c(palette = "deep",
                        direction = -1,
                        limits = c(0, 100000),      
                        oob = scales::squish,
                        na.value = NA, name = "Travel Cost") +
  theme_void() + theme(legend.position='bottom', legend.text = element_text(size = 7))

dist_plot = ggplot() +
  geom_spatraster(data = dist) + 
  geom_spatvector(data = aus, fill = NA) +
  geom_spatvector(data=vect(h5n1_detections), colour = "#FFAE42") +
  scale_fill_whitebox_c(palette = "deep",
                        direction = -1,
                        na.value = NA, 
                        name = "Outbreak\nDistance Decay") +
  theme_void() + theme(legend.position='bottom')

travel_plot + dist_plot + plot_annotation(tag_levels = 'a')
