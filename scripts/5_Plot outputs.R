
## Make some plots
library(tidyterra)
library(ggplot2)
library(patchwork)
library(tidyverse)
aus_albers <- "EPSG:9473"   # GDA2020 / Australian Albers
###################################
#### Read in the ranking maps
###################################
aus = vect("input_data/Australia Outline.shp")
scen_dir = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s0_unweighted"
scen_dir_s1 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s1_susceptibility_linear"
scen_dir_s2 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s2_susceptibility_mult"
scen_dir_s3 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s3_susceptibility_linear_outbreak_dist_current"
scen_dir_s4 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s4_susceptibility_linear_outbreak_dist_future"
scen_dir_s5 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s5_susceptibility_linear_outbreak_dens_current"
scen_dir_s6 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s6_susceptibility_linear_outbreak_dens_future"

s0 = rast(paste0(scen_dir, "/output/rankmap.tif"))
s1 = rast(paste0(scen_dir_s1, "/output/rankmap.tif"))
s2 = rast(paste0(scen_dir_s2, "/output/rankmap.tif"))
s3 = rast(paste0(scen_dir_s3, "/output/rankmap.tif"))
s4 = rast(paste0(scen_dir_s4, "/output/rankmap.tif"))
s5 = rast(paste0(scen_dir_s5, "/output/rankmap.tif"))
s6 = rast(paste0(scen_dir_s6, "/output/rankmap.tif"))

names(s0)<- "s0_unweighted"
names(s1)<- "s1_susceptibility_linear"
names(s2)<- "s2_susceptibility_mult"
names(s3)<- "s3_susceptibility_linear_outbreak_dist_current"
names(s4)<- "s4_susceptibility_linear_outbreak_dist_future"
names(s5)<- "s5_susceptibility_linear_outbreak_dens_current"
names(s6)<- "s6_susceptibility_linear_outbreak_dens_future"

h5n1_detections = read.csv("input_data/h5n1_detections_FAO_03092026.csv")
h5n1_detections = h5n1_detections %>% filter(Country == "Australia") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs("EPSG:4283"))
h5n1_detections_proj <- terra::project(vect(h5n1_detections), y=terra::crs(aus_albers))

new_outbreak = data.frame(longitude = 132.578741, latitude = -12.654039)
new_outbreak = st_as_sf(new_outbreak, coords = c("longitude", "latitude"), crs = st_crs("EPSG:4283"))
new_outbreak <- terra::project(vect(new_outbreak), y=terra::crs(aus_albers))
future_outbreak <- st_as_sf(rbind(h5n1_detections_proj, new_outbreak))


###################################
#### FIGURE ONE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################
s3_plot = ggplot() +
  geom_spatraster(data = s3*100) + 
  geom_spatvector(data = aus, fill = NA) +
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void() + 
  geom_spatvector(data = vect(h5n1_detections), colour = "#DC143C", size = 2) + 
  theme(legend.position='left')

s3_plot

###################################
#### FIGURE TWO - Option A ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################
# Weighting agreement
s0_plot = ggplot() +
  geom_spatraster(data = s3) + 
  geom_spatvector(data = aus, fill = NA) +
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void() + theme(legend.position='bottom')

change_plot = function(ras, points = NA){
  p <- ggplot() +
    geom_spatraster(data = ras) + 
    scale_fill_gradient2(
      low = "#762a83", 
      mid      = "#f7f7f7",
      high = "#1b7837",
      limits   = c(-0.5, 0.5),
      oob = scales::squish,
      midpoint = 0,
      na.value = NA,
      name     = "Change in\npriority"
    ) + 
    geom_spatvector(data = aus, fill = NA) +
    theme_void()
  if (!identical(points, NA)) {
    p <- p + geom_spatvector(data = vect(points), colour = "yellow", size = 2)
  }
  p
}

s1_change = s0-s1 
s3_change = s0-s3 
s4_change = s0-s4 
s2_change = s0-s2 
s5_change = s0-s5 
s6_change = s0-s6


plot_list <- list(change_plot(s1_change),
                  change_plot(s3_change, points = h5n1_detections),
                  change_plot(s4_change, points = future_outbreak),
                  change_plot(s2_change),
                  change_plot(s5_change, points = h5n1_detections),
                  change_plot(s6_change, points = future_outbreak))

grid_plot <- wrap_plots(plot_list, ncol = 3, nrow = 2) +
  plot_layout(guides = "collect")  &
  theme(legend.position='bottom')

s0_plot + grid_plot + plot_layout(widths = c(1, 2))

###################################
#### FIGURE TWO - Option B ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################
# Weighting agreement
s0_top = ifel(s0 >0.9, 1, NA)
s1_top = ifel(s1 >0.9, 1, NA)
s2_top = ifel(s2 >0.9, 1, NA)

weighting_agreement <- do.call(mosaic, c(list(s0_top, s1_top, s2_top), fun = "sum"))
weighting_agreement <- as.factor(weighting_agreement)
weight_agreement_plot = ggplot() +
  geom_spatvector(data = aus, fill = "lightgrey") +
  geom_spatraster(data = weighting_agreement) + 
  geom_spatvector(data = aus, fill = NA) +
  scale_fill_viridis_d(
    option = "viridis",
    na.value = NA,
    name     = "Number of scenarios"
  ) + theme_void() + theme(legend.position='bottom')

weight_agreement_plot

###################################
#### FIGURE THREE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################
s1_top = ifel(s1 >0.9, 1, NA)
s3_top = ifel(s3 >0.9, 1, NA)
s5_top = ifel(s5 >0.9, 1, NA)

outbreak_agreement <- do.call(mosaic, c(list(s1_top, s3_top, s5_top), fun = "sum"))

outbreak_agreement <- as.factor(outbreak_agreement)

outbreak_agreement_plot =  ggplot() +
  geom_spatvector(data = aus, fill = "lightgrey") +
  geom_spatraster(data = outbreak_agreement) + 
  geom_spatvector(data = aus, fill = NA) +
  scale_fill_viridis_d(
    option = "viridis",
    na.value = NA,
    name     = "Number of scenarios"
  ) + theme_void() + theme(legend.position='bottom')

weight_agreement_plot + outbreak_agreement_plot + plot_annotation(tag_levels = 'a') + 
  plot_layout(guides = "collect") & theme(legend.position='bottom')

###################################
#### Supplementary Figures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###################################

# Initial plots
s0_plot = ggplot() +
  geom_spatraster(data = s0) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void()

s1_plot = ggplot() +
  geom_spatraster(data = s1) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void()


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

s4_plot = ggplot() +
  geom_spatraster(data = s4) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void()

s5_plot = ggplot() +
  geom_spatraster(data = s5) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void()

s6_plot = ggplot() +
  geom_spatraster(data = s6) + 
  scale_fill_whitebox_c(
    palette = "viridi",
    na.value = NA,
    name     = "Rank"
  ) + theme_void()

s0_plot + s1_plot + s2_plot + s3_plot + s4_plot + s5_plot + s6_plot + plot_annotation(tag_levels = 'a') + 
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

# Change in priority due to Outbreak Locations
change = s5-s1

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
  theme_void()

###############################################
#### Supplementary Figure 1 - Input Layers ####
###############################################

cost_path = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/travel_cost.tif"

travel = rast(cost_path)
dist = rast(outbreak_dist)
dens = rast(outbreak_density)
future_dist = rast(future_outbreak_dist)
future_dens = rast(future_outbreak_density)
names(dist) <- "distance_from_outbreak"
names(dens) <- "density_of_outbreak"
names(travel) <- "travel_cost"

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
  geom_spatvector(data=vect(h5n1_detections), colour = "white", fill = "orangered", size = 2, shape = 21) +
  scale_fill_whitebox_c(palette = "deep",
                        direction = -1,
                        na.value = NA, 
                        name = "Distance\nfrom Outbreak") +
  theme_void() + theme(legend.position='bottom')

dens_plot = ggplot() +
  geom_spatraster(data = dens) + 
  geom_spatvector(data = aus, fill = NA) +
  geom_spatvector(data=vect(h5n1_detections), colour = "white", fill = "orangered", size = 2, shape = 21) +
  scale_fill_whitebox_c(palette = "deep",
                        direction = -1,
                        na.value = NA, 
                        name = "Density\nof Outbreak") +
  theme_void() + theme(legend.position='bottom')

dist_future_plot = ggplot() +
  geom_spatraster(data = future_dist) + 
  geom_spatvector(data = aus, fill = NA) +
  geom_spatvector(data=vect(future_outbreak), colour = "white", fill = "orangered", size = 2, shape = 21) +
  scale_fill_whitebox_c(palette = "deep",
                        direction = -1,
                        na.value = NA, 
                        name = "Distance\nfrom Outbreak") +
  theme_void() + theme(legend.position='bottom')

dens_future_plot = ggplot() +
  geom_spatraster(data = future_dens) + 
  geom_spatvector(data = aus, fill = NA) +
  geom_spatvector(data=vect(future_outbreak), colour = "white", fill = "orangered", size = 2, shape = 21) +
  scale_fill_whitebox_c(palette = "deep",
                        direction = -1,
                        na.value = NA, 
                        name = "Density\nof Outbreak") +
  theme_void() + theme(legend.position='bottom')


travel_plot + dist_plot + dens_plot + dist_future_plot + dens_future_plot + plot_annotation(tag_levels = 'a')





## Example species
spp.list = list.files("X:/Projects/bird_flu_surveillance/species", full=T)

spp = rast(spp.list[c(4,51,235,91)])

spp_plot_1 = ggplot() +
  geom_spatvector(data = aus, fill = "lightgrey") +
  geom_spatraster(data = spp$`768_Callocephalon_fimbriatum`, 
                  aes(fill = as.factor(after_stat(value)))) + 
  scale_fill_manual(
    values = c("1" = "yellow"), 
               na.value = "transparent") +
  theme_void() + theme(legend.position='none')

spp_plot_1

