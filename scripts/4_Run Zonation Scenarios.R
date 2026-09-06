library(ZonationR)
library(terra)

source("scripts/function_zonation_prep.R")

###############################################
#### Scenario 1: Weighted by Susceptbility ####
###############################################

## Feature List
scen_dir = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s1_susceptibility"
spp.dir = "X:/Projects/bird_flu_surveillance/species"
spp.list = list.files("X:/Projects/bird_flu_surveillance/species", full=T)

susceptibility_weights = read.table("weighted_zonation_run/h5n1_priority_weighted_features.txt", header=TRUE)


withr::with_dir(scen_dir, {
  # Create the feature list 
  feature_list(spp_file_dir = spp.dir, weight = susceptibility_weights$weight)
  
  # Create settings file
  settings_file(feature_list_file = "feature_list.txt")
  
  # Create the Zonation command file
  command_file(zonation_path = "C:/Program Files/Zonation5",
               marginal_loss_mode = "CAZ2",
               flags = "w")
  
  ## Run Zonation
  run_command_file(".")
})

# Visualise the ranking map
p1 <- priority_map(scen_dir)
plot(p1)


############################################################################
#### Scenario 2: Susceptibility and Travel Cost ####
############################################################################
cost_path = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/travel_cost.tif"
outbreaks = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/outbreak_distance_cond.tif"

dir.create("C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s2_susceptibility_cost", showWarnings = FALSE)
scen_dir_s2 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s2_susceptibility_cost"

withr::with_dir(scen_dir_s2, {
  # Create the feature list 
  feature_list(spp_file_dir = spp.dir, weight = susceptibility_weights$weight)
  
  # Create settings file
  settings_file(feature_list_file = "feature_list.txt", 
                    cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file(zonation_path = "C:/Program Files/Zonation5",
                   marginal_loss_mode = "CAZ2",
                   flags = "wX") # Flag w (use weights and condition)
  
  ## Run Zonation
  run_command_file(".")
})

cost_summary(scen_dir_s2, landscape_prop = c(0.1, 0.2, 0.5, 1))

############################################################################
#### Scenario 3: Susceptibility, Distance from Outbreak and Travel Cost ####
############################################################################
cost_path = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/travel_cost.tif"
outbreaks = "outbreak_distance_cond.tif"

dir.create("C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s3_susceptibility_cost_cond", showWarnings = FALSE)
scen_dir_s3 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s3_susceptibility_cost_cond"

withr::with_dir(scen_dir_s3, {
  # Create the feature list 
  feature_list_mod(spp_file_dir = spp.dir, weight = susceptibility_weights$weight, condition = TRUE)
  
  # Create condition link file
  condition_link_file(condition_file = outbreaks, link_number = 1, rescale = 0)
  
  # Create settings file
  settings_file_mod(feature_list_file = "feature_list.txt", 
                    condition_link_file = "condition_link_file.txt",
                    cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file_mod(zonation_path = "C:/Program Files/Zonation5",
               marginal_loss_mode = "CAZ2",
               flags = "wXc") # Flag w (use weights and condition)
  
  ## Run Zonation
  run_command_file(".")
})


##### Compare Scenarios #####
s1 = rast(paste0(scen_dir, "/output/rankmap.tif"))
s2 = rast(paste0(scen_dir_s2, "/output/rankmap.tif"))
s3 = rast(paste0(scen_dir_s3, "/output/rankmap.tif"))

names(s1)<- "z_weighted"
names(s2)<- "z_weighted_cost"
names(s3)<- "z_weighted_cost_distoutbreak"
