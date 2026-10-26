library(ZonationR)
library(terra)

source("scripts/function_zonation_prep.R")

## All scenarios consider travel cost
## Different scenarios test alternative species weights (x3)
## and different appraoches for quantifying distance from nearest outbreak (x2)

## Key files
cost_path = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/travel_cost.tif"
weights = read.csv("input_data/species_weight_scenarios.csv") %>% mutate(unweighted = 1)

spp.dir = "X:/Projects/bird_flu_surveillance/species"
spp.list = list.files("X:/Projects/bird_flu_surveillance/species", full=T)

outbreak_density = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/outbreak_density_cond.tif"
outbreak_dist = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/outbreak_distance_cond.tif"
future_outbreak_density = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/future_outbreak_density_cond.tif"
future_outbreak_dist = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/future_outbreak_distance_cond.tif"

###############################################
#### Scenario 0: No species weights ####
###############################################
## Feature List
scen_dir = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s0_unweighted"

withr::with_dir(scen_dir, {
  # Create the feature list 
  feature_list(spp_file_dir = spp.dir, weight = weights$unweighted)
  
  # Create settings file
  settings_file(feature_list_file = "feature_list.txt", 
                cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file(zonation_path = "C:/Program Files/Zonation5",
               marginal_loss_mode = "CAZ2",
               flags = "wX")
  
  ## Run Zonation
  run_command_file(".")
})



########################################################
#### Scenario 1: Weighted by National Risk - Linear ####
########################################################

## Feature List
scen_dir = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s1_susceptibility_linear"

withr::with_dir(scen_dir, {
  # Create the feature list 
  feature_list(spp_file_dir = spp.dir, weight = weights$weight_natrisk_linear)
  
  # Create settings file
  settings_file(feature_list_file = "feature_list.txt", 
                cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file(zonation_path = "C:/Program Files/Zonation5",
               marginal_loss_mode = "CAZ2",
               flags = "wX")
  
  ## Run Zonation
  run_command_file(".")
})


############################################################################
#### Scenario 2: Weighted by National Risk - Multiplicative ####
############################################################################

scen_dir_s2 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s2_susceptibility_mult"

withr::with_dir(scen_dir_s2, {
  # Create the feature list 
  feature_list(spp_file_dir = spp.dir, weight = weights$weight_natrisk_exp)
  
  # Create settings file
  settings_file(feature_list_file = "feature_list.txt", 
                cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file(zonation_path = "C:/Program Files/Zonation5",
               marginal_loss_mode = "CAZ2",
               flags = "wX")
  
  ## Run Zonation
  run_command_file(".")
})

############################################################################
#### Scenario 3: Susceptibility - Linear & Distance from Outbreak ####
############################################################################

scen_dir_s3 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s3_susceptibility_linear_outbreak_dist_current"

withr::with_dir(scen_dir_s3, {
  # Create the feature list 
  feature_list_mod(spp_file_dir = spp.dir, weight = weights$weight_natrisk_linear, condition = TRUE)
  
  # Create condition link file
  condition_link_file(condition_file = basename(outbreak_dist), link_number = 1, rescale = 0)
  
  # Create settings file
  settings_file_mod(feature_list_file = "feature_list.txt", 
                    condition_link_file = "condition_link_file.txt",
                    cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file_mod(zonation_path = "C:/Program Files/Zonation5",
               marginal_loss_mode = "CAZ2",
               flags = "wXc") # Flag w (use weights, cost and condition)
  
  ## Run Zonation
  run_command_file(".")
})

#############################################################################
#### Scenario 4: Susceptibility - Linear & Distance from Future Outbreak ####
#############################################################################
scen_dir_s4 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s4_susceptibility_linear_outbreak_dist_future"

withr::with_dir(scen_dir_s4, {
  # Create the feature list 
  feature_list_mod(spp_file_dir = spp.dir, weight = weights$weight_natrisk_linear, condition = TRUE)
  
  # Create condition link file
  condition_link_file(condition_file = basename(future_outbreak_dist), link_number = 1, rescale = 0)
  
  # Create settings file
  settings_file_mod(feature_list_file = "feature_list.txt", 
                    condition_link_file = "condition_link_file.txt",
                    cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file_mod(zonation_path = "C:/Program Files/Zonation5",
                   marginal_loss_mode = "CAZ2",
                   flags = "wXc") # Flag w (use weights, cost and condition)
  
  ## Run Zonation
  run_command_file(".")
})

############################################################################
#### Scenario 5: Susceptibility - Linear & Density of Outbreak ####
############################################################################

scen_dir_s5 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s5_susceptibility_linear_outbreak_dens_current"

withr::with_dir(scen_dir_s5, {
  # Create the feature list 
  feature_list_mod(spp_file_dir = spp.dir, weight = weights$weight_natrisk_linear, condition = TRUE)
  
  # Create condition link file
  condition_link_file(condition_file = basename(outbreak_density), link_number = 1, rescale = 0)
  
  # Create settings file
  settings_file_mod(feature_list_file = "feature_list.txt", 
                    condition_link_file = "condition_link_file.txt",
                    cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file_mod(zonation_path = "C:/Program Files/Zonation5",
                   marginal_loss_mode = "CAZ2",
                   flags = "wXc") # Flag w (use weights, cost and condition)
  
  ## Run Zonation
  run_command_file(".")
})



#############################################################################
#### Scenario 6: Susceptibility - Linear & Density of Future Outbreak ####
#############################################################################
scen_dir_s6 = "C:/Users/geary/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s6_susceptibility_linear_outbreak_dens_future"

withr::with_dir(scen_dir_s6, {
  # Create the feature list 
  feature_list_mod(spp_file_dir = spp.dir, weight = weights$weight_natrisk_linear, condition = TRUE)
  
  # Create condition link file
  condition_link_file(condition_file = basename(future_outbreak_density), link_number = 1, rescale = 0)
  
  # Create settings file
  settings_file_mod(feature_list_file = "feature_list.txt", 
                    condition_link_file = "condition_link_file.txt",
                    cost_layer = cost_path)
  
  # Create the Zonation command file
  command_file_mod(zonation_path = "C:/Program Files/Zonation5",
                   marginal_loss_mode = "CAZ2",
                   flags = "wXc") # Flag w (use weights, cost and condition)
  
  ## Run Zonation
  run_command_file(".")
})
