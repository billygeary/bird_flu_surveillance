library(terra)
library(dplyr)
library(RColorBrewer)
scen_dir_s3 = "~/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/z_runs/s3_susceptibility_linear_outbreak_dist_current"

s3 = rast(paste0(scen_dir_s3, "/output/rankmap.tif"))

select.sites <- function(map, n_sites, percent = 10) {
  pal <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(100))
  # Calculate the proportional threshold
  prop <- percent / 100
  
  # Mask all cells at or below the threshold (keep only top % of ranked cells)
  top_cells <- map
  top_cells[top_cells <= (1 - prop)] <- NA
  
  sites <- spatSample(top_cells,
                      size   = n_sites,
                      method = "random",
                      cells  = TRUE,
                      xy     = TRUE,
                      na.rm  = TRUE)
  # Extract XY coordinates directly from the named columns
  sites_XY <- sites[ , c("x", "y")]
  return(sites_XY)
}


# Get travel costs for each site
cost_path = "~/OneDrive - The University of Melbourne/Research/Projects/2026/bird_flu_surveillance/input_data/travel_cost.tif"
travel = rast(cost_path)

library(terra)
library(dplyr)
library(ggplot2)

rank_choices = c(5, 10, 15)
site_choices = seq(100, 1000, 50)
n_sims = 100

# Constant across all iterations, so calculate once
active_survey_cost = 360 * # Minutes per site
  60 /                     # Cost per hour (AUD)
  60                       # Conversion of minutes to hours

# Pre-allocate a list to collect results (much faster than rbind in a loop)
cost_list = vector("list", length(rank_choices) * length(site_choices) * n_sims)

set.seed(42)  # for reproducibility; remove or change as needed

for(sim in 1:n_sims){
  for(r in seq_along(rank_choices)){
    for(s in seq_along(site_choices)){
      
      n_sites = site_choices[s]
      rank_choice = rank_choices[r]
      
      # Randomly select sites from the top x% of Zonation ranked cells
      sites = select.sites(s3, n_sites, percent = rank_choice)
      sites = vect(sites)
      site_costs = extract(travel, sites)
      
      site_costs = site_costs$travel_time_to_cities_12 + active_survey_cost
      
      cost_list[[i]] = data.frame(sim = sim,
                                  rank = rank_choice,
                                  nsites = n_sites,
                                  mean_cost = mean(site_costs, na.rm = TRUE),
                                  max_cost = max(site_costs, na.rm=TRUE),
                                  total_cost = sum(site_costs, na.rm = TRUE))
      i = i + 1
    }
  }
}

cost_sims = bind_rows(cost_list)

# Average across simulations
cost_out = cost_sims %>%
  group_by(rank, nsites) %>%
  summarise(mean_cost = mean(total_cost),
            sd_cost = sd(total_cost),
            lower = quantile(total_cost, 0.025),
            upper = quantile(total_cost, 0.975),
            .groups = "drop")

write.csv(cost_out, "surveillance_cost_sims.csv")

library(scales)
ggplot(cost_out) + 
  geom_tile(aes(x = rank, y = nsites, fill = mean_cost)) + 
  scale_fill_viridis_c(name = "Mean total cost (AUD)", labels = label_comma()) +
  labs(x = "Top % of Zonation ranked cells", y = "Number of sites") + theme_bw()


