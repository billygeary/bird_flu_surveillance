# Plot Zonation outputs
# =============================================================================
# plot_zonation_rank_map.R
#
# Plots a Zonation rank raster (values 0-1) with tidyterra + ggplot2, using
# a binned viridis scale in 10% categories.
# =============================================================================

library(terra)
library(tidyterra)
library(ggplot2)

# -----------------------------------------------------------------------------
# PARAMETERS - edit these
# -----------------------------------------------------------------------------

rank_raster_path <- "weighted_zonation_run/rankmap.tif"

# -----------------------------------------------------------------------------
# LOAD AND PLOT
# -----------------------------------------------------------------------------

rank_r <- rast(rank_raster_path)

p <- ggplot() +
  geom_spatraster(data = rank_r) +
  scale_fill_viridis_b(
    name       = "Priority\nrank",
    breaks     = seq(0, 1, by = 0.1),
    limits     = c(0, 1),
    labels     = scales::percent_format(accuracy = 1),
    na.value   = NA,
    guide      = guide_coloursteps(show.limits = TRUE)
  ) +
  labs(
    title    = "Zonation priority ranking (weighted)",
  ) +
  theme_minimal() +
  theme(
    axis.title  = element_blank(),
    panel.grid  = element_blank()
  )

p

top10_r = ifel(rank_r > 0.9, 1, 0)
top10_r <- as.factor(top10_r)  # ensures 0/1 are treated as discrete categories

ggplot() +
  geom_spatraster(data = top10_r) +
  scale_fill_manual(
    name   = "Top 10%\npriority",
    values = c("0" = "grey", "1" = "yellow"),
    labels = c("0" = "Outside top 10%", "1" = "Top 10%"),
    na.value = NA,
    na.translate = FALSE
  ) +
  theme_minimal() +
  theme(axis.title = element_blank(), panel.grid = element_blank())
