# Create weighting file
library(tidyverse)

ausgov_list = read.csv("input_data/h5birdflu_ausgov_susceptibility.csv")
snes = read.csv("input_data/snes_species.csv") 

# Clean susceptibility column names
suscept <- ausgov_list %>%
  rename(
    taxa_group     = `Taxa.group`,
    family_group   = `Family.group`,
    common_name_s  = `Common.name`,
    sci_name_s     = `Scientific.name`,
    susceptibility = `Susceptibility.score`,
    cons_status_s  = `Conservation.status`,
    nat_risk       = `National.risk.score`
  ) %>%
  mutate(sci_name_s_clean = str_trim(sci_name_s))

# ── Prepare SPRAT ──────────────────────────────────────────────────────────────

sprat_base <- snes %>%
  mutate(
    # Clean the primary scientific name
    sci_name_clean   = str_trim(SCIENTIFIC_NAME),
    # Clean the EPBC name field (may contain pipe-separated synonyms)
    epbc_names_clean = str_trim(EPBC_NAMES),
    # Strip population qualifiers e.g. "(Barrow Island)" for fallback matching
    sci_no_qual      = str_trim(str_remove(sci_name_clean, "\\s*\\(.*\\)$")),
    # Normalised common name for last-resort matching
    common_clean     = str_to_lower(str_trim(VERNACULAR_NAME))
  )

# Expand EPBC_NAMES on pipe separator — each synonym gets its own row
sprat_epbc_expanded <- sprat_base %>%
  separate_rows(epbc_names_clean, sep = "\\|") %>%
  mutate(epbc_names_clean = str_trim(epbc_names_clean))

# ── Pass 1: SCIENTIFIC_NAME == Scientific name ─────────────────────────────────

pass1 <- sprat_base %>%
  inner_join(
    suscept,
    by = c("sci_name_clean" = "sci_name_s_clean")
  ) %>%
  mutate(match_type = "1_scientific_name")

cat("Pass 1 (SCIENTIFIC_NAME):", nrow(pass1), "\n")
matched_taxon_ids <- pass1$LISTED_TAXON_ID

# ── Pass 2: EPBC_NAMES (any synonym) == Scientific name ───────────────────────

pass2 <- sprat_epbc_expanded %>%
  filter(!LISTED_TAXON_ID %in% matched_taxon_ids) %>%
  inner_join(
    suscept,
    by = c("epbc_names_clean" = "sci_name_s_clean")
  ) %>%
  mutate(match_type = "2_epbc_synonym") %>%
  # If a SPRAT record matched multiple EPBC synonyms, keep first
  distinct(LISTED_TAXON_ID, sci_name_s, .keep_all = TRUE)

cat("Pass 2 (EPBC synonym):", nrow(pass2), "\n")
matched_taxon_ids <- c(matched_taxon_ids, pass2$LISTED_TAXON_ID)

# ── Pass 3: SCIENTIFIC_NAME without qualifier == Scientific name ───────────────
# Handles cases like "Tursiops aduncus (Arafura/Timor Sea populations)"
# matching to "Tursiops aduncus"

pass3 <- sprat_base %>%
  filter(!LISTED_TAXON_ID %in% matched_taxon_ids) %>%
  inner_join(
    suscept,
    by = c("sci_no_qual" = "sci_name_s_clean")
  ) %>%
  mutate(match_type = "3_sci_no_qualifier")

cat("Pass 3 (sci no qualifier):", nrow(pass3), "\n")
matched_taxon_ids <- c(matched_taxon_ids, pass3$LISTED_TAXON_ID)

# ── Pass 4: Common name == Common name (normalised, lower case) ────────────────

suscept_cn <- suscept %>%
  mutate(common_clean = str_to_lower(str_trim(common_name_s)))

pass4 <- sprat_base %>%
  filter(!LISTED_TAXON_ID %in% matched_taxon_ids) %>%
  # VERNACULAR_NAME may be pipe/comma separated — try first name only
  mutate(common_first = str_to_lower(str_trim(str_extract(VERNACULAR_NAME, "^[^,|]+")))) %>%
  inner_join(
    suscept_cn,
    by = c("common_first" = "common_clean")
  ) %>%
  mutate(match_type = "4_common_name")

cat("Pass 4 (common name):", nrow(pass4), "\n")
matched_taxon_ids <- c(matched_taxon_ids, pass4$LISTED_TAXON_ID)

# ── Combine all passes ─────────────────────────────────────────────────────────

joined <- bind_rows(pass1, pass2, pass3, pass4) %>%
  arrange(LISTED_TAXON_ID, match_type) %>%
  distinct(LISTED_TAXON_ID, .keep_all = TRUE)

cat("\nTotal matched:", nrow(joined), "\n")

# ── Manual crosswalk for unmatched records ─────────────────────────────────────
# Map SPRAT LISTED_TAXON_ID to susceptibility sci_name_s

manual_crosswalk <- tribble(
  ~SCIENTIFIC_NAME, ~sci_name_s,
  # Replace the values below with your actual unmatched IDs and names
  "Cecropis daurica", NA,          # Vagrant         
  "Pygoscelis antarcticus", NA,    # Vagrant        
  "Onychogalea fraenata", "Onychogalea frenata",               
  "Pseudomys fieldi", "Pseudomys gouldii (previously P. fieldii)",                   
  "Cyclopsitta diophthalma coxeni", NA, # Missing
  "Morus capensis", NA, # Vagrant
  "Turnix olivii" , "Turnix varius", # Missing but given same as co-occurring spp                     
  "Thalassarche eremita", NA, # Vagrant          
  "Melanodryas cucullata melvillensis", "Melanodryas cucullata picata", # Missing but given same as similar spp 
  "Thinornis cucullatus", "Thinornis cucullatus tregellasi", # Two sub spp. Given higher weight              
  "Bettongia penicillata ogilbyi", "Bettongia penicillata"
)

# Join manual crosswalk to susceptibility data
pass5 <- manual_crosswalk %>%
  filter(!is.na(SCIENTIFIC_NAME)) %>%
  left_join(
    sprat_base,
    by = "SCIENTIFIC_NAME"
  ) %>%
  left_join(
    suscept,
    by = "sci_name_s"
  ) %>%
  mutate(match_type = "5_manual")

cat("Pass 5 (manual):", nrow(pass5), "\n")

# ── Add to final joined dataset ────────────────────────────────────────────────

joined_final <- bind_rows(joined, pass5) %>%
  arrange(LISTED_TAXON_ID, match_type) %>%
  distinct(LISTED_TAXON_ID, .keep_all = TRUE)

cat("Final total matched:", nrow(joined_final), "\n")

# ── Make weightigs ────────────────────────────────────────────────────────────────
joined_final_selected = joined_final %>%
  select(LISTED_TAXON_ID, SCIENTIFIC_NAME, EPBC_NAMES, VERNACULAR_NAME, THREATENED_STATUS,
         susceptibility, cons_status_s, nat_risk) %>%
  mutate(weight = case_when(nat_risk == "Extreme" ~ 1,
                            nat_risk == "Very high" ~ 0.8, 
                            nat_risk == "High" ~ 0.6,
                            nat_risk == "Moderate" ~ 0.4,
                            nat_risk == "Low" ~ 0.2,
                            .default = 0.2))

# ── Save ────────────────────────────────────────────────────────────────

write.csv(joined_final_selected, "input_data/h5birdflu_species_weights.csv")
