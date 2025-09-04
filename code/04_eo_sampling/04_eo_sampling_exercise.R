##########################################
# Workshop: Advanced Sampling Methodologies
# Instructor: Dr. Gianluca Boo, WorldPop
# Date: 09-17-2025
# Topic: Urbanization Categorization with GHSL SMOD and Admin-1 Units (Pakistan)
##########################################

# ================================
# 1. Setup
# ================================

# Load required libraries
library(tidyverse)
library(sf)             # spatial vector data
library(stars)          # raster data
library(exactextractr)  # zonal statistics

# Load Pakistan Admin-3 boundaries
# (Make sure you have this file in your project data folder)
pak_admin3 <- st_read("data/pak_adm_wfp_20220909_shp/pak_admbnda_adm3_wfp_20220909.shp")

# Load GHSL Settlement Model (SMOD) raster
# Download/export from Google Earth Engine for Pakistan region 
# https://developers.google.com/earth-engine/datasets/catalog/JRC_GHSL_P2023A_GHS_SMOD_V2-0#description
smod <- read_stars("data/GHSL_SMOD_Pakistan.tif")

# Inspect datasets
head(pak_admin3)
st_crs(pak_admin3)
plot(smod)
pak_admin3 |> st_geometry() |> plot(add = TRUE)

# ================================
# 2. Data Manipulation
# ================================

# Exercise 1: Count units
nrow(pak_admin3)

# Exercise 2: Check unique SMOD classes
unique(as.vector(smod[[1]]))

# Exercise 3: Extract dominant SMOD class (mode) per admin-1 unit
smod_mode <- exact_extract(smod, pak_admin3, 
                           function(values, coverage_fraction) {
                             tab <- tapply(coverage_fraction, values, sum, na.rm = TRUE)
                             as.integer(names(which.max(tab)))
                           })

pak_admin3$SMOD_mode <- smod_mode
pak_admin3 |> select(NAME_1, SMOD_mode)

# ================================
# 3. Data Visualization
# ================================

# Exercise 4: Map unites colored by dominant SMOD class
ggplot(pak_admin3) +
  geom_sf(aes(fill = factor(SMOD_mode))) +
  scale_fill_brewer(palette = "Set2", name = "Dominant SMOD Class") +
  labs(title = "Dominant Settlement Type in Pakistan (Admin-1 Level)")

# Exercise 5: Compare proportions of SMOD classes across units
smod_table <- exact_extract(smod, pak_admin3, 
                            function(values, coverage_fraction) {
                              tab <- tapply(coverage_fraction, values, sum, na.rm = TRUE)
                              tab / sum(tab)
                            }, progress = TRUE)

# Convert to tidy format
smod_df <- map2_dfr(smod_table, pak_admin3$NAME_1, function(x, units) {
  tibble(units, class = names(x), prop = as.numeric(x))
})

ggplot(smod_df, aes(x = units, y = prop, fill = class)) +
  geom_col(position = "fill") +
  coord_flip() +
  labs(title = "SMOD Class Composition by Units (Pakistan)",
       y = "Proportion", x = "Unit")

# ================================
# 4. Advanced Analysis
# ================================

# Exercise 6: Categorize unit by dominant SMOD mode
pak_admin3 <- pak_admin3 |> 
  mutate(Category = case_when(
    SMOD_mode %in% c(30, 31) ~ "Urban",
    SMOD_mode %in% c(22, 23) ~ "Semi-urban",
    SMOD_mode %in% c(10, 11, 12) ~ "Rural",
    TRUE ~ "Other"
  ))

pak_admin3 |> select(NAME_1, SMOD_mode, Category)

# Final map
ggplot(pak_admin3) +
  geom_sf(aes(fill = Category)) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Categorisation of Pakistan Units by Settlement Type")
