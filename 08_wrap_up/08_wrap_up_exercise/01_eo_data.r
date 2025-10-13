#### Reading the data sets in R ####
# set working directory
setwd("~/Desktop/08_wrap_up_exercise/data")

# load libraries
library(sf); library(terra); library(tidyverse)

# load gdw data
gdw <- 
  "narowal_gdw.tif" |> 
  rast()

# access the different bands
gdw |> 
  names()

# plot the crops band
gdw$built |> 
  plot()

# load wp data
wp <- 
  "narowal_wp.tif" |> 
  rast()

# access the different bands
wp |> 
  names()

# plot the crops band
wp |> 
  plot()


#### Extracting the values to the EA boundaries ####
# load the EAs
eas <- st_read("NAROWAL_preEA_04_01.shp")

# extract mean gdw band for eas
eas_gdw <- 
  gdw |> 
  terra::extract(eas, fun = mean, na.rm = TRUE) |> 
  select(-ID)

# extract total population for eas
eas_wp <- 
  wp |> 
  terra::extract(eas, fun = sum, na.rm = TRUE) |> 
  select(-ID)

# extract total population for eas
eas <- 
  eas |> 
  cbind(eas_gdw) |> 
  cbind(eas_wp)


#### Explore the data ####
# explore column names
colnames(eas)

# plot one map per class
ggplot() +
  geom_sf(data = eas, aes(fill=pak_pop_2025_CN_100m_R2025A_v1),  color = NA) +
  scale_fill_viridis_c()+
  theme_void()+
  labs(title="Narowal EAs", fill= "Population count")

# plot one map with all classes
eas_long <- 
  eas |> 
  pivot_longer(cols=c(water:bare), names_to="landcover", values_to="coverage") |> 
  select(landcover, coverage, geometry)

ggplot() +
  geom_sf(data = eas_long, aes(fill=coverage),  color = NA) +
  scale_fill_viridis_c()+
  theme_void()+
  facet_wrap(vars(landcover))+
  labs(title="Narowal EAs", fill= "Average\nprobability")

st_write(eas, "eas_eo_data.shp")

