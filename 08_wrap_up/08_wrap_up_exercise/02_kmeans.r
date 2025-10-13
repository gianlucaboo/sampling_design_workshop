#### IMPORTANT ####

# Please do download the Cumulative_Flood_Extent_28082025.rar data set
# and save it in the data folder

# Do change the path in the setwd() function below to your path

#### Reading the data sets in R ####
# set working directory
setwd("~/Desktop/08_wrap_up_exercise/data")

# load libraries 
library(tidyverse); library(sf); library(cluster); library(ggridges)

# read eas eo data
eas <- 
  "data/eas_eo_data.shp" |> 
  st_read()

floods <- 
  "data/Cumulative_Flood_Extent_28082025/Cumulative_Flood_Extent_28082025.shp" |> 
  st_read() |> 
  st_transform(32642) |> 
  st_transform(st_crs(eas))

eas_flooded <- 
  eas |> 
  st_filter(floods)

#### Exploratory data analysis ####
ggplot() +
  geom_sf(data = floods, fill="blue",  color = NA) +
  geom_sf(data = eas, fill=NA,  color = "black") +
  theme_void()+
  labs(title="Narowal EAs and flood extent")

# EAs and flooded areas
ggplot() +
  geom_sf(data = eas_flooded, fill=NA,  color = "black") +
  theme_void()+
  labs(title="Narowal flooded EAs")

#Flooded EAs
eas_eo <- 
  eas_flooded |> 
  st_drop_geometry() |> 
  dplyr::select(water:bare)

#Land cover coverage
eas_eo_long <- 
  eas_eo |> 
  pivot_longer(cols=everything(), names_to="landcover", values_to="coverage")
  
ggplot(eas_eo_long, aes(x = coverage, y = landcover, group = landcover)) + 
  geom_density_ridges()+
  theme_minimal()

#### K-means clustering ####
# clustering is using all variables
colnames(eas_eo)

eas_eo <- 
  eas_eo |> 
  dplyr::select(crops, built)

## Run the clustering algorithm for different k
set.seed(1)
k_means_multiple_fit <- 
  c(1:9) |> 
  map (function(k) {
    kmeans(eas_eo, centers = k) |>
      _$cluster |> 
      data.frame()
  }) |> 
  list_cbind() |> 
  cbind(eas_flooded) |> 
  rename_with(~ paste0("cluster_", seq_along(.)), .cols = 1:9) |> 
  pivot_longer(cols=starts_with("cluster_"), names_to="k", values_to="Cluster") |> 
  mutate(k=k |>  str_sub(-1, -1),
         Cluster=Cluster |> as.factor()) |> 
  st_as_sf()

# Plot the clusters for dikfferent k 
ggplot() +
  geom_sf(data = eas, fill="grey90",  color = "grey90") +
  geom_sf(data = k_means_multiple_fit, aes(fill=Cluster),  color = NA) +
  geom_sf(data = floods, fill=NA,  color = "blue") +
  theme_void()+
  facet_wrap(vars(k))+
  labs(title="Narowal EAs", fill= "Number of Clusters\n(k=n)")

## Run the clustering algorithm for different k
k_means_multiple_withinss <- 
  c(1:9) |> 
  map (function(k) {
    kmeans(eas_eo, centers = k) |>
      _$tot.withinss |> 
      data.frame()
  }) |> 
  list_cbind() |> 
  pivot_longer(cols=everything(), names_to="k", values_to="tot.withinss") |> 
  mutate(k=k |>  str_sub(-1, -1) |> as.numeric())

## Plot the tot.withinss
ggplot(k_means_multiple_withinss, aes(x=k, y=tot.withinss)) +
  geom_line()+
  geom_point(size=3) +
  theme_minimal()

k_means_multiple_fit |> 
  filter(k==2) |> 
  st_write("eas_strata.shp")
