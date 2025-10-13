library(sf); library(tidyverse)

getwd()
setwd("/Users/worldpop/Documents/GitHub/sampling_design_workshop/04_eo_sampling")

admin_boundaries <- 
  "data/pak_adm_wfp_20220909_shp/pak_admbnda_adm3_wfp_20220909.shp" |> 
  st_read(quiet = T) |> 
  select(-ADM3_REF) |> 
  mutate(Shape_Leng=Shape_Leng*10) |> 
  group_by(ADM2_EN) |> 
  summarise(ADM2_AREA=sum(Shape_Area))

admin_boundaries |> 
  st_geometry() |> 
  plot()

admin_boundaries

library(terra)
elevation <- 
  "data/pak_elevation_merit103_10km_v1.tif" |> 
  rast()

elevation |> 
  plot()

st_crs(admin_boundaries)
crs(elevation)

admin_boundaries <- 
  admin_boundaries |> 
  st_transform(crs(elevation))

st_crs(admin_boundaries)


### SUMMARISING ###

admin_elevation_mean <- 
  elevation |> 
  terra::extract(admin_boundaries, fun = mean, na.rm = TRUE)


admin_boundaries<- 
  admin_boundaries |> 
  mutate(elevation_mean=admin_elevation_mean$elevation_merit103_100m_v1, .before=geometry)



admin_boundaries |> 
  st_drop_geometry() |> 
  dplyr::select(ADM2_EN, elevation_mean)


library(ggplot2)
mean_plot <- ggplot() +
  geom_sf(data = admin_boundaries, aes(fill=elevation_mean),   color= "black", alpha = 0.3) +
  scale_fill_viridis_c()+
  #scale_fill_grey()+
  geom_sf_text(data = admin_boundaries, aes(label = ADM2_EN), size = 3) +
  #theme_void()+
  theme_dark()+
  labs(title="Mean Elevation", fill= "Elevation (m)")


library(ggplot2)
elevation_df <- as.data.frame(elevation, xy = TRUE)
colnames(elevation_df) <- c("x", "y", "elevation")

library(ggplot2)
elevation_df <- as.data.frame(elevation, xy = TRUE)

colnames(elevation_df) <- c("x", "y", "elevation")


elev_plot <- ggplot(elevation_df) +
  geom_raster(aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c() +
 # coord_equal() +
  theme_void()+
  labs(title = "Title", fill = "Legend (m)")


library(ggpubr)

plot_combined <- ggarrange(mean_plot, plot,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)

plot_combined
