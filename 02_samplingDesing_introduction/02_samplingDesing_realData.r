library(readxl); library(tidyverse)

# getting and setting working directory 
getwd()
setwd("/Users/worldpop/Documents/GitHub/sampling_design_workshop/02_samplingDesing_introduction/data")

# import data
data <- read_xlsx("Stratified sampling.xlsx")

# create id column
data <- data |> 
  mutate(id=1:nrow(data), .before=Province) |> 
  filter(!is.na(RU))

# sample size
n <- 1000

#### RANDOM SAMPLE ####
# random sample of ids
set.seed(28)
s_ids_random <- sample(data$id, size = n, replace = FALSE)

s_random <- 
  data |>  
  filter(id %in% s_ids_random)

# explore mean 
mean(data$Establishments, na.rm = T)
mean(s_random$Establishments, na.rm = T)

#### STRATIFIED SAMPLE ####
# number of observation per strata
s_strata <- 
  data |> 
  group_by(RU) |> 
  summarise(h=n()) |> 
  ungroup() |> 
  mutate(h_tot=sum(h)) |> 
  mutate(W_h=h/h_tot) |> 
  mutate(n_h=round(n * W_h, digits = 0))
  
# stratified sampling
s_stratified <- 
  data |> 
  left_join(s_strata) |>
  group_by(RU) |> # group by stratum
  group_modify(~ {
    nh <- unique(.x$n_h)       
    slice_sample(.x, n = as.integer(nh))
  }) |> 
  ungroup() |> 
  relocate(id, .before=RU) |> 
  select(-c(h, h_tot)) |> 
  group_by(RU) |> 
  mutate(ybar_h=mean(Establishments),
         s_strata_m=W_h*ybar_h)

pop_plot <- 
  ggplot(data, aes(x = Establishments)) +
  geom_histogram(binwidth = 10, fill="grey70")+
  geom_vline(aes(xintercept = mean(data$Establishments) ), color = "black", linewidth = 0.5, linetype = "dashed") +
  geom_text(aes(x = mean(data$Establishments), y = 0, 
                label = paste0("Mean = ", round(mean(data$Establishments), 1))),
            color = "black", angle = 90, vjust = -0.5, hjust = 0) +
  labs(title="Population")+
  theme_minimal() +
  facet_wrap(vars(RU))

random_plot <- 
  ggplot(s_random, aes(x = Establishments)) +
  theme_minimal()+
  labs(title="Random sample")+
  geom_histogram(binwidth = 10, fill="grey70")+
  geom_vline(aes(xintercept = mean(s_random$Establishments)), color = "black", linewidth = 0.5, linetype = "dashed") +
  geom_text(aes(x = mean(s_random$Establishments), 
                y = 0, 
                label = paste0("Mean = ", round(mean(s_random$Establishments), 1))),
            color = "black", angle = 90, vjust = -0.5, hjust = 0) +
  facet_wrap(vars(RU))


stratified_plot <- 
  ggplot(s_stratified, aes(x = Establishments)) +
  geom_histogram(binwidth = 10, fill="grey70")+
  geom_vline(aes(xintercept=mean(s_stratified$Establishments)), color = "black", linewidth = 0.5, linetype = "dashed") +
  geom_text(aes(x = mean(s_stratified$Establishments), 
                y = 0, 
                label = paste0("Mean = ", round(mean(s_stratified$Establishments)), 1)),
            color = "black", angle = 90, vjust = -0.5, hjust = 0) +
  labs(title="Stratified sample")+
  theme_minimal()+
  facet_wrap(vars(RU))



library(cowplot)

combined_plot <- plot_grid(pop_plot, stratified_plot, random_plot, 
                           nrow = 3, align = "v")
combined_plot

