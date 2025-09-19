library(readxl)     # load readxl for importing Excel files
library(tidyverse)  # load tidyverse (ggplot2, dplyr, etc.)

# getting and setting working directory 
getwd()   # check current working directory
setwd("/Users/worldpop/Documents/GitHub/sampling_design_workshop/02_samplingDesing_introduction/data") 
# set working directory to project data folder

# import data
data <- read_xlsx("Stratified sampling.xlsx")  # read Excel file into a tibble

# create id column
data <- data |> 
  mutate(id = 1:nrow(data), .before = Province) |> # create unique ID column before Province
  filter(!is.na(RU))                               # remove rows with missing RU (stratum)

# sample size
n <- 100   # define total sample size

#### RANDOM SAMPLE ####
# random sample of ids
set.seed(28)   # set seed for reproducibility
s_ids_random <- sample(data$id, size = n, replace = FALSE) 
# randomly select 100 unique IDs (no replacement)

s_random <- 
  data |>  
  filter(id %in% s_ids_random)   # keep only sampled IDs → random sample

# explore mean 
mean(data$Establishments, na.rm = TRUE)       # mean of population
mean(s_random$Establishments, na.rm = TRUE)   # mean of random sample

#### STRATIFIED SAMPLE ####
# number of observation per strata
s_strata <- 
  data |> 
  group_by(RU) |>                # group by stratum
  summarise(h = n()) |>          # count number of obs per stratum (h)
  ungroup() |> 
  mutate(h_tot = sum(h)) |>      # total population size
  mutate(W_h = h / h_tot) |>     # stratum weight = proportion of pop
  mutate(n_h = round(n * W_h, digits = 0))  
# allocate sample size per stratum (rounded)

# stratified sampling
s_stratified <- 
  data |> 
  left_join(s_strata) |>                # attach stratum sample sizes
  group_by(RU) |>                       # group by stratum
  group_modify(~ {                      # within each stratum:
    nh <- unique(.x$n_h)                # extract allocated sample size
    slice_sample(.x, n = as.integer(nh))# randomly sample nh units
  }) |> 
  ungroup() |> 
  relocate(id, .before = RU) |>         # move ID column before RU
  select(-c(h, h_tot)) |>               # drop unneeded columns
  group_by(RU) |> 
  mutate(ybar_h = mean(Establishments), # mean within each stratum
         s_strata_m = W_h * ybar_h)     # weighted mean contribution

# plot population
pop_plot <- 
  ggplot(data, aes(x = Establishments)) +
  geom_histogram(binwidth = 10, fill = "blue") +  # histogram of Establishments
  geom_vline(aes(xintercept = mean(Establishments)), 
             color = "red", linewidth = 0.5, linetype = "dashed") + 
  geom_text(aes(x = mean(Establishments), y = 0, 
                label = paste0("Mean = ", round(mean(Establishments), 0))),
            color = "black", angle = 90, vjust = -0.5, hjust = 0) + 
  labs(title = "Population", y = "Frequency") +
  theme_minimal() +
  facet_wrap(vars(RU))   # separate plots per stratum

# plot random sample
random_plot <- 
  ggplot(s_random, aes(x = Establishments)) +
  theme_minimal() +
  labs(title = "Random sample", y = "Frequency") +
  geom_histogram(binwidth = 10, fill = "blue") +
  geom_vline(aes(xintercept = mean(Establishments)), 
             color = "red", linewidth = 0.5, linetype = "dashed") +
  geom_text(aes(x = mean(Establishments), 
                y = 0, 
                label = paste0("Mean = ", round(mean(Establishments), 0))),
            color = "black", angle = 90, vjust = -0.5, hjust = 0) +
  facet_wrap(vars(RU))   # separate plots per stratum

# plot stratified sample
stratified_plot <- 
  ggplot(s_stratified, aes(x = Establishments)) +
  geom_histogram(binwidth = 10, fill = "blue") +
  geom_vline(aes(xintercept = mean(Establishments)), 
             color = "red", linewidth = 0.5, linetype = "dashed") +
  geom_text(aes(x = mean(Establishments), 
                y = 0, 
                label = paste0("Mean = ", round(mean(Establishments)), 0)),
            color = "black", angle = 90, vjust = -0.5, hjust = 0) +
  labs(title = "Stratified sample", y = "Frequency") +
  theme_minimal() +
  facet_wrap(vars(RU))   # separate plots per stratum

# combine plots
library(cowplot)   # load cowplot for combining plots

combined_plot <- plot_grid(pop_plot, stratified_plot, random_plot, 
                           nrow = 3, align = "v", labels = "auto")

combined_plot <- plot_grid(
  ggdraw() + draw_label("Comparison of Population, Stratified Sample, and Random Sample",
                        fontface = 'bold', x = 0.5, hjust = 0.5, size = 16),
  combined_plot,
  ncol = 1,
  rel_heights = c(0.1, 1)  # make title smaller height than plots
)

combined_plot   # display combined plot