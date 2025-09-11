##########################################
# Workshop: Advanced Sampling Methodologies
# Instructor: Dr. Gianluca Boo, WorldPop
# Date: 09-17-2025
# Topic: Cdvanced R programming with the tidyverse
##########################################

# ================================
# 1. Setup
# ================================

# Load tidyverse
library(tidyverse)

# Import GDP per capita dataset
# Steps:
# 1. Download from: https://ourworldindata.org/grapher/gdp-per-capita-worldbank?overlay=download-data
# 2. Save it in your project folder as "data/gdp_per_capita.csv"
gdp_data <- read_csv("data/gdp_per_capita.csv")

# Inspect dataset
head(gdp_data)
colnames(gdp_data)
summary(gdp_data)

# ================================
# 2. Data Manipulation
# ================================

# Exercise 1 (Beginner):
# Filter GDP data for the United States
us_gdp <- gdp_data |> filter(Entity == "United States")
head(us_gdp)

# Exercise 2 (Beginner):
# Find GDP per capita for the United States in 2020
us_gdp |> filter(Year == 2020)

# Exercise 3 (Intermediate):
# Calculate the average GDP per capita for each continent in 2010
# Hint: group_by() + summarise()
gdp_data |> 
  filter(Year == 2010) |> 
  group_by(`World bank region`) |> 
  summarise(avg_gdp = mean(`GDP per capita`, na.rm = TRUE))

# ================================
# 3. Data Visualization
# ================================

# Exercise 4 (Intermediate):
# Plot GDP per capita of the United States over time
ggplot(us_gdp, aes(x = Year, y = `GDP per capita`)) +
  geom_line(color = "blue") +
  labs(title = "US GDP per Capita Over Time",
       y = "GDP per Capita (USD)",
       x = "Year")

# Exercise 5 (Intermediate):
# Compare GDP per capita over time for United States, India, and China
gdp_data |> 
  filter(Entity %in% c("United States", "India", "China")) |> 
  ggplot(aes(x = Year, y = `GDP per capita`, color = Entity)) +
  geom_line(size = 1) +
  labs(title = "GDP per Capita Over Time",
       y = "GDP per Capita (USD)",
       x = "Year")

# Exercise 6 (Advanced):
# Create a faceted plot of GDP per capita trends for G7 countries
g7_countries <- c("United States", "United Kingdom", "Canada", 
                  "France", "Germany", "Italy", "Japan")

gdp_data |> 
  filter(Entity %in% g7_countries) |> 
  ggplot(aes(x = Year, y = `GDP per capita`, color = Entity)) +
  geom_line(size = 1) +
  facet_wrap(~Entity, scales = "free_y") +
  labs(title = "GDP per Capita Trends (G7 Countries)",
       y = "GDP per Capita (USD)",
       x = "Year")

# ================================
# 4. Data Cleaning with tidyr
# ================================

# Exercise 7 (Advanced):
# Reshape GDP data to compare 2000 vs 2020 for selected countries
comparison <- gdp_data |> 
  filter(Year %in% c(2000, 2020),
         Entity %in% c("United States", "India", "China", "Brazil", "South Africa")) |> 
  select(Entity, Year, `GDP per capita`) |> 
  pivot_wider(names_from = Year, values_from = `GDP per capita`)

comparison

# Add a new column showing growth
comparison <- comparison |> 
  mutate(growth = (`2020` - `2000`) / `2000` * 100)

comparison