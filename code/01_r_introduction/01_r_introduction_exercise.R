##########################################
# Workshop: Advanced Sampling Methodologies
# Instructor: Dr. Gianluca Boo, WorldPop
# Date: 09-17-2025
# Topic: Introduction to R Programming
##########################################

# ================================
# 1. R as a Calculator
# ================================

# Simple arithmetic
1 + 2 * 8

# Assign values to variables
a <- log(10)
b <- 5
b + b

# Exercise 1 (Beginner):
# Calculate the square root of 144 and store it in a variable named sqrt_val
# Hint: use sqrt()
sqrt_val <- sqrt(144)
sqrt_val

# ================================
# 2. Working Directory
# ================================

# Check current working directory
getwd()

# Set a working directory (replace path with your project folder)
# setwd("C:/Users/YourName/Documents/R_Workshop")

# Exercise 2 (Beginner):
# Create a folder for your project and set it as working directory
# Hint: use setwd()

# ================================
# 3. R Packages
# ================================

# Install and load a package (if not installed)
# install.packages("dplyr")
library(dplyr)

# Exercise 3 (Beginner):
# Load the ggplot2 package and check its version
library(ggplot2)
packageVersion("ggplot2")

# ================================
# 4. R Objects and Data Types
# ================================

# Numeric
x <- c(1, 2, 3, 4)
# Character
y <- c("male", "female")
# Logical
z <- c(TRUE, FALSE, TRUE)
# Factor
gender <- factor(c("boy", "girl", "girl", "boy"))

# Check class and type
class(x); typeof(x)
class(gender); typeof(gender)

# Exercise 4 (Beginner):
# Create a vector of your top 5 favorite fruits and convert it into a factor
fruits <- factor(c("apple","banana","mango","orange","kiwi"))
fruits

# ================================
# 5. Data Structures
# ================================

# --- Vectors ---
numbers <- c(1, 2, 3, 4, 5)
numbers * 2  # vectorized operation

# Exercise 5 (Beginner):
# Create a numeric vector of 10 random integers between 1 and 100
set.seed(123)
rand_nums <- sample(1:100, 10)
rand_nums

# --- Matrices ---
rownames <- c("math", "science", "history", "English", "law")
colnames <- c("male", "female")
subject_matrix <- matrix(c(10,9,11,13,12,18,17,13,6,5),
                         nrow = 5, ncol = 2, byrow = TRUE,
                         dimnames = list(rownames, colnames))

# Add a row
subject_matrix <- rbind(subject_matrix, geography = c(4, 2))

# Exercise 6 (Intermediate):
# Add a new column "total" to subject_matrix calculating the sum of male and female scores
subject_matrix <- cbind(subject_matrix, total = subject_matrix[,1] + subject_matrix[,2])
subject_matrix

# --- Data Frames ---
cats <- data.frame(
  coat = c("Persian", "black", "tabby"),
  weight = c(2.1, 5.0, 3.2),
  likes_string = c(TRUE, FALSE, TRUE)
)

# Exercise 7 (Intermediate):
# Add a new column "age" with values 2, 4, 3
cats$age <- c(2, 4, 3)
cats

# --- Lists ---
my_list <- list(
  name = "Alice",
  age = 25,
  scores = c(90, 85, 92),
  passed = TRUE
)

# Access elements
my_list$name
my_list[[2]]

# Exercise 8 (Intermediate):
# Add a new element to my_list called "hobbies" containing 3 hobbies
my_list$hobbies <- c("reading","cycling","cooking")
my_list

# --- Arrays ---
arr <- array(1:12, dim = c(2, 3, 2))
arr[1, 2, 1]  # element at row 1, col 2, slice 1

# ================================
# 6. Importing Data
# ================================

# Manual import
csv_data <- read.csv("data/world_data.csv")

# Exercise 9 (Intermediate):
# Import GDP per capita data from Our World in Data
# Steps:
# 1. Download CSV: https://ourworldindata.org/grapher/gdp-per-capita-worldbank?overlay=download-data
# 2. Save in your project folder as "data/gdp_per_capita.csv"
# 3. Load into R using read_csv() from readr

library(readr)
gdp_data <- read_csv("data/gdp_per_capita.csv")
head(gdp_data)
colnames(gdp_data)
summary(gdp_data)

# Exercise 10 (Advanced):
# Find GDP per capita for the United States in 2020
gdp_data[gdp_data$Entity == "United States" & gdp_data$Year == 2020, ]

# Exercise 11 (Advanced):
# List all countries in the dataset
unique(gdp_data$Entity)

# Exercise 12 (Advanced):
# Plot GDP per capita over time for the United States
library(ggplot2)
ggplot(subset(gdp_data, Entity == "United States"), aes(x = Year, y = `GDP per capita`)) +
  geom_line(color = "blue") +
  labs(title = "US GDP per Capita Over Time", y = "GDP per Capita (USD)")

# ================================
# 7. Saving Your Script
# ================================

# Always save your work:
# File → Save or Ctrl+S / Cmd+S
# Add comments using #

# ================================
# 8. Closing R Session
# ================================

# Quit session (do not save workspace image)
# q()
