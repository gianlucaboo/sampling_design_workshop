##########################################
# Workshop: Advanced Sampling Methodologies
# Instructor: Dr. Gianluca Boo, WorldPop
# Date: 09-24-2025
# Topic: Simulation and Optimization of Sampling Strategies
##########################################

# ================================
# 1. Setup
# ================================

# Load required libraries
library(tidyverse)
library(parallel)    # for core detection
library(furrr)       # parallel mapping
library(purrr)       # functional programming

# Set random seed for reproducibility
set.seed(123)

# ================================
# 2. Law of Large Numbers (LLN)
# ================================

# Exercise 1: Demonstrate LLN
# Draw samples from Uniform(0,1) and compute cumulative mean
n <- 10000
x <- runif(n)
cum_means <- cumsum(x) / (1:n)

df_lln <- tibble(n = 1:n, cum_mean = cum_means)

ggplot(df_lln, aes(x = n, y = cum_mean)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(title = "Law of Large Numbers", x = "Sample size", y = "Sample mean")

# ================================
# 3. Central Limit Theorem (CLT)
# ================================

# Exercise 2: Demonstrate CLT
n_sim <- 5000
n_samp <- 30
means <- replicate(n_sim, mean(runif(n_samp, 0, 1)))

ggplot(tibble(sample_mean = means), aes(x = sample_mean)) +
  geom_histogram(aes(y = ..density..), bins = 40, fill = "skyblue", color = "white") +
  stat_function(fun = dnorm, args = list(mean = 0.5, sd = (1/sqrt(12))/sqrt(n_samp)),
                color = "red", size = 1.2) +
  labs(title = "Central Limit Theorem", x = "Sample mean", y = "Density")

# ================================
# 4. Monte Carlo Simulation
# ================================

# Exercise 3: Estimate mean under SRS
mu <- 5; sigma <- 2; n <- 30; R <- 2000
means_mc <- replicate(R, mean(rnorm(n, mu, sigma)))

ggplot(tibble(est = means_mc), aes(x = est)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue") +
  geom_vline(xintercept = mu, color = "red", linetype = "dashed") +
  labs(title = "Monte Carlo: Distribution of Sample Mean", x = "Estimated mean")

# ================================
# 5. Sample Size and Precision
# ================================

# Exercise 4: Standard error vs sample size
sample_sizes <- 1:100
results <- map_dfr(sample_sizes, function(n) {
  means <- replicate(R, mean(rnorm(n, mu, sigma)))
  tibble(n, se_empirical = sd(means))
})

ggplot(results, aes(x = n, y = se_empirical)) +
  geom_line(color = "steelblue") +
  geom_line(aes(y = sigma/sqrt(n)), color = "red", linetype = "dashed") +
  labs(title = "Precision vs Sample Size", x = "Sample size", y = "Empirical SE")

# ================================
# 6. Stratified Sampling
# ================================

# Exercise 5: Compare proportional vs Neyman allocation
N_h <- c(4000, 3000, 3000)
simulate_strata <- function(allocation = "prop", n = 600) {
  W_h <- N_h / sum(N_h)
  if(allocation == "prop") n_h <- round(n * W_h)
  if(allocation == "neyman") n_h <- round(n * c(10,20,12) / sum(N_h * c(10,20,12)))
  
  map_dfr(1:R, function(i) {
    samp <- map2_dfr(1:3, n_h, ~ tibble(y = rnorm(.y, 50 + 5*.x, 10 + 2*.x), h = .x))
    tibble(ybar = mean(samp$y))
  })
}

res_prop   <- simulate_strata("prop")
res_neyman <- simulate_strata("neyman")

# Compare SE
tibble(method = c("Proportional", "Neyman"),
       se = c(sd(res_prop$ybar), sd(res_neyman$ybar)))

# ================================
# 7. Parallel Computation
# ================================

# Exercise 6: Speed up Monte Carlo with parallelism
n_cores <- detectCores()
plan(multisession, workers = n_cores - 1)

res_parallel <- future_map_dfr(sample_sizes, function(n) {
  means <- replicate(R, mean(rnorm(n, mu, sigma)))
  tibble(n, se_empirical = sd(means))
})

ggplot(res_parallel, aes(x = n, y = se_empirical)) +
  geom_line(color = "darkgreen") +
  labs(title = "Parallel Monte Carlo Simulation", x = "Sample size", y = "SE")
