##########################################
# Workshop: Advanced Sampling Methodologies
# Instructor: Dr. Gianluca Boo, WorldPop
# Date: 09-17-2025
# Topic: Introduction to sampling design
##########################################

# ================================
# 1. Simple Random Sampling (SRS)
# ================================

# Population data
population <- c(10, 12, 15, 20, 25, 30, 35, 40, 45, 50)

# Exercise 1 (Beginner):
# Draw a simple random sample of size 4 from the population
# Hint: use sample()
srs_sample <- sample(population, 4)
srs_sample

# Exercise 2 (Beginner):
# Calculate the sample mean and sample variance
mean(srs_sample)
var(srs_sample)

# ================================
# 2. Stratified Sampling
# ================================

# Population divided into strata
stratum <- factor(c(rep("A",5), rep("B",5)))
values <- population

# Exercise 3 (Intermediate):
# Draw 2 samples from each stratum and compute the stratified mean
library(dplyr)
stratified_sample <- data.frame(values, stratum) %>%
  group_by(stratum) %>%
  slice_sample(n = 2)
stratified_sample

# Weighted stratified mean
stratified_mean <- stratified_sample %>%
  summarise(weighted_mean = sum(values)/length(values))
stratified_mean

# ================================
# 3. Cluster Sampling
# ================================

# Population divided into clusters
cluster <- factor(c(rep(1,3), rep(2,4), rep(3,3)))
values <- population

# Exercise 4 (Intermediate):
# Randomly select 1 cluster and compute the cluster mean
selected_cluster <- values[cluster == sample(levels(cluster), 1)]
mean(selected_cluster)

# ================================
# 4. PPS (Probability Proportional to Size)
# ================================

# Cluster sizes
cluster_sizes <- c(5, 10, 15)
values <- c(1:30)

# Exercise 5 (Advanced):
# Sample 5 elements with probability proportional to cluster size
prob <- rep(cluster_sizes / sum(cluster_sizes), cluster_sizes)
pps_sample <- sample(values, 5, prob = prob)
pps_sample

# Compute weighted sample mean
weights <- 1/prob[pps_sample]
sum(pps_sample * weights)/sum(weights)

# ================================
# 5. Multistage Sampling (2-stage)
# ================================

# Population organized in 3 clusters, each with 5 elements
clusters <- list(
  cluster1 = 1:5,
  cluster2 = 6:10,
  cluster3 = 11:15
)

# Exercise 6 (Advanced):
# Stage 1: Randomly select 2 clusters
selected_clusters <- clusters[sample(1:3, 2)]
selected_clusters

# Stage 2: Randomly select 2 elements from each selected cluster
stage2_sample <- lapply(selected_clusters, function(x) sample(x, 2))
stage2_sample

# Compute the overall sample mean
all_samples <- unlist(stage2_sample)
mean(all_samples)

