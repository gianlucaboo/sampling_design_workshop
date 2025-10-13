### Defining three functions for simple random sampling, stratified sampling, and stratified weighted sampling ####
library(furrr); library(tidyverse); library(sf)

#-------------------------------
# 1. Simple Random Sampling (SRS)
#-------------------------------
sample_srs <- function(data, y_col, n){
  samp <- data |> 
    slice_sample(n = n)
  
  ybar <- mean(samp[[y_col]])
  return(list(estimate = ybar))
}


#-------------------------------
# 2. Weighted sampling
#-------------------------------
sample_weighted <- function(data, y_col, weigh_col,  n){
  samp <- data |> 
    slice_sample(n = n, weight_by=weigh_col)
  
  ybar <- mean(samp[[y_col]])
  return(list(estimate = ybar))
}


#---------------------------------------
# 2. Stratified Sampling (prop / neyman)
#---------------------------------------
sample_stratified <- function(data, y_col, strata_col, n, allocation = "prop") {
  # --- Compute stratum sizes and weights ---
  strata_sizes <- data |>
    count(.data[[strata_col]], name = "N_h")
  N <- sum(strata_sizes$N_h)
  W_h <- strata_sizes$N_h / N
  names(W_h) <- strata_sizes[[strata_col]]
  
  # --- Allocation ---
  if (allocation == "prop") {
    n_h <- round(n * W_h)
  }
  if (allocation == "neyman") {
    s_h <- data |>
      group_by(.data[[strata_col]]) |>
      summarise(s = sd(.data[[y_col]]), .groups = "drop") |>
      pull(s)
    n_h <- round(n * (strata_sizes$N_h * s_h) /
                   sum(strata_sizes$N_h * s_h))
  }
  names(n_h) <- strata_sizes[[strata_col]]
  
  # Ensure all strata have a non-zero sample size. If 0, set to 1.
  n_h[n_h == 0] <- 1
  
  # --- Stratified sampling ---
  samp <- data |>
    group_by(.data[[strata_col]]) |>
    group_map(~ {
      stratum_key <- .y[[1]]
      ni <- n_h[as.character(stratum_key)]
      # Ensure ni is not larger than the stratum size
      ni <- min(ni, nrow(.x))
      slice_sample(.x, n = ni)
    }, .keep = TRUE) |>
    bind_rows()
  
  # --- Stratified estimator (weighted mean) ---
  weights <- strata_sizes |>
    mutate(W = N_h / sum(N_h)) |>
    select(!!strata_col, W)
  
  ybar <- samp |>
    group_by(.data[[strata_col]]) |>
    summarise(m = mean(.data[[y_col]]), .groups = "drop") |>
    left_join(weights, by = strata_col) |>
    summarise(ybar_st = sum(m * W)) |>
    pull(ybar_st)
  
  return(list(estimate = ybar))
}

#---------------------------------------------------------
# 3. Stratified Weighted Sampling (PPS within each stratum)
#---------------------------------------------------------
sample_stratified_weighted <- function(data, y_col, strata_col, weight_col,
                                       n, allocation = "prop") {
  # stratum sizes
  strata_sizes <- data |>
    count(.data[[strata_col]], name = "N_h")
  N <- sum(strata_sizes$N_h)
  W_h <- strata_sizes$N_h / N
  names(W_h) <- strata_sizes[[strata_col]]
  
  # allocation
  if (allocation == "prop") {
    n_h <- round(n * W_h)
  } else if (allocation == "neyman") {
    s_h <- data |>
      group_by(.data[[strata_col]]) |>
      summarise(s = sd(.data[[y_col]]), .groups = "drop") |>
      pull(s)
    n_h <- round(n * (strata_sizes$N_h * s_h) /
                   sum(strata_sizes$N_h * s_h))
  } else {
    stop("allocation must be 'prop' or 'neyman'")
  }
  names(n_h) <- strata_sizes[[strata_col]]
  
  # Ensure all strata have a non-zero sample size.
  # This prevents errors if a stratum is allocated 0 samples.
  n_h[n_h == 0] <- 1 
  
  # PPS within strata
  samp <- data |>
    group_by(.data[[strata_col]]) |>
    group_map(~ {
      stratum <- as.character(.y[[1]])
      ni <- n_h[stratum]
      if (is.na(ni) || ni == 0) return(.x[0, ]) # Empty sample
      
      # Ensure ni is not larger than the stratum size
      ni <- min(ni, nrow(.x))
      
      # Handle cases where the weight column has NA or 0 values
      weights <- .x[[weight_col]]
      if(any(is.na(weights) | weights <= 0)) {
        # If weights are problematic, fall back to SRS within the stratum
        warning("Problematic weights in stratum. Using SRS within stratum.")
        return(dplyr::slice_sample(.x, n = ni, replace = FALSE))
      }
      
      dplyr::slice_sample(.x, n = ni, weight_by = weights, replace = FALSE)
    }, .keep = TRUE) |>
    bind_rows()
  
  # inclusion probabilities & design weights
  samp <- samp |>
    group_by(.data[[strata_col]]) |>
    mutate(
      # Get stratum size (N_h) and stratum sample size (n_h)
      N_h_val = sum(.data[[weight_col]]), # Use this for total stratum weight
      n_h_val = n_h[as.character(.data[[strata_col]][1])],
      # Correct inclusion probability for PPS within a stratum
      pi = n_h_val * .data[[weight_col]] / N_h_val,
      d = 1 / pi
    ) |>
    ungroup()
  
  # Your Horvitz-Thompson estimator formula is then valid with the corrected pi
  ybar_ht <- sum(samp[[y_col]] * samp$d) / sum(samp$d)
  
  list(estimate = ybar_ht)
}

#---------------------------------------------------------
# 4. Test different sampling designs
#---------------------------------------------------------
# Corrected run_design function
run_design <- function(design, fun, data, y_col, strata_col = NULL, weight_col = NULL, allocation = NULL, n = 30:nrow(data), reps = 500) {
  
  # Capture all extra arguments to pass to the sampling functions
  args_list <- list(data = data, y_col = y_col)
  if (!is.null(strata_col)) args_list$strata_col <- strata_col
  if (!is.null(weight_col)) args_list$weight_col <- weight_col
  if (!is.null(allocation)) args_list$allocation <- allocation
  
  res <- purrr::map(n, function(i) {
    ests <- furrr::future_map(
      1:reps,
      ~ {
        # Update the n argument for the current iteration
        current_args <- c(args_list, list(n = i))
        
        # Execute the function with all collected arguments
        out <- rlang::exec(fun, !!!current_args)
        out$estimate
      }, .options = furrr::furrr_options(seed = TRUE)
    ) |> unlist(use.names = FALSE)
    
    tibble(
      design=design,
      n = i,
      mean = mean(ests, na.rm = TRUE), # Add na.rm to handle any NA from estimations
      se = sd(ests, na.rm = TRUE)
    )
  }) |> list_rbind()
  
  res
}

#---------------------------------------------------------
# 5. Apply to the flood example
#---------------------------------------------------------
library(tidyverse); library(sf)

# set working directory
setwd("~/Documents/GitHub/sampling_design_workshop/08_wrap_up/08_wrap_up_exercise")

#### Read the dataset ####
eas <- 
  sf::st_read("data/eas_strata.shp") |> 
  mutate(y = crops + built, h = Cluster, w = p__2025) |> 
  select(y, h, w, geometry) |> 
  mutate(w = ifelse(is.na(w), 1, w)) |> 
  sf::st_drop_geometry()


# simple test design
set.seed(1)
sample_srs(data=eas, y_col="y", n=10)
sample_srs(data=eas, y_col="y", n=50)

mean(eas$y)

sample_stratified_weighted(eas, y_col ="y",
                           strata_col = "h",
                           weight_col = "w",
                           n=5)

#### Develop the simulations
set.seed(123)  # master seed

future::plan(multisession, workers = 6)  # use 4 cores increase speed ~x4

res_srs <- run_design(
  design="SRS",
  fun = sample_srs,
  data = eas,
  y_col = "y",
  n = 30:nrow(eas),
  reps = 20
)

res_stratified_prop <- 
  run_design(
    design="Stratified sampling (proportional)",
    fun = sample_stratified,
    data = eas,
    y_col = "y",
    strata_col = "h",
    allocation = "prop",
    n = 30:nrow(eas),
    reps = 20
  )

res_stratified_neyman <- run_design(
  design="Stratified sampling (Neyman)",
  fun = sample_stratified,
  data = eas,
  y_col = "y", 
  strata_col = "h", 
  allocation = "neyman",
  n = 30:nrow(eas),
  reps = 20
)

res_stratified_prop_weighted <- run_design(
  design="Weighted stratified sampling (proportional)",
  fun = sample_stratified_weighted,
  data = eas,
  y_col = "y", 
  strata_col = "h", 
  weight_col = "w", 
  allocation = "prop",
  n = 30:nrow(eas),
  reps = 20
)

res_stratified_neyman_weighted <- run_design(
  design="Weighted stratified sampling (Neyman)",
  fun = sample_stratified_weighted,
  data = eas,
  y_col = "y", 
  strata_col = "h", 
  weight_col = "w", 
  allocation = "neyman",
  n = 30:nrow(eas),
  reps = 20
)

future::plan(sequential) # End the parallel session

res <- 
  res_srs |> 
  rbind(res_stratified_prop) |> 
  rbind(res_stratified_neyman) |> 
  rbind(res_stratified_prop_weighted) |> 
  rbind(res_stratified_neyman_weighted)

rm(res_srs, res_stratified_prop, res_stratified_neyman, 
   res_stratified_prop_weighted, res_stratified_neyman_weighted)

#---------------------------------------------------------
# 5. Select sampling design and sample size
#---------------------------------------------------------

# 1. Decide the criterion up front: e.g., target SE, target CV, or minimize MSE with a cost constraint.

# 2. Use the true population value if available to measure bias and MSE. 
# If not available, you can use the largest sample (or full data) as proxy.

# 3. Prefer designs with lower MSE, not only lower SE, especially if stratification 
# introduces bias (it usually doesn't for unbiased estimators but check).

# 4. Penalize complexity/cost: weighted/stratified designs 
# may need extra field effort; include that in the cost.

# 5. Check Monte Carlo stability: make sure reps is large enough so se 
# estimates are stable (you can compare reps=500 vs reps=2000 for a few n).

# 6. Watch strata with zero allocation: small n with proportional allocations 
# might yield n_h = 0 — handle those (either force a minimum n_h = 1 or drop strata when zero is acceptable).

# 7. Report precision and CI for your final chosen design and sample size.


# bias, mse, and relative se
mu <- mean(eas$y)   # population mean

res_eval <- 
  res |> 
  mutate(
    bias = mean - mu, # bias
    mse  = bias^2 + se^2, # MSE
    cv   = se / abs(mean)    # relative SE
  )

# mean with 95% Monte Carlo (empirical) intervals
res_eval |>
  mutate(ci_lo = mean - 1.96 * se, ci_hi = mean + 1.96 * se) |>
  ggplot(aes(x = n, y = mean, color = design)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = design), alpha = 0.15, color = NA) +
  geom_hline(yintercept = mu, linewidth=0.5, linetype = "dashed")+
  facet_wrap(~ design, scales = "free_y") +
  labs(y = "Estimator mean (±1.96*se)", 
       x = "Sample size n", 
       title = "Convergence of estimated mean to true population mean", 
       color="Design")

res_eval |> 
  ggplot(aes(x = n, y = bias, color = design)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ design, scales = "free_y") +
  labs(
    y = "Bias (mean - μ)",
    x = "Sample size n",
    title = "Bias of estimator relative to true population mean", 
    color="Design"
  )

res_eval |> 
  mutate(
    abs_error = abs(mean - mu)
  ) %>%
  ggplot(aes(x = n, y = abs_error, color = design)) +
  geom_line() +
  facet_wrap(~ design, scales = "free_y") +
  labs(
    y = "Absolute error |mean - μ|",
    x = "Sample size n",
    title = "Absolute error of estimator relative to true population mean"
  )

res |>  
  filter(design |> str_detect("Weighted", negate=T)) |> 
ggplot(aes(x = n, y = se, color = design)) +
  geom_line() +
  geom_point(size = 0.9) +
  labs(y = "Empirical SE", 
       x = "Sample size n", 
       title = "Empirical Standard Error",
       color="Design")
  
# smallest se for a given threshold
target_se <- 0.05   # set your acceptable standard error

best_by_se <- 
  res_eval |> 
  group_by(design) |>
  filter(se <= target_se) |>
  slice_min(n, with_ties = FALSE) |>
  ungroup()

best_by_se

# smallest n by relative CV threshold
target_cv <- 0.05  # 5% CV

best_by_cv <- 
  res_eval |>
  group_by(design) |>
  filter(cv <= target_cv) |>
  slice_min(n, with_ties = FALSE) |>
  ungroup()

best_by_cv

# smallest MSE
best_by_mse <- 
  res_eval |>
  group_by(design) |>
  slice_min(mse, with_ties = FALSE) |>
  ungroup()

best_by_mse
res_eval |> write_csv("eas_design_eval.csv")
