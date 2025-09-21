##########################################
# Workshop: Advanced Sampling Methodologies
# Instructor: Dr. Gianluca Boo, WorldPop
# Date: 09-23-2025
# Topic: AI and Machine Learning for Sampling
##########################################

# ================================
# 1. Setup
# ================================

# Load required libraries
library(tidyverse)
library(cluster)        # k-means
library(tidymodels)     # ML workflow
library(vip)            # variable importance

# Set seed
set.seed(123)

# ================================
# 2. K-means Clustering
# ================================

# Exercise 1: Explore the Iris dataset
head(iris, 2)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Iris dataset by Species")

# Exercise 2: Run k-means with k = 3
data <- iris |> select(-Species)
km_fit <- kmeans(scale(data), centers = 3)

iris_km <- iris |>
  mutate(Cluster = factor(km_fit$cluster))

ggplot(iris_km, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "K-means clustering (k=3)")

# Exercise 3: Elbow method to choose K
wss <- map_dbl(1:9, function(k) {
  kmeans(scale(data), centers = k)$tot.withinss
})

tibble(k = 1:9, wss) |>
  ggplot(aes(x = k, y = wss)) +
  geom_line() + geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Elbow Method", y = "Within-cluster sum of squares")

# ================================
# 3. Random Forest
# ================================

# Exercise 4: Train/test split
set.seed(123)
split <- initial_split(iris |> select(-Species), prop = 0.8)
train <- training(split)
test  <- testing(split)

# Exercise 5: Random forest model (predict Petal.Length)
rf_recipe <- recipe(Petal.Length ~ ., data = train)

rf_model <- rand_forest(trees = 500) |>
  set_engine("ranger", importance = "permutation") |>
  set_mode("regression")

rf_wf <- workflow() |> add_recipe(rf_recipe) |> add_model(rf_model)
rf_fit <- rf_wf |> fit(data = train)

# Exercise 6: Predictions and evaluation
rf_preds <- predict(rf_fit, test) |> bind_cols(test)
rf_preds |> metrics(truth = Petal.Length, estimate = .pred)

ggplot(rf_preds, aes(x = Petal.Length, y = .pred)) +
  geom_point(size = 3) +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(title = "Observed vs Predicted", x = "Observed", y = "Predicted")

# Exercise 7: Variable importance
rf_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 3)

# ================================
# 4. Tidymodels Workflow
# ================================

# Exercise 8: Linear regression example
lm_rec <- recipe(Sepal.Length ~ ., data = train)
lm_spec <- linear_reg() |> set_engine("lm")

lm_wf <- workflow() |> add_recipe(lm_rec) |> add_model(lm_spec)
lm_fit <- lm_wf |> fit(data = train)
lm_fit

# Exercise 9: Cross-validation
set.seed(123)
cv_folds <- vfold_cv(iris, v = 5)
cv_folds

# Exercise 10: Hyperparameter tuning
rf_tune <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("regression")

rf_grid <- grid_regular(mtry(range = c(1,4)), min_n(), levels = 3)

rf_tuned <- tune_grid(
  workflow() |> add_recipe(rf_recipe) |> add_model(rf_tune),
  resamples = cv_folds,
  grid = rf_grid
)

rf_tuned

# ================================
# 5. Application to Sampling
# ================================

# Exercise 11: Use K-means for strata creation
# (Pretend Sepal features are population covariates)
iris_strata <- iris_km |> count(Cluster)
iris_strata

# Exercise 12: Use Random Forest predictions for sample weighting
# (Predicted Petal.Length as proxy for inclusion probability)
weights <- rf_preds |> mutate(weight = .pred / sum(.pred))
weights
