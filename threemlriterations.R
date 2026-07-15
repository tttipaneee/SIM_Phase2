# =========================================================================
# MULTIPLE LINEAR REGRESSION ITERATIONS
# =========================================================================

# libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

library(tidyverse)
library(car)

# 1. load data
if (!file.exists("used_cars_cleaned.csv")) {
  stop("missing dataset.")
}

raw_cars <- read_csv("used_cars_cleaned.csv")

# clean string fields
cars_cleaned <- raw_cars %>%
  mutate(
    price = as.numeric(gsub("[\\$,]", "", price)),
    milage = as.numeric(gsub("[\\, mi.]", "", milage))
  ) %>%
  drop_na(price, milage, fuel_type, accident)

# Apply IQR filtering to establish the base N = 3,427 dataset
q1_p <- quantile(cars_cleaned$price, 0.25)
q3_p <- quantile(cars_cleaned$price, 0.75)
iqr_p <- q3_p - q1_p

q1_m <- quantile(cars_cleaned$milage, 0.25)
q3_m <- quantile(cars_cleaned$milage, 0.75)
iqr_m <- q3_m - q1_m

base_dataset <- cars_cleaned %>%
  filter(
    price >= (q1_p - 1.5 * iqr_p) & price <= (q3_p + 1.5 * iqr_p),
    milage >= (q1_m - 1.5 * iqr_m) & milage <= (q3_m + 1.5 * iqr_m)
  )

# setting seed
set.seed(123)

# =========================================================================
# ITERATION 1: BASELINE MODEL
# =========================================================================
cat("\nRunning Iteration 1: Baseline Model...\n")

# raw data columns
it1_data <- base_dataset %>%
  mutate(
    brand = as.factor(brand),
    transmission = as.factor(transmission),
    fuel_type = as.factor(fuel_type),
    accident = as.factor(accident)
  )

train_idx1 <- sample(1:nrow(it1_data), size = 0.8 * nrow(it1_data))
train1     <- it1_data[train_idx1, ]
test1      <- it1_data[-train_idx1, ]

# remove rows from test set that contain factor levels not found in train set
test1 <- test1 %>%
  filter(
    brand %in% unique(train1$brand),
    transmission %in% unique(train1$transmission),
    fuel_type %in% unique(train1$fuel_type),
    accident %in% unique(train1$accident)
  )

# fit model one (baseline features)
model_it1 <- lm(price ~ milage + model_year + brand + transmission + fuel_type + accident, data = train1)

# evaluate Model 1
preds_it1 <- predict(model_it1, newdata = test1)
rmse_it1  <- sqrt(mean((test1$price - preds_it1)^2, na.rm = TRUE))
mae_it1   <- mean(abs(test1$price - preds_it1), na.rm = TRUE)
rsq_it1   <- cor(test1$price, preds_it1, use = "complete.obs")^2


# =========================================================================
# ITERATION 2: ADDING ENGINEERED FEATURES AND INTERACTION MODEL
# =========================================================================
cat("Running Iteration 2: Engineered & Interaction Model...\n")

# apply feature engineering to the base dataset
it2_data <- base_dataset %>%
  mutate(
    # extract metrics via regex
    hp = as.numeric(str_extract(engine, "[0-9.]+(?=\\s*HP)")),
    liters = as.numeric(str_extract(engine, "[0-9.]+(?=\\s*(L|Liter))")),
    
    # lumping to prevent over-parameterization
    brand = fct_lump_n(as.factor(brand), 15),
    transmission = fct_lump_n(as.factor(transmission), 5),
    fuel_type = as.factor(fuel_type),
    accident = as.factor(accident)
  ) %>%
  # impute missing regex yields with column medians
  mutate(
    hp = ifelse(is.na(hp), median(hp, na.rm = TRUE), hp),
    liters = ifelse(is.na(liters), median(liters, na.rm = TRUE), liters)
  )

train_idx2 <- sample(1:nrow(it2_data), size = 0.8 * nrow(it2_data))
train2     <- it2_data[train_idx2, ]
test2      <- it2_data[-train_idx2, ]

# remove rows from test set that contain factor levels not found in train set
test2 <- test2 %>%
  filter(
    brand %in% unique(train2$brand),
    transmission %in% unique(train2$transmission),
    fuel_type %in% unique(train2$fuel_type),
    accident %in% unique(train2$accident)
  )

# Fit Model 2 (Log price, engineered performance features, categorical interaction)
model_it2 <- lm(log(price) ~ milage + model_year + hp + liters + brand + transmission + fuel_type * accident, data = train2)

# Evaluate Model 2 (Back-transforming log predictions to raw dollars)
preds_log_it2 <- predict(model_it2, newdata = test2)
preds_it2     <- exp(preds_log_it2)

rmse_it2  <- sqrt(mean((test2$price - preds_it2)^2, na.rm = TRUE))
mae_it2   <- mean(abs(test2$price - preds_it2), na.rm = TRUE)
rsq_it2   <- cor(test2$price, preds_it2, use = "complete.obs")^2


# =========================================================================
# ITERATION 3: FINAL MODEL (ELBOW FILTER CAPPED <= $50,000)
# =========================================================================
cat("Running Iteration 3: Final Capped Model...\n")

# filter out high-end volatility using the diagnostic elbow limit
it3_data <- it2_data %>%
  filter(price <= 50000)

train_idx3 <- sample(1:nrow(it3_data), size = 0.8 * nrow(it3_data))
train3     <- it3_data[train_idx3, ]
test3      <- it3_data[-train_idx3, ]

# remove rows from test set that contain factor levels not found in train set
test3 <- test3 %>%
  filter(
    brand %in% unique(train3$brand),
    transmission %in% unique(train3$transmission),
    fuel_type %in% unique(train3$fuel_type),
    accident %in% unique(train3$accident)
  )

# Fit Model 3 (Same structure as Model 2, run on the stabilized consumer cohort)
model_it3 <- lm(log(price) ~ milage + model_year + hp + liters + brand + transmission + fuel_type * accident, data = train3)

# Evaluate Model 3 (Back-transforming log predictions to raw dollars)
preds_log_it3 <- predict(model_it3, newdata = test3)
preds_it3     <- exp(preds_log_it3)

rmse_it3  <- sqrt(mean((test3$price - preds_it3)^2, na.rm = TRUE))
mae_it3   <- mean(abs(test3$price - preds_it3), na.rm = TRUE)
rsq_it3   <- cor(test3$price, preds_it3, use = "complete.obs")^2


# =========================================================================
# PRINT FINAL COMPARATIVE PERFORMANCE SUMMARY
# =========================================================================
cat("\n=========================================================================\n")
cat("                  SUMMARY PERFORMANCE COMPARISON TABLE                   \n")
cat("=========================================================================\n")

summary_df <- data.frame(
  Metric = c("Target Variable (Y)", "Market Cap Layer", "Sample Size (N Total)", "Test Set RMSE ($)", "Test Set MAE ($)", "Test Set R-Squared"),
  Iteration_1 = c("Raw Price", "None (IQR Only)", nrow(it1_data), round(rmse_it1, 2), round(mae_it1, 2), round(rsq_it1, 4)),
  Iteration_2 = c("Log Price", "None (IQR Only)", nrow(it2_data), round(rmse_it2, 2), round(mae_it2, 2), round(rsq_it2, 4)),
  Iteration_3 = c("Log Price", "Capped <= $50k",  nrow(it3_data), round(rmse_it3, 2), round(mae_it3, 2), round(rsq_it3, 4))
)

print(summary_df, row.names = FALSE)
cat("=========================================================================\n")

# Print coefficients for Iteration 3 to verify placeholders
cat("\n>>> FINAL ITERATION 3 COEFFICIENTS SUMMARY:\n")
options(scipen = 999)
print(round(summary(model_it3)$coefficients, 5))