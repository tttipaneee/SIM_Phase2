# ----------------------------
# Multiple Linear Regression
# ----------------------------
library(tidyverse)
library(forcats)
library(stringr)

# Load Data
if (file.exists("used_cars_cleaned.csv")) {
  cars_df <- read_csv("used_cars_cleaned.csv")
} else {
  stop("no dataset in directory")
}

# string extraction
cars_model_data <- cars_df %>%
  drop_na(fuel_type, accident) %>%
  mutate(
    # Extract numbers right before "HP" or " HP"
    hp = as.numeric(str_extract(engine, "[0-9.]+(?=\\s*HP)")),
    # Extract numbers right before "L" or "Liter"
    liters = as.numeric(str_extract(engine, "[0-9.]+(?=\\s*(L|Liter))")),
    
    # Convert standard categories to factors
    brand = fct_lump_n(as.factor(brand), 15),
    fuel_type = as.factor(fuel_type),
    accident = as.factor(accident),
    transmission = fct_lump_n(as.factor(transmission), 5)
  )

# 3. median imputation
# for engines not displaying displacement (electric motors etc.)
cars_model_data <- cars_model_data %>%
  mutate(
    hp = ifelse(is.na(hp), median(hp, na.rm = TRUE), hp),
    liters = ifelse(is.na(liters), median(liters, na.rm = TRUE), liters)
  ) %>%
  # drop unused original columns
  select(-model, -engine, -ext_col, -int_col)

# split into 80/20 split
set.seed(123)
train_indices <- sample(1:nrow(cars_model_data), size = 0.8 * nrow(cars_model_data))
train_data    <- cars_model_data[train_indices, ]
test_data     <- cars_model_data[-train_indices, ]

# fit model
car_model_upgraded <- lm(log(price) ~ ., data = train_data)

# get predictions
log_preds <- predict(car_model_upgraded, newdata = test_data)
predictions <- exp(log_preds)

# load dataframe
results <- data.frame(
  actual = test_data$price,
  predicted = predictions
)

# get metrics
rmse_val <- sqrt(mean((results$actual - results$predicted)^2, na.rm = TRUE))
mae_val  <- mean(abs(results$actual - results$predicted), na.rm = TRUE)
rsq_val  <- cor(results$actual, results$predicted, use = "complete.obs")^2

# print results
cat("\n--- Test Set Performance Metrics ---\n")
cat("RMSE (Root Mean Squared Error): $", round(rmse_val, 2), "\n")
cat("MAE (Mean Absolute Error):     $", round(mae_val, 2), "\n")
cat("R-squared (R²):                 ", round(rsq_val, 4), "\n")

# diagnostics
par(mfrow = c(2, 2))
plot(car_model_upgraded)
par(mfrow = c(1, 1))
