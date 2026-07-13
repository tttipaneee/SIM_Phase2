library(tidyverse)

# Load data
cars_data <- read_csv("used_cars.csv")

# Clean numerical features
cars_cleaned <- cars_data %>%
  mutate(
    # Remove '$' and ',' then convert to numeric
    price = as.numeric(str_replace_all(price, "[\\$,]", "")),
    # Remove ' mi.' and ',' then convert to numeric
    milage = as.numeric(str_replace_all(milage, "[ mi.,]", "")),
    # Convert appropriate character columns to factors
    brand = as.factor(brand),
    fuel_type = as.factor(fuel_type),
    accident = as.factor(accident),
    clean_title = as.factor(clean_title)
  )

# Preview summary
summary(cars_cleaned)