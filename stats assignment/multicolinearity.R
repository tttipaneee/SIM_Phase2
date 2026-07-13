library(GGally)

# Correlation matrix for continuous variables
cars_cleaned %>%
  select(price, milage, model_year) %>%
  ggcorr(label = TRUE, label_round = 2)