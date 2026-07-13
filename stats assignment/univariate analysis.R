ggplot(cars_cleaned, aes(x = price)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Car Prices")

# Check if log transformation normalizes it
ggplot(cars_cleaned, aes(x = log(price))) +
  geom_histogram(fill = "salmon", color = "black", bins = 30) +
  labs(title = "Distribution of Log-Price")