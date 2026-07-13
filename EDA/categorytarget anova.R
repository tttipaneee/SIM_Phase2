# Price by Fuel Type
ggplot(cars_cleaned, aes(x = fuel_type, y = price)) +
  geom_boxplot() +
  coord_flip() # Helpful if names overlap