# Price vs Mileage
ggplot(cars_cleaned, aes(x = milage, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") + 
  geom_smooth(method = "loess", color = "blue") 