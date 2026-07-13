# Check mileage distribution
ggplot(cars_cleaned, aes(x = milage)) +
  geom_histogram(bins = 30)

# Check car age instead of just model_year
cars_cleaned <- cars_cleaned %>% 
  mutate(car_age = 2026 - model_year) # Assuming current year context