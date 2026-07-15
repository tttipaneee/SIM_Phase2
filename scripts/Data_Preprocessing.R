install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

dataset <- read.csv("data/used_cars.csv")

# 5. Clean string characters to make variables numeric
dataset <- dataset %>%
  mutate(
    price = as.numeric(gsub("[$,]", "", price)),
    milage = as.numeric(gsub("[,a-zA-Z ]", "", milage))
  )

# 6. Check the structure again to confirm they are now 'numeric'
str(dataset)

# 7. Count missing values in every column
colSums(is.na(dataset))
# 8. Generate summary statistics for the numerical columns
summary(dataset)

# 9. Handle missing values
# Drop the clean_title column and remove any remaining rows with NA
dataset_clean <- dataset %>%
  select(-clean_title) %>%
  na.omit()

# Check to ensure all NAs are gone
colSums(is.na(dataset_clean))

# 10. Visualize outliers before treatment
ggplot(dataset_clean, aes(y = price)) + 
  geom_boxplot(fill = "skyblue") +
  ggtitle("Boxplot of Used Car Prices (Before Treatment)") +
  theme_minimal()

# 11. Treat outliers for 'price' using the IQR method
Q1_price <- quantile(dataset_clean$price, 0.25)
Q3_price <- quantile(dataset_clean$price, 0.75)
IQR_price <- Q3_price - Q1_price
upper_bound_price <- Q3_price + 1.5 * IQR_price

# Treat outliers for 'milage' using the IQR method
Q1_milage <- quantile(dataset_clean$milage, 0.25)
Q3_milage <- quantile(dataset_clean$milage, 0.75)
IQR_milage <- Q3_milage - Q1_milage
upper_bound_milage <- Q3_milage + 1.5 * IQR_milage

# Filter the dataset to remove extreme values
dataset_final <- dataset_clean %>%
  filter(price <= upper_bound_price & milage <= upper_bound_milage)

# 11b. Visualize the distribution for after 
ggplot(dataset_final, aes(y = price)) + 
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Boxplot of Used Car Prices (After IQR Treatment)") +
  theme_minimal()

# 12. Install and load the moments package for skewness
install.packages("moments")
library(moments)

# 13. Calculate SD and Skewness 
sd(dataset_final$price)
skewness(dataset_final$price)

sd(dataset_final$milage)
skewness(dataset_final$milage)

# 14. Generate a Correlation Matrix for numerical variables
numeric_vars <- dataset_final %>% select(price, milage, model_year)
cor(numeric_vars)

# Save the cleaned dataset to the data folder
write.csv(dataset_final, "data/used_cars_cleaned.csv", row.names = FALSE)

# 15. Install and load the corrplot package
install.packages("corrplot")
library(corrplot)

# 16. Create the correlation matrix object
cor_matrix <- cor(numeric_vars)

# 17. Generate the Correlation Heatmap
corrplot(cor_matrix, 
         method = "color",          # Fills the squares with color
         addCoef.col = "black",     # Prints the r-values inside the squares
         tl.col = "black",          # Makes text labels black
         tl.srt = 45,               # Tilts the text labels for readability
         title = "Correlation Heatmap of Numerical Variables",
         mar = c(0,0,1,0))          # Fixes title margin spacing

