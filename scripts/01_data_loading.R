install.packages("dplyr")
install.packages("ggplot2")

# 2. Load the packages into your current session
library(dplyr)
library(ggplot2)

# 3. Read the data
# We use "data/filename.csv" because your R Project automatically knows 
# where its main folder is. This is called a "relative path".
dataset <- read.csv("data/used_cars.csv")

# 4. Check the data
# This will print the first 6 rows of your dataset in the Console below.
head(dataset)