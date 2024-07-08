# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))


install.packages("conjoint")
install.packages("reshape2")

library(conjoint)
library(reshape2)
library(dplyr)

# Load the data
pizza_data <- read.csv("E:/VCU Bridge course/pizza_data.csv")

# Explore the data
head(pizza_data)
str(pizza_data)
summary(pizza_data)

# Convert categorical variables to factors
pizza_data$brand <- as.factor(pizza_data$brand)
pizza_data$price <- as.factor(pizza_data$price)
pizza_data$weight <- as.factor(pizza_data$weight)
pizza_data$crust <- as.factor(pizza_data$crust)
pizza_data$cheese <- as.factor(pizza_data$cheese)
pizza_data$size <- as.factor(pizza_data$size)
pizza_data$toppings <- as.factor(pizza_data$toppings)
pizza_data$spicy <- as.factor(pizza_data$spicy)

# Define attributes and levels based on your data
attributes <- list(
  brand = levels(pizza_data$brand),
  price = levels(pizza_data$price),
  weight = levels(pizza_data$weight),
  crust = levels(pizza_data$crust),
  cheese = levels(pizza_data$cheese),
  size = levels(pizza_data$size),
  toppings = levels(pizza_data$toppings),
  spicy = levels(pizza_data$spicy)
)

# Create the design matrix
design <- expand.grid(attributes)

# Assuming 'ranking' column represents respondent evaluations
ratings <- pizza_data$ranking

# Combine the design matrix with ratings
conjoint_data <- cbind(design, ratings)

# Convert ratings to numeric if needed
conjoint_data$ratings <- as.numeric(conjoint_data$ratings)

# Run the conjoint analysis using linear model
ca_model <- lm(ratings ~ brand + price + weight + crust + cheese + size + toppings + spicy, data = conjoint_data)

# Print the results
summary(ca_model)


# Print part-worth utilities
utilities <- summary(ca_model)$coefficients
print(utilities)

# Plot the part-worth utilities
plot(utilities)
