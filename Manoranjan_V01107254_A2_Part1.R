getwd()
setwd('E:/VCU Bridge course')

# Load necessary libraries
library(dplyr)
library(readr)
library(stats)

# Load the dataset
csv_file_path <- "E:/VCU Bridge course/NSSO68.csv"
data <- read_csv(csv_file_path)

# Display the first few rows of the dataset
head(data)

# Define the columns to be loaded
selected_columns <- c('state_1', 'foodtotal_v', 'foodtotal_q', 'MPCE_URP', 'MPCE_MRP')

# Select only the required columns
data <- data %>% select(all_of(selected_columns))

# Display the first few rows of the selected columns
head(data)

# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# Check data types
str(data)

# Fit the multiple regression model
model <- lm(foodtotal_v ~ foodtotal_q + MPCE_URP + MPCE_MRP, data = data)

# Print the summary of the regression
summary(model)

library(car)

# Calculate VIF for each independent variable
vif(model)

# Log-transform the dependent variable
data <- data %>% mutate(log_foodtotal_v = log(foodtotal_v + 1))  # Adding 1 to avoid log(0)

# Fit the model again with log-transformed dependent variable
model_log <- lm(log_foodtotal_v ~ foodtotal_q + MPCE_URP + MPCE_MRP, data = data)

# Print the summary of the new regression
summary(model_log)

# Extract the coefficients
coefficients <- coef(model)
intercept <- coefficients[1]
coef_foodtotal_q <- coefficients[2]
coef_MPCE_URP <- coefficients[3]
coef_MPCE_MRP <- coefficients[4]

# Create the regression equation
regression_equation <- paste("foodtotal_v = ", round(intercept, 4), " + ", round(coef_foodtotal_q, 4), " * foodtotal_q + ", round(coef_MPCE_URP, 4), " * MPCE_URP + ", round(coef_MPCE_MRP, 4), " * MPCE_MRP")
print(regression_equation)

# Calculate the fitted values
fitted_values <- predict(model, data)

# Calculate the error term (residuals)
error_term <- data$foodtotal_v - fitted_values

# Display the first few error terms
head(error_term)

