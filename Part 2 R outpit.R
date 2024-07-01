# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))


# Load necessary libraries
library(readr)
library(dplyr)

# Load the data
data <- read_csv("E:\\VCU Bridge course\\NSSO68.csv")

# View the first few rows and columns
head(data)
colnames(data)

# Create a binary indicator for non-vegetarian status
data$non_veg <- ifelse(data$nonvegtotal_q > 0 | data$eggsno_q > 0 | data$fishprawn_q > 0 | data$goatmeat_q > 0 | data$beef_q > 0 | data$pork_q > 0 | data$chicken_q > 0 | data$othrbirds_q > 0, 1, 0)

# Convert non_veg column to a factor
data$non_veg <- as.factor(data$non_veg)

# Ensure the columns 'Age', 'MPCE_URP', and 'Education' are present
# Check column names
colnames(data)

# Fit a probit regression model using these columns
# Replace 'Age', 'MPCE_URP', and 'Education' with the actual column names from your dataset if they differ
probit_model <- glm(non_veg ~ Age + MPCE_URP + Education, family = binomial(link = "probit"), data = data)

# Summary of the model
summary(probit_model)
