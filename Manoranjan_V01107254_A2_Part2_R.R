# Install necessary packages if not already installed
#install.packages(c("dplyr", "tidyverse", "readxl", "stringr", "car", "stringdist"))


# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Load required libraries
library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)
library(car)
library(stringdist)

# File paths
csv_file_path <- 'E:\\VCU Bridge course\\IPL_ball_by_ball_updated till 2024.csv'
excel_file_path <- 'E:\\VCU Bridge course\\IPL SALARIES 2024.xlsx'

# Load the CSV file
ipl_data <- read.csv(csv_file_path)

# Load the Excel file
salary_data <- read_excel(excel_file_path)

# Convert 'Season' column to string if it's not already
ipl_data$Season <- as.character(ipl_data$Season)

# Adjust the seasons to include only 2021, 2022, 2023, and 2024
seasons_to_include <- c('2021', '2022', '2023', '2024')
subset_ipl_data <- ipl_data %>% filter(Season %in% seasons_to_include)

# Rename columns to remove spaces and replace periods with underscores
names(subset_ipl_data) <- gsub(" ", "_", names(subset_ipl_data))
names(subset_ipl_data) <- gsub("\\.", "_", names(subset_ipl_data))

# Manually correct common mismatches
matched_names_cache <- list(
  'RG_Sharma' = 'Rohit Sharma',
  'CA_Lynn' = 'Chris Lynn',
  'SA_Yadav' = 'Suryakumar Yadav',
  'Ishan_Kishan' = 'Ishan Kishan',
  'HH_Pandya' = 'Hardik Pandya',
  'KA_Pollard' = 'Kieron Pollard',
  'KH_Pandya' = 'Krunal Pandya',
  'M_Jansen' = 'Marco Jansen',
  'JJ_Bumrah' = 'Jasprit Bumrah',
  'Washington_Sundar' = 'Washington Sundar',
  'V_Kohli' = 'Virat Kohli',
  'RM_Patidar' = 'Rajat Patidar',
  'GJ_Maxwell' = 'Glenn Maxwell',
  'AB_de_Villiers' = 'AB de Villiers',
  'Shahbaz_Ahmed' = 'Shahbaz Ahmed',
  'DT_Christian' = 'Dan Christian',
  'KA_Jamieson' = 'Kyle Jamieson',
  'HV_Patel' = 'Harshal Patel',
  'Mohammed_Siraj' = 'Mohammed Siraj',
  'RD_Gaikwad' = 'Ruturaj Gaikwad',
  'YS_Chahal' = 'Yuzvendra Chahal',
  'RD_Chahar' = 'Rahul Chahar',
  'CR_Woakes' = 'Chris Woakes',
  'Avesh_Khan' = 'Avesh Khan',
  'R_Ashwin' = 'Ravichandran Ashwin',
  'TK_Curran' = 'Tom Curran',
  'A_Mishra' = 'Amit Mishra',
  'MP_Stoinis' = 'Marcus Stoinis',
  'DL_Chahar' = 'Deepak Chahar',
  'SM_Curran' = 'Sam Curran',
  'F_du_Plessis' = 'Faf du Plessis'
)

match_player_name_cached <- function(short_name, full_names, threshold = 70) {
  if (short_name %in% names(matched_names_cache)) {
    return(matched_names_cache[[short_name]])
  }
  match <- stringdist::amatch(short_name, full_names, maxDist = threshold)
  matched_name <- ifelse(!is.na(match), full_names[match], NA)
  matched_names_cache[[short_name]] <- matched_name
  return(matched_name)
}

full_player_names <- salary_data$Player

# Match names for Striker, Bowler, and Non_Striker columns using dplyr
subset_ipl_data <- subset_ipl_data %>%
  mutate(Striker_Full = sapply(Striker, match_player_name_cached, full_names = full_player_names),
         Bowler_Full = sapply(Bowler, match_player_name_cached, full_names = full_player_names),
         Non_Striker_Full = sapply(Non_Striker, match_player_name_cached, full_names = full_player_names))

# Display a sample of the matched data
print("Sample of Data with Matched Full Names:")
print(head(subset_ipl_data %>% select(Striker, Striker_Full, Bowler, Bowler_Full, Non_Striker, Non_Striker_Full), 20))

# Merge based on matched names
merged_data <- subset_ipl_data %>%
  left_join(salary_data, by = c("Striker_Full" = "Player")) %>%
  left_join(salary_data, by = c("Bowler_Full" = "Player"), suffix = c("", "_Bowler")) %>%
  left_join(salary_data, by = c("Non_Striker_Full" = "Player"), suffix = c("", "_Non_Striker"))

# Display the merged data
print("Merged Data:")
print(head(merged_data))

# Updated convert_to_lakhs function
convert_to_lakhs <- function(salary) {
  if (is.na(salary)) return(NA)
  salary <- tolower(salary)
  if (str_detect(salary, "crore")) {
    salary <- gsub("[^0-9.]", "", salary)  # Remove non-numeric characters except the decimal point
    return(as.numeric(salary) * 100)
  } else if (str_detect(salary, "lakh")) {
    salary <- gsub("[^0-9.]", "", salary)  # Remove non-numeric characters except the decimal point
    return(as.numeric(salary))
  }
  return(NA)
}

# Apply conversion and identify problematic rows
salary_data <- salary_data %>%
  mutate(Salary_Lakhs = sapply(Salary, convert_to_lakhs))

# Identify and print problematic rows
problematic_rows <- salary_data %>% filter(is.na(Salary_Lakhs))
print(problematic_rows)

# Handle the NAs properly
salary_data <- salary_data %>%
  filter(!is.na(Salary_Lakhs))

# Calculate performance metrics
performance_metrics <- subset_ipl_data %>%
  group_by(Striker_Full) %>%
  summarise(Total_Runs = sum(runs_scored, na.rm = TRUE),
            Balls_Faced = n(),
            Wickets_Taken = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Performance_Index = Total_Runs + (Wickets_Taken * 25))

# Merge with salary data
merged_performance <- performance_metrics %>%
  left_join(salary_data, by = c("Striker_Full" = "Player"))

# Perform regression analysis
model <- lm(Salary_Lakhs ~ Performance_Index, data = merged_performance)

# Print the summary of the regression model
print(summary(model))

# Function to perform extended regression analysis
extended_regression_analysis <- function(X, y) {
  # Fit the original regression model
  model <- lm(y ~ X)
  print("Original Model Summary:")
  print(summary(model))
  
  # Calculate VIF for each independent variable if there are more than one predictors
  if (ncol(model.matrix(model)) > 2) {
    vif_data <- vif(model)
    print("\nVariance Inflation Factor (VIF):")
    print(vif_data)
  } else {
    print("\nVIF is not applicable for models with fewer than 2 terms.")
  }
  
  # Log-transform the dependent variable
  y_log <- log(y)
  
  # Fit the model again with log-transformed dependent variable
  model_log <- lm(y_log ~ X)
  print("\nLog-Transformed Model Summary:")
  print(summary(model_log))
}

# Perform extended regression analysis for batsmen
print("Batsmen Analysis")
batsmen_metrics <- performance_metrics %>%
  filter(!is.na(Performance_Index)) %>%
  left_join(salary_data, by = c("Striker_Full" = "Player"))

X_batsmen <- batsmen_metrics$Performance_Index
y_batsmen <- batsmen_metrics$Salary_Lakhs
extended_regression_analysis(X_batsmen, y_batsmen)

# Calculate performance metrics for bowlers
performance_metrics_bowlers <- subset_ipl_data %>%
  group_by(Bowler_Full) %>%
  summarise(Total_Runs = sum(runs_scored, na.rm = TRUE),
            Balls_Faced = n(),
            Wickets_Taken = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Performance_Index = Total_Runs + (Wickets_Taken * 25))

# Merge with salary data for bowlers
merged_performance_bowlers <- performance_metrics_bowlers %>%
  left_join(salary_data, by = c("Bowler_Full" = "Player"))

# Perform extended regression analysis for bowlers
print("\nBowlers Analysis")
X_bowlers <- merged_performance_bowlers$Performance_Index
y_bowlers <- merged_performance_bowlers$Salary_Lakhs
extended_regression_analysis(X_bowlers, y_bowlers)
