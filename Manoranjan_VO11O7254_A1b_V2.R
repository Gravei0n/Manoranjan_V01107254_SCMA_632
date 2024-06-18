# Install necessary packages if not already installed
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
install.packages('stringdist')

# Load necessary libraries
library(dplyr)
library(lubridate)
library(readxl)
library(stringr)
library(stringdist)

# Set file path
file_path <- "E:/VCU Bridge course/IPL_ball_by_ball_updated till 2024.csv"
salary_file_path <- "E:/VCU Bridge course/IPL SALARIES 2024.xlsx"

# Load data
ipl_bbb <- read.csv(file_path, stringsAsFactors = FALSE)
ipl_salary <- read_excel(salary_file_path)

# Process year information
ipl_bbb$year <- year(dmy(ipl_bbb$Date))

# Calculate total runs scored and wickets taken by each player in 2024
total_runs <- ipl_bbb %>%
  group_by(year, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE))

total_wickets <- ipl_bbb %>%
  group_by(year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE))

# Filter for the year 2024
R2024 <- total_runs %>% filter(year == 2024)
W2024 <- total_wickets %>% filter(year == 2024)

# Merge runs and wickets into a single dataframe
performance_2024 <- merge(R2024, W2024, by.x = 'Striker', by.y = 'Bowler', all = TRUE)
performance_2024[is.na(performance_2024)] <- 0  # Fill NA values with 0

# Sum runs and wickets for total performance
performance_2024$total_performance <- performance_2024$runs_scored + performance_2024$wicket_confirmation

# Function to match names using stringdist package
match_names <- function(name, names_list) {
  distances <- stringdist::stringdist(name, names_list, method = 'jw')
  match <- which.min(distances)
  if (distances[match] < 0.2) {
    return(names_list[match])
  } else {
    return(NA)
  }
}

# Create a new column in df_salary with matched names from performance_2024
df_salary <- ipl_salary
df_salary$Matched_Player <- sapply(df_salary$Player, match_names, names_list = performance_2024$Striker)

# Merge the DataFrames on the matched names
df_merged <- merge(df_salary, performance_2024, by.x = 'Matched_Player', by.y = 'Striker')

# Calculate the correlation
correlation <- cor(df_merged$Rs, df_merged$total_performance, use = "complete.obs")

# Print correlation
cat("Correlation between Salary and Total Performance:", correlation, "\n")

# Specific analysis for Surya Kumar Yadav
player_name_in_bbb <- "SA Yadav"
player_name_in_salary <- "Surya Kumar Yadav"

# Filter for the specific player
player_performance <- performance_2024 %>% filter(Striker == player_name_in_bbb) %>% pull(total_performance)
player_salary <- df_salary %>% filter(Player == player_name_in_salary) %>% pull(Rs)

cat("Total Performance (Runs + Wickets) of", player_name_in_salary, "in 2024:", player_performance, "\n")
cat("Salary of", player_name_in_salary, "in 2024:", player_salary, "\n")
