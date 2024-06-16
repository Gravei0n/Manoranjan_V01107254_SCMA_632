# Step 1: Set the working directory and verify it
setwd('E:\\VCU Bridge course\\R-WD')
print(getwd())

# Step 2: Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Step 3: Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Step 4: Reading the file into R
data <- read.csv("E:/VCU Bridge course/R-WD/Assignments/A1a/NSSO68.csv")

# Step 5: Filtering for UP
df <- data %>%
  filter(state_1 == "UP")

# Display dataset info
cat("Dataset Information After Filtering for UP:\n")
print(names(df))
print(head(df))
print(dim(df))

# Step 7: Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information After Filtering for UP:\n")
print(missing_info)

# Step 8: Subsetting the data
upnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day, ricetotal_q, wheattotal_q, sugartotal_q)

# Check the subset data
cat("Subset Data Before Imputation:\n")
print(head(upnew))

# Step 9: Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

# Apply the imputation function to all relevant columns
upnew <- upnew %>%
  mutate(across(c(Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day, ricetotal_q, wheattotal_q, sugartotal_q), impute_with_mean))

# Check the data after imputation
cat("Data After Imputation:\n")
print(head(upnew))

# Step 10: Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricetotal_q", "wheattotal_q", "sugartotal_q")
for (col in outlier_columns) {
  upnew <- remove_outliers(upnew, col)
}

# Check the data after outlier removal
cat("Data After Outlier Removal:\n")
print(head(upnew))

# Step 11: Summarize consumption
upnew$total_consumption <- rowSums(upnew[, c("ricetotal_q", "wheattotal_q", "sugartotal_q")], na.rm = TRUE)

# Check the data after adding total_consumption
cat("Data After Adding Total Consumption:\n")
print(head(upnew))

# Step 12: Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- upnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption, na.rm = TRUE)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Step 13: Rename districts and sectors
district_mapping <- c("04" = "Moradabad", "67" = "Gorakhpur", "58" = "Allahabad", "45" = "Kanpur")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

upnew$District <- as.character(upnew$District)
upnew$Sector <- as.character(upnew$Sector)
upnew$District <- ifelse(upnew$District %in% names(district_mapping), district_mapping[upnew$District], upnew$District)
upnew$Sector <- ifelse(upnew$Sector %in% names(sector_mapping), sector_mapping[upnew$Sector], upnew$Sector)

# Step 14: Test for differences in mean consumption between urban and rural
rural <- upnew %>%
  filter(Sector == "RURAL") %>%
  select(total_consumption) %>%
  unlist()

urban <- upnew %>%
  filter(Sector == "URBAN") %>%
  select(total_consumption) %>%
  unlist()

z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)

if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a difference between mean consumptions of urban and rural.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of urban and rural.\n")
}
