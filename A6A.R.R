# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Install required packages if not already installed
required_packages <- c("quantmod", "xlsx", "forecast", "caret", "randomForest", "rpart", "Metrics")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load required libraries
library(quantmod)
library(xlsx)
library(forecast)
library(caret)
library(randomForest)
library(rpart)
library(Metrics)

# Define ticker and date range
ticker <- "TATASTEEL.NS"
start_date <- "1999-01-01"
end_date <- "2024-03-31"

# Download stock data
getSymbols(Symbols = ticker, src = "yahoo", from = start_date, to = end_date)
data <- get(ticker)

# Save data to Excel
write.xlsx(data, file = "E:/VCU Bridge course/tata_steel.xlsx", row.names = TRUE)

# Prepare data
df <- data[, "TATASTEEL.NS.Adjusted", drop = FALSE]
colnames(df) <- c("Adj_Close")

# Check for and handle missing values
sum(is.na(df))
df <- na.approx(df) # Interpolate missing values

# Convert to monthly data
df_monthly <- to.monthly(df, indexAt = "lastof", OHLC = FALSE)

# Plot data
plot(df_monthly, main = "TATASTEEL.NS Adj Close Price", ylab = "Adj Close Price", xlab = "Date", col = "blue")

# Check the range of the training data
print(head(df_monthly))
print(tail(df_monthly))

# Time series decomposition
decomposed <- decompose(ts(df_monthly$Adj_Close, frequency = 12), type = "multiplicative")
plot(decomposed)

# Train-test split
set.seed(123)
n <- nrow(df_monthly)
train_data <- df_monthly[1:floor(0.8 * n), ]
test_data <- df_monthly[(floor(0.8 * n) + 1):n, ]

# Holt-Winters Forecasting using ets from forecast package
hw_model <- ets(as.numeric(train_data$Adj_Close))
hw_forecast <- forecast(hw_model, h = nrow(test_data))
plot(hw_forecast)

# Model evaluation for Holt-Winters
y_pred_hw <- as.numeric(hw_forecast$mean)
y_true_hw <- as.numeric(test_data$Adj_Close)
print(paste("Holt-Winters RMSE:", rmse(y_true_hw, y_pred_hw)))
print(paste("Holt-Winters MAE:", mae(y_true_hw, y_pred_hw)))
print(paste("Holt-Winters MAPE:", mape(y_true_hw, y_pred_hw)))

# ARIMA Forecasting
auto_arima_model <- auto.arima(as.numeric(train_data$Adj_Close), seasonal = TRUE)
arima_forecast <- forecast(auto_arima_model, h = nrow(test_data))
plot(arima_forecast)

# Model evaluation for ARIMA
y_pred_arima <- as.numeric(arima_forecast$mean)
print(paste("ARIMA RMSE:", rmse(y_true_hw, y_pred_arima)))
print(paste("ARIMA MAE:", mae(y_true_hw, y_pred_arima)))
print(paste("ARIMA MAPE:", mape(y_true_hw, y_pred_arima)))

# Create lagged features for Random Forest and Decision Tree
create_lagged_features <- function(data, lags = 1) {
  lagged_data <- data.frame(data)
  for (i in 1:lags) {
    lagged_data <- cbind(lagged_data, lag(data, k = i))
    colnames(lagged_data)[ncol(lagged_data)] <- paste0("Lag_", i)
  }
  return(na.omit(lagged_data))
}

train_data_rf <- create_lagged_features(train_data, lags = 1)
test_data_rf <- create_lagged_features(test_data, lags = 1)

# Random Forest model
rf_model <- randomForest(Adj_Close ~ ., data = train_data_rf)
rf_pred <- predict(rf_model, newdata = test_data_rf)
print(paste("Random Forest RMSE:", rmse(test_data_rf$Adj_Close, rf_pred)))
print(paste("Random Forest MAE:", mae(test_data_rf$Adj_Close, rf_pred)))
print(paste("Random Forest MAPE:", mape(test_data_rf$Adj_Close, rf_pred)))

# Decision Tree model
dt_model <- rpart(Adj_Close ~ ., data = train_data_rf)
dt_pred <- predict(dt_model, newdata = test_data_rf)
print(paste("Decision Tree RMSE:", rmse(test_data_rf$Adj_Close, dt_pred)))
print(paste("Decision Tree MAE:", mae(test_data_rf$Adj_Close, dt_pred)))
print(paste("Decision Tree MAPE:", mape(test_data_rf$Adj_Close, dt_pred)))
