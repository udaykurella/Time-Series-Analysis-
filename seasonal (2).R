# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(tseries)
library(forecast)  # For STL decomposition and SARIMA modeling

# Import the data
data <- read.csv("/Users/admin/Desktop/admin/uday/seasonal.csv")

# Dataset Information
cat("Dataset Information:\n")
cat("Dimensions of the dataset (rows x columns): ", dim(data), "\n")
cat("Column Names: ", colnames(data), "\n")
cat("Data Types: \n")
print(sapply(data, class))

# View the first few rows of the dataset
head(data)

# Check the structure of the dataset to get an overview
str(data)

# Get summary statistics to understand the data distribution
summary(data)

# Check for missing values
colSums(is.na(data))

# Check for duplicates
sum(duplicated(data))

# Limit the dataset to the first 500 rows for testing purposes
data_sub <- data[1:500, ]

# Convert the DATE column to Date format
data_sub$DATE <- as.Date(as.character(data_sub$DATE), format = "%Y-%m-%d")

# Visualize data using a line plot (e.g., plotting 'IPN31152N' vs time)
ggplot(data_sub, aes(x = DATE, y = IPN31152N)) +  # Replace 'IPN31152N' with the column you want to plot
  geom_line() +
  labs(title = "Line Plot of IPN31152N", x = "Time", y = "IPN31152N") +
  theme_minimal()

# Identify seasonal patterns (e.g., yearly cycles)
# Extract year and month from the DATE column
data_sub$Year <- format(data_sub$DATE, "%Y")
data_sub$Month <- format(data_sub$DATE, "%m")

# Aggregate data by Year and Month to identify seasonal trends
seasonal_data <- data_sub %>%
  group_by(Year, Month) %>%
  summarise(Average_Value = mean(IPN31152N, na.rm = TRUE))  # Replace 'IPN31152N' with the column you're interested in

# Visualize seasonal patterns
ggplot(seasonal_data, aes(x = factor(Month), y = Average_Value, color = factor(Year))) +
  geom_boxplot() +
  labs(title = "Distribution of Average IPN31152N by Month", x = "Month", y = "Average IPN31152N") +
  theme_minimal()


# Handle missing values and outliers
# Handling missing values (imputation or removal)
data_cleaned <- data_sub %>%
  mutate(IPN31152N = ifelse(is.na(IPN31152N), mean(IPN31152N, na.rm = TRUE), IPN31152N))  # Replace missing values with the mean

# Handling outliers using the IQR method (Interquartile Range)
Q1 <- quantile(data_cleaned$IPN31152N, 0.25)
Q3 <- quantile(data_cleaned$IPN31152N, 0.75)
IQR <- Q3 - Q1
outlier_lower <- Q1 - 1.5 * IQR
outlier_upper <- Q3 + 1.5 * IQR

data_cleaned <- data_cleaned %>%
  filter(IPN31152N >= outlier_lower & IPN31152N <= outlier_upper)  # Remove outliers

# Check cleaned data
head(data_cleaned)

# Perform Augmented Dickey-Fuller (ADF) test on the original data
adf_test <- adf.test(data_cleaned$IPN31152N, alternative = "stationary")

# Print ADF test result
print(adf_test)

# Check if p-value is greater than 0.05 (indicating non-stationarity)
if(adf_test$p.value > 0.05) {
  print("Data is non-stationary. Applying seasonal differencing.")
  
  # Apply seasonal differencing (adjust the 'lag' to suit your seasonal cycle)
  IPN31152N_diff <- diff(data_cleaned$IPN31152N, lag = 12)
  
  # Adjust the length of the differenced data
  # Prepend NA values to the differenced series to match the original length
  IPN31152N_diff <- c(rep(NA, 12), IPN31152N_diff)
  
  # Add the differenced data as a new column in the dataset
  data_cleaned$IPN31152N_diff <- IPN31152N_diff
  
  # Remove NAs before performing the ADF test on differenced data
  IPN31152N_diff_clean <- na.omit(data_cleaned$IPN31152N_diff)
  
  # Perform the ADF test again on the differenced (cleaned) data
  adf_test_diff <- adf.test(IPN31152N_diff_clean, alternative = "stationary")
  print(adf_test_diff)
  
  # Check if differenced data is stationary
  if(adf_test_diff$p.value <= 0.05) {
    print("Differenced data is stationary.")
  } else {
    print("Differenced data is still non-stationary.")
  }
} else {
  print("Data is stationary.")
}

# Convert the time series to a format that STL can work with
# 'IPN31152N' is the column of interest and 'DATE' in Date format

ts_data <- ts(data_cleaned$IPN31152N, frequency = 12)  # Adjust frequency to match the seasonal cycle

# Perform STL decomposition
stl_decomp <- stl(ts_data, s.window = "periodic")

# Plot the decomposition
plot(stl_decomp)

# Extract and view the decomposed components (trend, seasonal, residual)
trend_component <- stl_decomp$time.series[, "trend"]
seasonal_component <- stl_decomp$time.series[, "seasonal"]
residual_component <- stl_decomp$time.series[, "remainder"]

# Print the first few values of each component
head(trend_component)
head(seasonal_component)
head(residual_component)

# Fit a SARIMA model
# Plot ACF and PACF to help identify parameters for the SARIMA model
acf(ts_data)  # ACF plot to identify q and Q
pacf(ts_data)  # PACF plot to identify p and P

# Fit the SARIMA model (Example: SARIMA(1, 1, 1)(1, 1, 1)[12])
sarima_model <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, 
                           max.p = 3, max.q = 3, max.P = 1, max.Q = 1)

# Print the SARIMA model summary
summary(sarima_model)

# Forecast using the fitted SARIMA model
forecasted_values <- forecast(sarima_model, h = 12)  # Forecasting the next 12 data points (adjust based on your needs)

# Plot the forecast
plot(forecasted_values)

# Train the SARIMA model with the specified parameters
sarima_model_fitted <- Arima(ts_data, order = c(1, 1, 1), seasonal = c(1, 1, 1), 
                             xreg = NULL)  # xreg is NULL if no external regressors are used

# Check the model summary
summary(sarima_model_fitted)

# Validate using residual diagnostics
# Plot the residuals
residuals <- residuals(sarima_model_fitted)
par(mfrow = c(1, 2))  # Plot residuals on two panels
plot(residuals, main = "Residuals of SARIMA Model", ylab = "Residuals", type = "l")
acf(residuals, main = "ACF of Residuals")

# Perform Ljung-Box test to check if residuals are white noise (randomness)
ljung_box_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
print(ljung_box_test)

# If p-value is greater than 0.05, residuals are considered to be random (no autocorrelation).
if(ljung_box_test$p.value > 0.05) {
  print("Residuals are random, model is well fitted.")
} else {
  print("Residuals are not random, consider re-evaluating the model.")
}

# Plot the residuals again for visual confirmation
par(mfrow = c(1, 1))
plot(residuals, main = "Residuals of SARIMA Model")

# Forecast future values using the fitted SARIMA model
forecasted_values <- forecast(sarima_model_fitted, h = 12)  # Adjust 'h' as needed

# Plot the forecasted values
plot(forecasted_values, main = "Forecasted Values Using SARIMA Model", xlab = "Time", ylab = "IPN31152N")

# Print the forecasted values
print(forecasted_values)

# Evaluate performance using metrics like MAE, RMSE, AIC, or BIC.
actual_values <- tail(data_cleaned$IPN31152N, 12)  # Adjust as per your test set
predicted_values <- forecasted_values$mean  # Extract the forecasted values

# MAE (Mean Absolute Error)
mae <- mean(abs(actual_values - predicted_values))
cat("MAE (Mean Absolute Error):", mae, "\n")

# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((actual_values - predicted_values)^2))
cat("RMSE (Root Mean Squared Error):", rmse, "\n")

# AIC (Akaike Information Criterion)
cat("AIC:", AIC(sarima_model_fitted), "\n")

# BIC (Bayesian Information Criterion)
cat("BIC:", BIC(sarima_model_fitted), "\n")

# Evaluate the forecast accuracy
accuracy_metrics <- accuracy(forecasted_values, actual_values)
print(accuracy_metrics)
