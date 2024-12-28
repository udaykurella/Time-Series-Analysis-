# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(tseries) # For Augmented Dickey-Fuller test
library(forecast) # For ARIMA model and diagnostics

# Import Data
data_path <- "/Users/admin/Desktop/admin/afras/project/non_seasonal.csv"
data <- read.csv(data_path)

# Dataset Information
cat("Dataset Info:\n")
cat("Data_set volume", dim(data), "\n")
cat("Column Names: ", colnames(data), "\n")
cat("Data Types: \n")
print(sapply(data, class))
summary(data)

# fierst 5 rows of the dataset
head(data)

# Convert 'Date' to Date format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Step 2: Visualize Data
numeric_cols <- sapply(data, is.numeric)

# Line plot for 'Close' prices against Date
if ("Close" %in% colnames(data)) {
  ggplot(data, aes(x = Date, y = Close)) +
    geom_line(color = "blue") +
    labs(title = "Time Series of Close Prices", x = "Date", y = "Close")
}

# Log-transformed histogram for 'Close' column 
log_histogram <- ggplot(data, aes(x = log(Close + 1))) +
  geom_histogram(fill = "purple", bins = 30, alpha = 0.7) +
  labs(title = "Histogram of Log-Transformed Close Prices", x = "Log(Close + 1)", y = "Frequency")

# Show all plots
gridExtra::grid.arrange(corr_plot, log_histogram, ncol = 2)

# Step 3: Check for Missing Values
missing_values <- colSums(is.na(data))
cat("Missing values per column:\n")
print(missing_values)

# Handle missing values (imputation)
data_imputed <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Step 4: Check for Outliers
boxplots <- lapply(names(data)[numeric_cols], function(col) {
  ggplot(data, aes_string(y = col)) +
    geom_boxplot(fill = "orange", alpha = 0.7) +
    labs(title = paste("Boxplot of", col), y = col)
})
ggarrange(plotlist = boxplots, ncol = 2, nrow = ceiling(length(boxplots) / 2))

# Outlier treatment
data_cleaned <- data_imputed %>%
  mutate(across(where(is.numeric), ~replace(., . < quantile(., 0.25) - 1.5 * IQR(.) |
                                              . > quantile(., 0.75) + 1.5 * IQR(.), NA))) %>%
  drop_na()

# Step 5: Stationarity Check
# Perform Augmented Dickey-Fuller (ADF) test on the 'Close' column
if ("Close" %in% colnames(data_cleaned)) {
  close_series <- data_cleaned$Close
  
  # Perform ADF test
  adf_test_result <- adf.test(close_series, alternative = "stationary")
  print(adf_test_result)
  
  # Check p-value
  if (adf_test_result$p.value > 0.05) {
    cat("The series is not stationary. Applying differencing...\n")
    
    # Apply first-order differencing
    data_cleaned$Close_diff <- c(NA, diff(close_series))
    
    # Remove NA values caused by differencing
    data_cleaned <- na.omit(data_cleaned)
    
    # Re-check stationarity after differencing
    adf_test_result_diff <- adf.test(data_cleaned$Close_diff, alternative = "stationary")
    print(adf_test_result_diff)
    
    if (adf_test_result_diff$p.value <= 0.05) {
      cat("The differenced series is stationary.\n")
    } else {
      cat("Further differencing or transformations may be required.\n")
    }
  } else {
    cat("The series is stationary. No differencing needed.\n")
  }
}

# Step 6: ARIMA Model Selection
# ACF and PACF plots
Acf(close_series, main = "ACF Plot")
Pacf(close_series, main = "PACF Plot")

# Fit ARIMA model
# Use auto.arima to automatically select p, d, q
arima_model <- auto.arima(close_series)

# Display model summary
cat("ARIMA Model Summary:\n")
print(summary(arima_model))

# Step 7: Model Fitting (Residual diagnostics and Ljung-Box Test)
# Residual diagnostics
checkresiduals(arima_model)

# Ljung-Box test for autocorrelation of residuals
ljung_box_test <- Box.test(residuals(arima_model), lag = 20, type = "Ljung-Box")
cat("Ljung-Box Test Result:\n")
print(ljung_box_test)

# Step 8: Forecasting
# Forecast future values using the fitted ARIMA model

# Define the forecast horizon (e.g., forecast the next 10 periods)
forecast_horizon <- 10
forecast_results <- forecast(arima_model, h = forecast_horizon)

# Plot the forecast
autoplot(forecast_results) +
  labs(title = paste("Forecast for the Next", forecast_horizon, "Periods"), x = "Time", y = "Forecasted Close Price")

# Print the forecasted values
cat("Forecasted Values:\n")
print(forecast_results)

# Save forecast results to a CSV file
forecast_path <- "/Users/admin/Desktop/admin/afras/project/arima_forecast.csv"
write.csv(forecast_results, forecast_path)

# Step 9: Accuracy Evaluation
# Calculate Accuracy Metrics (MAE, RMSE, AIC, BIC)

# Actual values (for comparison using last observed value)
actual_values <- tail(close_series, forecast_horizon)

# Predicted values
predicted_values <- forecast_results$mean

# MAE Mean Absolute Error
mae <- mean(abs(actual_values - predicted_values))
cat("Mean Absolute Error:", mae, "\n")

# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((actual_values - predicted_values)^2))
cat("Root Mean Squared Error", rmse, "\n")

# AIC (Akaike Information Criterion)
aic_value <- AIC(arima_model)
cat("Akaike Information Criterion:", aic_value, "\n")

# BIC (Bayesian Information Criterion)
bic_value <- BIC(arima_model)
cat("Bayesian Information Criterion :", bic_value, "\n")


# Evaluate the forecast accuracy
accuracy_metrics <- accuracy(forecasted_values, actual_values)
print(accuracy_metrics)


# Save model details to a file
model_summary_path <- "/Users/admin/Desktop/admin/afras/project/arima_model_summary.txt"
writeLines(capture.output(summary(arima_model)), model_summary_path)

# Save cleaned and differenced data to CSV
cleaned_data_path <- "/Users/admin/Desktop/admin/afras/project/cleaned_non_seasonal.csv"
write.csv(data, cleaned_data_path, row.names = FALSE)
