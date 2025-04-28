----------------------------------------------------------------------------------
  #### BSTS
library(forecast)
library(tseries)
library(lubridate)
library(dplyr)
library(xts)
library(bsts)

data <- read.csv("/Users/siobhanflood/Desktop/Taxi_Trips_Counts.csv")

# Convert date column to Date type
data$date <- as.Date(data$date, format="%m/%d/%Y")

# Ensure floor_hour remains as a separate column
data$floor_hour <- as.integer(data$floor_hour)  # Ensure it's numeric

# Convert floor_hour into a time format
data$datetime <- as.POSIXct(paste(data$date, sprintf("%02d:00:00", data$floor_hour)), format="%Y-%m-%d %H:%M:%S")

# Ensure Taxi.ID is treated as ride counts
data <- data %>%
  select(datetime, date, floor_hour, Taxi.ID) %>%
  rename(ride_count = Taxi.ID)

# View first few rows to confirm changes
head(data)

datetime       date floor_hour ride_count
1 2023-10-30 23:00:00 2023-10-30         23         68
2 2023-10-30 22:00:00 2023-10-30         22         86
3 2023-10-30 22:00:00 2023-10-30         22         89
4 2023-10-30 22:00:00 2023-10-30         22         96
5 2023-10-30 22:00:00 2023-10-30         22        110
6 2023-10-30 21:00:00 2023-10-30         21        105

# Define training and test sets
train_data <- data %>% filter(datetime >= "2023-10-01" & datetime < "2023-10-30")
test_data <- data %>% filter(datetime >= "2023-10-30" & datetime < "2023-10-31")

# Convert to time-series object
y_train <- ts(train_data$ride_count, frequency=24)  # Hourly data

# Define state space model with trend and seasonal components
ss <- AddLocalLinearTrend(list(), y_train)  # Trend Component
ss <- AddSeasonal(ss, y_train, nseasons=24)  # Hourly Seasonality

# Fit BSTS Model
bsts_model <- bsts(y_train, 
                   state.specification = ss, 
                   niter = 1000)  # Number of iterations

# Define forecast horizon
horizon <- 24

# Predict next 24 hours
bsts_pred <- predict(bsts_model, horizon = horizon)

# Extract median predictions from bsts_pred (ensure it is numeric)
forecast_values <- bsts_pred$median

# Create a DataFrame for visualization
forecast_df <- data.frame(
  datetime = seq(from = as.POSIXct("2023-10-30 00:00:00"), by = "hour", length.out = horizon),
  predicted_ridership = forecast_values)

# Plot Predictions
library(ggplot2)
ggplot(forecast_df, aes(x = datetime, y = predicted_ridership)) +
  geom_line(color = "red", size = 1) +
  ggtitle("BSTS Forecast for October 30th, 2023") +
  xlab("Hour") + ylab("Predicted Ride Count")







