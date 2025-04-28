# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

data <- read.csv("Taxi_Trips_Count.csv")

file_path <- file.choose()
data <- read.csv(file_path)

head(data)


data$Trip.Start.Timestamp <- as.POSIXct(data$Trip.Start.Timestamp, format="%m/%d/%Y %I:%M:%S %p")
head(data)
colnames(data)[colnames(data) == "Trip.Start.Timestamp"] <- "datetime"

# Check if the column has been renamed correctly
head(data)

############# delete check with prof
data$date <- floor_date(data$datetime , unit = "day")
data$floor_hour <- hour(data$datetime )#group by both day and hour, then sum the Taxi.ID

# Group the data by date and hour, and then summarize the trip count
result <- data %>%
  group_by(date, floor_hour) %>%
  summarise(Trip_Count = sum(Taxi.ID, na.rm = TRUE), .groups = "drop")

# Now, aggregate by date to get daily trip counts
daily_agg <- result %>%
  group_by(date) %>%
  summarise(daily_trip_count = sum(Trip_Count, na.rm = TRUE))

# Filter for October (Month 10)
october_data <- daily_agg %>%
  filter(month(date) == 10)  # Filters the data for the month of October

# Print the aggregated result for October
print(october_data)

# Create the plot for October
ggplot(october_data, aes(x = date, y = daily_trip_count)) +
  geom_line(color = "blue") +           # Line plot
  labs(title = "Daily Taxi Trip Counts in October", 
       x = "Date", 
       y = "Number of Taxi Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Print the aggregated result to check
print(daily_agg)

# Add a new column to indicate whether the date is a weekday or weekend
october_data <- october_data %>%
  mutate(day_type = ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# Create a more visually appealing plot for October with different colors for weekdays and weekends
ggplot(october_data, aes(x = date, y = daily_trip_count)) +
  geom_line(color = "royalblue", size = 1.2) +  # Line color and thickness
  geom_point(aes(color = day_type), size = 3) +  # Points with different colors for weekdays and weekends
  labs(title = "Daily Taxi Trip Counts in October", 
       x = "Date", 
       y = "Number of Taxi Trips", 
       color = "Day Type") +  # Adding color legend label
  theme_minimal() +                             # Minimal theme for a clean look
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "darkgray"),  # Customize x-axis labels
    axis.text.y = element_text(size = 10, color = "darkgray"),  # Customize y-axis labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "darkblue"),  # Center and style the title
    plot.background = element_rect(fill = "white", color = "white"),  # Set background color
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Major gridlines in light gray
    panel.grid.minor = element_line(color = "lightgray", size = 0.25)  # Minor gridlines in light gray
  ) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "3 days") +  # Customize x-axis labels (displaying date as "Oct 01")
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red"))  
