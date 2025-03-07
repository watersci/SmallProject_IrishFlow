# Flow was downloaded from 
# https://waterlevel.ie/hydro-data/#/overview/Waterlevel/station/11668/Ballymahon/Flow?period=P1Y

library(tidyverse)  # For data manipulation
library(ggplot2)    # For static plots
library(readr)  # For reading CSV files
library(dplyr)  # For data manipulation
library(lubridate) # Date col in flow needs fixing
library(forecast) # Fill in the missing flow data
library(imputeTS) # Fill in missing flow data
library(trend)  # See if there is a pattern in the annual trend

# Read the flow file, skipping the first 12 rows
flow_data <- read_csv("26021_ballymahon_q_complete.csv", skip = 12)

# Convert DD/MM/YYYY to Date format, change the mean flow col header to something easier
flow_data = flow_data %>%
  mutate(date = dmy(Day),
         flow_cms = `Day Mean`)  

# the flag -1 means no data that day
flow_data = flow_data[-which(flow_data$Quality == -1),]

# Create a full sequence of dates from min to max date
full_dates <- tibble(date = seq(from = min(flow_data$date),
                                to = max(flow_data$date),
                                by = "day"))

# Join full date sequence with the existing flow data
flow_complete <- full_dates %>%
  left_join(flow_data %>% select(date, flow_cms), by = "date")

rm(flow_data,full_dates)

# Add a flag for the missing values for plotting 
flow_complete <- flow_complete %>%
  mutate(missing_flag = ifelse(is.na(flow_cms), "Missing (Imputed)", "Original Data"))

# Ensure `flow_cms` is filled using `na_kalman()`
flow_complete$flow_cms <- na_kalman(flow_complete$flow_cms, model = "StructTS")

# ggplot(flow_complete, aes(x = date, y = flow_cms, color = missing_flag)) +
#   geom_point(alpha = 0.6) +  # Scatter plot with some transparency
#   geom_vline(xintercept = as.numeric(as.Date(paste0(unique(year(flow_complete$date)), "-01-01"))),
#              linetype = "dashed", color = "black", alpha = 0.5) +  # Vertical lines for Jan 1st
#   labs(title = "River Flow Over Time",
#        x = "Date",
#        y = "Flow (m³/s)",
#        color = "Data Type") +
#   scale_color_manual(values = c("Original Data" = "blue", "Missing (Imputed)" = "red")) +
#   theme_minimal()

# There's a big chunk of missing 1976, let's just trim it to 1977
flow_complete <- flow_complete %>%
  filter(date >= as.Date("1977-01-01")) %>%
  arrange(date)

# Convert to a time series object (daily frequency)
flow_ts <- ts(flow_complete$flow_cms, start = c(year(min(flow_complete$date))), frequency = 365.25)

# Perform STL decomposition (Seasonal-Trend decomposition)
flow_decomp <- stl(flow_ts, s.window = "periodic")  # periodic assumes seasonality follows a fixed cycle

# Plot the decomposition
autoplot(flow_decomp) +
  labs(title = "Seasonal Decomposition of River Flow",
       x = "Time",
       y = "Flow (m³/s)")

# Extract trend and add it back to the dataframe
flow_complete$trend <- flow_decomp$time.series[, "trend"]

# Plot the trend component
ggplot(flow_complete, aes(x = date, y = trend)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Extracted Trend in River Flow",
       x = "Date",
       y = "Trend (m³/s)") +
  theme_minimal()

# Perform Mann-Kendall Trend Test
mk_test <- mk.test(flow_complete$trend)

# Print test results
print(mk_test)

#### Yes, the flows are statistically significantly increasing

# Fit a linear regression model to the trend data
lm_trend <- lm(trend ~ date, data = flow_complete)

# Get the slope (rate of change per day)
slope <- summary(lm_trend)$coefficients["date", "Estimate"]

# Convert slope to an annual rate of change (since dates are in days)
annual_change <- slope * 365.25  # Convert from daily to yearly change

# Calculate total change over the period
years <- as.numeric(difftime(max(flow_complete$date), min(flow_complete$date), units = "days")) / 365.25
total_change <- annual_change * years

# Percentage change relative to the start of the dataset
percent_change <- (total_change / first(flow_complete$trend)) * 100

# Print results
cat("Estimated annual increase in river flow:", round(annual_change, 3), "m³/s per year\n")
cat("Total increase over", round(years, 1), "years:", round(total_change, 3), "m³/s\n")
cat("Percentage change over the period:", round(percent_change, 2), "%\n")


