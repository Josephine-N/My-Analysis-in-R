library(tidyverse)
library(fable)
library(tsibble)
library(feasts)
library(urca)
library(ggtime)
library(ggplot2)

# Convert a standard data frame to a tsibble
# 'date' is your time column, 'value' is what you're predicting
ts_data <- as_tsibble(Line_graph, index = year)

# Training the Model
fit <- ts_data %>%
  model(
    arima_model = ARIMA(number_of_coffee_grains),
    ets_model = ETS(number_of_coffee_grains),
    naive_model = NAIVE(number_of_coffee_grains)
  )

# Forecast for the next 12 periods (e.g., 12 months)
fc <- fit %>%
  forecast(h = "12 months")

# Visualize the results
fc %>%
  autoplot(ts_data, level = NULL) +
  labs(
    title = "Time Series Forecast", 
    y = "Number of Coffee Grains", 
    x = "Year"
  )
