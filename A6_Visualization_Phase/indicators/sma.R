# ==============================================================================
# sma.R  --  Simple Moving Average (SMA)
# BDA400 Assignment 5 | Bahaa Zuraik
#
# Implemented with base R only (no libraries), following the course template
# and pseudocode.
#
# Arguments
#   data   numeric vector of values (e.g. closing prices)
#   period integer, number of data points in each averaging window
# Returns
#   numeric vector of length (length(data) - period + 1): one mean per window
# ==============================================================================
sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  sma_values <- numeric(length(data) - period + 1)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:(length(data) - period + 1)) {
    # Calculate the mean of the current window of 'period' data points
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    # Store the mean value in the sma_values array
    sma_values[i] <- mean_value
  }

  return(sma_values)
}
