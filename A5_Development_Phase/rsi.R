# ==============================================================================
# rsi.R  --  Relative Strength Index (RSI) with Wilder's smoothing
# BDA400 Assignment 5 | Bahaa Zuraik
#
# Follows the course pseudocode step by step. RSI = 100 - 100 / (1 + RS),
# RS = average gain / average loss.
#
# Arguments
#   data   numeric vector of prices
#   period integer, look-back period (typically 14)
# Returns
#   numeric vector, same length as data; the first 'period' entries are NA.
#   If length(data) <= period there is not enough data, so a warning is raised
#   and a vector of NA is returned (this is what the pseudocode's loop does).
# ==============================================================================
rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, length(data))

  # Not enough data to produce even one RSI value
  if (length(data) <= period) {
    warning("Data length must be greater than the period to compute RSI; returning NA")
    return(rsi_values)
  }

  # Calculate the average gains and average losses for the first 'period' data points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Calculate RSI values using Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    rs <- avg_gain / avg_loss
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }

  return(rsi_values)
}
