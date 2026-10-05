# ==============================================================================
# linreg.R  --  Simple Linear Regression over a window of a data series
# BDA400 Assignment 5 | Bahaa Zuraik
#
# Follows the course pseudocode exactly, including its index arithmetic:
#   start_index = max(1, n - regressionLength + regressionOffset)
#   end_index   = min(n, n - regressionOffset)
# With regressionOffset = 0 this selects elements (n - regressionLength) .. n,
# i.e. regressionLength + 1 points, because the pseudocode is written for
# 0-based thinking while R is 1-based. This is documented in the report.
#
# Arguments
#   regressionSource  numeric vector (the series)
#   regressionLength  number of points the window is meant to span
#   regressionOffset  number of points skipped at the end of the series
# Returns
#   list(slope, intercept, predicted_values); x is the index 1..length(subset)
# ==============================================================================
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # Calculate the starting index for the regressionSource
  start_index <- max(1, n - regressionLength + regressionOffset)

  # Calculate the ending index for the regressionSource
  end_index <- min(n, n - regressionOffset)

  # Guard (not in the pseudocode): an offset that crosses the start would make
  # R silently count backwards, so fail loudly instead.
  if (start_index >= end_index) {
    stop("regressionOffset leaves fewer than two points to regress")
  }

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Calculate the index values for the regression points
  index_values <- seq_along(source_subset)

  # Calculate the sum of index values and the sum of source_subset
  sum_index  <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index  <- mean(index_values)
  mean_source <- mean(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator   <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope     <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regressionSource
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(
    slope            = slope,
    intercept        = intercept,
    predicted_values = predicted_values
  )
  return(result)
}
