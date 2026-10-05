# ==============================================================================
# crossover.R  --  Crossover signal between two series
# BDA400 Assignment 5 | Bahaa Zuraik
#
# Marks the points where arr1 crosses ABOVE arr2 ("Up"), crosses BELOW arr2
# ("Down"), or neither ("None"), exactly as in the course pseudocode.
#
# Arguments
#   arr1, arr2 numeric vectors of the same length
# Returns
#   character vector, same length as arr1. Element 1 is always "None".
#   Comparisons that involve NA never signal (isTRUE guards them), so series
#   with a warm-up period of NA values can be passed in safely.
# ==============================================================================
crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- character(length(arr1))
  crossover_signals[1] <- "None"

  # Check for crossovers at each data point
  for (i in seq_along(arr1)[-1]) {
    if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1])) {
      crossover_signals[i] <- "Up"
    } else if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
      crossover_signals[i] <- "Down"
    } else {
      crossover_signals[i] <- "None"
    }
  }

  return(crossover_signals)
}
