# ==============================================================================
# crossunder.R  --  Crossunder signal between two series
# BDA400 Assignment 5 | Bahaa Zuraik
#
# Flags the points where arr1 moves from >= arr2 to < arr2, exactly as in the
# course pseudocode: the output is "True" / "False" text, with element 1 set
# to "None" because no previous point exists to compare against.
#
# Arguments
#   arr1, arr2 numeric vectors of the same length
# Returns
#   character vector, same length as arr1. Comparisons that involve NA are
#   reported as "False" (isTRUE guard).
# ==============================================================================
crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- character(length(arr1))
  crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point
  for (i in seq_along(arr1)[-1]) {
    if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
      crossunder_signals[i] <- "True"
    } else {
      crossunder_signals[i] <- "False"
    }
  }

  return(crossunder_signals)
}
