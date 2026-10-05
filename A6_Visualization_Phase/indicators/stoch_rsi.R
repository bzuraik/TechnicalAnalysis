# ==============================================================================
# stoch_rsi.R  --  Stochastic RSI (StochRSI)
# BDA400 Assignment 5 | Bahaa Zuraik
#
# Uses rsi() from rsi.R and sma() from sma.R (our own implementations).
#
# Arguments
#   data     numeric vector of prices
#   period   RSI look-back period
#   k_period window of the %K smoothing (SMA of the normalised RSI)
#   d_period window of the %D smoothing (SMA of %K)
# Returns
#   list(k_line, d_line). As in the pseudocode these come from sma(), so they
#   are shorter than the input (leading NA values from the RSI warm-up are kept
#   and propagate through the averages).
# ==============================================================================
.here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) ".")
if (!exists("rsi", mode = "function")) source(file.path(.here, "rsi.R"))
if (!exists("sma", mode = "function")) source(file.path(.here, "sma.R"))

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI: normalise RSI to 0..1 over its observed range.
  # na.rm = TRUE because the first 'period' RSI values are NA by construction.
  if (all(is.na(rsi_values))) {
    min_rsi <- NA_real_
    max_rsi <- NA_real_
  } else {
    min_rsi <- min(rsi_values, na.rm = TRUE)
    max_rsi <- max(rsi_values, na.rm = TRUE)
  }
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  return(result)
}
