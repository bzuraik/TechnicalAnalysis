# ==============================================================================
# macd.R  --  Moving Average Convergence Divergence (MACD)
# BDA400 Assignment 5 | Bahaa Zuraik
#
# Uses ema() from ema.R (our own implementation, no libraries).
#
# Arguments
#   data          numeric vector of values (e.g. closing prices)
#   short_period  period of the short-term EMA
#   long_period   period of the long-term EMA
#   signal_period period of the signal-line EMA
# Returns
#   list(macd_line, signal_line, histogram), each the same length as data
# ==============================================================================
if (!exists("ema", mode = "function")) {
  .here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) ".")
  source(file.path(.here, "ema.R"))
}

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line (the difference between short_ema and long_ema)
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line   = macd_line,
    signal_line = signal_line,
    histogram   = histogram
  )
  return(result)
}
