# ==============================================================================
# technical_analysis_functions.R
# BDA400 Assignment 2: Technical Analysis using R, Preliminary Stage
# Author: Bahaa Zuraik
#
# Utility functions to (1) import stock data listed in portfolio.txt,
# (2) compute basic statistics, and (3) display the data and statistics.
# ==============================================================================

# ---- 0. Packages --------------------------------------------------------------
# Install any missing package once, then load it.
required_packages <- c("quantmod", "TTR")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# ---- 1. Read the portfolio file ------------------------------------------------
# Returns a character vector of ticker symbols, one per line of the file.
# Blank lines and lines starting with '#' are ignored; spaces are trimmed.
read_portfolio <- function(portfolio_file = "portfolio.txt") {
  if (!file.exists(portfolio_file)) stop("Portfolio file not found: ", portfolio_file)
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  if (length(symbols) == 0) stop("No stock symbols found in ", portfolio_file)
  toupper(symbols)
}

# ---- 2. Import stock data ----------------------------------------------------
# Reads portfolio.txt and downloads each symbol with quantmod::getSymbols().
# Returns a NAMED LIST of data frames, one per symbol, with the columns
# Date, Open, High, Low, Close, Volume, Adjusted.
# If assign_global = TRUE each data frame is also created in the global
# environment as df_<SYMBOL> (for example df_AAPL).
# A symbol that fails to download is skipped with a warning, not a crash.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = "2024-01-01", to = "2024-12-31",
                            assign_global = FALSE) {
  symbols <- read_portfolio(portfolio_file)
  stock_list <- list()
  for (sym in symbols) {
    raw <- tryCatch(
      # getSymbols() treats 'to' as exclusive, so add one day to include the end date itself
      getSymbols(sym, src = "yahoo", from = as.Date(from), to = as.Date(to) + 1, auto.assign = FALSE),
      error = function(e) { warning("Could not download ", sym, ": ", conditionMessage(e)); NULL }
    )
    if (is.null(raw) || NROW(raw) == 0) next
    df <- data.frame(Date = index(raw), coredata(raw), row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    df <- df[df$Date <= as.Date(to), ]
    rownames(df) <- NULL
    stock_list[[sym]] <- df
    if (assign_global) assign(paste0("df_", gsub("[^A-Za-z0-9]", "_", sym)), df, envir = .GlobalEnv)
  }
  if (length(stock_list) == 0) stop("No stock data could be downloaded")
  stock_list
}

# ---- 3. Compute statistics -------------------------------------------------------
# Mode of a continuous price series: the most frequent closing price after
# rounding to the nearest whole dollar (ties go to the lower price).
stat_mode <- function(x) {
  tab <- table(round(x))
  as.numeric(names(tab)[which.max(tab)])
}

# Takes ONE stock's data frame and returns a list with
#   $summary         one-row data frame: mean, median, mode, standard deviation,
#                    min, max of Close, average volume, and the latest moving average
#   $moving_average  numeric vector, same length as the data (NA for the first ma_period-1 days)
calculate_statistics <- function(df, symbol = NA_character_, ma_period = 20) {
  if (!all(c("Date", "Close", "Volume") %in% names(df))) stop("df must contain Date, Close and Volume")
  close <- df$Close
  ma <- as.numeric(SMA(close, n = ma_period))          # moving average (TTR)
  summary_df <- data.frame(
    Symbol        = symbol,
    Days          = length(close),
    Mean          = mean(close),
    Median        = median(close),
    Mode          = stat_mode(close),
    SD            = sd(close),                          # sample standard deviation (n - 1)
    Min           = min(close),
    Max           = max(close),
    Avg_Volume    = mean(df$Volume),
    Last_Close    = tail(close, 1),
    Last_MA       = tail(ma, 1),
    stringsAsFactors = FALSE
  )
  names(summary_df)[names(summary_df) == "Last_MA"] <- paste0("Last_SMA", ma_period)
  list(summary = summary_df, moving_average = ma)
}

# Runs calculate_statistics() for every stock in the list.
# Returns list(table = one data frame with a row per symbol, details = per-symbol results).
calculate_all_statistics <- function(stock_list, ma_period = 20) {
  details <- lapply(names(stock_list), function(s) calculate_statistics(stock_list[[s]], s, ma_period))
  names(details) <- names(stock_list)
  list(table = do.call(rbind, lapply(details, function(d) d$summary)), details = details)
}

# ---- 4. Display utilities ----------------------------------------------------------
# (a) Table view: first and last rows of one stock, formatted like a price history page.
display_stock_table <- function(df, symbol, n = 5) {
  fmt <- function(d) {
    out <- d
    out$Date <- format(out$Date, "%b %d, %Y")
    out[c("Open", "High", "Low", "Close", "Adjusted")] <- lapply(out[c("Open", "High", "Low", "Close", "Adjusted")], function(v) sprintf("%.2f", v))
    out$Volume <- format(out$Volume, big.mark = ",", scientific = FALSE, trim = TRUE)
    out
  }
  cat("\n==== ", symbol, ": price history (first ", n, " and last ", n, " of ", nrow(df), " trading days) ====\n", sep = "")
  print(fmt(head(df, n)), row.names = FALSE)
  cat("   ...\n")
  print(fmt(tail(df, n)), row.names = FALSE)
}

# (b) Quote-summary view: the key figures a finance site shows at the top of a stock page.
display_quote_summary <- function(df, symbol) {
  last <- tail(df, 1); prev <- tail(df, 2)[1, ]
  chg <- last$Close - prev$Close
  cat("\n---- ", symbol, " quote summary (", format(last$Date, "%b %d, %Y"), ") ----\n", sep = "")
  cat(sprintf("Close            %10.2f   (%+.2f, %+.2f%%)\n", last$Close, chg, 100 * chg / prev$Close))
  cat(sprintf("Previous close   %10.2f\n", prev$Close))
  cat(sprintf("Day's range      %10.2f - %.2f\n", last$Low, last$High))
  cat(sprintf("Period range     %10.2f - %.2f\n", min(df$Low), max(df$High)))
  cat(sprintf("Volume           %s\n", format(last$Volume, big.mark = ",")))
  cat(sprintf("Avg. volume      %s\n", format(round(mean(df$Volume)), big.mark = ",")))
}

# (c) Statistics table for the whole portfolio.
display_statistics <- function(stats_table) {
  cat("\n==== Portfolio statistics (Close prices) ====\n")
  out <- stats_table
  out$Days <- as.character(out$Days)
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(v) ifelse(abs(v) >= 1e6, format(round(v), big.mark = ",", trim = TRUE), sprintf("%.2f", v)))
  print(out, row.names = FALSE)
}

# (d) Charts. Saves a line chart with the moving average and a candlestick chart per stock.
plot_stock <- function(df, symbol, ma, ma_period = 20, out_dir = "charts") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  png(file.path(out_dir, paste0(symbol, "_line_ma.png")), width = 900, height = 520)
  plot(df$Date, df$Close, type = "l", lwd = 2, col = "#1F3864", xlab = "Date", ylab = "Close price (USD)",
       main = paste0(symbol, ": close price and ", ma_period, "-day moving average"))
  lines(df$Date, ma, col = "#C00000", lwd = 2)
  abline(h = mean(df$Close), lty = 2, col = "grey40")
  legend("topleft", legend = c("Close", paste0(ma_period, "-day SMA"), "Mean close"),
         col = c("#1F3864", "#C00000", "grey40"), lwd = c(2, 2, 1), lty = c(1, 1, 2), bty = "n")
  dev.off()
  xt <- xts(df[c("Open", "High", "Low", "Close", "Volume")], order.by = df$Date)
  png(file.path(out_dir, paste0(symbol, "_candles.png")), width = 900, height = 560)
  chartSeries(xt, name = paste0(symbol, " candlestick"), TA = "addVo()", theme = chartTheme("white"))
  dev.off()
  invisible(TRUE)
}
