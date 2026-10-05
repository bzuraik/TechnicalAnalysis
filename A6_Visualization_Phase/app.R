# ==============================================================================
# app.R  --  Portfolio dashboard (R Shiny)
# BDA400 Assignment 6: Technical Analysis using R, Visualization Phase
# Author: Bahaa Zuraik
#
# Run:  setwd("<folder containing app.R>");  shiny::runApp()
#
# Sections follow the assignment steps:
#   Step 1  Data collection and setup      (packages, Yahoo Finance download, error handling)
#   Step 2  Visualising stock data         (Shiny skeleton, widgets, line / candlestick / area charts)
#   Step 3  Overlaying technical indicators(Moving Averages, RSI, MACD, Stochastic RSI, volume; on/off toggles)
#   Step 4  Trading rules and annotations  (moving-average crossover signals, Buy / Sell / Hold labels)
# The indicator functions are my own base-R implementations from Assignment 5 (folder ./indicators).
# ==============================================================================

# ==============================================================================
# STEP 1: DATA COLLECTION AND SETUP
# ==============================================================================

# ---- 1.1 Packages -------------------------------------------------------------
# Install any package that is missing (first run only), then load it.
required_packages <- c("shiny", "ggplot2", "quantmod", "patchwork")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# ---- 1.2 Indicator functions from Assignment 5 (no libraries) -----------------
for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi", "crossover", "crossunder")) {
  source(file.path("indicators", paste0(f, ".R")))
}

# ---- 1.3 Portfolio symbols (portfolio.txt from Assignment 2) ------------------
read_symbols <- function(file = "portfolio.txt") {
  default <- c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA")
  if (!file.exists(file)) return(default)
  s <- trimws(readLines(file, warn = FALSE))
  s <- toupper(s[s != "" & !startsWith(s, "#")])
  if (length(s) == 0) default else s
}

# ---- 1.4 Fetch historical data (Yahoo Finance via quantmod) -------------------
# Returns a data frame: Date, Open, High, Low, Close, Volume.
# Friendly errors (stop with a readable message) are shown in the app through validate().
fetch_stock <- function(symbol, start_date, end_date) {
  symbol <- toupper(trimws(symbol))
  if (!nzchar(symbol)) stop("Enter a stock symbol.")
  if (as.Date(start_date) >= as.Date(end_date)) stop("The start date must be before the end date.")
  raw <- tryCatch(
    # 'to' is exclusive in getSymbols, so add one day to include the end date
    getSymbols(symbol, src = "yahoo", from = as.Date(start_date), to = as.Date(end_date) + 1, auto.assign = FALSE),
    error = function(e) stop("Could not download '", symbol, "' from Yahoo Finance. Check the symbol and your internet connection.", call. = FALSE)
  )
  if (is.null(raw) || NROW(raw) == 0) stop("Yahoo Finance returned no data for '", symbol, "' in this date range.")
  df <- data.frame(Date = index(raw), coredata(raw), row.names = NULL)
  names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df <- df[!is.na(df$Close) & df$Date <= as.Date(end_date), c("Date", "Open", "High", "Low", "Close", "Volume")]
  rownames(df) <- NULL
  df
}

# ---- 1.5 Second data source: user-uploaded CSV --------------------------------
# Needs the columns Date, Open, High, Low, Close (any capitalisation); Volume is optional.
read_uploaded_csv <- function(path) {
  d <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) stop("The file could not be read as a CSV."))
  names(d) <- tools::toTitleCase(tolower(trimws(names(d))))
  need <- c("Date", "Open", "High", "Low", "Close")
  miss <- setdiff(need, names(d))
  if (length(miss) > 0) stop("The CSV is missing the column(s): ", paste(miss, collapse = ", "))
  if (!"Volume" %in% names(d)) d$Volume <- 0
  d$Date <- as.Date(d$Date)
  if (any(is.na(d$Date))) stop("Some values in the Date column are not valid dates (use YYYY-MM-DD).")
  d <- d[!is.na(d$Close), c("Date", need[-1], "Volume")]
  d <- d[order(d$Date), ]
  rownames(d) <- NULL
  d
}

# ==============================================================================
# STEP 2 (logic): TIME FRAMES, CHART BUILDING BLOCKS
# ==============================================================================

# ---- 2.1 Time frame: Daily / Weekly / Monthly ---------------------------------
# Weekly and monthly bars are built from the daily bars: Open = first open, High = max high,
# Low = min low, Close = last close, Volume = sum. The bar is dated on its last trading day.
aggregate_timeframe <- function(df, frame = "Daily") {
  if (frame == "Daily" || nrow(df) == 0) return(df)
  key <- if (frame == "Weekly") {
    format(df$Date - (as.integer(format(df$Date, "%u")) - 1), "%Y-%m-%d")   # Monday of that week
  } else {
    format(df$Date, "%Y-%m")
  }
  groups <- split(seq_len(nrow(df)), factor(key, levels = unique(key)))
  out <- do.call(rbind, lapply(groups, function(ix) {
    data.frame(Date = df$Date[ix[length(ix)]], Open = df$Open[ix[1]], High = max(df$High[ix]),
               Low = min(df$Low[ix]), Close = df$Close[ix[length(ix)]], Volume = sum(df$Volume[ix]))
  }))
  rownames(out) <- NULL
  out
}

# The functions sma(), stoch_rsi() ... return shorter vectors; pad on the left with NA so every
# indicator lines up with the bars of the chart.
align_right <- function(v, n) c(rep(NA_real_, n - length(v)), v)

# ==============================================================================
# STEP 4 (logic): TRADING RULES
# ==============================================================================
# Rule: moving-average crossover.
#   short MA crosses ABOVE long MA  -> "Buy"
#   short MA crosses BELOW long MA  -> "Sell"
#   otherwise                        -> "Hold"
# Optional RSI filter (customisable): a Buy is only kept when RSI < overbought level, and a
# Sell only when RSI > oversold level; a signal that fails the filter becomes "Hold".
generate_signals <- function(df, short_n = 20, long_n = 50, use_rsi_filter = FALSE,
                             rsi_period = 14, overbought = 70, oversold = 30) {
  n <- nrow(df)
  if (short_n >= long_n) stop("The short moving average must be shorter than the long moving average.")
  if (n < long_n) stop("Not enough bars: ", n, " available but the long moving average needs ", long_n,
                       ". Widen the date range or shorten the moving averages.")
  short_ma <- align_right(sma(df$Close, short_n), n)
  long_ma  <- align_right(sma(df$Close, long_n), n)
  up   <- crossover(short_ma, long_ma)
  down <- crossunder(short_ma, long_ma)
  signal <- ifelse(up == "Up", "Buy", ifelse(down == "True", "Sell", "Hold"))
  r <- rep(NA_real_, n)
  if (use_rsi_filter && n > rsi_period) {
    r <- rsi(df$Close, rsi_period)
    keep_buy  <- !is.na(r) & r < overbought
    keep_sell <- !is.na(r) & r > oversold
    signal[signal == "Buy"  & !keep_buy]  <- "Hold"
    signal[signal == "Sell" & !keep_sell] <- "Hold"
  }
  data.frame(Date = df$Date, Close = df$Close, Short_MA = short_ma, Long_MA = long_ma, RSI = r, Signal = signal,
             stringsAsFactors = FALSE)
}

# Long-only back-test of the signals: buy the close on a Buy, sell the close on the next Sell,
# close any open trade at the last bar. Returns the trades and a summary versus buy-and-hold.
backtest_signals <- function(sig) {
  trades <- data.frame(Entry_Date = as.Date(character()), Entry = numeric(), Exit_Date = as.Date(character()),
                       Exit = numeric(), Return_pct = numeric(), Status = character(), stringsAsFactors = FALSE)
  holding <- FALSE; e_date <- NULL; e_px <- NA
  for (i in seq_len(nrow(sig))) {
    if (sig$Signal[i] == "Buy" && !holding) { holding <- TRUE; e_date <- sig$Date[i]; e_px <- sig$Close[i] }
    else if (sig$Signal[i] == "Sell" && holding) {
      trades[nrow(trades) + 1, ] <- list(e_date, e_px, sig$Date[i], sig$Close[i], 100 * (sig$Close[i] / e_px - 1), "closed")
      holding <- FALSE
    }
  }
  if (holding) {
    last <- nrow(sig)
    trades[nrow(trades) + 1, ] <- list(e_date, e_px, sig$Date[last], sig$Close[last], 100 * (sig$Close[last] / e_px - 1), "open")
  }
  strat <- if (nrow(trades) > 0) 100 * (prod(1 + trades$Return_pct / 100) - 1) else 0
  bh <- 100 * (sig$Close[nrow(sig)] / sig$Close[1] - 1)
  list(trades = trades, strategy_return = strat, buy_hold_return = bh,
       n_buy = sum(sig$Signal == "Buy"), n_sell = sum(sig$Signal == "Sell"), n_hold = sum(sig$Signal == "Hold"),
       win_rate = if (nrow(trades) > 0) 100 * mean(trades$Return_pct > 0) else NA_real_)
}

# ==============================================================================
# STEP 2 + 3 + 4 (drawing): THE CHART
# ==============================================================================
COL <- list(up = "#2e7d32", down = "#c62828", navy = "#1f3864", orange = "#ef6c00", teal = "#00838f", grey = "#7f7f7f")

# opts: symbol, frame, chart_type, indicators, short_n, long_n, rsi_period, overbought, oversold,
#       macd_fast, macd_slow, macd_signal, show_signals, label_hold
build_chart <- function(df, sig, o) {
  n <- nrow(df)
  bar_w <- switch(o$frame, Daily = 0.35, Weekly = 2, Monthly = 9)
  xr <- range(df$Date) + c(-bar_w, bar_w)
  base_theme <- theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), axis.title.x = element_blank(),
                 plot.margin = margin(2, 6, 2, 6), legend.position = "top", legend.title = element_blank(), axis.title.y = element_text(size = 8.5))
  df$Up <- df$Close >= df$Open

  # ---- Price panel: chart type (Step 2) ---------------------------------------
  p <- ggplot(df, aes(Date))
  if (o$chart_type == "Candlestick") {
    p <- p + geom_segment(aes(x = Date, xend = Date, y = Low, yend = High, colour = Up), linewidth = 0.4, show.legend = FALSE) +
      geom_rect(aes(xmin = Date - bar_w, xmax = Date + bar_w, ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = Up),
                show.legend = FALSE) +
      scale_fill_manual(values = c(`TRUE` = COL$up, `FALSE` = COL$down)) +
      scale_colour_manual(values = c(`TRUE` = COL$up, `FALSE` = COL$down), aesthetics = "colour", guide = "none")
  } else if (o$chart_type == "Area") {
    p <- p + geom_ribbon(aes(ymin = min(Low) * 0.98, ymax = Close), fill = COL$navy, alpha = 0.18) +
      geom_line(aes(y = Close), colour = COL$navy, linewidth = 0.8)
  } else {
    p <- p + geom_line(aes(y = Close), colour = COL$navy, linewidth = 0.8)
  }

  # ---- Overlay: moving averages (Step 3) --------------------------------------
  notes <- character()
  if ("Moving Averages" %in% o$indicators && !is.null(sig)) {
    ma <- data.frame(Date = rep(sig$Date, 2), Value = c(sig$Short_MA, sig$Long_MA),
                     Line = factor(rep(c(paste0("SMA ", o$short_n), paste0("SMA ", o$long_n)), each = n),
                                  levels = c(paste0("SMA ", o$short_n), paste0("SMA ", o$long_n))))   # short = solid, long = dashed
    p <- p + geom_line(data = ma[!is.na(ma$Value), ], aes(Date, Value, linetype = Line), colour = COL$orange, linewidth = 0.8, inherit.aes = FALSE) +
      scale_linetype_manual(values = c("solid", "dashed")[seq_len(nlevels(ma$Line))], name = NULL)
  }

  # ---- Annotations: trading signals (Step 4) ----------------------------------
  if (o$show_signals && !is.null(sig)) {
    rng <- diff(range(c(df$Low, df$High)))
    buys  <- sig[sig$Signal == "Buy", ];  sells <- sig[sig$Signal == "Sell", ]
    if (nrow(buys) > 0) {
      bl <- merge(buys[c("Date")], df[c("Date", "Low")], by = "Date")
      p <- p + geom_point(data = bl, aes(Date, Low - 0.03 * rng), shape = 24, size = 3, fill = COL$up, colour = "white", inherit.aes = FALSE) +
        geom_text(data = bl, aes(Date, Low - 0.075 * rng, label = "Buy"), colour = COL$up, fontface = "bold", size = 3.3, inherit.aes = FALSE)
    }
    if (nrow(sells) > 0) {
      sl <- merge(sells[c("Date")], df[c("Date", "High")], by = "Date")
      p <- p + geom_point(data = sl, aes(Date, High + 0.03 * rng), shape = 25, size = 3, fill = COL$down, colour = "white", inherit.aes = FALSE) +
        geom_text(data = sl, aes(Date, High + 0.075 * rng, label = "Sell"), colour = COL$down, fontface = "bold", size = 3.3, inherit.aes = FALSE)
    }
    if (isTRUE(o$label_hold)) {
      holds <- merge(sig[sig$Signal == "Hold", "Date", drop = FALSE], df[c("Date", "Close")], by = "Date")
      p <- p + geom_text(data = holds, aes(Date, Close, label = "Hold"), colour = COL$grey, size = 2.2, vjust = 2.2, inherit.aes = FALSE)
    }
  }
  p <- p + labs(title = sprintf("%s  |  %s bars  |  %s chart", o$symbol, o$frame, o$chart_type), y = "Price (USD)") +
    coord_cartesian(xlim = xr) + base_theme + theme(plot.title = element_text(face = "bold", colour = COL$navy))
  panels <- list(p); heights <- 4.2

  # ---- Volume ---------------------------------------------------------------
  if ("Volume" %in% o$indicators) {
    panels[[length(panels) + 1]] <- ggplot(df, aes(Date, Volume / 1e6, fill = Up)) + geom_col(width = bar_w * 1.8, show.legend = FALSE) +
      scale_fill_manual(values = c(`TRUE` = COL$up, `FALSE` = COL$down)) + labs(y = "Volume (M)") + coord_cartesian(xlim = xr) + base_theme
    heights <- c(heights, 1)
  }
  # ---- RSI --------------------------------------------------------------------
  if ("RSI" %in% o$indicators) {
    r <- if (n > o$rsi_period) rsi(df$Close, o$rsi_period) else rep(NA_real_, n)
    pr <- ggplot(data.frame(Date = df$Date, RSI = r), aes(Date, RSI)) +
      annotate("rect", xmin = xr[1], xmax = xr[2], ymin = o$overbought, ymax = 100, fill = COL$down, alpha = 0.08) +
      annotate("rect", xmin = xr[1], xmax = xr[2], ymin = 0, ymax = o$oversold, fill = COL$up, alpha = 0.08) +
      geom_hline(yintercept = c(o$oversold, o$overbought), linetype = "dotted", colour = COL$grey) +
      geom_line(colour = COL$navy, na.rm = TRUE) + scale_y_continuous(limits = c(0, 100)) +
      labs(y = paste0("RSI(", o$rsi_period, ")")) + coord_cartesian(xlim = xr) + base_theme
    if (all(is.na(r))) pr <- pr + annotate("text", x = mean(xr), y = 50, label = "Not enough bars for this RSI period")
    panels[[length(panels) + 1]] <- pr; heights <- c(heights, 1.5)
  }
  # ---- MACD -------------------------------------------------------------------
  if ("MACD" %in% o$indicators) {
    m <- macd(df$Close, o$macd_fast, o$macd_slow, o$macd_signal)
    md <- data.frame(Date = df$Date, MACD = m$macd_line, Signal = m$signal_line, Hist = m$histogram)
    panels[[length(panels) + 1]] <- ggplot(md, aes(Date)) +
      geom_col(aes(y = Hist, fill = Hist >= 0), width = bar_w * 1.8, alpha = 0.55, show.legend = FALSE) +
      scale_fill_manual(values = c(`TRUE` = COL$up, `FALSE` = COL$down)) +
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
      geom_line(aes(y = MACD), colour = COL$navy) + geom_line(aes(y = Signal), colour = COL$orange) +
      labs(y = sprintf("MACD(%d,%d,%d)", o$macd_fast, o$macd_slow, o$macd_signal)) + coord_cartesian(xlim = xr) + base_theme
    heights <- c(heights, 1.5)
  }
  # ---- Stochastic RSI -----------------------------------------------------------
  if ("Stochastic RSI" %in% o$indicators) {
    st <- if (n > o$rsi_period + 6) stoch_rsi(df$Close, o$rsi_period, 3, 3) else NULL
    sd_ <- data.frame(Date = df$Date, K = if (is.null(st)) NA_real_ else align_right(st$k_line, n),
                      D = if (is.null(st)) NA_real_ else align_right(st$d_line, n))
    panels[[length(panels) + 1]] <- ggplot(sd_, aes(Date)) + geom_hline(yintercept = c(0.2, 0.8), linetype = "dotted", colour = COL$grey) +
      geom_line(aes(y = K), colour = COL$navy, na.rm = TRUE) + geom_line(aes(y = D), colour = COL$orange, na.rm = TRUE) +
      labs(y = "StochRSI (%K, %D)") + coord_cartesian(xlim = xr, ylim = c(-0.05, 1.05)) + base_theme
    heights <- c(heights, 1.3)
  }
  # x-axis labels only on the bottom panel
  for (i in seq_along(panels)[-length(panels)]) panels[[i]] <- panels[[i]] + theme(axis.text.x = element_blank())
  wrap_plots(panels, ncol = 1, heights = heights)
}

# ==============================================================================
# STEP 2: SHINY APP SKELETON AND WIDGETS
# ==============================================================================
SYMBOLS <- read_symbols()
INDICATORS <- c("Moving Averages", "Volume", "RSI", "MACD", "Stochastic RSI")

ui <- fluidPage(
  titlePanel("Portfolio Dashboard: price charts, technical indicators and trading signals"),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Data"),
      radioButtons("source", "Data source", c("Yahoo Finance", "Upload CSV"), inline = TRUE),
      conditionalPanel("input.source == 'Yahoo Finance'",
        selectizeInput("symbol", "Stock symbol (pick or type one)", choices = SYMBOLS, selected = SYMBOLS[1],
                       options = list(create = TRUE))),
      conditionalPanel("input.source == 'Upload CSV'",
        fileInput("csv", "CSV with Date, Open, High, Low, Close[, Volume]", accept = ".csv"),
        textInput("csv_name", "Label for the chart", "My data")),
      dateRangeInput("date_range", "Select Date Range:", start = "2024-01-01", end = "2024-12-31"),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      h4("Chart"),
      selectInput("chart_type", "Chart type", c("Line", "Candlestick", "Area")),
      checkboxGroupInput("indicators", "Technical indicators (tick to overlay)", choices = INDICATORS, selected = c("Moving Averages", "RSI")),
      h4("Trading rule: moving-average crossover"),
      fluidRow(column(6, numericInput("short_n", "Short MA", 20, min = 2, max = 200)),
               column(6, numericInput("long_n", "Long MA", 50, min = 3, max = 400))),
      checkboxInput("show_signals", "Annotate Buy / Sell signals on the chart", TRUE),
      checkboxInput("label_hold", "Also label every Hold bar", FALSE),
      checkboxInput("use_rsi_filter", "Confirm signals with RSI", FALSE),
      conditionalPanel("input.use_rsi_filter || input.indicators.indexOf('RSI') >= 0 || input.indicators.indexOf('Stochastic RSI') >= 0",
        fluidRow(column(4, numericInput("rsi_period", "RSI n", 14, min = 2, max = 60)),
                 column(4, numericInput("overbought", "Over-bought", 70, min = 50, max = 95)),
                 column(4, numericInput("oversold", "Over-sold", 30, min = 5, max = 50)))),
      conditionalPanel("input.indicators.indexOf('MACD') >= 0",
        fluidRow(column(4, numericInput("macd_fast", "Fast", 12, min = 2, max = 100)),
                 column(4, numericInput("macd_slow", "Slow", 26, min = 3, max = 200)),
                 column(4, numericInput("macd_signal", "Signal", 9, min = 2, max = 100))))
    ),
    mainPanel(width = 9,
      tabsetPanel(id = "tabs",
        tabPanel("Chart", br(), plotOutput("stock_chart", height = "720px"), br(), uiOutput("signal_summary")),
        tabPanel("Signals", br(), checkboxInput("show_hold_rows", "Show Hold rows", FALSE),
                 downloadButton("download_signals", "Download signals (CSV)"), br(), br(), tableOutput("signals_table")),
        tabPanel("Trades", br(), uiOutput("trade_summary"), tableOutput("trades_table")),
        tabPanel("Data", br(), tableOutput("data_table")),
        tabPanel("Portfolio", br(), p("Latest position of every symbol in portfolio.txt under the current date range, time frame and moving-average settings."),
                 actionButton("load_portfolio", "Load portfolio overview"), br(), br(), tableOutput("portfolio_table"))
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ---- Data (Step 1 + 2): download or upload, filter by date range, aggregate ------
  raw_data <- reactive({
    if (identical(input$source, "Upload CSV")) {
      validate(need(!is.null(input$csv), "Upload a CSV file to begin."))
      tryCatch(read_uploaded_csv(input$csv$datapath), error = function(e) validate(conditionMessage(e)))
    } else {
      validate(need(nzchar(input$symbol), "Enter a stock symbol."))
      tryCatch(fetch_stock(input$symbol, input$date_range[1], input$date_range[2]), error = function(e) validate(conditionMessage(e)))
    }
  }) |> bindCache(input$source, input$symbol, input$date_range, input$csv$datapath)

  label <- reactive(if (identical(input$source, "Upload CSV")) input$csv_name else toupper(trimws(input$symbol)))

  bars <- reactive({
    d <- raw_data()
    d <- d[d$Date >= input$date_range[1] & d$Date <= input$date_range[2], ]
    validate(need(nrow(d) >= 5, "Fewer than 5 trading days in this date range. Choose a wider range."))
    aggregate_timeframe(d, input$time_frame)
  })

  # ---- Trading rules (Step 4) ---------------------------------------------------
  signals <- reactive({
    validate(need(is.numeric(input$short_n) && is.numeric(input$long_n), "Enter both moving-average lengths."))
    tryCatch(generate_signals(bars(), input$short_n, input$long_n, input$use_rsi_filter, input$rsi_period, input$overbought, input$oversold),
             error = function(e) validate(conditionMessage(e)))
  })
  bt <- reactive(backtest_signals(signals()))

  # ---- Chart (Steps 2-4) ----------------------------------------------------------
  output$stock_chart <- renderPlot({
    d <- bars()
    sig <- tryCatch(signals(), error = function(e) NULL, shiny.silent.error = function(e) NULL)
    if (is.null(sig) && "Moving Averages" %in% input$indicators) validate("Moving averages need more bars: widen the date range or shorten the averages.")
    validate(need(!("MACD" %in% input$indicators) || input$macd_fast < input$macd_slow, "MACD: the fast period must be shorter than the slow period."))
    build_chart(d, sig, list(symbol = label(), frame = input$time_frame, chart_type = input$chart_type, indicators = input$indicators,
      short_n = input$short_n, long_n = input$long_n, rsi_period = input$rsi_period, overbought = input$overbought, oversold = input$oversold,
      macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal,
      show_signals = input$show_signals, label_hold = input$label_hold))
  }, res = 96)

  output$signal_summary <- renderUI({
    b <- bt(); s <- signals(); last_sig <- tail(s[s$Signal != "Hold", ], 1)
    HTML(sprintf("<b>Signals in this view:</b> %d Buy, %d Sell, %d Hold. <b>Latest signal:</b> %s. <b>Rule:</b> Buy when SMA %d crosses above SMA %d, Sell when it crosses below%s.",
      b$n_buy, b$n_sell, b$n_hold,
      if (nrow(last_sig) == 0) "none yet" else sprintf("%s on %s at %.2f", last_sig$Signal, format(last_sig$Date), last_sig$Close),
      input$short_n, input$long_n, if (input$use_rsi_filter) sprintf(" (RSI filter: Buy only if RSI < %d, Sell only if RSI > %d)", input$overbought, input$oversold) else ""))
  })

  output$signals_table <- renderTable({
    s <- signals(); if (!isTRUE(input$show_hold_rows)) s <- s[s$Signal != "Hold", ]
    s$Date <- format(s$Date); s
  }, digits = 2, na = "")
  output$download_signals <- downloadHandler(
    filename = function() paste0(label(), "_signals.csv"),
    content = function(file) write.csv(signals(), file, row.names = FALSE))

  output$trade_summary <- renderUI({
    b <- bt()
    HTML(sprintf("<b>Long-only back-test of the rule:</b> %d trade(s), win rate %s, strategy return %+.2f%% versus buy-and-hold %+.2f%% over the same period. (Close prices, no costs; for study only, not investment advice.)",
      nrow(b$trades), if (is.na(b$win_rate)) "n/a" else sprintf("%.0f%%", b$win_rate), b$strategy_return, b$buy_hold_return))
  })
  output$trades_table <- renderTable({ t <- bt()$trades; t$Entry_Date <- format(t$Entry_Date); t$Exit_Date <- format(t$Exit_Date); t }, digits = 2)

  output$data_table <- renderTable({ d <- bars(); d$Date <- format(d$Date); d$Volume <- format(d$Volume, big.mark = ",", scientific = FALSE); d }, digits = 2)

  # ---- Portfolio overview ----------------------------------------------------------
  portfolio <- eventReactive(input$load_portfolio, {
    rows <- lapply(SYMBOLS, function(s) {
      tryCatch({
        d <- aggregate_timeframe(fetch_stock(s, input$date_range[1], input$date_range[2]), input$time_frame)
        g <- generate_signals(d, input$short_n, input$long_n, input$use_rsi_filter, input$rsi_period, input$overbought, input$oversold)
        n <- nrow(g); act <- g[g$Signal != "Hold", ]
        data.frame(Symbol = s, Last_Date = format(g$Date[n]), Close = g$Close[n], Change_pct = 100 * (g$Close[n] / g$Close[n - 1] - 1),
                   Trend = if (g$Short_MA[n] > g$Long_MA[n]) "Short MA above long MA" else "Short MA below long MA",
                   Last_Signal = if (nrow(act) == 0) "none" else paste(tail(act$Signal, 1), format(tail(act$Date, 1))), stringsAsFactors = FALSE)
      }, error = function(e) data.frame(Symbol = s, Last_Date = NA, Close = NA, Change_pct = NA, Trend = conditionMessage(e), Last_Signal = NA))
    })
    do.call(rbind, rows)
  })
  output$portfolio_table <- renderTable(portfolio(), digits = 2, na = "")
}

shinyApp(ui, server)
