# ==============================================================================
# test_indicators.R -- runs every indicator on the handout examples plus extra
# edge cases, prints the results, and saves them to r_results.json so they can
# be cross-checked against an independent implementation.
# Run from this folder:  Rscript test_indicators.R
# ==============================================================================
for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi",
            "crossover", "crossunder")) source(paste0(f, ".R"))

res <- list()
show <- function(title, x) { cat("\n--", title, "--\n"); print(x) }

# ---- Handout example data ----------------------------------------------------
d1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
d2 <- c(100, 105, 110, 115, 120, 125, 130)
d3 <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
a1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
a2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

res$sma     <- sma(d1, period = 3);                       show("sma(d1, 3)", res$sma)
res$ema     <- ema(d1, period = 3);                       show("ema(d1, 3)", res$ema)
res$macd    <- macd(d2, short_period = 3, long_period = 5, signal_period = 2)
show("macd(d2, 3, 5, 2)", res$macd)
res$stdev   <- stdev(d1);                                 show("stdev(d1)", res$stdev)
res$linreg  <- linreg(d1, regressionLength = 5, regressionOffset = 0)
show("linreg(d1, 5, 0)", res$linreg)
res$linreg2 <- linreg(d1, regressionLength = 6, regressionOffset = 1)
show("linreg(d1, 6, 1)", res$linreg2)
res$rsi     <- rsi(d3, period = 5);                       show("rsi(d3, 5)", res$rsi)
res$stoch   <- stoch_rsi(d3, period = 5, k_period = 2, d_period = 2)
show("stoch_rsi(d3, 5, 2, 2)", res$stoch)
res$cross_up   <- crossover(a1, a2);                      show("crossover(a1, a2)", res$cross_up)
res$cross_down <- crossunder(a1, a2);                     show("crossunder(a1, a2)", res$cross_down)

# ---- Edge cases and error handling ------------------------------------------
cat("\n-- error handling --\n")
chk <- function(label, expr) {
  out <- tryCatch({ expr; "NO ERROR" }, error = function(e) conditionMessage(e),
                  warning = function(w) paste("WARNING:", conditionMessage(w)))
  cat(sprintf("%-34s -> %s\n", label, out)); out
}
res$err <- list(
  sma_short      = chk("sma(1:3, 5)", sma(1:3, 5)),
  linreg_len     = chk("linreg(1:5, 9, 0)", linreg(1:5, 9, 0)),
  linreg_off     = chk("linreg(1:9, 4, 4)", linreg(1:9, 4, 4)),
  cross_len      = chk("crossover(1:3, 1:4)", crossover(1:3, 1:4)),
  under_len      = chk("crossunder(1:3, 1:4)", crossunder(1:3, 1:4)),
  rsi_short      = chk("rsi(d3, 14)", rsi(d3, 14))
)
res$sma_exact   <- sma(c(5, 5, 5, 5), 4)                      # exactly one window
res$stdev_const <- stdev(c(7, 7, 7, 7))                       # zero spread
res$rsi_up      <- rsi(c(1:8), 3)                             # only gains -> 100
res$rsi_dn      <- rsi(c(8:1), 3)                             # only losses -> 0
res$cross_na    <- crossover(c(NA, 1, 3, 2), c(NA, 2, 2, 2))  # NA warm-up safe
show("sma(c(5,5,5,5), 4)", res$sma_exact)
show("stdev(c(7,7,7,7))", res$stdev_const)
show("rsi(1:8, 3) all gains", res$rsi_up)
show("rsi(8:1, 3) all losses", res$rsi_dn)
show("crossover with NA warm-up", res$cross_na)

# ---- Longer, reproducible series for the independent cross-check -------------
set.seed(400)
long <- round(100 + cumsum(rnorm(60, 0.2, 1.5)), 2)
res$long       <- long
res$long_sma   <- sma(long, 10)
res$long_ema   <- ema(long, 10)
res$long_macd  <- macd(long, 12, 26, 9)
res$long_std   <- stdev(long)
res$long_lr    <- linreg(long, 20, 3)
res$long_rsi   <- rsi(long, 14)
res$long_stoch <- stoch_rsi(long, 14, 3, 3)
fast <- sma(long, 5); slow <- sma(long, 15)
res$long_fast  <- fast[(length(fast) - length(slow) + 1):length(fast)]  # align ends
res$long_slow  <- slow
res$long_up    <- crossover(res$long_fast, res$long_slow)
res$long_down  <- crossunder(res$long_fast, res$long_slow)

writeLines(jsonlite::toJSON(res, digits = NA, auto_unbox = TRUE, na = "null"), "r_results.json")
cat("\nSaved r_results.json\n")
