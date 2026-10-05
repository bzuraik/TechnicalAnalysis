# ==============================================================================
# a2_main.R  --  run the whole Assignment 2 workflow
# Usage:  setwd("<folder with these files>");  source("a2_main.R")
# ==============================================================================
source("technical_analysis_functions.R")
options(width = 130)

START <- "2024-01-01"; END <- "2024-12-31"; MA_PERIOD <- 20

cat("Symbols in portfolio.txt:", paste(read_portfolio("portfolio.txt"), collapse = ", "), "\n")

# 1. Import
stocks <- load_stock_data("portfolio.txt", from = START, to = END, assign_global = TRUE)
cat("\nLoaded", length(stocks), "data frames:", paste(names(stocks), collapse = ", "), "\n")
for (s in names(stocks)) cat(sprintf("  df_%-6s %4d rows x %d columns, %s to %s\n", s, nrow(stocks[[s]]), ncol(stocks[[s]]),
                                      format(min(stocks[[s]]$Date)), format(max(stocks[[s]]$Date))))
cat("\nStructure of df_AAPL:\n"); str(df_AAPL)

# 2. Statistics
res <- calculate_all_statistics(stocks, ma_period = MA_PERIOD)

# 3. Display
for (s in names(stocks)) { display_stock_table(stocks[[s]], s, n = 3); display_quote_summary(stocks[[s]], s) }
display_statistics(res$table)
for (s in names(stocks)) plot_stock(stocks[[s]], s, res$details[[s]]$moving_average, MA_PERIOD)
cat("\nCharts saved to ./charts\n")

# 4. Save the exact data and statistics used, for the independent cross-check
dir.create("data", showWarnings = FALSE)
for (s in names(stocks)) write.csv(stocks[[s]], file.path("data", paste0(s, ".csv")), row.names = FALSE)
write.csv(res$table, file.path("data", "statistics_r.csv"), row.names = FALSE)
