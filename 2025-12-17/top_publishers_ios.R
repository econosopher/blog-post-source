#!/usr/bin/env Rscript

# Top 10 Game Publishers by YTD Revenue (iOS)

suppressPackageStartupMessages({
  library(pacman)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::load_all("../../sensortowerR")
  p_load(dplyr, scales, lubridate)
})

message("=== Top 10 Game Publishers by YTD Revenue (iOS) ===\n")

# Get top 10 publishers for games category, YTD
publishers <- st_top_publishers(
  measure = "revenue",
  os = "ios",
  category = 6014,  # Games
  time_range = "day",
  date = as.Date("2025-01-01"),
  end_date = Sys.Date() - 1,
  country = "WW",
  limit = 10,
  include_apps = TRUE
)

message("\n=== TOP 10 GAME PUBLISHERS BY YTD REVENUE (iOS) ===\n")

for (i in 1:nrow(publishers)) {
  row <- publishers[i, ]
  message(sprintf("%2d. %s", row$rank, row$publisher_name))
  message(sprintf("    Revenue: %s", dollar(row$revenue_usd, suffix = "M", scale = 1e-6, accuracy = 0.1)))

  # Show top games if available
  if ("apps" %in% names(row) && !is.null(row$apps[[1]]) && nrow(row$apps[[1]]) > 0) {
    top_games <- head(row$apps[[1]]$app_name, 3)
    message(sprintf("    Top games: %s\n", paste(top_games, collapse = ", ")))
  } else {
    message("")
  }
}

total_revenue <- sum(publishers$revenue_usd, na.rm = TRUE)
message(sprintf("\nTop 10 Combined: %s", dollar(total_revenue, suffix = "B", scale = 1e-9, accuracy = 0.01)))
