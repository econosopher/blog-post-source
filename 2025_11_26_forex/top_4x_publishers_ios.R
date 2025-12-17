#!/usr/bin/env Rscript

# Top 10 4X Strategy Publishers by YTD Revenue (iOS)
# Groups games by publisher and ranks by total revenue

suppressPackageStartupMessages({
  library(pacman)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::load_all("../../sensortowerR")
  p_load(dplyr, scales, lubridate)
})

message("=== Top 10 4X Strategy Publishers by YTD Revenue (iOS) ===\n")

# Same 4X strategy custom filter from existing analysis
custom_filter_id <- "600a22c0241bc16eb899fd71"
start_date <- as.Date("2025-01-01")
end_date <- Sys.Date() - 1  # Yesterday

message(sprintf("Date range: %s to %s\n", start_date, end_date))

# Fetch top 4X games for iOS - get more games to capture publisher diversity
raw_data <- st_top_charts(
  measure = "revenue",
  os = "ios",
  category = "6014",  # Games category
  regions = "WW",
  date = start_date,
  end_date = end_date,
  comparison_attribute = "absolute",
  time_range = "day",
  custom_fields_filter_id = custom_filter_id,
  custom_tags_mode = "include_unified_apps",
  limit = 100,  # Get more to capture all publishers
  device_type = "total",
  enrich_response = TRUE,
  deduplicate_apps = TRUE
)

if (nrow(raw_data) == 0) {
  stop("No data returned from st_top_charts")
}

message(sprintf("Found %d 4X strategy games\n", nrow(raw_data)))

# Extract publisher info for each app
extract_first_id <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  as.character(x[[1]])
}

publisher_df <- purrr::map_dfr(raw_data$app_id, function(id) {
  info <- tryCatch(st_app_lookup(id, verbose = FALSE), error = function(e) NULL)
  publisher <- if (!is.null(info) && !is.null(info$publisher_name) && nzchar(info$publisher_name)) {
    info$publisher_name
  } else {
    NA_character_
  }
  app_name <- if (!is.null(info) && !is.null(info$name) && nzchar(info$name)) {
    info$name
  } else {
    NA_character_
  }
  tibble(
    app_id = id,
    publisher_name = publisher,
    app_name_lookup = app_name
  )
})

# Join publisher data - column names vary based on OS type
games_data <- raw_data %>%
  left_join(publisher_df, by = "app_id") %>%
  mutate(revenue = as.numeric(revenue))

# Add game name column - check custom_tags first, then lookup
if ("custom_tags.unified_product_name" %in% names(games_data)) {
  games_data$game <- games_data$`custom_tags.unified_product_name`
} else if ("unified_app_name" %in% names(games_data)) {
  games_data$game <- games_data$unified_app_name
} else if ("app_name_lookup" %in% names(games_data)) {
  games_data$game <- games_data$app_name_lookup
} else {
  games_data$game <- games_data$app_id
}

# Use publisher from custom_tags if lookup failed
if ("custom_tags.Publisher Name" %in% names(games_data)) {
  games_data$publisher_name <- coalesce(games_data$publisher_name, games_data$`custom_tags.Publisher Name`)
}

message(sprintf("Sample games: %s", paste(head(games_data$game, 5), collapse = ", ")))
message(sprintf("Sample publishers: %s", paste(head(games_data$publisher_name, 5), collapse = ", ")))

# Group by publisher and sum revenue
publisher_ranking <- games_data %>%
  filter(!is.na(publisher_name)) %>%
  group_by(publisher_name) %>%
  summarise(
    total_revenue = sum(revenue, na.rm = TRUE),
    num_games = n(),
    games = paste(game, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number())

# Display results
message("\n=== TOP 10 4X STRATEGY PUBLISHERS BY YTD REVENUE (iOS) ===\n")
message(sprintf("Period: %s to %s\n", start_date, end_date))

for (i in 1:nrow(publisher_ranking)) {
  row <- publisher_ranking[i, ]
  message(sprintf("%2d. %s", row$rank, row$publisher_name))
  message(sprintf("    Revenue: %s | Games: %d",
                  dollar(row$total_revenue, suffix = "M", scale = 1e-6, accuracy = 0.1),
                  row$num_games))
  # Show top games (truncate if too many)
  games_list <- strsplit(row$games, ", ")[[1]]
  if (length(games_list) > 3) {
    games_display <- paste(c(games_list[1:3], "..."), collapse = ", ")
  } else {
    games_display <- row$games
  }
  message(sprintf("    Games: %s\n", games_display))
}

# Summary
total_market <- sum(publisher_ranking$total_revenue)
message(sprintf("Total Top 10 Publisher Revenue: %s",
                dollar(total_market, suffix = "B", scale = 1e-9, accuracy = 0.01)))
