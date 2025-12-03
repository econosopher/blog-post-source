#!/usr/bin/env Rscript

# Moonton Portfolio Analysis
# Creates portfolio summary table and charts in Deconstructor of Fun theme

suppressPackageStartupMessages({
  library(sensortowerR)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gt)
  library(scales)
  library(lubridate)
  library(glue)
})

message("=== Moonton Portfolio Analysis ===\n")

# Define Deconstructor of Fun theme colors
dof_colors <- list(
  primary = "#1a1a1a",
  secondary = "#666666",
  accent = "#E94560",
  background = "#FFFFFF",
  highlight = "#FFC857",
  green = "#1a9850",
  red = "#d73027"
)

# Get Moonton publisher apps
message("Fetching Moonton publisher apps...")
moonton_apps <- st_publisher_apps("647eb849d9d91f31a54f1792")

# Filter to main apps with meaningful data (exclude test packages and very small apps)
main_apps <- moonton_apps %>%
  filter(!grepl("测试|test", unified_app_name, ignore.case = TRUE)) %>%
  select(unified_app_id, unified_app_name)

message(glue("Found {nrow(main_apps)} Moonton apps"))
print(main_apps)

# Define date range - go back as far as possible
end_date <- floor_date(Sys.Date(), "month") - days(1)  # End of last complete month
start_date <- as.Date("2019-01-01")

message(glue("\nFetching data from {start_date} to {end_date}..."))

# Create app list for batch metrics
app_ids <- main_apps$unified_app_id

# ============================================================================
# PART 1: Fetch Revenue and Downloads using st_metrics (unified)
# ============================================================================
message("\n--- Fetching Revenue and Downloads (Unified) ---")

sales_data_list <- list()

for (i in seq_along(app_ids)) {
  app_id <- app_ids[i]
  app_name <- main_apps$unified_app_name[i]
  message(glue("  [{i}/{length(app_ids)}] Fetching: {app_name}"))

  tryCatch({
    result <- st_metrics(
      os = "unified",
      unified_app_id = app_id,
      start_date = start_date,
      end_date = end_date,
      countries = "WW",
      date_granularity = "monthly",
      verbose = FALSE
    )

    if (!is.null(result) && nrow(result) > 0) {
      result$unified_app_id <- app_id
      result$app_name <- app_name
      sales_data_list[[app_id]] <- result
    }
  }, error = function(e) {
    message(glue("    Warning: {e$message}"))
  })

  Sys.sleep(0.3)  # Rate limiting
}

# Combine all sales data
if (length(sales_data_list) > 0) {
  sales_data <- bind_rows(sales_data_list)
  # Convert revenue from cents to dollars
  if ("revenue" %in% names(sales_data)) {
    sales_data$revenue <- sales_data$revenue / 100
  }
  message(glue("\nSales data: {nrow(sales_data)} rows across {length(unique(sales_data$app_name))} apps"))
} else {
  message("Warning: No sales data retrieved")
  sales_data <- NULL
}

# ============================================================================
# PART 2: Fetch MAU using st_batch_metrics
# ============================================================================
message("\n--- Fetching Monthly Active Users (MAU) ---")

mau_data <- tryCatch({
  result <- st_batch_metrics(
    os = "unified",
    app_list = app_ids,
    metrics = "mau",
    date_range = list(start_date = start_date, end_date = end_date),
    countries = "WW",
    granularity = "monthly",
    parallel = FALSE,
    verbose = TRUE
  )

  if (!is.null(result) && nrow(result) > 0) {
    # Check if app_name already exists before joining
    if (!"app_name" %in% names(result)) {
      result <- result %>%
        left_join(main_apps %>% select(unified_app_id, unified_app_name),
                  by = c("app_id" = "unified_app_id")) %>%
        rename(app_name = unified_app_name)
    }
  }
  result
}, error = function(e) {
  message(glue("Warning: Could not fetch MAU data: {e$message}"))
  NULL
})

if (!is.null(mau_data)) {
  message(glue("MAU data: {nrow(mau_data)} rows"))
}

# ============================================================================
# PART 3: Fetch iOS MAU by Country for country share chart
# ============================================================================
message("\n--- Fetching iOS MAU by Country ---")

# Top countries for mobile gaming (Southeast Asia focus for Moonton)
top_countries <- c("ID", "PH", "MY", "TH", "VN", "BR", "IN", "MX", "US", "JP",
                   "TW", "KR", "RU", "TR", "SA", "EG", "DE", "GB", "FR", "PK")

ios_country_mau <- tryCatch({
  result <- st_batch_metrics(
    os = "ios",
    app_list = app_ids,
    metrics = "mau",
    date_range = list(start_date = as.Date("2024-01-01"), end_date = end_date),
    countries = top_countries,
    granularity = "monthly",
    parallel = FALSE,
    verbose = TRUE
  )

  if (!is.null(result) && nrow(result) > 0) {
    # Check if app_name already exists before joining
    if (!"app_name" %in% names(result)) {
      result <- result %>%
        left_join(main_apps %>% select(unified_app_id, unified_app_name),
                  by = c("app_id" = "unified_app_id")) %>%
        rename(app_name = unified_app_name)
    }
  }
  result
}, error = function(e) {
  message(glue("Warning: Could not fetch iOS country MAU: {e$message}"))
  NULL
})

# ============================================================================
# Save intermediate data
# ============================================================================
dir.create("data", showWarnings = FALSE)
if (!is.null(sales_data)) {
  saveRDS(sales_data, "data/moonton_sales_data.rds")
  message("Saved: data/moonton_sales_data.rds")
}
if (!is.null(mau_data)) {
  saveRDS(mau_data, "data/moonton_mau_data.rds")
  message("Saved: data/moonton_mau_data.rds")
}
if (!is.null(ios_country_mau)) {
  saveRDS(ios_country_mau, "data/moonton_ios_country_mau.rds")
  message("Saved: data/moonton_ios_country_mau.rds")
}
saveRDS(main_apps, "data/moonton_apps.rds")

message("\n=== Data fetching complete ===")

# Print data summaries
if (!is.null(sales_data)) {
  message("\n--- Sales Data Summary ---")
  print(head(sales_data, 10))
  message(glue("\nDate range: {min(sales_data$date)} to {max(sales_data$date)}"))
  message(glue("Apps with data: {paste(unique(sales_data$app_name), collapse=', ')}"))
}

if (!is.null(mau_data)) {
  message("\n--- MAU Data Summary ---")
  print(head(mau_data, 10))
}

if (!is.null(ios_country_mau)) {
  message("\n--- iOS Country MAU Summary ---")
  print(head(ios_country_mau, 10))
}

message("\n✓ Data collection script completed successfully!")
