#!/usr/bin/env Rscript

# Test Script: Publisher Portfolio Workflow
# Tests the new aggregate_related flag and publisher search functionality

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(glue)
})

# Load the local sensortowerR package
devtools::load_all("~/Documents/vibe_coding_projects/sensortowerR")

message("=== Publisher Portfolio Workflow Tests ===\n")

# Load environment variables
readRenviron("~/.Renviron")

# ============================================================================
# TEST 1: Moonton Publisher Search
# ============================================================================
message("--- TEST 1: Moonton Publisher Search ---")

tryCatch({
  moonton_publisher <- st_app_info("Moonton", entity_type = "publisher")
  message("Found ", nrow(moonton_publisher), " Moonton publisher(s)")
  print(moonton_publisher)
  message("TEST 1: PASSED\n")
}, error = function(e) {
  message("TEST 1: FAILED - ", e$message, "\n")
})

# ============================================================================
# TEST 2: Moonton Apps with aggregate_related = FALSE (baseline)
# ============================================================================
message("--- TEST 2: Moonton Apps (aggregate_related = FALSE) ---")

tryCatch({
  moonton_apps_raw <- st_publisher_apps(
    unified_id = moonton_publisher$unified_publisher_id[1],
    aggregate_related = FALSE,
    verbose = TRUE
  )
  message("Found ", nrow(moonton_apps_raw), " apps (raw)")
  print(moonton_apps_raw %>% select(any_of(c("unified_app_id", "unified_app_name"))))
  message("TEST 2: PASSED\n")
}, error = function(e) {
  message("TEST 2: FAILED - ", e$message, "\n")
})

# ============================================================================
# TEST 3: Moonton Apps with aggregate_related = TRUE
# ============================================================================
message("--- TEST 3: Moonton Apps (aggregate_related = TRUE) ---")

tryCatch({
  moonton_apps <- st_publisher_apps(
    unified_id = moonton_publisher$unified_publisher_id[1],
    aggregate_related = TRUE,
    verbose = TRUE
  )
  message("Found ", nrow(moonton_apps), " apps (aggregated)")
  print(moonton_apps %>% select(any_of(c("unified_app_id", "unified_app_name"))))
  message("TEST 3: PASSED\n")
}, error = function(e) {
  message("TEST 3: FAILED - ", e$message, "\n")
})

# ============================================================================
# TEST 4: Lilith Publisher Search
# ============================================================================
message("--- TEST 4: Lilith Publisher Search ---")

tryCatch({
  lilith_publisher <- st_app_info("Lilith", entity_type = "publisher")
  message("Found ", nrow(lilith_publisher), " Lilith publisher(s)")
  print(lilith_publisher)
  message("TEST 4: PASSED\n")
}, error = function(e) {
  message("TEST 4: FAILED - ", e$message, "\n")
})

# ============================================================================
# TEST 5: Lilith Apps with aggregate_related = TRUE
# ============================================================================
message("--- TEST 5: Lilith Apps (aggregate_related = TRUE) ---")

tryCatch({
  lilith_apps <- st_publisher_apps(
    unified_id = lilith_publisher$unified_publisher_id[1],
    aggregate_related = TRUE,
    verbose = TRUE
  )
  message("Found ", nrow(lilith_apps), " apps (aggregated)")
  print(lilith_apps %>% select(any_of(c("unified_app_id", "unified_app_name"))))
  message("TEST 5: PASSED\n")
}, error = function(e) {
  message("TEST 5: FAILED - ", e$message, "\n")
})

# ============================================================================
# TEST 6: Full Pipeline Test - Moonton Sales Data
# ============================================================================
message("--- TEST 6: Full Pipeline - Moonton Sales Data ---")

tryCatch({
  # Get first 3 apps only for speed
  app_ids <- head(moonton_apps$unified_app_id, 3)

  sales <- st_unified_sales_report(
    unified_app_id = app_ids,
    countries = "WW",
    start_date = "2024-01-01",
    end_date = "2024-11-30",
    date_granularity = "monthly",
    verbose = TRUE
  )

  message("Retrieved ", nrow(sales), " sales records")

  # Summarize by app
  summary <- sales %>%
    group_by(unified_app_id) %>%
    summarise(
      total_revenue = sum(revenue, na.rm = TRUE),
      total_downloads = sum(downloads, na.rm = TRUE),
      .groups = "drop"
    )

  print(summary)
  message("TEST 6: PASSED\n")
}, error = function(e) {
  message("TEST 6: FAILED - ", e$message, "\n")
})

# ============================================================================
# TEST 7: Piped Workflow Test
# ============================================================================
message("--- TEST 7: Piped Workflow Test ---")

tryCatch({
  # Full piped workflow
  result <- st_app_info("Moonton", entity_type = "publisher") %>%
    slice(1) %>%
    pull(unified_publisher_id) %>%
    st_publisher_apps(aggregate_related = TRUE, verbose = FALSE) %>%
    head(2) %>%
    pull(unified_app_id)

  message("Piped workflow returned ", length(result), " app IDs")
  print(result)
  message("TEST 7: PASSED\n")
}, error = function(e) {
  message("TEST 7: FAILED - ", e$message, "\n")
})

# ============================================================================
# TEST 8: Error Handling - Invalid Publisher ID
# ============================================================================
message("--- TEST 8: Error Handling - Invalid Publisher ID ---")

tryCatch({
  result <- st_publisher_apps(unified_id = "invalid_id")
  message("TEST 8: FAILED - Should have thrown an error\n")
}, error = function(e) {
  if (grepl("Invalid unified_id format", e$message)) {
    message("Correctly caught error: ", e$message)
    message("TEST 8: PASSED\n")
  } else {
    message("Wrong error type: ", e$message)
    message("TEST 8: PARTIAL\n")
  }
})

# ============================================================================
# SUMMARY
# ============================================================================
message("=== Tests Complete ===")
message("Review output above for any FAILED tests")
