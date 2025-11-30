# Match-3 Games Data Pull from SensorTower
# CORRECT APPROACH: Search for apps by name -> Get IDs -> Fetch data for each
#
# Data requested:
# - Retention cohorts: D1, D7, D14, D30, D60 (D90 not available)
# - Monthly active users (MAU)
# - Monthly revenue
# - Audience (age, gender) - primarily US only
#
# Countries: US, GB, CA, DE, FR, IT, JP
# Platforms: iOS, Android
# Date Range: Last 12 calendar months
# Granularity: Monthly

suppressPackageStartupMessages({
  library(pacman)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::load_all("../../sensortowerR")
  p_load(dplyr, tidyr, purrr, lubridate, stringr, readr, glue)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

games <- c(
  "Royal Match",
  "Match Villains",
  "Matching Story",
  "Piggy Kingdom",
  "Dream Mania",
  "Castle Crush",
  "Kitchen Masters",
  "Dream Family",
  "Farm Match"
)

countries <- c("US", "GB", "CA", "DE", "FR", "IT", "JP")

# Date range: Last 12 calendar months
end_date <- floor_date(Sys.Date(), "month") - days(1)
start_date <- floor_date(end_date, "month") - months(11)

output_dir <- "/Users/phillip/Documents/vibe_coding_projects/blog-post-source/2025-11-30"

cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n=== MATCH-3 GAMES DATA PULL ===\n")
cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n\n")

cat(glue("Date Range: {start_date} to {end_date}\n"))
cat(glue("Games: {length(games)}\n"))
cat(glue("Countries: {paste(countries, collapse = ', ')}\n"))
cat("Platforms: iOS, Android\n\n")

# =============================================================================
# STEP 1: Search for apps by name and get their IDs
# =============================================================================
cat("=== STEP 1: Searching for apps by name ===\n\n")

app_lookup_results <- map_dfr(games, function(game_name) {
  cat(glue("  Searching: '{game_name}'... "))
  Sys.sleep(0.3)  # Rate limiting

  tryCatch({
    # Search for the app
    result <- st_app_info(term = game_name, limit = 10)

    if (nrow(result) > 0) {
      # Find best match (exact or closest)
      best_match <- result %>%
        mutate(
          name_lower = tolower(unified_app_name),
          search_lower = tolower(game_name),
          # Check for exact match or contains
          exact_match = name_lower == search_lower,
          contains_match = str_detect(name_lower, fixed(search_lower))
        ) %>%
        arrange(desc(exact_match), desc(contains_match)) %>%
        slice(1)

      found_name <- best_match$unified_app_name[1]
      cat(glue("Found: '{found_name}'\n"))

      tibble(
        search_term = game_name,
        app_name = found_name,
        unified_app_id = best_match$unified_app_id[1]
      )
    } else {
      cat("NOT FOUND\n")
      tibble(
        search_term = game_name,
        app_name = NA_character_,
        unified_app_id = NA_character_
      )
    }
  }, error = function(e) {
    cat(glue("ERROR: {e$message}\n"))
    tibble(
      search_term = game_name,
      app_name = NA_character_,
      unified_app_id = NA_character_
    )
  })
})

cat("\n=== App Lookup Results ===\n")
print(app_lookup_results, n = 20)

# Filter to found apps only
found_apps <- app_lookup_results %>%
  filter(!is.na(unified_app_id))

missing_apps <- app_lookup_results %>%
  filter(is.na(unified_app_id)) %>%
  pull(search_term)

cat(glue("\nFound: {nrow(found_apps)} of {length(games)} games\n"))
if (length(missing_apps) > 0) {
  cat(glue("Missing: {paste(missing_apps, collapse = ', ')}\n"))
}

if (nrow(found_apps) == 0) {
  stop("No apps found! Cannot continue.")
}

# =============================================================================
# STEP 2: Fetch monthly revenue/downloads for each app/country/platform
# =============================================================================
cat("\n\n=== STEP 2: Fetching monthly revenue/downloads ===\n\n")

all_sales_data <- list()

for (i in seq_len(nrow(found_apps))) {
  app_row <- found_apps[i, ]
  app_name <- app_row$app_name
  unified_id <- app_row$unified_app_id

  cat(glue("[{i}/{nrow(found_apps)}] {app_name}\n"))

  for (os in c("ios", "android")) {
    for (country in countries) {
      cat(glue("  {os}/{country}... "))

      tryCatch({
        sales <- st_sales_report(
          os = os,
          unified_app_id = unified_id,
          countries = country,
          start_date = start_date,
          end_date = end_date,
          date_granularity = "monthly",
          verbose = FALSE
        )

        if (!is.null(sales) && nrow(sales) > 0) {
          # Ensure consistent column types
          sales <- sales %>%
            mutate(
              across(where(is.numeric) & matches("app_id|id$"), as.character),
              app_name = app_name,
              unified_app_id = unified_id,
              country = country,
              platform = os
            )

          key <- paste(unified_id, os, country, sep = "_")
          all_sales_data[[key]] <- sales
          cat(glue("{nrow(sales)} rows\n"))
        } else {
          cat("no data\n")
        }
      }, error = function(e) {
        cat("error\n")
      })

      Sys.sleep(0.3)  # Rate limiting
    }
  }
}

# Combine all sales data
if (length(all_sales_data) > 0) {
  sales_combined <- bind_rows(all_sales_data)
  cat(glue("\n\nTotal sales data rows: {nrow(sales_combined)}\n"))
} else {
  sales_combined <- tibble()
  cat("\nNo sales data retrieved\n")
}

# =============================================================================
# STEP 3: Fetch MAU/DAU time-series data
# =============================================================================
cat("\n\n=== STEP 3: Fetching MAU/DAU time-series data ===\n\n")

unified_ids <- found_apps$unified_app_id

# Fetch MAU time-series for US (where most engagement data is available)
mau_data <- tryCatch({
  st_batch_metrics(
    os = "unified",
    app_list = unified_ids,
    metrics = c("mau"),
    date_range = list(start_date = start_date, end_date = end_date),
    countries = "US",
    granularity = "monthly",
    verbose = TRUE
  )
}, error = function(e) {
  cat(glue("Error fetching MAU data: {e$message}\n"))
  tibble()
})

if (nrow(mau_data) > 0) {
  cat(glue("\nRetrieved MAU time-series: {nrow(mau_data)} rows\n"))

  # Add app names back
  mau_data <- mau_data %>%
    left_join(found_apps %>% select(unified_app_id, app_name),
              by = c("original_id" = "unified_app_id"))
} else {
  cat("No MAU time-series data retrieved\n")
}

# =============================================================================
# STEP 4: Fetch enriched metrics (retention, demographics - SNAPSHOTS)
# =============================================================================
cat("\n\n=== STEP 4: Fetching enriched metrics (retention, demographics) ===\n")
cat("Note: These are aggregate SNAPSHOTS, not time-series data\n\n")

enriched_data <- tryCatch({
  st_app_enriched(
    unified_app_ids = unified_ids,
    os = "unified",
    regions = "WW"
  )
}, error = function(e) {
  cat(glue("Error fetching enriched data: {e$message}\n"))
  tibble()
})

if (nrow(enriched_data) > 0) {
  cat(glue("Retrieved enriched data for {nrow(enriched_data)} apps\n"))

  # Show what metrics are available
  retention_cols <- names(enriched_data)[str_detect(names(enriched_data), "^retention")]
  mau_cols <- names(enriched_data)[str_detect(names(enriched_data), "mau|dau|wau")]
  demo_cols <- names(enriched_data)[str_detect(names(enriched_data), "gender|age")]

  cat(glue("\nAvailable retention columns: {paste(retention_cols, collapse = ', ')}\n"))
  cat(glue("Available active user columns: {paste(mau_cols, collapse = ', ')}\n"))
  cat(glue("Available demographics columns: {paste(demo_cols, collapse = ', ')}\n"))
} else {
  cat("No enriched data retrieved\n")
  cat("Note: Enriched metrics may not be available for smaller apps\n")
}

# =============================================================================
# STEP 5: Save outputs
# =============================================================================
cat("\n\n=== STEP 5: Saving outputs ===\n\n")

# 1. Save app lookup reference
write_csv(app_lookup_results, file.path(output_dir, "match3_app_lookup.csv"))
cat("Saved: match3_app_lookup.csv\n")

# 2. Save monthly sales data
if (nrow(sales_combined) > 0) {
  write_csv(sales_combined, file.path(output_dir, "match3_monthly_sales.csv"))
  cat(glue("Saved: match3_monthly_sales.csv ({nrow(sales_combined)} rows)\n"))
}

# 3. Save MAU time-series data
if (exists("mau_data") && nrow(mau_data) > 0) {
  write_csv(mau_data, file.path(output_dir, "match3_mau_timeseries.csv"))
  cat(glue("Saved: match3_mau_timeseries.csv ({nrow(mau_data)} rows)\n"))
}

# 4. Save enriched metrics (snapshots)
if (nrow(enriched_data) > 0) {
  write_csv(enriched_data, file.path(output_dir, "match3_enriched_metrics.csv"))
  cat(glue("Saved: match3_enriched_metrics.csv ({nrow(enriched_data)} rows)\n"))
}

# 5. Create comprehensive combined CSV
cat("\nCreating comprehensive combined dataset...\n")

if (nrow(sales_combined) > 0) {
  if (nrow(enriched_data) > 0) {
    # Join sales data with enriched metrics
    combined_data <- sales_combined %>%
      left_join(
        enriched_data %>% select(-any_of(c("app_name", "unified_app_name"))),
        by = "unified_app_id"
      )
  } else {
    combined_data <- sales_combined
  }

  write_csv(combined_data, file.path(output_dir, "match3_comprehensive.csv"))
  cat(glue("Saved: match3_comprehensive.csv ({nrow(combined_data)} rows)\n"))
}

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n=== DATA PULL SUMMARY ===\n")
cat("=" |> rep(60) |> paste(collapse = ""))
cat("\n\n")

cat(glue("Date Range: {start_date} to {end_date}\n"))
cat(glue("Games Requested: {length(games)}\n"))
cat(glue("Games Found: {nrow(found_apps)}\n"))

if (length(missing_apps) > 0) {
  cat(glue("Games NOT Found: {paste(missing_apps, collapse = ', ')}\n"))
}

cat(glue("Countries: {paste(countries, collapse = ', ')}\n"))
cat("Platforms: iOS, Android\n")

cat("\n=== Files Created ===\n")
cat("- match3_app_lookup.csv: App ID reference\n")
if (nrow(sales_combined) > 0) {
  cat(glue("- match3_monthly_sales.csv: {nrow(sales_combined)} rows (revenue/downloads time-series)\n"))
}
if (exists("mau_data") && nrow(mau_data) > 0) {
  cat(glue("- match3_mau_timeseries.csv: {nrow(mau_data)} rows (MAU time-series)\n"))
}
if (nrow(enriched_data) > 0) {
  cat(glue("- match3_enriched_metrics.csv: {nrow(enriched_data)} rows (retention/demographics snapshots)\n"))
}
cat("- match3_comprehensive.csv: Combined dataset\n")

cat("\n=== Data Availability Notes ===\n")
cat("1. Retention cohorts: D1, D7, D14, D30, D60 (D90 NOT available)\n")
cat("2. Audience data (age/gender): Primarily US market only\n")
cat("3. Enriched metrics are SNAPSHOTS, not time-series\n")
cat("4. Some apps may have missing data due to:\n")
cat("   - App not available in that country\n")
cat("   - Insufficient data for estimates\n")
cat("   - Platform-specific availability\n")
cat("   - Smaller apps may lack retention/demographics\n")

cat("\n\nScript completed!\n")
