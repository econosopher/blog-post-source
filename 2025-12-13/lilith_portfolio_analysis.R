#!/usr/bin/env Rscript

# Lilith Games Portfolio Analysis
# Creates portfolio summary table using the new aggregate_related workflow
# This script demonstrates the improved sensortowerR package (v0.8.8+)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(gt)
  library(scales)
  library(lubridate)
  library(glue)
})

# Load the local sensortowerR package
devtools::load_all("~/Documents/vibe_coding_projects/sensortowerR")

message("=== Lilith Games Portfolio Analysis ===\n")

# Load environment variables
readRenviron("~/.Renviron")

# Create output directory
dir.create("output", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# Load Game Economist Consulting GT theme
gec_theme_loaded <- tryCatch({
  source("~/Documents/vibe_coding_projects/gec-theme/gec_gt_theme.R", chdir = TRUE)
  TRUE
}, error = function(e) {
  message("Note: GEC theme not loaded (", e$message, ")")
  FALSE
})

# ============================================================================
# STEP 1: Get Lilith Portfolio Using New Workflow
# ============================================================================
message("Step 1: Getting Lilith Portfolio...")

# Use the new piped workflow with aggregate_related = TRUE
lilith_publisher <- st_app_info("Lilith", entity_type = "publisher")
message("  Found ", nrow(lilith_publisher), " Lilith publisher(s)")
print(lilith_publisher)

# Get the main Lilith Games publisher (first match is usually correct)
publisher_id <- lilith_publisher$unified_publisher_id[1]
message("\n  Using publisher: ", lilith_publisher$unified_publisher_name[1])

# Get all apps with proper regional SKU aggregation
message("\n  Fetching apps with aggregate_related = TRUE...")
lilith_apps <- st_publisher_apps(
  unified_id = publisher_id,
  aggregate_related = TRUE,
  verbose = TRUE
)

message("\n  Found ", nrow(lilith_apps), " unique apps")

# Display the portfolio
print(lilith_apps %>% select(any_of(c("unified_app_id", "unified_app_name"))))

# ============================================================================
# STEP 2: Fetch Sales Data Using Unified API
# ============================================================================
message("\nStep 2: Fetching Sales Data...")

# Define date range
end_date <- floor_date(Sys.Date(), "month") - days(1)
start_date <- as.Date("2023-01-01")

# Filter to main games (exclude test apps, small apps, etc.)
# We'll fetch data for all and filter later based on revenue
app_ids <- lilith_apps$unified_app_id

# Check for cached data
sales_file <- "data/lilith_sales_data.rds"
force_refetch <- TRUE  # Set to FALSE to use cached data

if (file.exists(sales_file) && !force_refetch) {
  message("  Loading cached sales data...")
  sales_data <- readRDS(sales_file)
} else {
  message("  Fetching sales data using st_unified_sales_report()...")

  # Fetch in batches of 10 to avoid API limits
  batch_size <- 10
  n_batches <- ceiling(length(app_ids) / batch_size)

  sales_data_list <- list()

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(app_ids))
    batch_ids <- app_ids[start_idx:end_idx]

    message(glue("  Batch {i}/{n_batches}: apps {start_idx}-{end_idx}"))

    tryCatch({
      result <- st_unified_sales_report(
        unified_app_id = batch_ids,
        countries = "WW",
        start_date = start_date,
        end_date = end_date,
        date_granularity = "monthly",
        verbose = FALSE
      )

      if (!is.null(result) && nrow(result) > 0) {
        sales_data_list[[i]] <- result
      }
    }, error = function(e) {
      message(glue("    Warning: {e$message}"))
    })

    Sys.sleep(0.5)  # Rate limiting
  }

  if (length(sales_data_list) > 0) {
    sales_data <- bind_rows(sales_data_list)
    saveRDS(sales_data, sales_file)
    message("  Saved sales data to cache")
  }
}

message(glue("  Sales data: {nrow(sales_data)} records"))

# Add app names to sales data
sales_data <- sales_data %>%
  left_join(
    lilith_apps %>% select(any_of(c("unified_app_id", "unified_app_name"))),
    by = "unified_app_id"
  )

# ============================================================================
# STEP 3: Fetch MAU Data
# ============================================================================
message("\nStep 3: Fetching MAU Data...")

mau_file <- "data/lilith_mau_data.rds"

if (file.exists(mau_file) && !force_refetch) {
  message("  Loading cached MAU data...")
  mau_data <- readRDS(mau_file)
} else {
  message("  Fetching MAU data using st_batch_metrics()...")

  tryCatch({
    mau_data <- st_batch_metrics(
      os = "unified",
      app_list = app_ids,
      metrics = "mau",
      date_range = list(start_date = start_date, end_date = end_date),
      countries = "WW",
      granularity = "monthly"
    )

    if (!is.null(mau_data) && nrow(mau_data) > 0) {
      saveRDS(mau_data, mau_file)
      message("  Saved MAU data to cache")
    }
  }, error = function(e) {
    message(glue("  Warning: Could not fetch MAU data: {e$message}"))
    mau_data <- NULL
  })
}

if (!is.null(mau_data) && nrow(mau_data) > 0) {
  message(glue("  MAU data: {nrow(mau_data)} records"))

  # Add app names to MAU data (join on original_id which contains unified_app_id)
  mau_data <- mau_data %>%
    left_join(
      lilith_apps %>% select(any_of(c("unified_app_id", "unified_app_name"))),
      by = c("original_id" = "unified_app_id")
    )
}

# ============================================================================
# STEP 4: Fetch Rankings Data
# ============================================================================
message("\nStep 4: Fetching Rankings Data...")

ranks_file <- "data/lilith_rankings_data.rds"

if (file.exists(ranks_file) && !force_refetch) {
  message("  Loading cached rankings data...")
  rank_data <- readRDS(ranks_file)
} else {
  message("  Fetching top charts for rankings...")

  tryCatch({
    top_games <- st_top_charts(
      measure = "revenue",
      os = "unified",
      category = "6014",  # Games
      regions = "WW",
      time_range = "month",
      date = format(floor_date(Sys.Date(), "month") - days(1), "%Y-%m-%d"),
      limit = 1500
    )

    # Add global rank and sub-genre rank
    top_games <- top_games %>%
      mutate(global_rank = row_number()) %>%
      group_by(`aggregate_tags.Game Sub-genre`) %>%
      mutate(subgenre_rank = row_number()) %>%
      ungroup()

    # Extract rankings
    rank_data <- top_games %>%
      select(unified_app_id, unified_app_name, global_rank, subgenre_rank,
             subgenre = `aggregate_tags.Game Sub-genre`)

    saveRDS(rank_data, ranks_file)
    message("  Saved rankings data")
  }, error = function(e) {
    message(glue("  Warning: Could not fetch rankings: {e$message}"))
    rank_data <- NULL
  })
}

if (!is.null(rank_data)) {
  message(glue("  Rankings data: {nrow(rank_data)} apps"))
}

# ============================================================================
# STEP 5: Create Portfolio Summary Table
# ============================================================================
message("\nStep 5: Creating Portfolio Summary Table...")

# Aggregate data by app for 2023, 2024, and 2025 YTD
current_month <- month(Sys.Date()) - 1  # Use last complete month

# Create revenue summary
revenue_summary <- sales_data %>%
  mutate(Year = year(date), Month = month(date)) %>%
  filter(Year >= 2023) %>%
  # YTD filtering: Jan to current_month for each year
  filter(Month <= current_month | Year < year(Sys.Date())) %>%
  group_by(unified_app_name, Year) %>%
  summarise(
    Revenue = sum(revenue, na.rm = TRUE),
    Downloads = sum(downloads, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot to wide format
revenue_wide <- revenue_summary %>%
  pivot_wider(
    names_from = Year,
    values_from = c(Revenue, Downloads),
    names_sep = "_",
    values_fill = 0
  )

# Add MAU data if available
if (!is.null(mau_data) && nrow(mau_data) > 0) {
  mau_summary <- mau_data %>%
    mutate(Year = year(date), Month = month(date)) %>%
    filter(Year >= 2023) %>%
    filter(Month <= current_month | Year < year(Sys.Date())) %>%
    group_by(unified_app_name, Year) %>%
    summarise(
      MAU = mean(value, na.rm = TRUE),
      .groups = "drop"
    )

  mau_wide <- mau_summary %>%
    pivot_wider(
      names_from = Year,
      values_from = MAU,
      names_prefix = "MAU_",
      values_fill = 0
    )

  # Join with revenue data
  table_data <- revenue_wide %>%
    left_join(mau_wide, by = "unified_app_name")
} else {
  table_data <- revenue_wide
}

# Filter to apps with 2025 activity and significant revenue
table_data <- table_data %>%
  filter(Revenue_2025 > 100000 | Revenue_2024 > 1000000) %>%  # At least $100K in 2025 or $1M in 2024
  arrange(desc(Revenue_2025))

# Merge with ranking data if available (subgenre and subgenre_rank only)
if (!is.null(rank_data)) {
  table_data <- table_data %>%
    left_join(
      rank_data %>%
        select(unified_app_name, subgenre_rank, subgenre) %>%
        distinct(unified_app_name, .keep_all = TRUE),
      by = "unified_app_name"
    )
}

# Calculate YoY growth
table_data <- table_data %>%
  mutate(
    Revenue_Growth = ifelse(Revenue_2024 > 0,
                            round((Revenue_2025 - Revenue_2024) / Revenue_2024 * 100, 0),
                            NA),
    Downloads_Growth = ifelse(Downloads_2024 > 0,
                              round((Downloads_2025 - Downloads_2024) / Downloads_2024 * 100, 0),
                              NA)
  )

# Add MAU growth if available
if ("MAU_2024" %in% names(table_data) && "MAU_2025" %in% names(table_data)) {
  table_data <- table_data %>%
    mutate(
      MAU_Growth = ifelse(MAU_2024 > 0,
                          round((MAU_2025 - MAU_2024) / MAU_2024 * 100, 0),
                          NA)
    )
}

# Add portfolio rank
table_data <- table_data %>%
  mutate(Rank = row_number())

# Translate Chinese game names to English
table_data <- table_data %>%
  mutate(unified_app_name = case_when(
    unified_app_name == "剑与远征 - AFK" ~ "AFK Arena (CN)",
    unified_app_name == "小冰冰传奇-官方怀旧服" ~ "Soul Clash (Retro)",
    TRUE ~ unified_app_name
  ))

message(glue("  Final table has {nrow(table_data)} apps"))

# ============================================================================
# STEP 6: Create GT Table
# ============================================================================
message("\nStep 6: Creating GT Table...")

# Select columns for display
# Order: #, Game, Subgenre, Sub-Genre Rank, then metrics with YoY % to the left
display_cols <- c("Rank", "unified_app_name")

# Add Subgenre and Sub-Genre Rank after Game
if ("subgenre_rank" %in% names(table_data)) {
  display_cols <- c(display_cols, "subgenre", "subgenre_rank")
}

# Add metrics
display_cols <- c(display_cols,
                  "Revenue_Growth", "Revenue_2025", "Revenue_2024", "Revenue_2023",
                  "Downloads_Growth", "Downloads_2025", "Downloads_2024", "Downloads_2023")

# Add MAU columns if available
if ("MAU_2025" %in% names(table_data)) {
  display_cols <- c(display_cols, "MAU_Growth", "MAU_2025", "MAU_2024", "MAU_2023")
}

# Create GT table
gt_table <- table_data %>%
  select(any_of(display_cols)) %>%
  gt() %>%
  tab_header(
    title = md("**LILITH GAMES PORTFOLIO ANALYSIS**"),
    subtitle = glue("YTD Revenue, Downloads & MAU (Jan-{month.abb[current_month]})")
  ) %>%
  # Format revenue columns
  fmt(
    columns = any_of(c("Revenue_2025", "Revenue_2024", "Revenue_2023")),
    fns = function(x) {
      ifelse(is.na(x) | x == 0, "\u2014",
        ifelse(x >= 1e9, paste0("$", format(round(x / 1e9, 1), nsmall = 1), "B"),
          ifelse(x >= 1e6, paste0("$", round(x / 1e6), "M"),
            paste0("$", round(x / 1e3), "K"))))
    }
  ) %>%
  # Format downloads columns
  fmt_number(
    columns = any_of(c("Downloads_2025", "Downloads_2024", "Downloads_2023")),
    decimals = 1,
    suffixing = TRUE
  ) %>%
  # Format MAU columns
  fmt_number(
    columns = any_of(c("MAU_2025", "MAU_2024", "MAU_2023")),
    decimals = 1,
    suffixing = TRUE
  ) %>%
  # Format growth as percent
  fmt_percent(
    columns = any_of(c("Revenue_Growth", "Downloads_Growth", "MAU_Growth")),
    decimals = 0,
    scale_values = FALSE
  ) %>%
  # Replace NA with dash
  sub_missing(
    columns = everything(),
    missing_text = "\u2014"
  ) %>%
  # Column labels
  cols_label(
    unified_app_name = "Game",
    subgenre = "Subgenre",
    subgenre_rank = "Sub-Genre Rank",
    Rank = "#",
    Revenue_Growth = "YoY %",
    Revenue_2025 = "2025",
    Revenue_2024 = "2024",
    Revenue_2023 = "2023",
    Downloads_Growth = "YoY %",
    Downloads_2025 = "2025",
    Downloads_2024 = "2024",
    Downloads_2023 = "2023",
    MAU_Growth = "YoY %",
    MAU_2025 = "2025",
    MAU_2024 = "2024",
    MAU_2023 = "2023"
  ) %>%
  # Spanners (YoY % to the left)
  tab_spanner(
    label = "Revenue (YTD)",
    columns = any_of(c("Revenue_Growth", "Revenue_2025", "Revenue_2024", "Revenue_2023"))
  ) %>%
  tab_spanner(
    label = "Downloads (YTD)",
    columns = any_of(c("Downloads_Growth", "Downloads_2025", "Downloads_2024", "Downloads_2023"))
  ) %>%
  tab_spanner(
    label = "Avg MAU (YTD)",
    columns = any_of(c("MAU_Growth", "MAU_2025", "MAU_2024", "MAU_2023"))
  ) %>%
  tab_source_note(
    source_note = "Game Economist Consulting | Sensor Tower Data"
  )

# Apply GEC theme if loaded
if (gec_theme_loaded && exists("theme_gec_gt")) {
  gt_table <- gt_table %>%
    theme_gec_gt() %>%
    # Style title (Monument Extended, uppercase, left-aligned)
    tab_style(
      style = list(
        cell_text(
          size = px(20),
          color = gec_colors$secondary,
          align = "left"
        ),
        cell_fill(color = "white")
      ),
      locations = cells_title("title")
    ) %>%
    # Style subtitle (League Spartan, left-aligned)
    tab_style(
      style = list(
        cell_text(
          size = px(14),
          color = gec_colors$grey_dark,
          align = "left"
        ),
        cell_fill(color = "white")
      ),
      locations = cells_title("subtitle")
    ) %>%
    # Color code growth columns (Revenue, Downloads, MAU)
    data_color(
      columns = any_of(c("Revenue_Growth", "Downloads_Growth", "MAU_Growth")),
      fn = scales::col_numeric(
        palette = c("#d73027", "#fee08b", "#1a9850"),
        domain = c(-100, 100),
        na.color = "transparent"
      )
    )
}

# Save table (PNG only) - wider to accommodate all columns including MAU
gtsave(gt_table, "output/lilith_portfolio_table.png", vwidth = 1600, vheight = 800)
message("  Saved table to output/lilith_portfolio_table.png")

message("\n=== Analysis Complete ===")
