#!/usr/bin/env Rscript

# Lilith Games Portfolio Analysis - Simple Version
# Uses st_unified_sales_report() for proper regional SKU aggregation

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(gt)
  library(lubridate)
  library(glue)
})

# Load sensortowerR
devtools::load_all("~/Documents/vibe_coding_projects/sensortowerR")

message("=== Lilith Games Portfolio Analysis ===\n")

# Create output directory
dir.create("output", showWarnings = FALSE)

# Load GEC theme
gec_theme_loaded <- tryCatch({
  source("~/Documents/vibe_coding_projects/gec-theme/gec_gt_theme.R", chdir = TRUE)
  TRUE
}, error = function(e) FALSE)

# ============================================================================
# STEP 1: Get Lilith Publisher and Apps
# ============================================================================
message("Step 1: Finding Lilith publisher...")

publisher_info <- st_app_info("Lilith", entity_type = "publisher")
stopifnot("Publisher not found" = nrow(publisher_info) > 0)

publisher_id <- publisher_info$unified_publisher_id[1]
message("  Found: ", publisher_info$unified_publisher_name[1])

message("\nStep 2: Fetching publisher apps (with regional SKU aggregation)...")
apps <- st_publisher_apps(
  unified_id = publisher_id,
  aggregate_related = TRUE,  # Aggregates regional SKUs
  verbose = TRUE
)

stopifnot("No apps found" = nrow(apps) > 0)
message("  Found ", nrow(apps), " unique apps")

# ============================================================================
# STEP 2: Fetch Sales Data Using Unified API (aggregates regional SKUs)
# ============================================================================
message("\nStep 3: Fetching sales data (st_unified_sales_report)...")

end_date <- floor_date(Sys.Date(), "month") - days(1)
start_date <- as.Date("2023-01-01")
current_month <- month(end_date)

# Fetch sales data - st_unified_sales_report aggregates all regional SKUs
sales_data <- st_unified_sales_report(
  unified_app_id = apps$unified_app_id,
  countries = "WW",
  start_date = start_date,
  end_date = end_date,
  date_granularity = "monthly",
  verbose = TRUE
)

# VALIDATION: Check results
stopifnot("No sales data returned" = !is.null(sales_data) && nrow(sales_data) > 0)
message("\n  VALIDATION: Received ", nrow(sales_data), " sales records")
message("  Apps with data: ", n_distinct(sales_data$unified_app_id))
message("  Date range: ", min(sales_data$date), " to ", max(sales_data$date))
message("  Total revenue: $", format(sum(sales_data$revenue, na.rm = TRUE), big.mark = ","))

# ============================================================================
# STEP 3: Fetch MAU Data
# ============================================================================
message("\nStep 4: Fetching MAU data...")

mau_data <- tryCatch({
  st_batch_metrics(
    os = "unified",
    app_list = apps$unified_app_id,
    metrics = "mau",
    date_range = list(start_date = start_date, end_date = end_date),
    countries = "WW",
    granularity = "monthly",
    parallel = FALSE
  )
}, error = function(e) {
  message("  Warning: Could not fetch MAU: ", e$message)
  NULL
})

if (!is.null(mau_data) && nrow(mau_data) > 0) {
  message("  VALIDATION: Received ", nrow(mau_data), " MAU records")
  message("  Apps with MAU data: ", n_distinct(mau_data$original_id))
}

# ============================================================================
# STEP 4: Fetch Rankings Data
# ============================================================================
message("\nStep 5: Fetching rankings data...")

rank_data <- tryCatch({
  top_games <- st_top_charts(
    measure = "revenue",
    os = "unified",
    category = "6014",  # Games
    regions = "WW",
    time_range = "month",
    date = format(floor_date(Sys.Date(), "month") - days(1), "%Y-%m-%d"),
    limit = 1500
  )

  top_games %>%
    mutate(global_rank = row_number()) %>%
    group_by(`aggregate_tags.Game Sub-genre`) %>%
    mutate(subgenre_rank = row_number()) %>%
    ungroup() %>%
    select(unified_app_id, subgenre = `aggregate_tags.Game Sub-genre`, subgenre_rank)
}, error = function(e) {
  message("  Warning: Could not fetch rankings: ", e$message)
  NULL
})

if (!is.null(rank_data)) {
  message("  VALIDATION: Rankings for ", nrow(rank_data), " games")
}

# ============================================================================
# STEP 5: Process and Aggregate Data
# ============================================================================
message("\nStep 6: Processing data...")

# Get app names using the helper function (handles canonical ID mapping)
app_names <- st_get_app_names(apps)

sales_with_names <- sales_data %>%
  left_join(app_names, by = "unified_app_id")

# Aggregate sales by year (YTD comparison)
sales_summary <- sales_with_names %>%
  mutate(Year = year(date), Month = month(date)) %>%
  filter(Month <= current_month) %>%
  group_by(unified_app_id, app_name, Year) %>%
  summarise(
    revenue = sum(revenue, na.rm = TRUE),
    downloads = sum(downloads, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = c(revenue, downloads),
    names_sep = "_",
    values_fill = 0
  )

# Process MAU data if available
if (!is.null(mau_data) && nrow(mau_data) > 0) {
  mau_summary <- mau_data %>%
    mutate(Year = year(date), Month = month(date)) %>%
    filter(Month <= current_month) %>%
    group_by(original_id, Year) %>%
    summarise(mau = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = Year,
      values_from = mau,
      names_prefix = "mau_",
      values_fill = 0
    ) %>%
    rename(unified_app_id = original_id)

  # Join MAU with sales
  portfolio_summary <- sales_summary %>%
    left_join(mau_summary, by = "unified_app_id")
} else {
  portfolio_summary <- sales_summary
}

# Join with rankings data if available
if (!is.null(rank_data)) {
  portfolio_summary <- portfolio_summary %>%
    left_join(rank_data, by = "unified_app_id")
}

# Filter to significant apps and calculate YoY
table_data <- portfolio_summary %>%
  filter(revenue_2025 > 100000 | revenue_2024 > 1000000) %>%
  mutate(
    revenue_yoy = ifelse(revenue_2024 > 0,
                         round((revenue_2025 - revenue_2024) / revenue_2024 * 100, 0),
                         NA_real_),
    downloads_yoy = ifelse(downloads_2024 > 0,
                           round((downloads_2025 - downloads_2024) / downloads_2024 * 100, 0),
                           NA_real_)
  )

# Add MAU YoY if available
if ("mau_2024" %in% names(table_data) && "mau_2025" %in% names(table_data)) {
  table_data <- table_data %>%
    mutate(mau_yoy = ifelse(mau_2024 > 0,
                            round((mau_2025 - mau_2024) / mau_2024 * 100, 0),
                            NA_real_))
}

table_data <- table_data %>%
  arrange(desc(revenue_2025)) %>%
  mutate(rank = row_number())

# Translate Chinese names
table_data <- table_data %>%
  mutate(app_name = case_when(
    app_name == "剑与远征 - AFK" ~ "AFK Arena (CN)",
    app_name == "小冰冰传奇-官方怀旧服" ~ "Soul Clash (Retro)",
    TRUE ~ app_name
  ))

# VALIDATION: Check processed data
stopifnot("No apps passed filters" = nrow(table_data) > 0)
message("  Apps in final table: ", nrow(table_data))

# Create portfolio total row
portfolio_total <- table_data %>%
  summarise(
    rank = NA_integer_,
    app_name = "PORTFOLIO TOTAL",
    across(starts_with("revenue_20"), ~ sum(., na.rm = TRUE)),
    across(starts_with("downloads_20"), ~ sum(., na.rm = TRUE)),
    across(starts_with("mau_20"), ~ sum(., na.rm = TRUE))
  ) %>%
  mutate(
    revenue_yoy = round((revenue_2025 - revenue_2024) / revenue_2024 * 100, 0),
    downloads_yoy = round((downloads_2025 - downloads_2024) / downloads_2024 * 100, 0)
  )

# Add MAU YoY to portfolio total if available
if ("mau_2024" %in% names(portfolio_total) && "mau_2025" %in% names(portfolio_total)) {
  portfolio_total <- portfolio_total %>%
    mutate(mau_yoy = round((mau_2025 - mau_2024) / mau_2024 * 100, 0))
}

# Combine with portfolio total first
table_data <- bind_rows(portfolio_total, table_data %>% select(-unified_app_id))

message("  Final table rows: ", nrow(table_data))

# ============================================================================
# STEP 5: Create GT Table
# ============================================================================
message("\nStep 6: Creating GT table...")

# Select columns - include subgenre/rank and MAU if available
has_mau <- "mau_2025" %in% names(table_data)
has_subgenre <- "subgenre" %in% names(table_data)

select_cols <- c("rank", "app_name")

if (has_subgenre) {
  select_cols <- c(select_cols, "subgenre", "subgenre_rank")
}

select_cols <- c(select_cols,
                 "revenue_yoy", "revenue_2025", "revenue_2024", "revenue_2023",
                 "downloads_yoy", "downloads_2025", "downloads_2024", "downloads_2023")

if (has_mau) {
  select_cols <- c(select_cols, "mau_yoy", "mau_2025", "mau_2024", "mau_2023")
}

gt_table <- table_data %>%
  select(any_of(select_cols)) %>%
  gt() %>%
  tab_header(
    title = md("**LILITH GAMES PORTFOLIO ANALYSIS**"),
    subtitle = glue("YTD Revenue, Downloads{if(has_mau) ' & MAU' else ''} (Jan-{month.abb[current_month]} 2025 vs 2024)")
  ) %>%
  fmt(
    columns = starts_with("revenue_20"),
    fns = function(x) {
      ifelse(is.na(x) | x == 0, "\u2014",
             ifelse(x >= 1e9, paste0("$", format(round(x / 1e9, 1), nsmall = 1), "B"),
                    ifelse(x >= 1e6, paste0("$", round(x / 1e6), "M"),
                           paste0("$", round(x / 1e3), "K"))))
    }
  ) %>%
  fmt(
    columns = starts_with("downloads_20"),
    fns = function(x) {
      ifelse(is.na(x) | x < 1, "\u2014",
             ifelse(x >= 1e9, paste0(format(round(x / 1e9, 1), nsmall = 1), "B"),
                    ifelse(x >= 1e6, paste0(format(round(x / 1e6, 1), nsmall = 1), "M"),
                           ifelse(x >= 1e3, paste0(round(x / 1e3), "K"),
                                  as.character(round(x))))))
    }
  ) %>%
  fmt(
    columns = starts_with("mau_20"),
    fns = function(x) {
      ifelse(is.na(x) | x < 1, "\u2014",
             ifelse(x >= 1e9, paste0(format(round(x / 1e9, 1), nsmall = 1), "B"),
                    ifelse(x >= 1e6, paste0(format(round(x / 1e6, 1), nsmall = 1), "M"),
                           ifelse(x >= 1e3, paste0(round(x / 1e3), "K"),
                                  as.character(round(x))))))
    }
  ) %>%
  fmt(
    columns = ends_with("_yoy"),
    fns = function(x) ifelse(is.na(x), "\u2014", paste0(round(x), "%"))
  ) %>%
  sub_missing(columns = everything(), missing_text = "\u2014") %>%
  cols_label(
    rank = "#",
    app_name = "Game",
    revenue_yoy = "YoY %",
    revenue_2025 = "2025", revenue_2024 = "2024", revenue_2023 = "2023",
    downloads_yoy = "YoY %",
    downloads_2025 = "2025", downloads_2024 = "2024", downloads_2023 = "2023"
  ) %>%
  tab_spanner(label = "Revenue (YTD)", columns = c(revenue_yoy, revenue_2025, revenue_2024, revenue_2023)) %>%
  tab_spanner(label = "Downloads (YTD)", columns = c(downloads_yoy, downloads_2025, downloads_2024, downloads_2023)) %>%
  tab_source_note("Game Economist Consulting | Sensor Tower Data")

# Add subgenre labels if available
if (has_subgenre) {
  gt_table <- gt_table %>%
    cols_label(
      subgenre = "Subgenre",
      subgenre_rank = "Rank"
    )
}

# Add MAU labels and spanner if available
if (has_mau) {
  gt_table <- gt_table %>%
    cols_label(
      mau_yoy = "YoY %",
      mau_2025 = "2025", mau_2024 = "2024", mau_2023 = "2023"
    ) %>%
    tab_spanner(label = "Avg MAU (YTD)", columns = c(mau_yoy, mau_2025, mau_2024, mau_2023))
}

# Apply GEC theme if loaded
if (gec_theme_loaded && exists("theme_gec_gt")) {
  gt_table <- gt_table %>%
    theme_gec_gt() %>%
    tab_style(
      style = list(
        cell_text(size = px(20), color = gec_colors$secondary, align = "left"),
        cell_fill(color = "white")
      ),
      locations = cells_title("title")
    ) %>%
    tab_style(
      style = list(
        cell_text(size = px(14), color = gec_colors$grey_dark, align = "left"),
        cell_fill(color = "white")
      ),
      locations = cells_title("subtitle")
    ) %>%
    data_color(
      columns = ends_with("_yoy"),
      fn = scales::col_numeric(
        palette = c("#d73027", "#fee08b", "#1a9850"),
        domain = c(-100, 100),
        na.color = "transparent"
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold", size = px(13)),
        cell_fill(color = "#e8e8e8"),
        cell_borders(sides = c("top", "bottom"), color = gec_colors$secondary, weight = px(2))
      ),
      locations = cells_body(rows = 1)
    )
}

# Save (adjust width based on columns included)
output_file <- "output/lilith_portfolio_table.png"
img_width <- 1400
if (has_subgenre) img_width <- img_width + 250
if (has_mau) img_width <- img_width + 350
gtsave(gt_table, output_file, vwidth = img_width, vheight = 600)
message("\nSaved: ", output_file)

message("\n=== Done ===")
