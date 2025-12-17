#!/usr/bin/env Rscript

# Top 20 iOS Game Publishers - YTD vs 2024 Comparison Table
# With top game and revenue concentration
# Includes corporate consolidation for M&A (e.g., Scopely + Niantic)

suppressPackageStartupMessages({
  library(pacman)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::load_all("../../sensortowerR")
  p_load(dplyr, gt, gtExtras, scales, lubridate, glue, purrr)
})

# Load publisher consolidation mapping
source("publisher_consolidation.R")

# Load DoF theme if available
dof_theme_loaded <- tryCatch({
  source("../../dof-theme/dof_gt_theme.R", chdir = TRUE)
  TRUE
}, error = function(e) {
  message("Note: DoF theme not loaded")
  FALSE
})

message("=== Top 10 iOS Game Publishers Table ===\n")

# Define date ranges - use same YTD period for both years
ytd_start_2025 <- as.Date("2025-01-01")
ytd_end_2025 <- Sys.Date() - 1
ytd_days <- as.numeric(ytd_end_2025 - ytd_start_2025) + 1

# 2024 same YTD period (Jan 1 - Dec 16, 2024)
ytd_start_2024 <- as.Date("2024-01-01")
ytd_end_2024 <- as.Date(format(ytd_end_2025, "2024-%m-%d"))  # Same month/day as 2025

message(sprintf("2025 YTD: %s to %s (%d days)", ytd_start_2025, ytd_end_2025, ytd_days))
message(sprintf("2024 YTD: %s to %s (same period)\n", ytd_start_2024, ytd_end_2024))

# Fetch 2025 YTD publishers (need multiple calls due to limit of 10)
message("Fetching 2025 YTD data...")
publishers_2025_p1 <- st_top_publishers(
  measure = "revenue",
  os = "ios",
  category = 6014,
  time_range = "day",
  date = ytd_start_2025,
  end_date = ytd_end_2025,
  country = "WW",
  limit = 10,
  offset = 0,
  include_apps = TRUE
)

publishers_2025_p2 <- st_top_publishers(
  measure = "revenue",
  os = "ios",
  category = 6014,
  time_range = "day",
  date = ytd_start_2025,
  end_date = ytd_end_2025,
  country = "WW",
  limit = 10,
  offset = 10,
  include_apps = TRUE
)

publishers_2025 <- bind_rows(publishers_2025_p1, publishers_2025_p2) %>%
  mutate(rank_2025 = row_number())

# Fetch 2024 YTD publishers (same period for fair comparison)
message("\nFetching 2024 YTD data...")
publishers_2024_p1 <- st_top_publishers(
  measure = "revenue",
  os = "ios",
  category = 6014,
  time_range = "day",
  date = ytd_start_2024,
  end_date = ytd_end_2024,
  country = "WW",
  limit = 10,
  offset = 0,
  include_apps = TRUE
)

publishers_2024_p2 <- st_top_publishers(
  measure = "revenue",
  os = "ios",
  category = 6014,
  time_range = "day",
  date = ytd_start_2024,
  end_date = ytd_end_2024,
  country = "WW",
  limit = 10,
  offset = 10,
  include_apps = TRUE
)

publishers_2024 <- bind_rows(publishers_2024_p1, publishers_2024_p2) %>%
  mutate(rank_2024 = row_number())

# Extract top game and its revenue for each publisher (2025)
extract_top_game <- function(apps_df, publisher_revenue) {
  if (is.null(apps_df) || !is.data.frame(apps_df) || nrow(apps_df) == 0) {
    return(list(name = NA_character_, revenue = NA_real_, pct = NA_real_))
  }

  # Debug: show available columns for first call
  message("  Apps columns: ", paste(names(apps_df), collapse = ", "))

  # Find name column (could be app_name, name, unified_app_name, etc.)
  name_col <- if ("app_name" %in% names(apps_df)) "app_name"
              else if ("name" %in% names(apps_df)) "name"
              else if ("unified_app_name" %in% names(apps_df)) "unified_app_name"
              else NULL

  # Find revenue column
  rev_col <- if ("revenue_usd" %in% names(apps_df)) "revenue_usd"
             else if ("revenue_absolute" %in% names(apps_df)) "revenue_absolute"
             else NULL

  if (is.null(name_col)) {
    message("  Warning: No name column found")
    return(list(name = NA_character_, revenue = NA_real_, pct = NA_real_))
  }

  if (is.null(rev_col)) {
    return(list(name = apps_df[[name_col]][1], revenue = NA_real_, pct = NA_real_))
  }

  top_app <- apps_df %>%
    arrange(desc(.data[[rev_col]])) %>%
    slice(1)

  top_rev <- if (rev_col == "revenue_absolute") top_app[[rev_col]] / 100 else top_app[[rev_col]]

  list(
    name = top_app[[name_col]],
    revenue = top_rev,
    pct = if (!is.na(publisher_revenue) && publisher_revenue > 0) top_rev / publisher_revenue else NA_real_
  )
}

# Process 2025 data with top games
publishers_2025_processed <- publishers_2025 %>%
  rowwise() %>%
  mutate(
    top_game_info = list(extract_top_game(apps, revenue_usd)),
    top_game = top_game_info$name,
    top_game_revenue = top_game_info$revenue,
    top_game_pct = top_game_info$pct
  ) %>%
  ungroup() %>%
  select(publisher_id, publisher_name, revenue_2025 = revenue_usd, rank_2025,
         top_game, top_game_pct)

# Process 2024 data
publishers_2024_processed <- publishers_2024 %>%
  select(publisher_id, publisher_name, revenue_2024 = revenue_usd, rank_2024)

# Merge datasets
combined <- publishers_2025_processed %>%
  left_join(publishers_2024_processed, by = c("publisher_id", "publisher_name")) %>%
  mutate(
    # YoY change (same period comparison - no annualization needed)
    yoy_change = if_else(
      !is.na(revenue_2024) & revenue_2024 > 0,
      (revenue_2025 - revenue_2024) / revenue_2024,
      NA_real_
    ),
    # Rank change (positive = moved up)
    rank_change = if_else(!is.na(rank_2024), rank_2024 - rank_2025, NA_integer_)
  ) %>%
  arrange(rank_2025)

# Clean up publisher names for display
combined <- combined %>%
  mutate(
    publisher_display = case_when(
      grepl("Tencent", publisher_name) ~ "Tencent",
      grepl("网易", publisher_name) ~ "NetEase",
      publisher_name == "Century Games Pte. Ltd." ~ "Century Games",
      publisher_name == "FUNFLY PTE. LTD." ~ "FunPlus",
      publisher_name == "BANDAI NAMCO Entertainment Inc." ~ "Bandai Namco",
      TRUE ~ publisher_name
    )
  )

# Display results
message("\n=== TOP 20 iOS GAME PUBLISHERS ===\n")
for (i in 1:nrow(combined)) {
  row <- combined[i, ]
  yoy_str <- if (!is.na(row$yoy_change)) sprintf("(%+.0f%% YoY)", row$yoy_change * 100) else ""
  rank_str <- if (!is.na(row$rank_change) && row$rank_change != 0) {
    if (row$rank_change > 0) sprintf(" [+%d]", row$rank_change) else sprintf(" [%d]", row$rank_change)
  } else ""
  top_game_str <- if (!is.na(row$top_game)) sprintf(" | Top: %s (%.0f%%)", row$top_game, row$top_game_pct * 100) else ""
  message(sprintf("%2d. %s%s: $%.1fM YTD %s%s",
                  row$rank_2025, row$publisher_display, rank_str, row$revenue_2025 / 1e6, yoy_str, top_game_str))
}

# Create GT Table
message("\nCreating GT table...")

table_data <- combined %>%
  select(rank_2025, rank_change, publisher_display, revenue_2025, revenue_2024, yoy_change, top_game, top_game_pct)

publisher_table <- table_data %>%
  gt() %>%
  tab_header(
    title = "TOP 20 iOS GAME PUBLISHERS",
    subtitle = glue("YTD Revenue ({format(ytd_start_2025, '%b %d')} - {format(ytd_end_2025, '%b %d, %Y')}) vs Same Period 2024")
  ) %>%
  cols_label(
    rank_2025 = "#",
    rank_change = "",
    publisher_display = "Publisher",
    revenue_2025 = "2025 YTD",
    revenue_2024 = "2024",
    yoy_change = "YoY*",
    top_game = "Top Game",
    top_game_pct = "% of Rev"
  ) %>%
  # Format revenue columns
  fmt_currency(
    columns = c(revenue_2025, revenue_2024),
    currency = "USD",
    suffixing = TRUE,
    decimals = 0
  ) %>%
  # Format rank change with arrows
  text_transform(
    locations = cells_body(columns = rank_change),
    fn = function(x) {
      vals <- suppressWarnings(as.integer(x))
      sapply(vals, function(v) {
        if (is.na(v)) return("<span style='color:#888'>NEW</span>")
        if (v > 0) return(sprintf("<span style='color:#1a9850'>▲%d</span>", v))
        if (v < 0) return(sprintf("<span style='color:#d73027'>▼%d</span>", abs(v)))
        "<span style='color:#888'>—</span>"
      })
    }
  ) %>%
  # Format YoY as percentage with arrow
  text_transform(
    locations = cells_body(columns = yoy_change),
    fn = function(x) {
      vals <- suppressWarnings(as.numeric(x))
      sapply(vals, function(v) {
        if (is.na(v)) return("—")
        pct <- round(v * 100)
        if (pct > 0) return(sprintf("<span style='color:#1a9850'>▲ %d%%</span>", pct))
        if (pct < 0) return(sprintf("<span style='color:#d73027'>▼ %d%%</span>", abs(pct)))
        "0%"
      })
    }
  ) %>%
  fmt_percent(
    columns = top_game_pct,
    decimals = 0
  ) %>%
  # Handle missing values
  sub_missing(missing_text = "—") %>%
  # Revenue heatmap
  data_color(
    columns = revenue_2025,
    method = "numeric",
    palette = c("#fee8c8", "#fdbb84", "#e34a33"),
    domain = NULL
  ) %>%
  # Concentration heatmap (higher = more concentrated)
  data_color(
    columns = top_game_pct,
    method = "numeric",
    palette = c("#edf8fb", "#b2e2e2", "#66c2a4", "#238b45"),
    domain = c(0.2, 1),
    na_color = "#f7f7f7"
  ) %>%
  # Alignment
  cols_align(align = "center", columns = c(rank_2025, rank_change, yoy_change, top_game_pct)) %>%
  cols_align(align = "left", columns = c(publisher_display, top_game)) %>%
  cols_align(align = "right", columns = c(revenue_2025, revenue_2024)) %>%
  # Footnotes
  tab_footnote(
    footnote = "YoY compares same period (Jan 1 - Dec 16) for both years",
    locations = cells_column_labels(columns = yoy_change)
  ) %>%
  tab_source_note(
    source_note = glue("Source: Sensor Tower | iOS Worldwide | Games Category | Pulled {format(Sys.Date(), '%b %d, %Y')}")
  ) %>%
  # Table styling
  opt_table_font(
    font = list(
      google_font(name = "League Spartan"),
      default_fonts()
    )
  ) %>%
  tab_options(
    table.background.color = "#FFFFFF",
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "#1a1a1a",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "#1a1a1a",
    heading.background.color = "#FFFFFF",
    heading.title.font.size = px(22),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(13),
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "#1a1a1a",
    column_labels.background.color = "#f5f5f5",
    column_labels.font.weight = "bold",
    column_labels.font.size = px(12),
    column_labels.border.bottom.width = px(1),
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#fafafa",
    table.font.size = px(12),
    data_row.padding = px(8),
    source_notes.font.size = px(10),
    footnotes.font.size = px(10)
  )

# Apply DoF theme if loaded
if (dof_theme_loaded && exists("theme_dof_gt")) {
  publisher_table <- theme_dof_gt(publisher_table)
}

# Save outputs
output_dir <- file.path(getwd(), "output")
dir.create(output_dir, showWarnings = FALSE)

output_png <- file.path(output_dir, "top_ios_publishers_table.png")
gtsave(publisher_table, output_png, vwidth = 1200, vheight = 900)

message(sprintf("\nTable saved to: %s", output_png))

# Summary stats
total_2025 <- sum(combined$revenue_2025, na.rm = TRUE)
total_2024 <- sum(combined$revenue_2024, na.rm = TRUE)
message(sprintf("\nTop 10 Combined 2025 YTD: %s", dollar(total_2025, suffix = "B", scale = 1e-9, accuracy = 0.01)))
message(sprintf("Top 10 Combined 2024: %s", dollar(total_2024, suffix = "B", scale = 1e-9, accuracy = 0.01)))

# ============================================
# CONSOLIDATED VIEW (accounts for M&A)
# ============================================
message("\n\n=== CONSOLIDATED CORPORATE VIEW ===")
message("(Accounts for M&A: Scopely+Niantic, Take-Two+Peak, etc.)\n")

# Apply consolidation to 2025 data
consolidated_2025 <- combined %>%
  mutate(corporate_parent = consolidate_publisher(publisher_name)) %>%
  group_by(corporate_parent) %>%
  summarise(
    revenue_2025 = sum(revenue_2025, na.rm = TRUE),
    subsidiaries = paste(unique(publisher_display), collapse = " + "),
    num_entities = n(),
    top_game = top_game[which.max(top_game_pct)],
    top_game_pct = max(top_game_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(revenue_2025)) %>%
  mutate(rank_consolidated = row_number())

# Apply consolidation to 2024 data
consolidated_2024 <- publishers_2024 %>%
  mutate(corporate_parent = consolidate_publisher(publisher_name)) %>%
  group_by(corporate_parent) %>%
  summarise(
    revenue_2024 = sum(revenue_usd, na.rm = TRUE),
    .groups = "drop"
  )

# Merge
consolidated_combined <- consolidated_2025 %>%
  left_join(consolidated_2024, by = "corporate_parent") %>%
  mutate(
    # Same period comparison - no annualization needed
    yoy_change = if_else(
      !is.na(revenue_2024) & revenue_2024 > 0,
      (revenue_2025 - revenue_2024) / revenue_2024,
      NA_real_
    )
  )

# Show consolidated results
message("TOP 15 CORPORATE GROUPS (Consolidated):\n")
for (i in 1:min(15, nrow(consolidated_combined))) {
  row <- consolidated_combined[i, ]
  yoy_str <- if (!is.na(row$yoy_change)) sprintf(" (%+.0f%% YoY)", row$yoy_change * 100) else ""
  subs_str <- if (row$num_entities > 1) sprintf(" [%s]", row$subsidiaries) else ""
  message(sprintf("%2d. %s: $%.0fM%s%s",
                  i, row$corporate_parent, row$revenue_2025 / 1e6, yoy_str, subs_str))
}

# Create consolidated GT table
consolidated_table_data <- consolidated_combined %>%
  slice_head(n = 15) %>%
  select(rank_consolidated, corporate_parent, subsidiaries, num_entities,
         revenue_2025, revenue_2024, yoy_change, top_game, top_game_pct)

consolidated_table <- consolidated_table_data %>%
  gt() %>%
  tab_header(
    title = "TOP 15 iOS GAME PUBLISHERS (CONSOLIDATED)",
    subtitle = glue("Corporate Groups | YTD {format(ytd_start_2025, '%b %d')} - {format(ytd_end_2025, '%b %d, %Y')} vs Same Period 2024")
  ) %>%
  cols_label(
    rank_consolidated = "#",
    corporate_parent = "Corporate Group",
    subsidiaries = "Includes",
    num_entities = "Entities",
    revenue_2025 = "2025 YTD",
    revenue_2024 = "2024",
    yoy_change = "YoY*",
    top_game = "Top Game",
    top_game_pct = "% of Rev"
  ) %>%
  fmt_currency(
    columns = c(revenue_2025, revenue_2024),
    currency = "USD",
    suffixing = TRUE,
    decimals = 0
  ) %>%
  text_transform(
    locations = cells_body(columns = yoy_change),
    fn = function(x) {
      vals <- suppressWarnings(as.numeric(x))
      sapply(vals, function(v) {
        if (is.na(v)) return("—")
        pct <- round(v * 100)
        if (pct > 0) return(sprintf("<span style='color:#1a9850'>▲ %d%%</span>", pct))
        if (pct < 0) return(sprintf("<span style='color:#d73027'>▼ %d%%</span>", abs(pct)))
        "0%"
      })
    }
  ) %>%
  fmt_percent(columns = top_game_pct, decimals = 0) %>%
  sub_missing(missing_text = "—") %>%
  # Highlight rows with multiple entities
  tab_style(
    style = cell_fill(color = "#fff3cd"),
    locations = cells_body(rows = num_entities > 1)
  ) %>%
  data_color(
    columns = revenue_2025,
    method = "numeric",
    palette = c("#fee8c8", "#fdbb84", "#e34a33"),
    domain = NULL
  ) %>%
  cols_align(align = "center", columns = c(rank_consolidated, num_entities, yoy_change, top_game_pct)) %>%
  cols_align(align = "left", columns = c(corporate_parent, subsidiaries, top_game)) %>%
  tab_footnote(
    footnote = "Yellow rows = consolidated from multiple publisher entities",
    locations = cells_column_labels(columns = num_entities)
  ) %>%
  tab_source_note(
    source_note = glue("Source: Sensor Tower + Publisher M&A mapping | iOS WW | {format(Sys.Date(), '%b %d, %Y')}")
  ) %>%
  opt_table_font(font = list(google_font(name = "League Spartan"), default_fonts())) %>%
  tab_options(
    table.background.color = "#FFFFFF",
    table.border.top.width = px(3),
    table.border.top.color = "#1a1a1a",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "#1a1a1a",
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(12),
    column_labels.background.color = "#f5f5f5",
    column_labels.font.weight = "bold",
    row.striping.include_table_body = TRUE,
    table.font.size = px(11),
    data_row.padding = px(6)
  )

# Save consolidated table
output_consolidated <- file.path(output_dir, "top_ios_publishers_consolidated.png")
gtsave(consolidated_table, output_consolidated, vwidth = 1300, vheight = 700)
message(sprintf("\nConsolidated table saved to: %s", output_consolidated))
