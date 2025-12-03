#!/usr/bin/env Rscript

# Moonton Portfolio Visualizations
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
  library(httr)
  library(tibble)
})

message("=== Moonton Portfolio Visualizations ===\n")

# Load environment variables from .Renviron
readRenviron("~/.Renviron")

# Create output directory
dir.create("output", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# Load Deconstructor of Fun GT theme from vibe_coding_projects root
dof_theme_loaded <- tryCatch({
  source("~/Documents/vibe_coding_projects/dof-theme/dof_gt_theme.R", chdir = TRUE)
  TRUE
}, error = function(e) {
  message("Note: DoF theme not loaded (", e$message, ")")
  FALSE
})

# ============================================================================
# STEP 1: Load or Fetch Data
# ============================================================================
message("Step 1: Loading/Fetching Data...")

# Helper function to fetch unified sales data directly from API
# This properly aggregates ALL app IDs within a unified_app_id
fetch_unified_sales_data <- function(unified_app_id, start_date, end_date,
                                      auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")) {
  base_url <- "https://api.sensortower.com/v1/unified/sales_report_estimates"
  url <- glue("{base_url}?auth_token={auth_token}&app_ids={unified_app_id}&date_granularity=monthly&start_date={start_date}&end_date={end_date}&countries=WW")

  response <- httr::GET(url)
  if (httr::status_code(response) != 200) {
    stop("API request failed with status ", httr::status_code(response))
  }

  data <- httr::content(response, as = "parsed")

  if (length(data) == 0) return(NULL)

  # Convert to tibble
  result <- dplyr::bind_rows(lapply(data, function(x) {
    tibble::tibble(
      date = as.Date(x$date),
      country = x$country,
      revenue = x$unified_revenue / 100,  # Convert cents to dollars
      downloads = x$unified_units,
      unified_app_id = x$app_id
    )
  }))

  return(result)
}

# Define the TRUE unified_app_ids for Moonton's core portfolio
# These are the correct unified IDs that aggregate all regional publisher SKUs
moonton_portfolio <- data.frame(
  unified_app_id = c(
    "57955d280211a6718a000002",  # Mobile Legends: Bang Bang
    "67ec0bf3e540b65904256cc4",  # Watcher of Realms
    "67d8f73c98bef0c8b7282e47",  # Silver & Blood: Vampire RPG
    "6723cd013471a0a27dd43871",  # ACECRAFT
    "5c78999dba94be159497a35e",  # Mobile Legends: Adventure
    "671baf714bb86008d2263ce5",  # Magic Chess: Go Go
    "5d09e4b03ea9836770c1cf5a"   # One Punch Man: The Strongest
  ),
  unified_app_name = c(
    "Mobile Legends: Bang Bang",
    "Watcher of Realms",
    "Silver & Blood: Vampire RPG",
    "ACECRAFT",
    "Mobile Legends: Adventure",
    "Magic Chess: Go Go",
    "One Punch Man: The Strongest"
  ),
  stringsAsFactors = FALSE
)

main_apps <- moonton_portfolio
app_ids <- main_apps$unified_app_id
end_date <- floor_date(Sys.Date(), "month") - days(1)
start_date <- as.Date("2019-01-01")

# Check if we have cached data
sales_file <- "data/moonton_sales_data.rds"
mau_file <- "data/moonton_mau_data.rds"

# Force re-fetch if cache exists but we want fresh data from unified API
force_refetch <- TRUE  # Set to TRUE to refresh data with proper unified aggregation

if (file.exists(sales_file) && !force_refetch) {
  message("  Loading cached sales data...")
  sales_data <- readRDS(sales_file)
} else {
  message("  Fetching sales data using unified API (properly aggregates ALL regional SKUs)...")
  sales_data_list <- list()

  for (i in seq_along(app_ids)) {
    app_id <- app_ids[i]
    app_name <- main_apps$unified_app_name[i]
    message(glue("    [{i}/{length(app_ids)}] {app_name}"))

    tryCatch({
      # Use our custom function that calls the unified API directly
      result <- fetch_unified_sales_data(
        unified_app_id = app_id,
        start_date = start_date,
        end_date = end_date
      )

      if (!is.null(result) && nrow(result) > 0) {
        result$app_name <- app_name
        sales_data_list[[app_id]] <- result
      }
    }, error = function(e) {
      message(glue("      Warning: {e$message}"))
    })
    Sys.sleep(0.3)
  }

  if (length(sales_data_list) > 0) {
    sales_data <- bind_rows(sales_data_list)
    saveRDS(sales_data, sales_file)
  }
}

# Fetch MAU data
if (file.exists(mau_file)) {
  message("  Loading cached MAU data...")
  mau_data <- readRDS(mau_file)
} else {
  message("  Fetching MAU data...")
  mau_list <- list()

  for (i in seq_along(app_ids)) {
    app_id <- app_ids[i]
    app_name <- main_apps$unified_app_name[i]
    message(glue("    [{i}/{length(app_ids)}] {app_name}"))

    tryCatch({
      result <- st_batch_metrics(
        os = "ios",
        app_list = app_id,
        metrics = "mau",
        date_range = list(start_date = start_date, end_date = end_date),
        countries = "WW",
        granularity = "monthly",
        parallel = FALSE,
        verbose = FALSE
      )

      if (!is.null(result) && nrow(result) > 0 && "value" %in% names(result)) {
        result$app_name <- app_name
        result$unified_app_id <- app_id
        mau_list[[app_id]] <- result
      }
    }, error = function(e) {
      message(glue("      Warning: {e$message}"))
    })
    Sys.sleep(0.3)
  }

  if (length(mau_list) > 0) {
    mau_data <- bind_rows(mau_list)
    saveRDS(mau_data, mau_file)
  } else {
    mau_data <- NULL
  }
}

message(glue("  Sales data: {nrow(sales_data)} rows"))
if (!is.null(mau_data)) {
  message(glue("  MAU data: {nrow(mau_data)} rows"))
}

# ============================================================================
# STEP 1b: Fetch Global and Sub-Genre Rankings
# ============================================================================
message("\nStep 1b: Fetching Global Revenue Rankings...")

ranks_file <- "data/moonton_ranks.rds"
if (file.exists(ranks_file) && as.Date(file.mtime(ranks_file)) >= Sys.Date() - 1) {
  message("  Loading cached rankings data...")
  rank_data <- readRDS(ranks_file)
} else {
  message("  Fetching top charts for rankings...")
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

  # Save the full dataset
  saveRDS(top_games, "data/top_games_full.rds")

  # Extract Moonton games rankings
  rank_data <- top_games %>%
    select(unified_app_id, unified_app_name, global_rank, subgenre_rank,
           subgenre = `aggregate_tags.Game Sub-genre`)

  saveRDS(rank_data, ranks_file)
  message("  Saved rankings data")
}

message(glue("  Rankings data: {nrow(rank_data)} apps"))

# ============================================================================
# STEP 2: Create Portfolio Summary Table
# ============================================================================
message("\nStep 2: Creating Portfolio Summary Table...")

# Aggregate data by app for 2023, 2024, and 2025 YTD
current_month <- month(Sys.Date()) - 1  # Use last complete month

# Create revenue summary
revenue_summary <- sales_data %>%
  mutate(Year = year(date), Month = month(date)) %>%
  filter(Year >= 2023) %>%
  # YTD filtering: Jan to current_month for each year
  filter(Month <= current_month | Year < year(Sys.Date())) %>%
  group_by(app_name, Year) %>%
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
    group_by(app_name, Year) %>%
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
    left_join(mau_wide, by = "app_name")
} else {
  table_data <- revenue_wide
}

# Clean up app names for display
# With the new hardcoded portfolio, names are already clean
table_data <- table_data %>%
  mutate(
    display_name = case_when(
      grepl("Mobile Legends: Bang Bang", app_name) ~ "Mobile Legends: Bang Bang",
      grepl("Mobile Legends: Adventure", app_name) ~ "Mobile Legends: Adventure",
      grepl("Watcher of Realms", app_name) ~ "Watcher of Realms",
      grepl("Magic Chess", app_name) ~ "Magic Chess: Go Go",
      grepl("ACECRAFT", app_name) ~ "ACECRAFT",
      grepl("One Punch Man", app_name) ~ "One Punch Man: The Strongest",
      grepl("Silver.*Blood", app_name) ~ "Silver & Blood: Vampire RPG",
      TRUE ~ app_name
    )
  ) %>%
  filter(Revenue_2025 > 0)  # Filter to apps with 2025 activity

# Merge with ranking data
# Create a lookup for display names to unified app names in rank_data
rank_lookup <- rank_data %>%
  mutate(
    display_name = case_when(
      grepl("Mobile Legends: Bang Bang", unified_app_name) ~ "Mobile Legends: Bang Bang",
      grepl("Mobile Legends: Adventure", unified_app_name) ~ "Mobile Legends: Adventure",
      grepl("Watcher of Realms", unified_app_name) ~ "Watcher of Realms",
      grepl("Magic Chess", unified_app_name) ~ "Magic Chess: Go Go",
      grepl("Acecraft|ACECRAFT", unified_app_name, ignore.case = TRUE) ~ "ACECRAFT",
      grepl("One Punch Man", unified_app_name) ~ "One Punch Man: The Strongest",
      grepl("Silver.*Blood", unified_app_name, ignore.case = TRUE) ~ "Silver & Blood: Vampire RPG",
      TRUE ~ unified_app_name
    )
  ) %>%
  select(display_name, global_rank, subgenre_rank, subgenre) %>%
  distinct(display_name, .keep_all = TRUE)

# Join ranking data
table_data <- table_data %>%
  left_join(rank_lookup, by = "display_name")

# Calculate YoY growth
table_data <- table_data %>%
  mutate(
    Revenue_Growth = ifelse(Revenue_2024 > 0,
                            round((Revenue_2025 - Revenue_2024) / Revenue_2024 * 100, 0),
                            NA),
    Downloads_Growth = ifelse(Downloads_2024 > 0,
                              round((Downloads_2025 - Downloads_2024) / Downloads_2024 * 100, 0),
                              NA)
  ) %>%
  arrange(desc(Revenue_2025))  # All 7 games in portfolio

# Add rank
table_data <- table_data %>%
  mutate(Rank = row_number())

# Add MAU growth if MAU columns exist
if ("MAU_2024" %in% names(table_data) && "MAU_2025" %in% names(table_data)) {
  table_data <- table_data %>%
    mutate(
      MAU_Growth = ifelse(MAU_2024 > 0,
                          round((MAU_2025 - MAU_2024) / MAU_2024 * 100, 0),
                          NA)
    )
}

# Create portfolio total row
portfolio_total <- table_data %>%
  summarise(
    display_name = "Portfolio Total",
    Rank = NA,
    Revenue_2023 = sum(Revenue_2023, na.rm = TRUE),
    Revenue_2024 = sum(Revenue_2024, na.rm = TRUE),
    Revenue_2025 = sum(Revenue_2025, na.rm = TRUE),
    Downloads_2023 = sum(Downloads_2023, na.rm = TRUE),
    Downloads_2024 = sum(Downloads_2024, na.rm = TRUE),
    Downloads_2025 = sum(Downloads_2025, na.rm = TRUE)
  ) %>%
  mutate(
    Revenue_Growth = round((Revenue_2025 - Revenue_2024) / Revenue_2024 * 100, 0),
    Downloads_Growth = round((Downloads_2025 - Downloads_2024) / Downloads_2024 * 100, 0)
  )

# Add MAU totals if available
if ("MAU_2024" %in% names(table_data)) {
  portfolio_total <- portfolio_total %>%
    mutate(
      MAU_2023 = sum(table_data$MAU_2023, na.rm = TRUE),
      MAU_2024 = sum(table_data$MAU_2024, na.rm = TRUE),
      MAU_2025 = sum(table_data$MAU_2025, na.rm = TRUE),
      MAU_Growth = round((MAU_2025 - MAU_2024) / MAU_2024 * 100, 0)
    )
}

# Combine data for table - include sub-genre rank and MAU if available
select_cols <- c("display_name", "Rank", "subgenre", "subgenre_rank",
                 "Revenue_2023", "Revenue_2024", "Revenue_2025", "Revenue_Growth",
                 "Downloads_2023", "Downloads_2024", "Downloads_2025", "Downloads_Growth")

if ("MAU_2025" %in% names(table_data)) {
  select_cols <- c(select_cols, "MAU_2023", "MAU_2024", "MAU_2025", "MAU_Growth")
}

gt_data <- bind_rows(portfolio_total, table_data %>% select(any_of(select_cols)))

# Create GT table
portfolio_table <- gt_data %>%
  gt() %>%
  tab_header(
    title = "Moonton Portfolio Performance",
    subtitle = glue("Year-to-Date Metrics (January - {month.name[current_month]} 2025)")
  ) %>%
  # Format revenue columns
  fmt(
    columns = c(Revenue_2023, Revenue_2024, Revenue_2025),
    fns = function(x) {
      ifelse(is.na(x) | x == 0, "—",
        ifelse(x >= 1e9, paste0("$", format(round(x / 1e9, 2), nsmall = 2), "B"),
          ifelse(x >= 1e6, paste0("$", round(x / 1e6), "M"),
            paste0("$", round(x / 1e3), "K"))))
    }
  ) %>%
  # Format download columns
  fmt_number(
    columns = c(Downloads_2023, Downloads_2024, Downloads_2025),
    decimals = 0,
    suffixing = TRUE
  ) %>%
  # Format MAU columns if they exist
  fmt_number(
    columns = any_of(c("MAU_2023", "MAU_2024", "MAU_2025")),
    decimals = 0,
    suffixing = TRUE
  ) %>%
  # Replace NA with dash
  sub_missing(columns = everything(), missing_text = "—") %>%
  # Column labels - base labels (only include MAU labels if columns exist)
  cols_label(
    display_name = "Game",
    Rank = "#",
    subgenre = "Sub-Genre",
    subgenre_rank = "Genre #",
    Revenue_2025 = "2025",
    Revenue_2024 = "2024",
    Revenue_2023 = "2023",
    Revenue_Growth = "YoY",
    Downloads_2025 = "2025",
    Downloads_2024 = "2024",
    Downloads_2023 = "2023",
    Downloads_Growth = "YoY"
  ) %>%
  # Reorder columns to put sub-genre info next to rank
  cols_move(
    columns = c(subgenre, subgenre_rank),
    after = Rank
  ) %>%
  # Spanners
  tab_spanner(label = "Revenue (YTD)", columns = c(Revenue_2025, Revenue_2024, Revenue_2023, Revenue_Growth)) %>%
  tab_spanner(label = "Downloads (YTD)", columns = c(Downloads_2025, Downloads_2024, Downloads_2023, Downloads_Growth)) %>%
  tab_spanner(label = "Avg MAU (YTD)", columns = any_of(c("MAU_2025", "MAU_2024", "MAU_2023", "MAU_Growth"))) %>%
  # Style the portfolio total row
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(13)),
      cell_fill(color = "#e8e8e8"),
      cell_borders(sides = c("top", "bottom"), color = "#1a1a1a", weight = px(2))
    ),
    locations = cells_body(rows = 1)
  ) %>%
  # Color code YoY growth
  text_transform(
    locations = cells_body(columns = any_of(c("Revenue_Growth", "Downloads_Growth", "MAU_Growth"))),
    fn = function(x) {
      vals <- suppressWarnings(as.numeric(x))
      lapply(vals, function(v) {
        if (is.na(v)) return(html("—"))
        if (v > 0) return(html(sprintf("<span style='color:#1a9850'>▲ %d%%</span>", round(v))))
        if (v < 0) return(html(sprintf("<span style='color:#d73027'>▼ %d%%</span>", abs(round(v)))))
        html("0%")
      })
    }
  ) %>%
  # Notes
  tab_source_note("Source: Sensor Tower | Data as of December 2025") %>%
  tab_source_note("Revenue includes iOS and Android combined | China Android may be undercounted")

# Apply DOF theme (with bold title as default)
if (dof_theme_loaded && exists("theme_dof_gt")) {
  portfolio_table <- theme_dof_gt(portfolio_table)
} else {
  # Fallback inline styling if theme not loaded
  portfolio_table <- portfolio_table %>%
    opt_table_font(
      font = list(google_font(name = "Inter"), default_fonts())
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
      column_labels.background.color = "#f5f5f5",
      column_labels.font.weight = "bold",
      column_labels.font.size = px(11),
      column_labels.border.top.style = "solid",
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#1a1a1a",
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#fafafa",
      table.font.size = px(11),
      data_row.padding = px(6),
      source_notes.font.size = px(10),
      source_notes.background.color = "#f5f5f5"
    )
}

# Save the table
gtsave(portfolio_table, "output/moonton_portfolio_table.png", vwidth = 1800, vheight = 900)
message("  Saved: output/moonton_portfolio_table.png")

# ============================================================================
# STEP 3: Create Time Series Charts BY INDIVIDUAL TOP GAMES
# ============================================================================
message("\nStep 3: Creating Time Series Charts by Game...")

# Define top games to include (ranked by 2025 YTD revenue)
top_games <- c(
  "Mobile Legends: Bang Bang",
  "Watcher of Realms",
  "Mobile Legends: Adventure",
  "ACECRAFT"
)

# Clean up app names for display and filter to top games
game_data <- sales_data %>%
  mutate(
    display_name = case_when(
      grepl("Mobile Legends: Bang Bang", app_name) ~ "Mobile Legends: Bang Bang",
      grepl("Mobile Legends: Adventure", app_name) ~ "Mobile Legends: Adventure",
      grepl("Watcher of Realms", app_name) ~ "Watcher of Realms",
      grepl("ACECRAFT", app_name) ~ "ACECRAFT",
      TRUE ~ app_name
    )
  ) %>%
  filter(display_name %in% top_games) %>%
  group_by(date, display_name) %>%
  summarise(
    Revenue = sum(revenue, na.rm = TRUE),
    Downloads = sum(downloads, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(date >= as.Date("2020-01-01"))

# Add MAU data by game if available
if (!is.null(mau_data) && nrow(mau_data) > 0) {
  mau_by_game <- mau_data %>%
    mutate(
      display_name = case_when(
        grepl("Mobile Legends: Bang Bang", app_name) ~ "Mobile Legends: Bang Bang",
        grepl("Mobile Legends: Adventure", app_name) ~ "Mobile Legends: Adventure",
        grepl("Watcher of Realms", app_name) ~ "Watcher of Realms",
        grepl("ACECRAFT", app_name) ~ "ACECRAFT",
        TRUE ~ app_name
      )
    ) %>%
    filter(display_name %in% top_games) %>%
    group_by(date, display_name) %>%
    summarise(MAU = sum(value, na.rm = TRUE), .groups = "drop")

  game_data <- game_data %>%
    left_join(mau_by_game, by = c("date", "display_name"))
}

# Set factor order for consistent colors
game_data$display_name <- factor(game_data$display_name, levels = top_games)

# Define color palette for games
game_colors <- c(
  "Mobile Legends: Bang Bang" = "#E94560",
  "Watcher of Realms" = "#4575b4",
  "Mobile Legends: Adventure" = "#FFC857",
  "ACECRAFT" = "#1a9850"
)

# Chart 1: Revenue by Game over time
p_revenue <- ggplot(game_data, aes(x = date, y = Revenue / 1e6, color = display_name)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = game_colors) +
  scale_y_continuous(labels = dollar_format(suffix = "M"), expand = c(0.02, 0)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y", expand = c(0.02, 0)) +
  labs(
    title = "Moonton Top Games Monthly Revenue",
    subtitle = "Worldwide iOS + Android revenue (USD)",
    x = NULL,
    y = "Revenue",
    color = NULL,
    caption = "Source: Sensor Tower"
  ) +
  theme_dof() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 1))

ggsave("output/moonton_revenue_chart.png", p_revenue, width = 12, height = 7, dpi = 150, bg = "white")
message("  Saved: output/moonton_revenue_chart.png")

# Chart 2: Downloads by Game over time
p_downloads <- ggplot(game_data, aes(x = date, y = Downloads / 1e6, color = display_name)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = game_colors) +
  scale_y_continuous(labels = label_number(suffix = "M"), expand = c(0.02, 0)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y", expand = c(0.02, 0)) +
  labs(
    title = "Moonton Top Games Monthly Downloads",
    subtitle = "Worldwide iOS + Android downloads",
    x = NULL,
    y = "Downloads",
    color = NULL,
    caption = "Source: Sensor Tower"
  ) +
  theme_dof() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 1))

ggsave("output/moonton_downloads_chart.png", p_downloads, width = 12, height = 7, dpi = 150, bg = "white")
message("  Saved: output/moonton_downloads_chart.png")

# Chart 3: MAU by Game over time (if available)
if ("MAU" %in% names(game_data) && sum(!is.na(game_data$MAU)) > 0) {
  p_mau <- ggplot(game_data %>% filter(!is.na(MAU) & MAU > 0), aes(x = date, y = MAU / 1e6, color = display_name)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = game_colors) +
    scale_y_continuous(labels = label_number(suffix = "M"), expand = c(0.02, 0)) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b '%y", expand = c(0.02, 0)) +
    labs(
      title = "Moonton Top Games Monthly Active Users",
      subtitle = "Worldwide iOS + Android MAU",
      x = NULL,
      y = "MAU",
      color = NULL,
      caption = "Source: Sensor Tower"
    ) +
    theme_dof() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(nrow = 1))

  ggsave("output/moonton_mau_chart.png", p_mau, width = 12, height = 7, dpi = 150, bg = "white")
  message("  Saved: output/moonton_mau_chart.png")
}

# ============================================================================
# STEP 4: Create Combined Chart - Revenue by Game (stacked area)
# ============================================================================
message("\nStep 4: Creating Stacked Revenue Chart...")

# Create stacked area chart showing revenue contribution by game
p_stacked <- ggplot(game_data, aes(x = date, y = Revenue / 1e6, fill = display_name)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = game_colors) +
  scale_y_continuous(labels = dollar_format(suffix = "M"), expand = c(0, 0)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y", expand = c(0, 0)) +
  labs(
    title = "Moonton Top Games Revenue Composition",
    subtitle = "Monthly revenue stacked by game (USD)",
    x = NULL,
    y = "Revenue",
    fill = NULL,
    caption = "Source: Sensor Tower"
  ) +
  theme_dof() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("output/moonton_combined_chart.png", p_stacked, width = 12, height = 7, dpi = 150, bg = "white")
message("  Saved: output/moonton_combined_chart.png")

# ============================================================================
# STEP 5: Create iOS Country Revenue Share Chart
# ============================================================================
message("\nStep 5: Creating iOS Country Revenue Share Chart...")

# Fetch iOS revenue by country for Mobile Legends Bang Bang (the main game)
mlbb_id <- "57955d280211a6718a000002"
countries <- c("ID", "PH", "MY", "TH", "VN", "BR", "US", "MX", "JP", "TW",
               "KR", "IN", "RU", "TR", "SA", "EG", "DE", "GB", "FR", "PK")

country_revenue_list <- list()
message("  Fetching iOS revenue by country...")

for (cc in countries) {
  tryCatch({
    result <- st_metrics(
      os = "ios",
      unified_app_id = mlbb_id,
      start_date = as.Date("2024-01-01"),
      end_date = end_date,
      countries = cc,
      date_granularity = "monthly",
      verbose = FALSE
    )

    if (!is.null(result) && nrow(result) > 0) {
      result$country_code <- cc
      country_revenue_list[[cc]] <- result
    }
  }, error = function(e) {
    # Skip errors silently
  })
  Sys.sleep(0.2)
}

if (length(country_revenue_list) > 0) {
  # Note: st_metrics already returns revenue in dollars
  country_revenue <- bind_rows(country_revenue_list)

  # Aggregate by country and month
  country_monthly <- country_revenue %>%
    group_by(date, country_code) %>%
    summarise(Revenue = sum(revenue, na.rm = TRUE), .groups = "drop")

  # Calculate totals and filter to top countries
  country_totals <- country_monthly %>%
    group_by(country_code) %>%
    summarise(Total = sum(Revenue), .groups = "drop") %>%
    arrange(desc(Total)) %>%
    head(10)

  # Create labels for countries
  country_labels <- c(
    "ID" = "Indonesia", "PH" = "Philippines", "MY" = "Malaysia",
    "TH" = "Thailand", "VN" = "Vietnam", "BR" = "Brazil",
    "US" = "United States", "MX" = "Mexico", "JP" = "Japan",
    "TW" = "Taiwan", "KR" = "South Korea", "IN" = "India",
    "RU" = "Russia", "TR" = "Turkey", "SA" = "Saudi Arabia",
    "EG" = "Egypt", "DE" = "Germany", "GB" = "United Kingdom",
    "FR" = "France", "PK" = "Pakistan"
  )

  # Filter to top countries and add labels
  country_monthly_top <- country_monthly %>%
    filter(country_code %in% country_totals$country_code) %>%
    mutate(Country = country_labels[country_code]) %>%
    left_join(country_totals %>% select(country_code, Total), by = "country_code")

  # Calculate percentage share
  monthly_totals <- country_monthly_top %>%
    group_by(date) %>%
    summarise(MonthTotal = sum(Revenue), .groups = "drop")

  country_pct <- country_monthly_top %>%
    left_join(monthly_totals, by = "date") %>%
    mutate(Percentage = Revenue / MonthTotal * 100) %>%
    mutate(Country = factor(Country, levels = country_labels[country_totals$country_code]))

  # Create 100% stacked bar chart
  country_palette <- c(
    "#E94560", "#4575b4", "#FFC857", "#1a9850", "#f46d43",
    "#9970ab", "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"
  )

  p_country <- ggplot(country_pct, aes(x = date, y = Percentage, fill = Country)) +
    geom_area(position = "fill", alpha = 0.9) +
    scale_fill_manual(values = country_palette) +
    scale_y_continuous(labels = percent_format(scale = 100), expand = c(0, 0)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b '%y", expand = c(0, 0)) +
    labs(
      title = "Mobile Legends: Bang Bang Revenue by Country",
      subtitle = "iOS revenue share by country (2024)",
      x = NULL,
      y = "Share of iOS Revenue",
      fill = "Country",
      caption = "Source: Sensor Tower | Top 10 countries by total revenue shown"
    ) +
    theme_dof() +
    theme(
      legend.position = "right",
      legend.key.size = unit(0.8, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    guides(fill = guide_legend(ncol = 1))

  ggsave("output/moonton_country_share_chart.png", p_country, width = 14, height = 7, dpi = 150, bg = "white")
  message("  Saved: output/moonton_country_share_chart.png")
} else {
  message("  Warning: Could not fetch country-level data")
}

# ============================================================================
# Summary
# ============================================================================
message("\n=== Visualization Complete ===")
message("Output files saved to output/ folder:")
message("  - moonton_portfolio_table.png")
message("  - moonton_revenue_chart.png")
message("  - moonton_downloads_chart.png")
if (exists("p_mau")) message("  - moonton_mau_chart.png")
message("  - moonton_combined_chart.png")
if (exists("p_country")) message("  - moonton_country_share_chart.png")

message("\n✓ Script completed successfully!")
