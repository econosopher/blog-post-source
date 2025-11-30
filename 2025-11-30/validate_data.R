# Match-3 Games Data Validation Script
# Creates validation charts for each metric with facet_wrap and free y-axes

suppressPackageStartupMessages({
  library(pacman)
  p_load(dplyr, tidyr, ggplot2, readr, stringr, lubridate, scales)
})

# Load data
sales_data <- read_csv("match3_monthly_sales.csv", show_col_types = FALSE)
mau_data <- read_csv("match3_mau_timeseries.csv", show_col_types = FALSE)
enriched_data <- read_csv("match3_enriched_metrics.csv", show_col_types = FALSE)
app_lookup <- read_csv("match3_app_lookup.csv", show_col_types = FALSE)

# Create output directory for plots
plot_dir <- "validation_plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir)

# =============================================================================
# DATA AVAILABILITY SUMMARY
# =============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""))
cat("\n=== DATA AVAILABILITY SUMMARY BY GAME ===\n")
cat("=" |> rep(70) |> paste(collapse = ""))
cat("\n\n")

# Summarize sales data availability
sales_summary <- sales_data %>%
  group_by(app_name, platform) %>%
  summarise(
    countries = n_distinct(country),
    months = n_distinct(date),
    total_rows = n(),
    total_revenue = sum(total_revenue, na.rm = TRUE) + sum(revenue, na.rm = TRUE),
    total_downloads = sum(total_downloads, na.rm = TRUE) + sum(downloads, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue))

cat("=== SALES DATA (Revenue/Downloads) ===\n")
print(sales_summary, n = 30)

# MAU summary - now has all countries
mau_summary <- mau_data %>%
  group_by(app_name, country) %>%
  summarise(
    months = n_distinct(date),
    avg_mau = mean(mau, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_mau))

cat("\n=== MAU TIME-SERIES DATA (All Countries) ===\n")
# Show summary by app across all countries
mau_app_summary <- mau_data %>%
  group_by(app_name) %>%
  summarise(
    countries = n_distinct(country),
    months = n_distinct(date),
    avg_mau = mean(mau, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_mau))
print(mau_app_summary, n = 20)

cat("\n=== MAU By Country ===\n")
mau_country_summary <- mau_data %>%
  group_by(country) %>%
  summarise(
    apps = n_distinct(app_name),
    rows = n(),
    .groups = "drop"
  )
print(mau_country_summary, n = 10)

# Enriched data summary
cat("\n=== ENRICHED METRICS (Retention/Demographics) ===\n")
enriched_summary <- enriched_data %>%
  select(unified_app_name,
         retention_1d_us, retention_7d_us, retention_30d_us, retention_60d_us,
         mau_month_us, mau_month_ww, genders_us)

print(enriched_summary, n = 10)

# Missing apps in enriched data
all_apps <- app_lookup$app_name
enriched_apps <- enriched_data$unified_app_name
missing_enriched <- setdiff(all_apps, enriched_apps)

cat("\n=== APPS MISSING ENRICHED DATA ===\n")
cat(paste(missing_enriched, collapse = "\n"))
cat("\n")

# =============================================================================
# CHART 1: Monthly Revenue by Game (iOS + Android combined)
# =============================================================================

# Process iOS revenue
ios_revenue <- sales_data %>%
  filter(platform == "ios") %>%
  group_by(app_name, date) %>%
  summarise(
    revenue = sum(total_revenue, na.rm = TRUE),
    downloads = sum(total_downloads, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(platform = "iOS")

# Process Android revenue
android_revenue <- sales_data %>%
  filter(platform == "android") %>%
  group_by(app_name, date) %>%
  summarise(
    revenue = sum(revenue, na.rm = TRUE),
    downloads = sum(downloads, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(platform = "Android")

combined_revenue <- bind_rows(ios_revenue, android_revenue)

# Chart: Revenue by game faceted
p1 <- ggplot(combined_revenue, aes(x = as.Date(date), y = revenue/1e6, color = platform)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~app_name, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  scale_color_manual(values = c("iOS" = "#007AFF", "Android" = "#34A853")) +
  labs(
    title = "Monthly Revenue by Game",
    subtitle = "Faceted with free Y-axis scales - All countries combined",
    x = "Month",
    y = "Revenue (Millions USD)",
    color = "Platform"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave(file.path(plot_dir, "01_revenue_by_game.png"), p1, width = 14, height = 10, dpi = 150)
cat("\nSaved: 01_revenue_by_game.png\n")

# =============================================================================
# CHART 2: Monthly Downloads by Game
# =============================================================================

p2 <- ggplot(combined_revenue, aes(x = as.Date(date), y = downloads/1e6, color = platform)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~app_name, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = number_format(suffix = "M", scale = 1)) +
  scale_color_manual(values = c("iOS" = "#007AFF", "Android" = "#34A853")) +
  labs(
    title = "Monthly Downloads by Game",
    subtitle = "Faceted with free Y-axis scales - All countries combined",
    x = "Month",
    y = "Downloads (Millions)",
    color = "Platform"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave(file.path(plot_dir, "02_downloads_by_game.png"), p2, width = 14, height = 10, dpi = 150)
cat("Saved: 02_downloads_by_game.png\n")

# =============================================================================
# CHART 3: MAU Time-Series by Country (faceted by app, colored by country)
# =============================================================================

mau_plot_data <- mau_data %>%
  filter(!is.na(app_name))

p3 <- ggplot(mau_plot_data, aes(x = as.Date(date), y = mau/1e6, color = country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~app_name, scales = "free_y", ncol = 3) +
  scale_y_continuous(labels = number_format(suffix = "M", scale = 1)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Monthly Active Users (MAU) Time-Series by Country",
    subtitle = "All 7 countries - Faceted by game with free Y-axis scales",
    x = "Month",
    y = "MAU (Millions)",
    color = "Country"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave(file.path(plot_dir, "03_mau_timeseries.png"), p3, width = 14, height = 10, dpi = 150)
cat("Saved: 03_mau_timeseries.png\n")

# =============================================================================
# CHART 3b: MAU Time-Series faceted by Country (to see country patterns)
# =============================================================================

p3b <- ggplot(mau_plot_data, aes(x = as.Date(date), y = mau/1e6, color = app_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  facet_wrap(~country, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = number_format(suffix = "M", scale = 1)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Monthly Active Users (MAU) by Country",
    subtitle = "Faceted by country - All games compared",
    x = "Month",
    y = "MAU (Millions)",
    color = "Game"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(file.path(plot_dir, "03b_mau_by_country.png"), p3b, width = 14, height = 8, dpi = 150)
cat("Saved: 03b_mau_by_country.png\n")

# =============================================================================
# CHART 4: Revenue by Country (Top 5 games only)
# =============================================================================

top_games <- combined_revenue %>%
  group_by(app_name) %>%
  summarise(total_rev = sum(revenue, na.rm = TRUE)) %>%
  arrange(desc(total_rev)) %>%
  slice_head(n = 5) %>%
  pull(app_name)

country_revenue <- sales_data %>%
  filter(app_name %in% top_games) %>%
  mutate(
    revenue_val = case_when(
      platform == "ios" ~ total_revenue,
      platform == "android" ~ revenue,
      TRUE ~ 0
    )
  ) %>%
  group_by(app_name, country, date) %>%
  summarise(revenue = sum(revenue_val, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(country_revenue, aes(x = as.Date(date), y = revenue/1e6, color = country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~app_name, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Monthly Revenue by Country (Top 5 Games)",
    subtitle = "Combined iOS + Android - Faceted with free Y-axis scales",
    x = "Month",
    y = "Revenue (Millions USD)",
    color = "Country"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

ggsave(file.path(plot_dir, "04_revenue_by_country.png"), p4, width = 12, height = 10, dpi = 150)
cat("Saved: 04_revenue_by_country.png\n")

# =============================================================================
# CHART 5: Retention Comparison (Snapshot Data)
# =============================================================================

retention_data <- enriched_data %>%
  select(unified_app_name,
         retention_1d_us, retention_7d_us, retention_14d_us,
         retention_30d_us, retention_60d_us) %>%
  pivot_longer(
    cols = starts_with("retention"),
    names_to = "cohort",
    values_to = "retention"
  ) %>%
  mutate(
    cohort = case_when(
      str_detect(cohort, "1d") ~ "D1",
      str_detect(cohort, "7d") ~ "D7",
      str_detect(cohort, "14d") ~ "D14",
      str_detect(cohort, "30d") ~ "D30",
      str_detect(cohort, "60d") ~ "D60"
    ),
    cohort = factor(cohort, levels = c("D1", "D7", "D14", "D30", "D60")),
    retention = retention * 100  # Convert to percentage
  ) %>%
  filter(!is.na(retention))

p5 <- ggplot(retention_data, aes(x = cohort, y = retention, fill = unified_app_name)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", retention)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 60)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Retention Rates by Cohort (US Market)",
    subtitle = "Snapshot data from st_app_enriched() - Only 5 of 9 games have data",
    x = "Retention Cohort",
    y = "Retention Rate (%)",
    fill = "Game"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(plot_dir, "05_retention_comparison.png"), p5, width = 12, height = 7, dpi = 150)
cat("Saved: 05_retention_comparison.png\n")

# =============================================================================
# CHART 6: Retention Curves by Game
# =============================================================================

p6 <- ggplot(retention_data, aes(x = cohort, y = retention, group = unified_app_name, color = unified_app_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 60)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Retention Curves by Game (US Market)",
    subtitle = "D1 through D60 retention - D90 NOT available via API",
    x = "Retention Cohort",
    y = "Retention Rate (%)",
    color = "Game"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave(file.path(plot_dir, "06_retention_curves.png"), p6, width = 11, height = 7, dpi = 150)
cat("Saved: 06_retention_curves.png\n")

# =============================================================================
# CHART 7: Demographics - Gender Distribution
# =============================================================================

gender_data <- enriched_data %>%
  select(unified_app_name, genders_us) %>%
  filter(!is.na(genders_us)) %>%
  mutate(
    female_pct = as.numeric(str_extract(genders_us, "\\d+")) / 100,
    male_pct = 1 - female_pct
  ) %>%
  pivot_longer(
    cols = c(female_pct, male_pct),
    names_to = "gender",
    values_to = "percentage"
  ) %>%
  mutate(gender = ifelse(gender == "female_pct", "Female", "Male"))

p7 <- ggplot(gender_data, aes(x = unified_app_name, y = percentage, fill = gender)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", percentage * 100)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Female" = "#E91E63", "Male" = "#2196F3")) +
  coord_flip() +
  labs(
    title = "Gender Distribution by Game (US Market)",
    subtitle = "Demographics are SNAPSHOTS - US data only available",
    x = NULL,
    y = "Percentage",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(plot_dir, "07_gender_distribution.png"), p7, width = 10, height = 6, dpi = 150)
cat("Saved: 07_gender_distribution.png\n")

# =============================================================================
# CHART 8: MAU Comparison (Snapshot vs Time-Series)
# =============================================================================

# Get latest MAU from time-series (US only for comparison)
mau_latest <- mau_plot_data %>%
  filter(date == max(date), country == "US") %>%
  select(app_name, mau_timeseries = mau)

# Get MAU from enriched snapshot
mau_snapshot <- enriched_data %>%
  select(app_name = unified_app_name, mau_snapshot = mau_month_us)

mau_compare <- full_join(mau_latest, mau_snapshot, by = "app_name") %>%
  filter(!is.na(mau_timeseries) | !is.na(mau_snapshot)) %>%
  pivot_longer(
    cols = c(mau_timeseries, mau_snapshot),
    names_to = "source",
    values_to = "mau"
  ) %>%
  mutate(source = case_when(
    source == "mau_timeseries" ~ "Time-Series (Oct 2025)",
    source == "mau_snapshot" ~ "Snapshot (Enriched)"
  ))

p8 <- ggplot(mau_compare %>% filter(!is.na(mau)),
             aes(x = reorder(app_name, mau), y = mau/1e6, fill = source)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = number_format(suffix = "M", scale = 1)) +
  scale_fill_manual(values = c("Time-Series (Oct 2025)" = "#6B46C1",
                                "Snapshot (Enriched)" = "#F59E0B")) +
  labs(
    title = "MAU Comparison: Time-Series vs Snapshot",
    subtitle = "Comparing st_batch_metrics() time-series with st_app_enriched() snapshot",
    x = NULL,
    y = "Monthly Active Users (Millions)",
    fill = "Data Source"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(plot_dir, "08_mau_comparison.png"), p8, width = 11, height = 7, dpi = 150)
cat("Saved: 08_mau_comparison.png\n")

# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""))
cat("\n=== FINAL DATA AVAILABILITY SUMMARY ===\n")
cat("=" |> rep(70) |> paste(collapse = ""))
cat("\n\n")

summary_table <- app_lookup %>%
  mutate(
    has_ios_sales = app_name %in% (sales_data %>% filter(platform == "ios") %>% pull(app_name) %>% unique()),
    has_android_sales = app_name %in% (sales_data %>% filter(platform == "android") %>% pull(app_name) %>% unique()),
    has_mau_ts = app_name %in% (mau_plot_data$app_name %>% unique()),
    has_enriched = app_name %in% enriched_apps
  ) %>%
  select(app_name, has_ios_sales, has_android_sales, has_mau_ts, has_enriched)

# Calculate row counts
sales_rows <- sales_data %>% group_by(app_name) %>% summarise(sales_rows = n(), .groups = "drop")
mau_rows <- mau_plot_data %>% group_by(app_name) %>% summarise(mau_rows = n(), .groups = "drop")

summary_table <- summary_table %>%
  left_join(sales_rows, by = "app_name") %>%
  left_join(mau_rows, by = "app_name") %>%
  mutate(
    sales_rows = replace_na(sales_rows, 0),
    mau_rows = replace_na(mau_rows, 0)
  )

print(summary_table, n = 20)

cat("\n=== LEGEND ===\n")
cat("has_ios_sales: Revenue/downloads data available for iOS\n")
cat("has_android_sales: Revenue/downloads data available for Android\n")
cat("has_mau_ts: MAU time-series data available (US market)\n")
cat("has_enriched: Retention/demographics snapshot available\n")
cat("sales_rows: Number of sales data rows\n")
cat("mau_rows: Number of MAU time-series rows\n")

cat("\n\n=== NOTES ===\n")
cat("1. Retention cohorts: D1, D7, D14, D30, D60 (D90 NOT available)\n")
cat("2. Demographics: US market only\n")
cat("3. Enriched metrics: Snapshots, not time-series\n")
cat("4. 4 games missing enriched data (smaller apps)\n")
cat("\nValidation plots saved to: ", plot_dir, "\n")
