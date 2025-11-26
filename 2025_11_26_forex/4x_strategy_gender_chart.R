#!/usr/bin/env Rscript

# 4X Strategy Games - Female Gender Share Bar Chart
# US Market, iOS, Recent Data

suppressPackageStartupMessages({
  library(pacman)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::load_all("../../sensortowerR")
  p_load(dplyr, ggplot2, scales, glue, stringr)
})

# Load DoF theme if available
dof_theme_loaded <- tryCatch({
  source("../../dof-theme/dof_theme.R", chdir = TRUE)
  TRUE
}, error = function(e) {

  message("Note: DoF theme not loaded (", e$message, ")")
  FALSE
})

message("=== Generating 4X Strategy Games Female Gender Share Chart ===\n")

# Custom filter for 4X Strategy games
custom_filter_id <- "600a22c0241bc16eb899fd71"

# Use recent 30-day window for US, iOS
end_date <- as.Date("2025-11-24")
start_date <- end_date - 30

message(sprintf("Date range: %s to %s (US Market)\n", start_date, end_date))

# Fetch data - use unified for better enrichment, filter to US
raw_data <- st_top_charts(
  measure = "revenue",
  os = "unified",
  category = 6014,
  regions = "US",
  date = start_date,
  end_date = end_date,
  comparison_attribute = "absolute",
  time_range = "day",
  custom_fields_filter_id = custom_filter_id,
  custom_tags_mode = "include_unified_apps",
  limit = 20,
  device_type = "total",
  enrich_response = TRUE,
  deduplicate_apps = TRUE
)

if (nrow(raw_data) == 0) {
  stop("No data returned from st_top_charts")
}

message(sprintf("Found %d games\n", nrow(raw_data)))

# Helper to add missing columns
add_missing_col <- function(df, col, default = NA_real_) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}

raw_data <- add_missing_col(raw_data, "genders_us", NA_character_)

# Debug: print available columns
message("Available columns: ", paste(head(names(raw_data), 20), collapse = ", "))

# Find gender column
gender_col_name <- if ("genders_us" %in% names(raw_data)) {
  "genders_us"
} else if ("custom_tags.Genders (Last Quarter, US)" %in% names(raw_data)) {
  "custom_tags.Genders (Last Quarter, US)"
} else {
  NULL
}

message("Gender column: ", gender_col_name)

# Always look up app names to ensure we get proper names (not IDs)
message("Looking up app names...")
app_info <- purrr::map_dfr(raw_data$app_id, function(id) {
  info <- tryCatch(st_app_lookup(id, verbose = FALSE), error = function(e) NULL)
  app_name <- NA_character_
 if (!is.null(info)) {
    if ("unified_app_name" %in% names(info) && !is.na(info$unified_app_name) && nzchar(info$unified_app_name)) {
      app_name <- info$unified_app_name
    } else if ("app_name" %in% names(info) && !is.na(info$app_name) && nzchar(info$app_name)) {
      app_name <- info$app_name
    }
  }
  tibble(app_id = id, app_name_lookup = app_name)
})
raw_data <- raw_data %>% left_join(app_info, by = "app_id")
app_name_col <- "app_name_lookup"

message("App name column: ", app_name_col)

# Process gender data
gender_data <- raw_data %>%
  mutate(
    game = .data[[app_name_col]],
    gender_str = if (!is.null(gender_col_name)) .data[[gender_col_name]] else NA_character_,
    # Parse female percentage from gender string
    female_pct = case_when(
      !is.na(gender_str) & grepl("Female", gender_str) ~
        as.numeric(gsub("([0-9]+)% Female.*", "\\1", gender_str)),
      !is.na(gender_str) & grepl("Male", gender_str) ~
        100 - as.numeric(gsub("([0-9]+)% Male.*", "\\1", gender_str)),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(female_pct) & !is.na(game)) %>%
  arrange(desc(female_pct)) %>%
  select(game, female_pct)

if (nrow(gender_data) == 0) {
  stop("No gender data available for these games")
}

message(sprintf("Games with gender data: %d\n", nrow(gender_data)))

# Create the bar chart
chart <- ggplot(gender_data, aes(x = reorder(game, female_pct), y = female_pct)) +
  geom_col(aes(fill = female_pct), width = 0.75) +
  geom_text(
    aes(label = paste0(round(female_pct), "%")),
    hjust = -0.15,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#2196F3",   # Blue for lower female %
    high = "#E91E63",  # Pink for higher female %
    guide = "none"
  ) +
  scale_y_continuous(
    limits = c(0, max(gender_data$female_pct) * 1.15),
    labels = function(x) paste0(x, "%")
  ) +
  coord_flip() +
  labs(
    title = "Female Player Share in Top 4X Strategy Games",
    subtitle = glue("US Market | {format(start_date, '%b %d')} - {format(end_date, '%b %d, %Y')}"),
    x = NULL,
    y = "Female Share (%)",
    caption = "Source: Sensor Tower"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(color = "gray40", size = 11, hjust = 0),
    plot.caption = element_text(color = "gray50", size = 9, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 40, 20, 20)
  )

# Save outputs
output_dir <- file.path(getwd(), "output")
dir.create(output_dir, showWarnings = FALSE)

output_png <- file.path(output_dir, "4x_strategy_female_share.png")
output_csv <- file.path(output_dir, "4x_strategy_female_share_data.csv")

ggsave(output_png, chart, width = 10, height = 8, dpi = 150, bg = "white")
write.csv(gender_data, output_csv, row.names = FALSE)

message(sprintf("Chart saved to: %s", output_png))
message(sprintf("Data saved to: %s", output_csv))

# Summary
message(sprintf("\nAverage female share: %.1f%%", mean(gender_data$female_pct)))
message(sprintf("Range: %.0f%% - %.0f%%", min(gender_data$female_pct), max(gender_data$female_pct)))
