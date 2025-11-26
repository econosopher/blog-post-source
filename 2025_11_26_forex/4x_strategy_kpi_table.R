#!/usr/bin/env Rscript

# Top 4X Strategy Games KPI Table
# Year-to-date analysis sorted by revenue
# Uses URL parameters directly from Sensor Tower web interface

suppressPackageStartupMessages({
  library(pacman)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::load_all("../../sensortowerR")
  p_load(dplyr, gt, gtExtras, scales, lubridate, stringr, purrr, glue)
})

# Load Deconstructor of Fun GT theme when available
dof_theme_loaded <- tryCatch({
  source("../../dof-theme/dof_gt_theme.R", chdir = TRUE)
  TRUE
}, error = function(e) {
  message("Note: DoF theme not loaded (", e$message, ")")
  FALSE
})

message("=== Generating Top 4X Strategy Games KPI Table ===\n")

# Parse the Sensor Tower URL directly
sensor_tower_url <- "https://app.sensortower.com/market-analysis/top-apps?os=ios&measure=DAU&start_date=2025-01-01&end_date=2025-11-24&chart_plotting_type=line&granularity=daily&edit=1&ssia=6479020757&ssaa=com.kingsgroup.dcdly&uai=67abc0dae493b629704556db&comparison_attribute=absolute&comparison_period=pop&country=US&country=AU&country=CA&country=CN&country=FR&country=DE&country=GB&country=IT&country=JP&country=RU&country=KR&country=DZ&country=AO&country=AR&country=AT&country=AZ&country=BH&country=BD&country=BY&country=BE&country=BJ&country=BO&country=BR&country=BG&country=BF&country=KH&country=CM&country=CL&country=CO&country=CG&country=CR&country=CI&country=HR&country=CY&country=CZ&country=DK&country=DO&country=EC&country=EG&country=SV&country=EE&country=FI&country=GE&country=GH&country=GR&country=GT&country=HK&country=HU&country=IN&country=ID&country=IQ&country=IE&country=IL&country=JO&country=KZ&country=KE&country=KW&country=LA&country=LV&country=LB&country=LY&country=LT&country=LU&country=MO&country=MY&country=ML&country=MT&country=MX&country=MA&country=MZ&country=MM&country=NL&country=NZ&country=NI&country=NG&country=NO&country=OM&country=PK&country=PA&country=PY&country=PE&country=PH&country=PL&country=PT&country=QA&country=RO&country=SA&country=SN&country=RS&country=SG&country=SK&country=SI&country=ZA&country=ES&country=LK&country=SE&country=CH&country=TW&country=TZ&country=TH&country=TN&country=TR&country=UG&country=UA&country=AE&country=UY&country=UZ&country=VE&country=VN&country=YE&country=ZM&country=ZW&category=6014&device=iphone&device=ipad&metric=revenue&page=1&page_size=25&custom_fields_filter_mode=include_unified_apps&period=day&custom_fields_filter_id=600a22c0241bc16eb899fd71&duration=YTD"

# Parse URL to get API parameters
url_params <- st_parse_web_url(sensor_tower_url, verbose = TRUE)

# Extract key parameters from URL
custom_filter_id <- url_params$custom_fields_filter_id
start_date <- as.Date(url_params$date)
end_date <- as.Date(url_params$end_date)

# Get filter details for subtitle
filter_details <- tryCatch(
  st_custom_fields_filter_by_id(custom_filter_id),
  error = function(e) {
    message("Warning: unable to fetch custom filter details (", e$message, ")")
    NULL
  }
)

filter_label <- "4X Strategy Games"
if (!is.null(filter_details) && "custom_fields" %in% names(filter_details)) {
  cf <- filter_details$custom_fields
  if (nrow(cf) > 0) {
    filter_name <- cf$name[1]
    filter_values <- cf$values[[1]]
    if (is.list(filter_values)) {
      filter_values <- unlist(filter_values)
    }
    filter_values <- unique(na.omit(trimws(filter_values)))
    if (length(filter_values) > 0) {
      max_values <- 4
      truncated <- length(filter_values) > max_values
      display_values <- if (truncated) filter_values[1:max_values] else filter_values
      if (truncated) {
        filter_label <- glue("{filter_name}: {paste(display_values, collapse = ', ')}...")
      } else {
        filter_label <- glue("{filter_name}: {paste(display_values, collapse = ', ')}")
      }
    } else {
      filter_label <- filter_name
    }
  }
}

message(sprintf("Date range: %s to %s\n", start_date, end_date))

# Fetch top forex games sorted by revenue (user preference)
# Using unified OS to get cross-platform data, WW for worldwide
raw_data <- st_top_charts(
  measure = "revenue",
  os = "unified",
  category = url_params$category,
  regions = "WW",
  date = start_date,
  end_date = end_date,
  comparison_attribute = url_params$comparison_attribute,
  time_range = "day",
  custom_fields_filter_id = custom_filter_id,
  custom_tags_mode = url_params$custom_tags_mode,
  limit = 15,
  device_type = "total",
  enrich_response = TRUE,
  deduplicate_apps = TRUE
)

if (nrow(raw_data) == 0) {
  stop("No data returned from st_top_charts; check authentication and parameters.")
}

message(sprintf("Found %d 4X strategy games\n", nrow(raw_data)))

# Helper to add missing columns
add_missing_col <- function(df, col, default = NA_real_) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}

# Columns we might need
needed_cols <- c(
  "downloads_30d_us", "downloads_30d_ww",
  "downloads_alltime_us", "downloads_alltime_ww",
  "revenue_30d_us", "revenue_30d_ww",
  "mau_month_us", "mau_month_ww",
  "dau_30d_us", "dau_30d_ww",
  "release_date_us", "release_date_ww",
  "retention_1d_us", "retention_7d_us", "retention_30d_us",
  "age_us", "genders_us"
)
for (col in needed_cols) raw_data <- add_missing_col(raw_data, col)

# Process and rank by revenue
top_apps <- raw_data %>%
  arrange(desc(revenue)) %>%
  slice_head(n = 15) %>%
  mutate(rank = row_number())

# Extract publisher info
extract_first_id <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  as.character(x[[1]])
}

publisher_df <- map_dfr(top_apps$app_id, function(id) {
  info <- tryCatch(st_app_lookup(id, verbose = FALSE), error = function(e) NULL)
  publisher <- if (!is.null(info) && !is.null(info$publisher_name) && nzchar(info$publisher_name)) {
    info$publisher_name
  } else {
    NA_character_
  }
  tibble(
    app_id = id,
    publisher_name = publisher,
    ios_app_id = if (!is.null(info)) extract_first_id(info$ios_app_id) else NA_character_,
    android_app_id = if (!is.null(info)) extract_first_id(info$android_app_id) else NA_character_
  )
})

top_apps <- top_apps %>% left_join(publisher_df, by = "app_id")

# Process columns for display
period_days <- as.numeric(end_date - start_date) + 1

kpi_data <- top_apps %>%
  mutate(
    game = unified_app_name,
    publisher = publisher_name,

    # Revenue metrics
    revenue_ytd = as.numeric(revenue),
    annual_run_rate = revenue_ytd * (365 / period_days),
    revenue_30d = as.numeric(coalesce(revenue_30d_ww, revenue_30d_us)),

    # User metrics
    dau = as.numeric(coalesce(dau_30d_ww, dau_30d_us)),
    mau = as.numeric(coalesce(mau_month_ww, mau_month_us)),
    dau_mau_ratio = if_else(!is.na(dau) & !is.na(mau) & mau > 0, dau / mau, NA_real_),

    # Downloads
    downloads_30d = as.numeric(coalesce(downloads_30d_ww, downloads_30d_us)),
    downloads_alltime = as.numeric(coalesce(downloads_alltime_ww, downloads_alltime_us)),

    # Retention
    d1 = as.numeric(retention_1d_us),
    d7 = as.numeric(retention_7d_us),
    d30 = as.numeric(retention_30d_us),

    # Demographics
    age = round(as.numeric(age_us)),
    gender_col = genders_us,
    gender_pct = if_else(
      !is.na(gender_col) & grepl("Female", gender_col),
      as.numeric(gsub("([0-9]+)% Female.*", "\\1", gender_col)),
      if_else(
        !is.na(gender_col) & grepl("Male", gender_col),
        100 - as.numeric(gsub("([0-9]+)% Male.*", "\\1", gender_col)),
        NA_real_
      )
    ),
    female_pct = gender_pct,
    gender = if_else(
      !is.na(gender_pct),
      paste0(round(gender_pct), "% F"),
      NA_character_
    ),

    # Release date
    release_date = as.Date(coalesce(release_date_ww, release_date_us)),

    # Top country
    top_country = most_popular_country_revenue
  ) %>%
  select(
    rank, game, publisher, revenue_ytd, annual_run_rate, revenue_30d,
    dau, mau, dau_mau_ratio, downloads_30d, downloads_alltime,
    d1, d7, d30, age, gender, female_pct, release_date, top_country
  )

# Period label for display
period_label <- glue("YTD {format(start_date, '%b %d')} - {format(end_date, '%b %d, %Y')}")

# Build GT table
kpi_table <- kpi_data %>%
  gt() %>%
  tab_header(
    title = "TOP 4X STRATEGY GAMES",
    subtitle = glue("By WW Revenue | {period_label} | {filter_label}")
  ) %>%
  cols_label(
    rank = "#",
    game = "Game",
    publisher = "Publisher",
    revenue_ytd = "YTD Revenue",
    annual_run_rate = "Run Rate",
    revenue_30d = "30d Rev",
    dau = "DAU",
    mau = "MAU",
    dau_mau_ratio = "DAU/MAU",
    downloads_30d = "30d DL",
    downloads_alltime = "All-time DL",
    d1 = "D1",
    d7 = "D7",
    d30 = "D30",
    age = "Age",
    gender = "Gender",
    release_date = "Released",
    top_country = "Top Geo"
  ) %>%
  tab_spanner(
    label = "Revenue",
    columns = c(revenue_ytd, annual_run_rate, revenue_30d)
  ) %>%
  tab_spanner(
    label = "Active Users",
    columns = c(dau, mau, dau_mau_ratio)
  ) %>%
  tab_spanner(
    label = "Downloads",
    columns = c(downloads_30d, downloads_alltime)
  ) %>%
  tab_spanner(
    label = "Retention %",
    columns = c(d1, d7, d30)
  ) %>%
  tab_spanner(
    label = "Demographics",
    columns = c(age, gender)
  ) %>%
  # Format revenue columns
  fmt_currency(
    columns = c(revenue_ytd, annual_run_rate, revenue_30d),
    currency = "USD",
    suffixing = TRUE,
    decimals = 1
  ) %>%
  # Format user metrics
  fmt_number(
    columns = c(dau, mau),
    suffixing = TRUE,
    decimals = 2
  ) %>%
  fmt_percent(
    columns = dau_mau_ratio,
    decimals = 0
  ) %>%
  # Format downloads
  fmt_number(
    columns = c(downloads_30d, downloads_alltime),
    suffixing = TRUE,
    decimals = 1
  ) %>%
  # Format retention as percentages
  fmt_percent(
    columns = c(d1, d7, d30),
    decimals = 0
  ) %>%
  # Format date
  fmt_date(
    columns = release_date,
    date_style = "yMMMd"
  ) %>%
  # Handle missing values
  sub_missing(missing_text = "-") %>%
  # Revenue heatmap (YTD)
  data_color(
    columns = revenue_ytd,
    method = "numeric",
    palette = c("#d73027", "#fee08b", "#1a9850"),
    domain = NULL
  ) %>%
  # DAU/MAU ratio heatmap
  data_color(
    columns = dau_mau_ratio,
    method = "numeric",
    palette = c("#d73027", "#fee08b", "#1a9850"),
    domain = NULL
  ) %>%
  # Retention heatmaps
  data_color(
    columns = d1,
    method = "numeric",
    palette = c("#d73027", "#fee08b", "#1a9850"),
    domain = NULL
  ) %>%
  data_color(
    columns = d7,
    method = "numeric",
    palette = c("#d73027", "#fee08b", "#1a9850"),
    domain = NULL
  ) %>%
  data_color(
    columns = d30,
    method = "numeric",
    palette = c("#d73027", "#fee08b", "#1a9850"),
    domain = NULL
  ) %>%
  # Gender-based coloring
  tab_style(
    style = cell_text(color = "#e91e63", weight = "bold"),
    locations = cells_body(
      columns = gender,
      rows = !is.na(female_pct) & female_pct > 50
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#2196f3", weight = "bold"),
    locations = cells_body(
      columns = gender,
      rows = !is.na(female_pct) & female_pct <= 50
    )
  ) %>%
  # Hide helper column
  cols_hide(columns = female_pct) %>%
  # Alignment
  cols_align(align = "center", columns = c(rank, dau_mau_ratio, d1, d7, d30, age, gender, top_country)) %>%
  cols_align(align = "left", columns = c(game, publisher)) %>%
  cols_align(align = "right", columns = c(revenue_ytd, annual_run_rate, revenue_30d, dau, mau, downloads_30d, downloads_alltime)) %>%
  # Source note
  tab_source_note(
    source_note = glue("Source: Sensor Tower | Filter ID: {custom_filter_id} | Pulled {format(Sys.Date(), '%b %d, %Y')}")
  )

# Apply theme
if (dof_theme_loaded && exists("theme_dof_gt")) {
  kpi_table <- theme_dof_gt(kpi_table)
} else {
  kpi_table <- kpi_table %>% gt_theme_538()
}

# Table styling
kpi_table <- kpi_table %>%
  tab_options(
    table.font.size = px(12),
    column_labels.font.size = px(12),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(5),
    source_notes.font.size = px(10)
  )

# Save outputs
output_dir <- file.path(getwd(), "output")
dir.create(output_dir, showWarnings = FALSE)

output_png <- file.path(output_dir, "4x_strategy_games_kpi_table.png")
output_csv <- file.path(output_dir, "4x_strategy_games_kpi_data.csv")

gtsave(kpi_table, output_png, vwidth = 1600, vheight = 800)
write.csv(kpi_data %>% select(-female_pct), output_csv, row.names = FALSE)

message(sprintf("Table saved to: %s", output_png))
message(sprintf("Data saved to: %s", output_csv))

# Summary stats
total_revenue <- sum(kpi_data$revenue_ytd, na.rm = TRUE)
avg_dau <- mean(kpi_data$dau, na.rm = TRUE)
message(sprintf("\nTotal YTD Revenue: %s | Avg DAU: %s",
                scales::dollar(total_revenue, suffix = "B", scale = 1e-9, accuracy = 0.1),
                scales::comma(avg_dau)))
