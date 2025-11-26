#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(pacman)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::load_all("../../sensortowerR")
  p_load(dplyr, gt, gtExtras, scales, lubridate, stringr, purrr, glue)
})

# Load Deconstructor of Fun GT theme when available for consistent styling
dof_theme_loaded <- tryCatch({
  source("../../dof-theme/dof_gt_theme.R", chdir = TRUE)
  TRUE
}, error = function(e) {
  message("Note: DoF theme not loaded (", e$message, ")")
  FALSE
})

message("Fetching top apps from Sensor Tower...")

custom_filter_id <- "68d402dac5a19ebcfe99e5c5"

filter_details <- tryCatch(
  st_custom_fields_filter_by_id(custom_filter_id),
  error = function(e) {
    message("Warning: unable to fetch custom filter details (", e$message, ")")
    NULL
  }
)

filter_label <- "Custom Segment"
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
      max_values <- 6
      truncated <- length(filter_values) > max_values
      display_values <- if (truncated) filter_values[1:max_values] else filter_values
      enumerated <- paste0("(", seq_along(display_values), ") ", display_values)
      if (truncated) {
        enumerated <- c(enumerated, "...")
      }
      filter_label <- glue("{filter_name}: {paste(enumerated, collapse = ' | ')}")
    } else {
      filter_label <- filter_name
    }
  }
}
start_date <- as.Date("2025-06-25")
end_date <- as.Date("2025-09-22")

raw_data <- st_top_charts(
  measure = "revenue",
  os = "unified",
  category = 7012,
  regions = "WW",
  date = start_date,
  end_date = end_date,
  comparison_attribute = "absolute",
  time_range = "day",
  custom_fields_filter_id = custom_filter_id,
  custom_tags_mode = "include_unified_apps",
  limit = 25,
  device_type = "total"
)

if (nrow(raw_data) == 0) {
  stop("No data returned from st_top_charts; check authentication and parameters.")
}

add_missing_col <- function(df, col) {
  if (!col %in% names(df)) df[[col]] <- NA_real_
  df
}

needed_cols <- c(
  "downloads_30d_us", "downloads_30d_ww",
  "downloads_alltime_us", "downloads_alltime_ww",
  "revenue_30d_us", "revenue_30d_ww",
  "mau_month_us", "mau_month_ww",
  "release_date_us", "release_date_ww",
  "users_delta"
)
for (col in needed_cols) raw_data <- add_missing_col(raw_data, col)

raw_data <- raw_data %>%
  mutate(across(c(downloads_30d_us, downloads_30d_ww,
                  downloads_alltime_us, downloads_alltime_ww,
                  revenue_30d_us, revenue_30d_ww,
                  mau_month_us, mau_month_ww,
                  users_delta), as.numeric))

top_apps <- raw_data %>%
  arrange(desc(revenue)) %>%
  slice_head(n = 10) %>%
  mutate(rank = row_number())

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

selected_cols <- top_apps %>%
  mutate(
    revenue_value = as.numeric(revenue),
    revenue_delta_raw = as.numeric(revenue_delta),
    revenue_pct_raw = as.numeric(revenue_transformed_delta),
    revenue_30d_value = as.numeric(coalesce(revenue_30d_ww, revenue_value)),
    downloads_30d_value = as.numeric(coalesce(downloads_30d_us, downloads_30d_ww)),
    downloads_alltime_value = as.numeric(coalesce(downloads_alltime_us, downloads_alltime_ww)),
    mau_month_us_value = as.numeric(mau_month_us),
    mau_month_ww_value = as.numeric(mau_month_ww),
    release_date_us = as.Date(release_date_us),
    release_date_ww = as.Date(release_date_ww),
    top_country = most_popular_country_revenue,
    subgenre = `aggregate_tags.Game Sub-genre`
  ) %>%
  transmute(
    rank,
    app_id,
    game = unified_app_name,
    publisher = publisher_name,
    subgenre,
    revenue = revenue_value,
    revenue_delta_raw,
    revenue_pct_raw,
    revenue_30d = revenue_30d_value,
    downloads_30d = downloads_30d_value,
    downloads_alltime = downloads_alltime_value,
    mau_month_us = mau_month_us_value,
    mau_month_ww = mau_month_ww_value,
    release_date_us,
    release_date_ww,
    top_country,
    ios_app_id,
    android_app_id
  )

period_days <- as.numeric(end_date - start_date) + 1

country_to_label <- function(country_code) {
  lookup <- c(
    "US" = "United States", "CA" = "Canada", "GB" = "United Kingdom",
    "DE" = "Germany", "FR" = "France", "JP" = "Japan", "KR" = "South Korea",
    "CN" = "China", "AU" = "Australia", "ES" = "Spain", "IT" = "Italy"
  )
  ifelse(!is.na(country_code) & country_code %in% names(lookup), lookup[country_code], country_code)
}

format_short_number <- function(x) {
  out <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    x >= 1e9 ~ sprintf("%.1fB", x / 1e9),
    x >= 1e6 ~ sprintf("%.1fM", x / 1e6),
    x >= 1e3 ~ sprintf("%.1fK", x / 1e3),
    TRUE ~ scales::number(x, accuracy = 1, big.mark = ",")
  )
  stringr::str_replace(out, "\\.0([BKM])$", "\\1")
}

format_percent_value <- function(x) {
  scales::percent(x, accuracy = 0.1)
}

format_with_sign <- function(value, formatter) {
  out <- rep(NA_character_, length(value))
  non_missing <- !is.na(value)
  if (any(non_missing)) {
    zero_idx <- non_missing & value == 0
    pos_idx <- non_missing & value > 0
    neg_idx <- non_missing & value < 0
    if (any(zero_idx)) {
      out[zero_idx] <- formatter(value[zero_idx])
    }
    if (any(pos_idx)) {
      out[pos_idx] <- paste0("+", formatter(value[pos_idx]))
    }
    if (any(neg_idx)) {
      out[neg_idx] <- paste0("-", formatter(abs(value[neg_idx])))
    }
  }
  out
}

fetch_platform_revenue <- function(os, app_id, country) {
  if (is.na(app_id) || !nzchar(app_id)) return(0)
  params <- list(
    os = os,
    countries = country,
    start_date = start_date,
    end_date = end_date,
    date_granularity = "weekly",
    auto_segment = TRUE,
    verbose = FALSE
  )
  if (os == "ios") {
    params$ios_app_id <- app_id
  } else if (os == "android") {
    params$android_app_id <- app_id
  }
  res <- tryCatch(do.call(st_sales_report, params), error = function(e) NULL)
  if (is.null(res) || !"revenue" %in% names(res)) return(NA_real_)
  sum(res$revenue, na.rm = TRUE)
}

fetch_country_revenue <- function(ios_id, android_id, country) {
  if (is.na(country) || !nzchar(country)) return(NA_real_)
  ios_rev <- fetch_platform_revenue("ios", ios_id, country)
  android_rev <- fetch_platform_revenue("android", android_id, country)
  if (is.na(ios_rev) && is.na(android_rev)) return(NA_real_)
  sum(c(ios_rev, android_rev), na.rm = TRUE)
}

selected_cols <- selected_cols %>%
  mutate(
    top_country_revenue = pmap_dbl(list(ios_app_id, android_app_id, top_country), ~fetch_country_revenue(..1, ..2, ..3)),
    top_country_share = if_else(!is.na(top_country_revenue) & revenue > 0, top_country_revenue / revenue, NA_real_),
    annual_run_rate = revenue * (365 / period_days),
    subgenre_display = if_else(!is.na(subgenre) & nzchar(subgenre), subgenre, "Other"),
    top_country_label = country_to_label(top_country),
    top_country_display = if_else(!is.na(top_country_share), paste0(top_country_label, " (", format_percent_value(top_country_share), ")"), top_country_label)
  )

selected_cols <- selected_cols %>%
  mutate(
    revenue_delta_display = format_with_sign(revenue_delta_raw, format_short_number),
    revenue_pct_display = format_with_sign(revenue_pct_raw, format_percent_value),
    top_country = top_country_label
  )

period_label <- glue("{format(start_date, '%b %d')} - {format(end_date, '%b %d, %Y')}")

subtitle_text <- glue(
  "WW Revenue | {period_label} | {filter_label}"
)

display_cols <- selected_cols %>%
  select(
    rank,
    game,
    publisher,
    subgenre_display,
    annual_run_rate,
    revenue,
    revenue_delta_display,
    revenue_pct_display,
    revenue_30d,
    downloads_30d,
    downloads_alltime,
    mau_month_us,
    mau_month_ww,
    release_date_us,
    release_date_ww,
    top_country_display,
    app_id,
    revenue_delta_raw,
    revenue_pct_raw,
    subgenre,
    top_country,
    top_country_label,
    top_country_share,
    top_country_revenue
  )

table_gt <- display_cols %>%
  gt(rowname_col = "game") %>%
  tab_header(
    title = "Top Sports Apps by Revenue",
    subtitle = subtitle_text
  ) %>%
  tab_spanner(
    label = glue("Revenue Performance ({period_label})"),
    columns = c(annual_run_rate, revenue, revenue_delta_display, revenue_pct_display, revenue_30d)
  ) %>%
  tab_spanner(
    label = "Downloads",
    columns = c(downloads_30d, downloads_alltime)
  ) %>%
  tab_spanner(
    label = "Monthly Active Users",
    columns = c(mau_month_us, mau_month_ww)
  ) %>%
  tab_spanner(
    label = "Launch",
    columns = c(release_date_us, release_date_ww)
  ) %>%
  tab_spanner(
    label = "Geography",
    columns = top_country_display
  ) %>%
  cols_label(
    rank = "#",
    publisher = "Publisher",
    subgenre_display = "Sub-genre",
    annual_run_rate = "Annual Run Rate",
    revenue = glue("Revenue ({period_label})"),
    revenue_delta_display = "Δ",
    revenue_pct_display = "Δ%",
    revenue_30d = "30d Revenue",
    downloads_30d = "30d Downloads",
    downloads_alltime = "All-time Downloads",
    mau_month_us = "US",
    mau_month_ww = "WW",
    release_date_us = "US",
    release_date_ww = "WW",
    top_country_display = "Top $ Geo"
  ) %>%
  cols_align(
    columns = c(rank, revenue_delta_display, revenue_pct_display, top_country_display),
    align = "center"
  ) %>%
  cols_align(
    columns = c(publisher, subgenre_display),
    align = "left"
  ) %>%
  fmt_number(
    columns = c(downloads_30d, downloads_alltime, mau_month_us, mau_month_ww),
    decimals = 0,
    suffixing = TRUE
  ) %>%
  fmt_currency(
    columns = c(revenue, revenue_30d, annual_run_rate),
    currency = "USD",
    decimals = 0,
    suffixing = TRUE
  ) %>%
  fmt_date(
    columns = c(release_date_us, release_date_ww),
    date_style = "yMMMd"
  ) %>%
  sub_missing(columns = everything(), missing_text = "—") %>%
  cols_hide(columns = c(app_id, revenue_delta_raw, revenue_pct_raw, subgenre, top_country, top_country_label, top_country_share, top_country_revenue))

if (dof_theme_loaded && exists("theme_dof_gt")) {
  table_gt <- theme_dof_gt(table_gt)
} else {
  table_gt <- table_gt %>% gtExtras::gt_theme_538()
}

highlight_style <- list(
  cell_fill(color = if (dof_theme_loaded && exists("dof_colors")) dof_colors$light_pink else "#fde2ff"),
  cell_text(weight = "bold")
)

table_gt <- table_gt %>%
  tab_style(
    style = highlight_style,
    locations = cells_body(rows = publisher == "Take-Two Interactive")
  ) %>%
  tab_options(
    table.font.size = px(14),
    column_labels.font.size = px(14),
    heading.title.font.size = px(24),
    heading.subtitle.font.size = px(16),
    data_row.padding = px(6),
    source_notes.font.size = px(12)
  ) %>%
  tab_source_note(
    source_note = glue("Source: Sensor Tower st_top_charts (Revenue) | Pulled {format(Sys.Date(), '%b %d, %Y')}")
  )

output_dir <- file.path(getwd(), "output")
dir.create(output_dir, showWarnings = FALSE)
output_path <- file.path(output_dir, "top_mau_table.png")
gtsave(table_gt, output_path, vwidth = 1400, vheight = 900)

message("Saved table to ", output_path)
