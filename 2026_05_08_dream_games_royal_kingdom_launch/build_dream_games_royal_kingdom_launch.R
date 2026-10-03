#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(devtools)
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(ggrepel)
  library(lubridate)
  library(purrr)
  library(readr)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)
})

get_script_dir <- function() {
  file_arg <- commandArgs(trailingOnly = FALSE) |>
    (\(x) x[grepl("^--file=", x)])()

  if (length(file_arg) == 0) {
    normalizePath(getwd())
  } else {
    dirname(normalizePath(sub("^--file=", "", file_arg[[1]])))
  }
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

load_sensortower_token <- function() {
  if (!nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN"))) {
    candidate_paths <- c(
      "/Users/phillip/Documents/secrets/global.env",
      "/Users/phillip/Documents/secrets/home/.Renviron",
      "/Users/phillip/Documents/vibe_coding_projects/.env",
      path.expand("~/.Renviron")
    )

    for (path in candidate_paths[file.exists(candidate_paths)]) {
      try(readRenviron(path), silent = TRUE)
    }
  }

  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (!nzchar(auth_token)) {
    stop("SENSORTOWER_AUTH_TOKEN is not set.")
  }

  invisible(auth_token)
}

load_sensortower_package <- function() {
  sensor_pkg <- "/Users/phillip/Documents/vibe_coding_projects/videogameR-universe/SensorTowerR"
  if (!dir.exists(sensor_pkg)) {
    stop("SensorTowerR package folder not found: ", sensor_pkg)
  }

  devtools::load_all(sensor_pkg, quiet = TRUE)
}

theme_538 <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = 24, color = "#222222", hjust = 0),
      plot.subtitle = element_text(size = 12.5, color = "#4d4d4d", hjust = 0, margin = margin(b = 12)),
      plot.caption = element_text(size = 9, color = "#666666", hjust = 0, margin = margin(t = 14)),
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#eeeeee", linewidth = 0.35),
      panel.grid.major.y = element_line(color = "#d7d7d7", linewidth = 0.45),
      axis.title = element_blank(),
      axis.text = element_text(color = "#333333"),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(color = "#333333", size = 10),
      plot.margin = margin(20, 46, 28, 18)
    )
}

write_png <- function(plot, path, width = 12.6, height = 7.4) {
  ggsave(path, plot, width = width, height = height, dpi = 180, bg = "white")
  invisible(path)
}

format_money_short <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    abs(x) >= 1e9 ~ paste0("$", number(x / 1e9, accuracy = 0.1), "B"),
    abs(x) >= 1e6 ~ paste0("$", number(x / 1e6, accuracy = 0.1), "M"),
    abs(x) >= 1e3 ~ paste0("$", number(x / 1e3, accuracy = 1), "K"),
    TRUE ~ paste0("$", number(x, accuracy = 1))
  )
}

format_count_short <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    abs(x) >= 1e9 ~ paste0(number(x / 1e9, accuracy = 0.1), "B"),
    abs(x) >= 1e6 ~ paste0(number(x / 1e6, accuracy = 0.1), "M"),
    abs(x) >= 1e3 ~ paste0(number(x / 1e3, accuracy = 1), "K"),
    TRUE ~ number(x, accuracy = 1)
  )
}

market_label <- function(country) {
  recode(country, WW = "Global", US = "United States", .default = country)
}

market_slug <- function(country) {
  recode(country, WW = "ww", US = "us", .default = str_to_lower(country))
}

metric_label <- function(metric) {
  recode(
    metric,
    cumulative_downloads = "Cumulative Downloads",
    cumulative_revenue_usd = "Cumulative Revenue",
    cumulative_revenue_per_download_usd = "Launch-Aligned RPD",
    portfolio_revenue_usd = "Portfolio Revenue",
    lifecycle_revenue_usd = "Lifecycle Revenue",
    .default = metric
  )
}

app_definitions <- function() {
  tribble(
    ~title,           ~unified_app_id,             ~publisher_name, ~source_query,
    "Royal Match",   "5f16a8019f7b275235017614",  "Dream Games",   "Royal Match",
    "Royal Kingdom", "68522fdeea1d299c74cd6921",  "Dream Games",   "Royal Kingdom"
  )
}

first_existing_col <- function(data, cols, default = NA_character_) {
  existing <- intersect(cols, names(data))
  if (length(existing) == 0) {
    rep(default, nrow(data))
  } else {
    data[[existing[[1]]]]
  }
}

verify_app_mapping_row <- function(title, unified_app_id, publisher_name, source_query, auth_token) {
  results <- st_apps(query = source_query, os = "unified", limit = 10, auth_token = auth_token) %>%
    mutate(
      result_unified_app_id = as.character(first_existing_col(., c("unified_app_id", "app_id"))),
      result_name = as.character(first_existing_col(., c("unified_app_name", "name", "app_name"))),
      query_rank = row_number()
    )

  match <- results %>%
    filter(.data$result_unified_app_id == unified_app_id) %>%
    slice_head(n = 1)

  if (nrow(match) != 1) {
    options <- results %>%
      transmute(option = paste0(.data$query_rank, ": ", .data$result_name, " | ", .data$result_unified_app_id)) %>%
      pull(.data$option) %>%
      paste(collapse = "; ")
    stop("Could not verify Sensor Tower app mapping for ", title, ". Search options: ", options)
  }

  tibble(
    title = title,
    unified_app_id = unified_app_id,
    publisher_name = publisher_name,
    source_query = source_query,
    query_rank = match$query_rank[[1]],
    matched_query_name = match$result_name[[1]],
    source = "Sensor Tower unified app search verified against plan-confirmed IDs"
  )
}

build_app_mapping <- function(auth_token) {
  apps <- app_definitions()

  apps %>%
    pmap_dfr(\(...) verify_app_mapping_row(..., auth_token = auth_token)) %>%
    arrange(.data$title)
}

standardize_metrics <- function(metrics, apps, date_floor, date_ceiling, cadence) {
  expected_dates <- seq.Date(date_floor, date_ceiling, by = cadence)

  skeleton <- apps %>%
    select(title, unified_app_id) %>%
    crossing(country = c("WW", "US"), date = expected_dates)

  metrics %>%
    transmute(
      unified_app_id = as.character(.data$app_id),
      os = as.character(.data$os),
      country = as.character(.data$country),
      date = as.Date(.data$date),
      revenue_usd = as.numeric(.data$revenue),
      downloads = as.numeric(.data$downloads)
    ) %>%
    group_by(.data$unified_app_id, .data$country, .data$date) %>%
    summarise(
      revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
      downloads = sum(.data$downloads, na.rm = TRUE),
      source_rows = n(),
      .groups = "drop"
    ) %>%
    right_join(skeleton, by = c("unified_app_id", "country", "date")) %>%
    mutate(
      revenue_usd = replace_na(.data$revenue_usd, 0),
      downloads = replace_na(.data$downloads, 0),
      source_rows = replace_na(.data$source_rows, 0L),
      os = "unified"
    ) %>%
    select(title, unified_app_id, os, country, date, downloads, revenue_usd, source_rows) %>%
    arrange(.data$title, .data$country, .data$date)
}

read_existing_metrics <- function(path, date_floor, date_ceiling) {
  if (!file.exists(path)) {
    return(NULL)
  }

  existing <- read_csv(path, show_col_types = FALSE) %>%
    mutate(date = as.Date(.data$date))

  if (nrow(existing) == 0 || min(existing$date, na.rm = TRUE) > date_floor || max(existing$date, na.rm = TRUE) < date_ceiling) {
    return(NULL)
  }

  if (existing %>% count(.data$title, .data$country, .data$date) %>% filter(.data$n > 1) %>% nrow() > 0) {
    return(NULL)
  }

  existing
}

fetch_metrics_if_needed <- function(path,
                                    apps,
                                    date_floor,
                                    date_to,
                                    date_ceiling,
                                    granularity,
                                    cadence,
                                    auth_token) {
  existing <- read_existing_metrics(path, date_floor, date_ceiling)
  if (!is.null(existing)) {
    message("Reading cached metrics: ", path)
    return(existing)
  }

  message("Fetching Sensor Tower ", granularity, " metrics through ", date_to, "...")
  fetched <- st_metrics(
    app_id = apps$unified_app_id,
    metrics = c("revenue", "downloads"),
    os = "unified",
    countries = c("WW", "US"),
    date_from = date_floor,
    date_to = date_to,
    granularity = granularity,
    revenue_unit = "dollars",
    shape = "wide",
    cache = TRUE,
    auth_token = auth_token
  )

  standardized <- standardize_metrics(
    metrics = fetched,
    apps = apps,
    date_floor = date_floor,
    date_ceiling = date_ceiling,
    cadence = cadence
  )

  write_csv(standardized, path)
  standardized
}

build_weekly_cumulative <- function(weekly_title_metrics, launch_start, rpd_start) {
  weekly_title_metrics %>%
    filter(.data$date >= launch_start) %>%
    group_by(.data$title, .data$unified_app_id, .data$country) %>%
    arrange(.data$date, .by_group = TRUE) %>%
    mutate(
      cumulative_downloads = cumsum(.data$downloads),
      cumulative_revenue_usd = cumsum(.data$revenue_usd),
      cumulative_revenue_per_download_usd = if_else(
        .data$date >= rpd_start & .data$cumulative_downloads > 0,
        .data$cumulative_revenue_usd / .data$cumulative_downloads,
        NA_real_
      )
    ) %>%
    ungroup()
}

build_us_launch_weeks <- function(weekly_title_metrics) {
  weekly_title_metrics %>%
    filter(.data$country == "US", .data$downloads > 0) %>%
    group_by(.data$title, .data$unified_app_id) %>%
    summarise(us_launch_week = min(.data$date), .groups = "drop")
}

build_launch_aligned_rpd <- function(weekly_title_metrics) {
  us_launch_weeks <- build_us_launch_weeks(weekly_title_metrics)

  max_kingdom_lifecycle_week <- weekly_title_metrics %>%
    inner_join(us_launch_weeks, by = c("title", "unified_app_id")) %>%
    filter(.data$title == "Royal Kingdom", .data$country == "US", .data$date >= .data$us_launch_week) %>%
    mutate(lifecycle_week = as.integer((.data$date - .data$us_launch_week) / 7)) %>%
    summarise(max_lifecycle_week = max(.data$lifecycle_week, na.rm = TRUE)) %>%
    pull(.data$max_lifecycle_week)

  aligned_metrics <- weekly_title_metrics %>%
    inner_join(us_launch_weeks, by = c("title", "unified_app_id")) %>%
    filter(.data$date >= .data$us_launch_week) %>%
    mutate(lifecycle_week = as.integer((.data$date - .data$us_launch_week) / 7)) %>%
    filter(.data$lifecycle_week <= max_kingdom_lifecycle_week) %>%
    group_by(.data$title, .data$unified_app_id, .data$country, .data$us_launch_week) %>%
    arrange(.data$lifecycle_week, .by_group = TRUE) %>%
    mutate(
      cumulative_downloads = cumsum(.data$downloads),
      cumulative_revenue_usd = cumsum(.data$revenue_usd),
      cumulative_revenue_per_download_usd = if_else(
        .data$cumulative_downloads > 0,
        .data$cumulative_revenue_usd / .data$cumulative_downloads,
        0
      ),
      max_compared_lifecycle_week = max_kingdom_lifecycle_week
    ) %>%
    ungroup()

  launch_zero_rows <- us_launch_weeks %>%
    crossing(country = c("WW", "US")) %>%
    transmute(
      title,
      unified_app_id,
      os = "unified",
      country,
      date = .data$us_launch_week,
      downloads = 0,
      revenue_usd = 0,
      source_rows = 0,
      us_launch_week,
      lifecycle_week = 0L,
      cumulative_downloads = 0,
      cumulative_revenue_usd = 0,
      cumulative_revenue_per_download_usd = 0,
      max_compared_lifecycle_week = max_kingdom_lifecycle_week
    )

  aligned_metrics <- aligned_metrics %>%
    filter(.data$lifecycle_week > 0)

  bind_rows(launch_zero_rows, aligned_metrics) %>%
    arrange(.data$title, .data$country, .data$lifecycle_week)
}

build_monthly_portfolio <- function(monthly_title_metrics) {
  title_monthly <- monthly_title_metrics %>%
    group_by(.data$title, .data$unified_app_id, .data$country, .data$date) %>%
    summarise(
      revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
      downloads = sum(.data$downloads, na.rm = TRUE),
      .groups = "drop"
    )

  total_monthly <- title_monthly %>%
    group_by(.data$country, .data$date) %>%
    summarise(
      title = "Dream Games Portfolio",
      unified_app_id = "Royal Match + Royal Kingdom",
      revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
      downloads = sum(.data$downloads, na.rm = TRUE),
      .groups = "drop"
    )

  bind_rows(title_monthly, total_monthly) %>%
    arrange(.data$country, .data$date, .data$title)
}

build_monthly_lifecycle_revenue <- function(monthly_title_metrics) {
  us_launch_months <- monthly_title_metrics %>%
    filter(.data$country == "US", .data$downloads > 0) %>%
    group_by(.data$title, .data$unified_app_id) %>%
    summarise(us_launch_month = min(.data$date), .groups = "drop")

  max_kingdom_lifecycle_month <- monthly_title_metrics %>%
    inner_join(us_launch_months, by = c("title", "unified_app_id")) %>%
    filter(.data$title == "Royal Kingdom", .data$country == "US", .data$date >= .data$us_launch_month) %>%
    mutate(lifecycle_month = interval(.data$us_launch_month, .data$date) %/% months(1)) %>%
    summarise(max_lifecycle_month = max(.data$lifecycle_month, na.rm = TRUE)) %>%
    pull(.data$max_lifecycle_month)

  monthly_title_metrics %>%
    inner_join(us_launch_months, by = c("title", "unified_app_id")) %>%
    filter(.data$date >= .data$us_launch_month) %>%
    mutate(lifecycle_month = interval(.data$us_launch_month, .data$date) %/% months(1)) %>%
    filter(.data$lifecycle_month <= max_kingdom_lifecycle_month) %>%
    mutate(max_compared_lifecycle_month = max_kingdom_lifecycle_month) %>%
    arrange(.data$title, .data$country, .data$lifecycle_month)
}

launch_chart <- function(cumulative, country, metric, latest_week) {
  plot_data <- cumulative %>%
    filter(.data$country == .env$country) %>%
    filter(!is.na(.data[[metric]]))

  x_var <- if ("lifecycle_week" %in% names(plot_data)) "lifecycle_week" else "date"

  latest <- plot_data %>%
    group_by(.data$title) %>%
    arrange(.data[[x_var]], .by_group = TRUE) %>%
    slice_tail(n = 1) %>%
    ungroup()

  latest <- latest %>%
    mutate(
      latest_text = if (metric == "cumulative_revenue_usd") {
        format_money_short(.data[[metric]])
      } else if (metric == "cumulative_revenue_per_download_usd") {
        dollar(.data[[metric]], accuracy = 0.01)
      } else {
        format_count_short(.data[[metric]])
      },
      endpoint_label = paste0(.data$title, " ", .data$latest_text)
    )

  latest_summary <- latest %>%
    arrange(.data$title) %>%
    transmute(piece = paste0(.data$title, " ", .data$latest_text)) %>%
    pull(.data$piece) %>%
    paste(collapse = "; ")

  value_scale <- if (metric == "cumulative_revenue_usd") {
    scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale()), expand = expansion(mult = c(0, 0.16)))
  } else if (metric == "cumulative_revenue_per_download_usd") {
    scale_y_continuous(labels = label_dollar(accuracy = 0.01), expand = expansion(mult = c(0, 0.18)))
  } else {
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), expand = expansion(mult = c(0, 0.16)))
  }

  measure_noun <- if (metric == "cumulative_revenue_usd") {
    "revenue"
  } else if (metric == "cumulative_revenue_per_download_usd") {
    "revenue per download"
  } else {
    "downloads"
  }

  subtitle <- if (metric == "cumulative_revenue_per_download_usd") {
    glue("{market_label(country)}, anchored to US launch week.")
  } else {
    glue("{market_label(country)} since November 2024.")
  }

  caption <- if (metric == "cumulative_revenue_per_download_usd") {
    glue(
      "Source: Sensor Tower via SensorTowerR. Unified App Store + Google Play estimates; country = {country}; ",
      "weekly cadence; revenue in USD consumer-spend estimate. RPD uses cumulative revenue/downloads from each title's ",
      "first positive US-download week; WW RPD uses WW metrics after that US launch anchor."
    )
  } else {
    glue(
      "Source: Sensor Tower via SensorTowerR. Unified App Store + Google Play estimates; country = {country}; ",
      "weekly cadence; revenue in USD consumer-spend estimate. Cumulative values start with the week of 2024-11-11."
    )
  }

  label_nudge_x <- if (x_var == "lifecycle_week") 5 else 28

  p <- ggplot(plot_data, aes(.data[[x_var]], .data[[metric]], color = .data$title)) +
    geom_line(linewidth = 1.15)

  if (metric == "cumulative_revenue_per_download_usd") {
    p <- p +
      geom_hline(
        yintercept = 0,
        color = "#222222",
        linewidth = 0.35
      )
  }

  p <- p +
    geom_point(data = latest, size = 2.4) +
    geom_text_repel(
      data = latest,
      aes(label = .data$endpoint_label),
      size = 3.7,
      color = "#222222",
      nudge_x = label_nudge_x,
      direction = "y",
      hjust = 0,
      segment.color = "#888888",
      segment.size = 0.25,
      min.segment.length = 0,
      box.padding = 0.25,
      seed = 11
    ) +
    scale_color_manual(values = c("Royal Match" = "#2b6cb0", "Royal Kingdom" = "#d94841")) +
    value_scale

  p <- if (x_var == "lifecycle_week") {
    p +
      scale_x_continuous(
        breaks = pretty_breaks(n = 8),
        expand = expansion(mult = c(0.01, 0.2))
      ) +
      labs(x = "Weeks since US launch")
  } else {
    p +
      scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b\n%Y",
      expand = expansion(mult = c(0.01, 0.18))
    )
  }

  p +
    labs(
      title = if (metric == "cumulative_revenue_per_download_usd") {
        "Launch-Aligned RPD: Royal Match vs Royal Kingdom"
      } else {
        glue("{metric_label(metric)}: Royal Match vs Royal Kingdom")
      },
      subtitle = str_wrap(subtitle, width = 112),
      caption = str_wrap(caption, width = 124)
    ) +
    theme_538()
}

portfolio_chart <- function(portfolio_monthly, country, latest_month) {
  plot_data <- portfolio_monthly %>%
    filter(.data$country == .env$country, .data$title != "Dream Games Portfolio")

  total_data <- portfolio_monthly %>%
    filter(.data$country == .env$country, .data$title == "Dream Games Portfolio")

  latest_total <- total_data %>%
    arrange(.data$date) %>%
    slice_tail(n = 1)

  latest_text <- format_money_short(latest_total$revenue_usd[[1]])
  lifetime_text <- format_money_short(sum(total_data$revenue_usd, na.rm = TRUE))

  ggplot(plot_data, aes(.data$date, .data$revenue_usd, fill = .data$title)) +
    geom_area(alpha = 0.9, color = "white", linewidth = 0.2) +
    geom_line(
      data = total_data,
      aes(.data$date, .data$revenue_usd),
      inherit.aes = FALSE,
      color = "#222222",
      linewidth = 0.85
    ) +
    geom_text(
      data = latest_total,
      aes(.data$date, .data$revenue_usd, label = latest_text),
      inherit.aes = FALSE,
      hjust = -0.08,
      vjust = 0.4,
      size = 3.6,
      color = "#222222"
    ) +
    scale_fill_manual(values = c("Royal Match" = "#2b6cb0", "Royal Kingdom" = "#d94841")) +
    scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale()), expand = expansion(mult = c(0, 0.16))) +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b\n%Y",
      expand = expansion(mult = c(0.005, 0.08))
    ) +
    labs(
      title = "Dream Games Portfolio Revenue",
      subtitle = glue("{market_label(country)} monthly revenue; latest {latest_text}."),
      caption = str_wrap(
        glue(
          "Source: Sensor Tower via SensorTowerR. Unified App Store + Google Play estimates; country = {country}; ",
          "monthly cadence; revenue in USD consumer-spend estimate. Black line is total portfolio revenue."
        ),
        width = 124
      )
    ) +
    theme_538()
}

lifecycle_revenue_chart <- function(lifecycle_revenue, country) {
  plot_data <- lifecycle_revenue %>%
    filter(.data$country == .env$country)

  latest <- plot_data %>%
    group_by(.data$title) %>%
    arrange(.data$lifecycle_month, .by_group = TRUE) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(endpoint_label = paste0(.data$title, " ", format_money_short(.data$revenue_usd)))

  max_month <- max(plot_data$lifecycle_month, na.rm = TRUE)

  ggplot(plot_data, aes(.data$lifecycle_month, .data$revenue_usd, color = .data$title)) +
    geom_point(alpha = 0.32, size = 2.1) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 1.25, span = 0.32) +
    geom_text_repel(
      data = latest,
      aes(label = .data$endpoint_label),
      size = 3.7,
      color = "#222222",
      nudge_x = 1.8,
      direction = "y",
      hjust = 0,
      segment.color = "#888888",
      segment.size = 0.25,
      min.segment.length = 0,
      box.padding = 0.25,
      seed = 17
    ) +
    scale_color_manual(values = c("Royal Match" = "#2b6cb0", "Royal Kingdom" = "#d94841")) +
    scale_x_continuous(
      breaks = pretty_breaks(n = 8),
      expand = expansion(mult = c(0.01, 0.16))
    ) +
    scale_y_continuous(
      labels = label_dollar(scale_cut = cut_short_scale()),
      expand = expansion(mult = c(0, 0.16))
    ) +
    labs(
      title = "Lifecycle Revenue: Royal Match vs Royal Kingdom",
      subtitle = glue("{market_label(country)}, months since US launch."),
      x = "Months since US launch",
      caption = str_wrap(
        glue(
          "Source: Sensor Tower via SensorTowerR. Unified App Store + Google Play estimates; country = {country}; ",
          "monthly cadence; revenue in USD consumer-spend estimate. Points are observed months; smooth line is LOESS."
        ),
        width = 124
      )
    ) +
    theme_538()
}

build_validation_checks <- function(app_mapping,
                                    weekly_title_metrics,
                                    weekly_cumulative,
                                    weekly_launch_aligned_rpd,
                                    monthly_title_metrics,
                                    portfolio_monthly,
                                    lifecycle_revenue,
                                    launch_start,
                                    weekly_metrics_start,
                                    rpd_start,
                                    latest_week,
                                    monthly_start,
                                    latest_month) {
  app_checks <- tibble(
    check_name = c(
      "app_mapping_has_two_titles",
      "app_mapping_ids_match_plan",
      "app_mapping_query_verified"
    ),
    check_passed = c(
      nrow(app_mapping) == 2 && n_distinct(app_mapping$title) == 2,
      setequal(app_mapping$unified_app_id, app_definitions()$unified_app_id),
      all(!is.na(app_mapping$matched_query_name) & nzchar(app_mapping$matched_query_name))
    ),
    detail = c(
      paste(app_mapping$title, collapse = ", "),
      paste(app_mapping$unified_app_id, collapse = ", "),
      paste(app_mapping$matched_query_name, collapse = ", ")
    )
  )

  duplicate_checks <- tibble(
    check_name = c("weekly_no_duplicate_grain", "monthly_no_duplicate_grain"),
    check_passed = c(
      weekly_title_metrics %>% count(.data$title, .data$country, .data$date) %>% pull(.data$n) %>% max() == 1,
      monthly_title_metrics %>% count(.data$title, .data$country, .data$date) %>% pull(.data$n) %>% max() == 1
    ),
    detail = c("grain: title + country + week", "grain: title + country + month")
  )

  coverage_checks <- tibble(
    check_name = c(
      "weekly_metrics_start_present",
      "weekly_latest_complete_week_present",
      "monthly_start_present",
      "monthly_latest_complete_month_present",
      "weekly_expected_country_set",
      "monthly_expected_country_set"
    ),
    check_passed = c(
      min(weekly_title_metrics$date) == weekly_metrics_start,
      max(weekly_title_metrics$date) == latest_week,
      min(monthly_title_metrics$date) == monthly_start,
      max(monthly_title_metrics$date) == latest_month,
      setequal(unique(weekly_title_metrics$country), c("WW", "US")),
      setequal(unique(monthly_title_metrics$country), c("WW", "US"))
    ),
    detail = c(
      as.character(min(weekly_title_metrics$date)),
      as.character(max(weekly_title_metrics$date)),
      as.character(min(monthly_title_metrics$date)),
      as.character(max(monthly_title_metrics$date)),
      paste(sort(unique(weekly_title_metrics$country)), collapse = ", "),
      paste(sort(unique(monthly_title_metrics$country)), collapse = ", ")
    )
  )

  cumulative_reconcile <- weekly_cumulative %>%
    group_by(.data$title, .data$country) %>%
    summarise(
      downloads_sum = sum(.data$downloads, na.rm = TRUE),
      revenue_sum = sum(.data$revenue_usd, na.rm = TRUE),
      final_cumulative_downloads = last(.data$cumulative_downloads),
      final_cumulative_revenue_usd = last(.data$cumulative_revenue_usd),
      .groups = "drop"
    ) %>%
    mutate(
      downloads_match = abs(.data$downloads_sum - .data$final_cumulative_downloads) < 0.01,
      revenue_match = abs(.data$revenue_sum - .data$final_cumulative_revenue_usd) < 0.01
    )

  portfolio_reconcile <- monthly_title_metrics %>%
    group_by(.data$country, .data$date) %>%
    summarise(title_sum = sum(.data$revenue_usd, na.rm = TRUE), .groups = "drop") %>%
    left_join(
      portfolio_monthly %>%
        filter(.data$title == "Dream Games Portfolio") %>%
        select(country, date, portfolio_revenue_usd = revenue_usd),
      by = c("country", "date")
    ) %>%
    mutate(revenue_match = abs(.data$title_sum - .data$portfolio_revenue_usd) < 0.01)

  rpd_rows <- weekly_launch_aligned_rpd
  us_launch_weeks <- build_us_launch_weeks(weekly_title_metrics)
  us_first_positive_downloads <- weekly_title_metrics %>%
    filter(.data$country == "US", .data$downloads > 0) %>%
    group_by(.data$title, .data$unified_app_id) %>%
    summarise(first_positive_us_download_week = min(.data$date), .groups = "drop")
  rpd_launch_check <- rpd_rows %>%
    distinct(title, unified_app_id, us_launch_week) %>%
    left_join(us_first_positive_downloads, by = c("title", "unified_app_id")) %>%
    mutate(matches = .data$us_launch_week == .data$first_positive_us_download_week)
  rpd_week0 <- rpd_rows %>%
    filter(.data$lifecycle_week == 0)
  rpd_shared_index <- rpd_rows %>%
    distinct(title, country, date, lifecycle_week, us_launch_week) %>%
    pivot_wider(
      names_from = country,
      values_from = c(lifecycle_week, us_launch_week),
      names_sep = "_"
    ) %>%
    mutate(
      shared_index = .data$lifecycle_week_US == .data$lifecycle_week_WW,
      shared_anchor = .data$us_launch_week_US == .data$us_launch_week_WW
    )
  soft_launch_leak <- weekly_title_metrics %>%
    inner_join(us_launch_weeks, by = c("title", "unified_app_id")) %>%
    filter(.data$date < .data$us_launch_week) %>%
    semi_join(rpd_rows, by = c("title", "unified_app_id", "country", "date"))

  reconciliation_checks <- tibble(
    check_name = c(
      "weekly_downloads_reconcile_to_final_cumulative",
      "weekly_revenue_reconcile_to_final_cumulative",
      "monthly_title_revenue_reconciles_to_portfolio",
      "rpd_launch_aligned_has_both_titles",
      "rpd_us_launch_week_equals_first_positive_us_downloads",
      "rpd_lifecycle_week_starts_at_zero",
      "rpd_week_zero_is_zero",
      "rpd_excludes_soft_launch_rows",
      "rpd_us_and_ww_share_us_launch_index",
      "lifecycle_revenue_has_both_titles_and_countries",
      "revenue_unit_sanity"
    ),
    check_passed = c(
      all(cumulative_reconcile$downloads_match),
      all(cumulative_reconcile$revenue_match),
      all(portfolio_reconcile$revenue_match),
      setequal(unique(rpd_rows$title), c("Royal Match", "Royal Kingdom")),
      all(rpd_launch_check$matches),
      all(rpd_rows %>% group_by(.data$title, .data$country) %>% summarise(min_week = min(.data$lifecycle_week), .groups = "drop") %>% pull(.data$min_week) == 0),
      all(rpd_week0$cumulative_downloads == 0 & rpd_week0$cumulative_revenue_usd == 0 & rpd_week0$cumulative_revenue_per_download_usd == 0),
      nrow(soft_launch_leak) == 0,
      all(rpd_shared_index$shared_index & rpd_shared_index$shared_anchor, na.rm = TRUE),
      setequal(unique(lifecycle_revenue$title), c("Royal Match", "Royal Kingdom")) &&
        setequal(unique(lifecycle_revenue$country), c("WW", "US")),
      max(weekly_title_metrics$revenue_usd, na.rm = TRUE) < 5e8 &&
        max(monthly_title_metrics$revenue_usd, na.rm = TRUE) < 2e9
    ),
    detail = c(
      paste(cumulative_reconcile$title, cumulative_reconcile$country, cumulative_reconcile$downloads_match, collapse = "; "),
      paste(cumulative_reconcile$title, cumulative_reconcile$country, cumulative_reconcile$revenue_match, collapse = "; "),
      paste0(sum(portfolio_reconcile$revenue_match), "/", nrow(portfolio_reconcile), " monthly rows match"),
      paste(sort(unique(rpd_rows$title)), collapse = ", "),
      paste(rpd_launch_check %>% transmute(piece = paste0(.data$title, ": ", .data$us_launch_week)) %>% pull(.data$piece), collapse = "; "),
      paste(rpd_rows %>% group_by(.data$title, .data$country) %>% summarise(min_week = min(.data$lifecycle_week), .groups = "drop") %>% transmute(piece = paste0(.data$title, " ", .data$country, ": ", .data$min_week)) %>% pull(.data$piece), collapse = "; "),
      paste0(nrow(rpd_week0), " week-zero rows set to zero"),
      paste0(nrow(soft_launch_leak), " soft-launch rows in RPD output"),
      paste0(sum(rpd_shared_index$shared_index & rpd_shared_index$shared_anchor, na.rm = TRUE), "/", nrow(rpd_shared_index), " title-date rows share index"),
      paste(sort(unique(lifecycle_revenue$title)), collapse = ", "),
      "Revenue expected in dollars, not cents"
    )
  )

  bind_rows(app_checks, duplicate_checks, coverage_checks, reconciliation_checks)
}

copy_preview_outputs <- function(output_dir, preview_dir) {
  ensure_dir(preview_dir)
  old <- list.files(preview_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(old) > 0) {
    unlink(old)
  }

  pngs <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)
  copied <- file.copy(pngs, file.path(preview_dir, basename(pngs)), overwrite = TRUE)
  if (!all(copied)) {
    stop("Failed to copy one or more preview PNGs.")
  }

  tibble(
    output_path = pngs,
    preview_path = file.path(preview_dir, basename(pngs))
  )
}

script_dir <- get_script_dir()
data_dir <- ensure_dir(file.path(script_dir, "data"))
output_dir <- ensure_dir(file.path(script_dir, "output"))
preview_dir <- "/tmp/codex_preview/dream_games_royal_kingdom_launch"

launch_start <- as.Date("2024-11-11")
rpd_start <- as.Date("2024-11-18")
monthly_start <- as.Date("2021-01-01")
weekly_metrics_start <- floor_date(monthly_start, unit = "week", week_start = 1)

today <- Sys.Date()
latest_week <- floor_date(today, unit = "week", week_start = 1) - days(7)
latest_week_end <- latest_week + days(6)
latest_month <- floor_date(today, unit = "month") - months(1)
latest_month_end <- ceiling_date(latest_month, unit = "month") - days(1)

if (latest_week < launch_start) {
  stop("Latest complete week is before launch start.")
}
if (latest_month < monthly_start) {
  stop("Latest complete month is before monthly start.")
}

stale_pngs <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)
if (length(stale_pngs) > 0) {
  unlink(stale_pngs)
}

auth_token <- load_sensortower_token()
load_sensortower_package()

apps <- app_definitions()
app_mapping <- build_app_mapping(auth_token)
write_csv(app_mapping, file.path(data_dir, "app_mapping.csv"))

api_manifest <- tibble(
  pull_name = c("weekly_title_metrics", "monthly_title_metrics"),
  source = "Sensor Tower API via SensorTowerR::st_metrics",
  os = "unified",
  countries = "WW, US",
  app_ids = paste(apps$unified_app_id, collapse = ", "),
  metrics = "revenue, downloads",
  revenue_unit = "dollars",
  granularity = c("weekly", "monthly"),
  date_from = c(as.character(weekly_metrics_start), as.character(monthly_start)),
  date_to = c(as.character(latest_week_end), as.character(latest_month_end)),
  normalized_max_date = c(as.character(latest_week), as.character(latest_month))
)
write_csv(api_manifest, file.path(data_dir, "source_api_manifest.csv"))

weekly_title_metrics <- fetch_metrics_if_needed(
  path = file.path(data_dir, "weekly_title_metrics.csv"),
  apps = apps,
  date_floor = weekly_metrics_start,
  date_to = latest_week_end,
  date_ceiling = latest_week,
  granularity = "weekly",
  cadence = "week",
  auth_token = auth_token
)

monthly_title_metrics <- fetch_metrics_if_needed(
  path = file.path(data_dir, "monthly_title_metrics.csv"),
  apps = apps,
  date_floor = monthly_start,
  date_to = latest_month_end,
  date_ceiling = latest_month,
  granularity = "monthly",
  cadence = "month",
  auth_token = auth_token
)

weekly_cumulative <- build_weekly_cumulative(
  weekly_title_metrics = weekly_title_metrics,
  launch_start = launch_start,
  rpd_start = rpd_start
)
write_csv(weekly_cumulative, file.path(data_dir, "weekly_cumulative_metrics.csv"))

weekly_launch_aligned_rpd <- build_launch_aligned_rpd(
  weekly_title_metrics = weekly_title_metrics
)
write_csv(weekly_launch_aligned_rpd, file.path(data_dir, "weekly_launch_aligned_rpd.csv"))

portfolio_monthly <- build_monthly_portfolio(monthly_title_metrics)
write_csv(portfolio_monthly, file.path(data_dir, "monthly_portfolio_revenue.csv"))

lifecycle_revenue <- build_monthly_lifecycle_revenue(monthly_title_metrics)
write_csv(lifecycle_revenue, file.path(data_dir, "monthly_lifecycle_revenue.csv"))

chart_manifest <- bind_rows(
  expand_grid(
    country = c("WW", "US"),
    metric = c("cumulative_downloads", "cumulative_revenue_usd", "cumulative_revenue_per_download_usd")
  ) %>%
    mutate(
      chart_type = "royal_kingdom_launch",
      file_name = paste0(
        "royal_match_vs_royal_kingdom_",
        metric,
        "_",
        market_slug(.data$country),
        "_weekly_538.png"
      )
    ),
  tibble(
    chart_type = "dream_games_portfolio",
    country = c("WW", "US"),
    metric = "portfolio_revenue_usd",
    file_name = paste0("dream_games_portfolio_revenue_", market_slug(c("WW", "US")), "_monthly_538.png")
  ),
  tibble(
    chart_type = "dream_games_lifecycle_revenue",
    country = c("WW", "US"),
    metric = "lifecycle_revenue_usd",
    file_name = paste0("dream_games_lifecycle_revenue_", market_slug(c("WW", "US")), "_monthly_538.png")
  )
) %>%
  mutate(output_path = file.path(output_dir, .data$file_name))

launch_chart_rows <- chart_manifest %>%
  filter(.data$chart_type == "royal_kingdom_launch")
portfolio_chart_rows <- chart_manifest %>%
  filter(.data$chart_type == "dream_games_portfolio")
lifecycle_revenue_chart_rows <- chart_manifest %>%
  filter(.data$chart_type == "dream_games_lifecycle_revenue")

walk2(
  split(launch_chart_rows, seq_len(nrow(launch_chart_rows))),
  launch_chart_rows$output_path,
  \(row, path) {
    plot <- launch_chart(
      cumulative = if (row$metric[[1]] == "cumulative_revenue_per_download_usd") {
        weekly_launch_aligned_rpd
      } else {
        weekly_cumulative
      },
      country = row$country[[1]],
      metric = row$metric[[1]],
      latest_week = latest_week
    )
    write_png(plot, path)
  }
)

walk2(
  split(portfolio_chart_rows, seq_len(nrow(portfolio_chart_rows))),
  portfolio_chart_rows$output_path,
  \(row, path) {
    plot <- portfolio_chart(
      portfolio_monthly = portfolio_monthly,
      country = row$country[[1]],
      latest_month = latest_month
    )
    write_png(plot, path)
  }
)

walk2(
  split(lifecycle_revenue_chart_rows, seq_len(nrow(lifecycle_revenue_chart_rows))),
  lifecycle_revenue_chart_rows$output_path,
  \(row, path) {
    plot <- lifecycle_revenue_chart(
      lifecycle_revenue = lifecycle_revenue,
      country = row$country[[1]]
    )
    write_png(plot, path)
  }
)

validation_checks <- build_validation_checks(
  app_mapping = app_mapping,
  weekly_title_metrics = weekly_title_metrics,
  weekly_cumulative = weekly_cumulative,
  weekly_launch_aligned_rpd = weekly_launch_aligned_rpd,
  monthly_title_metrics = monthly_title_metrics,
  portfolio_monthly = portfolio_monthly,
  lifecycle_revenue = lifecycle_revenue,
  launch_start = launch_start,
  weekly_metrics_start = weekly_metrics_start,
  rpd_start = rpd_start,
  latest_week = latest_week,
  monthly_start = monthly_start,
  latest_month = latest_month
)
write_csv(validation_checks, file.path(data_dir, "validation_checks.csv"))

if (!all(validation_checks$check_passed)) {
  failed <- validation_checks %>%
    filter(!.data$check_passed) %>%
    transmute(msg = paste0(.data$check_name, ": ", .data$detail)) %>%
    pull(.data$msg)
  stop("Validation failed:\n", paste(failed, collapse = "\n"))
}

preview_manifest <- copy_preview_outputs(output_dir, preview_dir)
chart_manifest <- chart_manifest %>%
  left_join(preview_manifest, by = "output_path")
write_csv(chart_manifest, file.path(data_dir, "chart_outputs.csv"))

message("Dream Games Royal Kingdom launch workflow completed.")
message("Charts written: ", nrow(chart_manifest))
message("Latest complete week: ", latest_week)
message("Latest complete month: ", latest_month)
