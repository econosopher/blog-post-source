#!/usr/bin/env Rscript

# Squad Busters Active Users Overview
# Generates monthly active user trends and share charts since global launch.

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
script_dir <- if (length(script_path) == 0) normalizePath(getwd()) else normalizePath(dirname(script_path[1]))
pkg_path <- normalizePath(file.path(script_dir, "..", "..", "sensortowerR"))

suppressPackageStartupMessages({
  library(pacman)
  devtools::load_all(pkg_path)
  p_load(dplyr, tidyr, purrr, lubridate, ggplot2, scales, readr, stringr, httr2, glue)
})

# Ensure the API token is available
if (!nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN"))) {
  stop("SENSORTOWER_AUTH_TOKEN environment variable is not set.")
}

message("=== Squad Busters Active Users ===\n")

launch_date <- as.Date("2024-05-29")
start_date <- floor_date(launch_date, "month")
end_date <- Sys.Date()

metric <- "mau"
metric_period <- switch(metric,
  "dau" = "day",
  "wau" = "week",
  "mau" = "month",
  stop("Unsupported metric: ", metric)
)

countries <- "WW"

apps <- tibble::tribble(
  ~app_name,           ~unified_id,
  "Squad Busters",     "66279a1d70f1126b3489998d",
  "Clash of Clans",    "55c5025102ac64f9c0001f96",
  "Clash Royale",      "568a601402ac64a409000104",
  "Brawl Stars",       "594278840211a6e5560000b2",
  "Hay Day",           "55c5022602ac64f9c0001f7c",
  "Boom Beach",        "55c5137802ac64f9c000258b"
)

safe_lookup <- function(unified_id) {
  tryCatch(
    st_app_lookup(unified_id, verbose = FALSE),
    error = function(e) NULL
  )
}

lookup_ids <- apps %>%
  mutate(
    lookup = map(unified_id, safe_lookup),
    ios_id = map_chr(lookup, ~ if (!is.null(.x$ios_app_id) && nzchar(.x$ios_app_id)) as.character(.x$ios_app_id) else NA_character_),
    android_id = map_chr(lookup, ~ if (!is.null(.x$android_app_id) && nzchar(.x$android_app_id)) as.character(.x$android_app_id) else NA_character_)
  ) %>%
  select(-lookup)

fetch_active_platform <- function(platform_os, platform_id) {
  empty_tbl <- tibble(date = as.Date(character()), value = numeric())
  if (is.na(platform_id) || !nzchar(platform_id)) {
    return(empty_tbl)
  }
  base_url <- glue::glue("https://api.sensortower.com/v1/{platform_os}/usage/active_users")
  resp <- httr2::request(base_url) %>%
    httr2::req_url_query(
      app_ids = platform_id,
      countries = countries,
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      time_period = metric_period,
      auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
    ) %>%
    httr2::req_perform()
  if (httr2::resp_status(resp) != 200) {
    message(glue::glue("Warning: active user request failed for {platform_os} {platform_id} (status {httr2::resp_status(resp)})"))
    return(empty_tbl)
  }
  payload <- httr2::resp_body_json(resp)
  if (length(payload) == 0) return(empty_tbl)
  df <- tryCatch(dplyr::bind_rows(payload), error = function(e) empty_tbl)
  if (nrow(df) == 0) return(empty_tbl)
  if (platform_os == "ios") {
    df <- df %>% mutate(users = coalesce(iphone_users, 0) + coalesce(ipad_users, 0))
  } else {
    if (!"users" %in% names(df) && "android_users" %in% names(df)) {
      df <- df %>% mutate(users = android_users)
    }
    df <- df %>% mutate(users = coalesce(users, 0))
  }
  df %>%
    transmute(
      date = as.Date(date),
      value = as.numeric(coalesce(users, 0))
    )
}

fetch_active_total <- function(app_name, ios_id, android_id) {
  ios_df <- fetch_active_platform("ios", ios_id) %>% mutate(platform = "ios")
  android_df <- fetch_active_platform("android", android_id) %>% mutate(platform = "android")
  combined <- bind_rows(ios_df, android_df)
  if (nrow(combined) == 0) {
    return(tibble(date = as.Date(character()), active_users = numeric(), app_name = character()))
  }
  combined %>%
    group_by(date) %>%
    summarise(active_users = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(app_name = app_name)
}

active_data <- pmap_dfr(
  list(lookup_ids$app_name, lookup_ids$ios_id, lookup_ids$android_id),
  fetch_active_total
) %>%
  filter(date >= start_date) %>%
  mutate(month = date)

if (nrow(active_data) == 0) {
  stop("No active user data returned. Aborting.")
}

message(glue::glue("Fetched {nrow(active_data)} rows covering {length(unique(active_data$month))} months."))

# Save monthly series
output_dir <- file.path(script_dir, "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

active_data %>%
  arrange(app_name, month) %>%
  write_csv(file.path(output_dir, "squad_busters_mau_by_month.csv"))

# Chart 1: Squad Busters MAU trend
squad_series <- active_data %>% filter(app_name == "Squad Busters")

line_plot <- ggplot(squad_series, aes(x = month, y = active_users)) +
  geom_line(color = "#ff6b35", linewidth = 1.2) +
  geom_point(color = "#ff6b35", size = 2) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, scale_cut = cut_short_scale())) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(
    title = "Squad Busters Monthly Active Users",
    subtitle = "Worldwide MAU since global launch (combined iOS + Android)",
    x = NULL,
    y = "Monthly Active Users",
    caption = "Source: Sensor Tower API"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(output_dir, "squad_busters_mau_trend.png"), line_plot, width = 10, height = 6, dpi = 320)

# Chart 2: Squad Busters vs rest of Supercell portfolio
share_data <- active_data %>%
  mutate(group = if_else(app_name == "Squad Busters", "Squad Busters", "Other Supercell Games")) %>%
  group_by(month, group) %>%
  summarise(active_users = sum(active_users, na.rm = TRUE), .groups = "drop") %>%
  group_by(month) %>%
  mutate(share = active_users / sum(active_users, na.rm = TRUE)) %>%
  ungroup()

stack_plot <- ggplot(share_data, aes(x = month, y = share, fill = group)) +
  geom_col(position = "fill", width = 25) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_fill_manual(values = c("Squad Busters" = "#ff6b35", "Other Supercell Games" = "#14213d")) +
  labs(
    title = "Share of Supercell Active Users",
    subtitle = "Worldwide MAU share (Squad Busters vs. rest of portfolio)",
    x = NULL,
    y = "Share of Active Users",
    fill = NULL,
    caption = "Source: Sensor Tower API"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

ggsave(file.path(output_dir, "squad_vs_portfolio_share.png"), stack_plot, width = 10, height = 6, dpi = 320)

message("Charts saved to ", output_dir)
