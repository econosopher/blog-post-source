#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(devtools)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(tibble)
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

make_check <- function(name, passed, detail) {
  tibble(
    check_name = name,
    check_passed = isTRUE(passed),
    detail = as.character(detail)
  )
}

assert_no_failed_checks <- function(checks) {
  failed <- checks |> filter(!.data$check_passed)
  if (nrow(failed) > 0) {
    print(failed, n = Inf)
    stop("Validation checks failed: ", paste(failed$check_name, collapse = ", "))
  }
}

script_dir <- get_script_dir()
data_dir <- ensure_dir(file.path(script_dir, "data"))

load_sensortower_token()
load_sensortower_package()

analysis_date <- Sys.Date()
latest_complete_month_end <- floor_date(analysis_date, "month") - days(1)
market_start_date <- as.Date("2012-01-01")
monopoly_go_id <- "62be6a5fbab10c69c5a0a42a"

message("Fetching MONOPOLY GO! monthly WW unified revenue/downloads.")
monopoly_go_raw <- st_metrics(
  app_id = monopoly_go_id,
  metrics = c("revenue", "downloads"),
  os = "unified",
  countries = "WW",
  date_from = market_start_date,
  date_to = latest_complete_month_end,
  granularity = "monthly",
  revenue_unit = "dollars",
  shape = "wide",
  cache = TRUE,
  auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
)

monopoly_go_monthly <- monopoly_go_raw |>
  transmute(
    date = as.Date(.data$date),
    unified_app_id = as.character(.data$app_id),
    app_name = "MONOPOLY GO!",
    monopoly_go_revenue_usd = coalesce(as.numeric(.data$revenue), 0),
    monopoly_go_downloads = coalesce(as.numeric(.data$downloads), 0),
    source_endpoint = "st_metrics /v1/unified/sales_report_estimates",
    source_scope = "WW unified iOS+Android monthly",
    latest_complete_month = as.Date(format(latest_complete_month_end, "%Y-%m-01"))
  ) |>
  group_by(.data$date, .data$unified_app_id, .data$app_name, .data$source_endpoint, .data$source_scope, .data$latest_complete_month) |>
  summarise(
    monopoly_go_revenue_usd = sum(.data$monopoly_go_revenue_usd, na.rm = TRUE),
    monopoly_go_downloads = sum(.data$monopoly_go_downloads, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$date)

merge_ready <- monopoly_go_monthly |>
  mutate(
    month = format(.data$date, "%Y-%m"),
    monopoly_go_revenue_usd_millions = .data$monopoly_go_revenue_usd / 1e6
  ) |>
  select(
    all_of(c(
      "date",
      "month",
      "unified_app_id",
      "app_name",
      "monopoly_go_revenue_usd",
      "monopoly_go_revenue_usd_millions",
      "monopoly_go_downloads",
      "source_endpoint",
      "source_scope",
      "latest_complete_month"
    ))
  )

checks <- bind_rows(
  make_check(
    "monopoly_go_rows_present",
    nrow(merge_ready) > 0,
    paste("Rows:", nrow(merge_ready))
  ),
  make_check(
    "monopoly_go_id_matches",
    all(merge_ready$unified_app_id == monopoly_go_id),
    paste("Unified app ID:", monopoly_go_id)
  ),
  make_check(
    "monopoly_go_latest_complete_month",
    max(merge_ready$date, na.rm = TRUE) == as.Date(format(latest_complete_month_end, "%Y-%m-01")),
    paste("Latest row:", max(merge_ready$date, na.rm = TRUE), "latest complete month:", latest_complete_month_end)
  ),
  make_check(
    "monopoly_go_revenue_nonnegative",
    all(merge_ready$monopoly_go_revenue_usd >= 0, na.rm = TRUE),
    "All monthly revenue values are nonnegative."
  )
)

write_csv(monopoly_go_monthly, file.path(data_dir, "monopoly_go_monthly_metrics.csv"), na = "")
write_csv(merge_ready, file.path(data_dir, "monopoly_go_monthly_revenue_for_subgenre_merge.csv"), na = "")
write_csv(checks, file.path(data_dir, "monopoly_go_revenue_validation_checks.csv"), na = "")
assert_no_failed_checks(checks)

message("\nOutputs written:")
message("  ", file.path(data_dir, "monopoly_go_monthly_metrics.csv"))
message("  ", file.path(data_dir, "monopoly_go_monthly_revenue_for_subgenre_merge.csv"))
message("  ", file.path(data_dir, "monopoly_go_revenue_validation_checks.csv"))
