#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(devtools)
  library(dplyr)
  library(glue)
  library(gt)
  library(lubridate)
  library(purrr)
  library(readr)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)
})

get_script_dir <- function() {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]

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

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

load_sensortower_token <- function() {
  if (!nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN"))) {
    candidate_paths <- c(
      "/Users/phillip/Documents/secrets/global.env",
      "/Users/phillip/Documents/secrets/home/.Renviron",
      "/Users/phillip/Documents/vibe_coding_projects/.env",
      path.expand("~/.Renviron"),
      path.expand("~/.api_keys")
    )

    for (path in candidate_paths[file.exists(candidate_paths)]) {
      try(readRenviron(path), silent = TRUE)
    }
  }

  auth_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  if (!nzchar(auth_token)) {
    stop("SENSORTOWER_AUTH_TOKEN is not set. Load it through the canonical secrets/env path; do not hardcode tokens.")
  }

  invisible(auth_token)
}

load_sensortower_package <- function() {
  pkg_path <- "/Users/phillip/Documents/vibe_coding_projects/videogameR-universe/SensorTowerR"
  if (!dir.exists(pkg_path)) {
    stop("SensorTowerR package folder not found: ", pkg_path)
  }

  devtools::load_all(pkg_path, quiet = TRUE)
  invisible(TRUE)
}

make_check <- function(name, passed, detail) {
  tibble(
    check_name = name,
    check_passed = isTRUE(passed),
    detail = as.character(detail)
  )
}

assert_no_failed_checks <- function(checks) {
  failed <- checks %>% filter(!.data$check_passed)
  if (nrow(failed) > 0) {
    print(failed, n = Inf)
    stop("Validation checks failed: ", paste(failed$check_name, collapse = ", "))
  }
}

format_money_short <- function(x) {
  case_when(
    is.na(x) | x == 0 ~ "-",
    abs(x) >= 1e9 ~ paste0("$", number(x / 1e9, accuracy = 0.1), "B"),
    abs(x) >= 1e6 ~ paste0("$", number(x / 1e6, accuracy = 1), "M"),
    abs(x) >= 1e3 ~ paste0("$", number(x / 1e3, accuracy = 1), "K"),
    TRUE ~ paste0("$", number(x, accuracy = 1))
  )
}

format_count_short <- function(x) {
  case_when(
    is.na(x) | x == 0 ~ "-",
    abs(x) >= 1e9 ~ paste0(number(x / 1e9, accuracy = 0.1), "B"),
    abs(x) >= 1e6 ~ paste0(number(x / 1e6, accuracy = 1), "M"),
    abs(x) >= 1e3 ~ paste0(number(x / 1e3, accuracy = 1), "K"),
    TRUE ~ number(x, accuracy = 1)
  )
}

format_yoy <- function(x) {
  case_when(
    is.na(x) ~ "-",
    x > 0 ~ paste0("+", round(x), "%"),
    TRUE ~ paste0(round(x), "%")
  )
}

yoy_fill <- function(x) {
  case_when(
    is.na(x) ~ "transparent",
    x <= -50 ~ "#ef6b5a",
    x < 0 ~ "#f8d8d2",
    x == 0 ~ "#f7f7f7",
    x < 50 ~ "#d9ead3",
    TRUE ~ "#b7d7a8"
  )
}

safe_read_rds <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
}

clean_app_name <- function(x) {
  case_when(
    x == "剑与远征 - AFK" ~ "AFK Arena (CN)",
    x == "小冰冰传奇-官方怀旧服" ~ "Soul Clash (Retro)",
    TRUE ~ x
  )
}

fetch_app_mapping <- function(app_ids, cache_path) {
  if (file.exists(cache_path)) {
    cached <- read_csv(cache_path, show_col_types = FALSE)
    if (all(app_ids %in% cached$unified_app_id)) {
      return(cached %>% filter(.data$unified_app_id %in% app_ids))
    }
  }

  rows <- imap_dfr(app_ids, function(app_id, idx) {
    message(glue("  Mapping app {idx}/{length(app_ids)}: {app_id}"))
    out <- tryCatch(
      st_get_unified_mapping(
        app_id,
        os = "unified",
        auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
      ),
      error = function(e) {
        message("    Mapping failed: ", conditionMessage(e))
        tibble(input_id = app_id, unified_app_id = app_id, unified_app_name = NA_character_)
      }
    )
    Sys.sleep(0.15)
    as_tibble(out)
  }) %>%
    mutate(
      unified_app_id = as.character(.data$unified_app_id),
      unified_app_name = as.character(.data$unified_app_name)
    ) %>%
    distinct(.data$unified_app_id, .keep_all = TRUE)

  write_csv(rows, cache_path, na = "")
  rows
}

fetch_sales_data <- function(app_ids, date_from, date_to, cache_path) {
  if (file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    cached <- cached %>% mutate(date = as.Date(.data$date))
    if (nrow(cached) > 0 && max(cached$date, na.rm = TRUE) >= floor_date(date_to, "month")) {
      message("Using cached sales data: ", cache_path)
      return(cached)
    }
  }

  rows <- imap_dfr(app_ids, function(app_id, idx) {
    message(glue("  Fetching sales {idx}/{length(app_ids)}: {app_id}"))
    out <- tryCatch(
      st_metrics(
        app_id = app_id,
        metrics = c("revenue", "downloads"),
        os = "unified",
        countries = "WW",
        date_from = date_from,
        date_to = date_to,
        granularity = "monthly",
        revenue_unit = "dollars",
        shape = "wide",
        cache = TRUE,
        auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
      ),
      error = function(e) {
        message("    Sales fetch failed: ", conditionMessage(e))
        tibble()
      }
    )
    Sys.sleep(0.25)
    as_tibble(out)
  })

  rows <- rows %>%
    transmute(
      unified_app_id = as.character(.data$app_id),
      os = as.character(.data$os),
      country = as.character(.data$country),
      date = as.Date(.data$date),
      revenue = as.numeric(.data$revenue),
      downloads = as.numeric(.data$downloads)
    ) %>%
    arrange(.data$unified_app_id, .data$date, .data$country)

  saveRDS(rows, cache_path)
  rows
}

fetch_mau_data <- function(app_ids, date_from, date_to, cache_path) {
  if (file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    cached <- cached %>% mutate(date = as.Date(.data$date))
    if (nrow(cached) > 0 && max(cached$date, na.rm = TRUE) >= floor_date(date_to, "month")) {
      message("Using cached MAU data: ", cache_path)
      return(cached)
    }
  }

  rows <- imap_dfr(app_ids, function(app_id, idx) {
    message(glue("  Fetching MAU {idx}/{length(app_ids)}: {app_id}"))
    out <- tryCatch(
      st_active_users(
        os = "unified",
        app_list = app_id,
        metrics = "mau",
        date_range = list(start_date = date_from, end_date = date_to),
        countries = "WW",
        granularity = "monthly",
        parallel = FALSE,
        verbose = FALSE,
        auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
      ),
      error = function(e) {
        message("    MAU fetch failed: ", conditionMessage(e))
        tibble()
      }
    )
    Sys.sleep(0.25)
    as_tibble(out)
  })

  rows <- rows %>%
    filter(.data$metric == "mau") %>%
    mutate(
      original_id = as.character(.data$original_id),
      date = as.Date(.data$date),
      country = as.character(.data$country),
      metric = as.character(.data$metric),
      value = as.numeric(.data$value)
    )

  saveRDS(rows, cache_path)
  rows
}

message("=== Lilith Games Portfolio Refresh ===")

script_dir <- get_script_dir()
setwd(script_dir)

data_dir <- ensure_dir(file.path(script_dir, "data"))
cache_dir <- ensure_dir(file.path(data_dir, "cache"))
output_dir <- ensure_dir(file.path(script_dir, "output"))
preview_dir <- ensure_dir("/tmp/codex_preview/lilith_portfolio_refresh")

analysis_start <- as.Date("2023-01-01")
latest_complete_month_end <- floor_date(Sys.Date(), "month") - days(1)
latest_complete_month <- floor_date(latest_complete_month_end, "month")
latest_year <- year(latest_complete_month_end)
previous_year <- latest_year - 1
ytd_month <- month(latest_complete_month_end)
ytd_label <- glue("Jan-{month.abb[ytd_month]}")

old_sales_path <- file.path(data_dir, "lilith_sales_data.rds")
old_mau_path <- file.path(data_dir, "lilith_mau_data.rds")
old_rank_path <- file.path(data_dir, "lilith_rankings_data.rds")
old_sales <- safe_read_rds(old_sales_path)
old_mau <- safe_read_rds(old_mau_path)
old_rank <- safe_read_rds(old_rank_path)

if (is.null(old_sales) || nrow(old_sales) == 0) {
  stop("Old Lilith sales RDS is required for this refresh fallback: ", old_sales_path)
}

load_sensortower_token()
load_sensortower_package()

unlink(file.path(output_dir, "lilith_portfolio_table.png"))
unlink(file.path(output_dir, "lilith_portfolio_summary_refreshed.csv"))
unlink(file.path(output_dir, "lilith_portfolio_table.html"))
unlink(file.path(preview_dir, "*.png"))

baseline_app_ids <- old_sales %>%
  distinct(unified_app_id = as.character(.data$unified_app_id)) %>%
  arrange(.data$unified_app_id) %>%
  pull(.data$unified_app_id)

publisher_probe <- tryCatch(
  st_publisher_apps(
    unified_id = "59418862660953716600e6f7",
    aggregate_related = TRUE,
    auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
    verbose = FALSE
  ),
  error = function(e) {
    attr(tibble(), "error") <- conditionMessage(e)
    tibble()
  }
)
publisher_probe_error <- attr(publisher_probe, "error") %||% NA_character_

message("Using audited baseline app roster from old December sales data: ", length(baseline_app_ids), " unified apps")
app_mapping <- fetch_app_mapping(baseline_app_ids, file.path(data_dir, "app_mapping.csv"))

sales_cache_file <- file.path(cache_dir, paste0("lilith_sales_", format(analysis_start, "%Y%m%d"), "_", format(latest_complete_month_end, "%Y%m%d"), ".rds"))
mau_cache_file <- file.path(cache_dir, paste0("lilith_mau_", format(analysis_start, "%Y%m%d"), "_", format(latest_complete_month_end, "%Y%m%d"), ".rds"))
sales_data <- fetch_sales_data(baseline_app_ids, analysis_start, latest_complete_month_end, sales_cache_file)
mau_data <- fetch_mau_data(baseline_app_ids, analysis_start, latest_complete_month_end, mau_cache_file)

rank_lookup <- if (!is.null(old_rank)) {
  old_rank %>%
    transmute(
      unified_app_id = as.character(.data$unified_app_id),
      rank_app_name = as.character(.data$unified_app_name),
      global_rank = as.integer(.data$global_rank),
      subgenre_rank = as.integer(.data$subgenre_rank),
      subgenre = as.character(.data$subgenre)
    ) %>%
    distinct(.data$unified_app_id, .keep_all = TRUE)
} else {
  tibble(unified_app_id = character(), rank_app_name = character(), global_rank = integer(), subgenre_rank = integer(), subgenre = character())
}

app_metadata <- tibble(unified_app_id = baseline_app_ids) %>%
  left_join(
    app_mapping %>%
      transmute(
        unified_app_id = as.character(.data$unified_app_id),
        mapping_app_name = as.character(.data$unified_app_name),
        ios_app_id = as.character(.data$ios_app_id %||% NA_character_),
        android_app_id = as.character(.data$android_app_id %||% NA_character_),
        publisher_name = as.character(.data$publisher_name %||% NA_character_)
      ),
    by = "unified_app_id"
  ) %>%
  left_join(rank_lookup, by = "unified_app_id") %>%
  mutate(
    app_name = clean_app_name(coalesce(.data$rank_app_name, .data$mapping_app_name, .data$unified_app_id)),
    subgenre = coalesce(.data$subgenre, ""),
    subgenre_rank = as.integer(.data$subgenre_rank)
  )

write_csv(app_metadata, file.path(data_dir, "app_metadata.csv"), na = "")

sales_summary <- sales_data %>%
  mutate(Year = year(.data$date), Month = month(.data$date)) %>%
  filter(.data$Month <= ytd_month) %>%
  group_by(.data$unified_app_id, .data$Year) %>%
  summarise(
    revenue = sum(.data$revenue, na.rm = TRUE),
    downloads = sum(.data$downloads, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Year,
    values_from = c(revenue, downloads),
    names_sep = "_",
    values_fill = 0
  )

mau_summary <- mau_data %>%
  mutate(Year = year(.data$date), Month = month(.data$date)) %>%
  filter(.data$Month <= ytd_month) %>%
  group_by(unified_app_id = .data$original_id, .data$Year) %>%
  summarise(mau = mean(.data$value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Year,
    values_from = mau,
    names_prefix = "mau_",
    values_fill = 0
  )

portfolio_apps <- app_metadata %>%
  left_join(sales_summary, by = "unified_app_id") %>%
  left_join(mau_summary, by = "unified_app_id")

for (year_value in 2023:latest_year) {
  for (prefix in c("revenue", "downloads", "mau")) {
    col_name <- paste0(prefix, "_", year_value)
    if (!col_name %in% names(portfolio_apps)) {
      portfolio_apps[[col_name]] <- 0
    }
  }
}

revenue_year_cols <- paste0("revenue_", 2023:latest_year)
downloads_year_cols <- paste0("downloads_", 2023:latest_year)
mau_year_cols <- paste0("mau_", 2023:latest_year)

portfolio_apps <- portfolio_apps %>%
  filter(if_any(all_of(revenue_year_cols), ~ . >= 100000)) %>%
  mutate(
    revenue_yoy = if_else(.data[[paste0("revenue_", previous_year)]] > 0, round((.data[[paste0("revenue_", latest_year)]] - .data[[paste0("revenue_", previous_year)]]) / .data[[paste0("revenue_", previous_year)]] * 100, 0), NA_real_),
    downloads_yoy = if_else(.data[[paste0("downloads_", previous_year)]] > 0, round((.data[[paste0("downloads_", latest_year)]] - .data[[paste0("downloads_", previous_year)]]) / .data[[paste0("downloads_", previous_year)]] * 100, 0), NA_real_),
    mau_yoy = if_else(.data[[paste0("mau_", previous_year)]] > 0, round((.data[[paste0("mau_", latest_year)]] - .data[[paste0("mau_", previous_year)]]) / .data[[paste0("mau_", previous_year)]] * 100, 0), NA_real_)
  ) %>%
  arrange(desc(.data[[paste0("revenue_", latest_year)]])) %>%
  mutate(rank = row_number())

portfolio_total <- portfolio_apps %>%
  summarise(
    unified_app_id = NA_character_,
    app_name = "PORTFOLIO TOTAL",
    subgenre = "",
    subgenre_rank = NA_integer_,
    rank = NA_integer_,
    across(all_of(c(revenue_year_cols, downloads_year_cols, mau_year_cols)), ~ sum(., na.rm = TRUE))
  ) %>%
  mutate(
    revenue_yoy = if_else(.data[[paste0("revenue_", previous_year)]] > 0, round((.data[[paste0("revenue_", latest_year)]] - .data[[paste0("revenue_", previous_year)]]) / .data[[paste0("revenue_", previous_year)]] * 100, 0), NA_real_),
    downloads_yoy = if_else(.data[[paste0("downloads_", previous_year)]] > 0, round((.data[[paste0("downloads_", latest_year)]] - .data[[paste0("downloads_", previous_year)]]) / .data[[paste0("downloads_", previous_year)]] * 100, 0), NA_real_),
    mau_yoy = if_else(.data[[paste0("mau_", previous_year)]] > 0, round((.data[[paste0("mau_", latest_year)]] - .data[[paste0("mau_", previous_year)]]) / .data[[paste0("mau_", previous_year)]] * 100, 0), NA_real_)
  )

portfolio <- bind_rows(
  portfolio_total,
  portfolio_apps %>%
    select(any_of(names(portfolio_total)))
)

write_csv(portfolio, file.path(data_dir, "lilith_portfolio_summary_refreshed.csv"), na = "")
write_csv(portfolio, file.path(output_dir, "lilith_portfolio_summary_refreshed.csv"), na = "")

source_manifest <- tibble(
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  source = "Sensor Tower API via local SensorTowerR",
  package_path = "/Users/phillip/Documents/vibe_coding_projects/videogameR-universe/SensorTowerR",
  roster_source = "Old December Lilith sales RDS unified_app_id roster",
  publisher_apps_probe = if_else(nrow(publisher_probe) > 0, "available", "blocked"),
  publisher_apps_probe_error = publisher_probe_error,
  country = "WW",
  os = "unified",
  analysis_start = as.character(analysis_start),
  latest_complete_month = as.character(latest_complete_month),
  latest_complete_month_end = as.character(latest_complete_month_end),
  ytd_month = ytd_month,
  app_count = length(baseline_app_ids),
  refreshed_sales_rows = nrow(sales_data),
  refreshed_mau_rows = nrow(mau_data),
  old_sales_rows = if (!is.null(old_sales)) nrow(old_sales) else NA_integer_,
  old_mau_rows = if (!is.null(old_mau)) nrow(old_mau) else NA_integer_,
  old_rank_rows = if (!is.null(old_rank)) nrow(old_rank) else NA_integer_,
  sales_cache_file = sales_cache_file,
  mau_cache_file = mau_cache_file
)
write_csv(source_manifest, file.path(data_dir, "source_api_manifest.csv"), na = "")

old_sales_comparison <- tibble()
comparison_summary <- tibble()
if (!is.null(old_sales) && nrow(sales_data) > 0) {
  old_sales_norm <- old_sales %>%
    transmute(
      unified_app_id = as.character(.data$unified_app_id),
      country = as.character(.data$country),
      month = floor_date(as.Date(.data$date), "month"),
      revenue = as.numeric(.data$revenue),
      downloads = as.numeric(.data$downloads)
    )

  fresh_sales_norm <- sales_data %>%
    transmute(
      unified_app_id = as.character(.data$unified_app_id),
      country = as.character(.data$country),
      month = floor_date(as.Date(.data$date), "month"),
      revenue = as.numeric(.data$revenue),
      downloads = as.numeric(.data$downloads)
    )

  old_sales_comparison <- old_sales_norm %>%
    inner_join(
      fresh_sales_norm,
      by = c("unified_app_id", "country", "month"),
      suffix = c("_old", "_fresh")
    ) %>%
    mutate(
      revenue_abs_diff = .data$revenue_fresh - .data$revenue_old,
      revenue_pct_diff = if_else(.data$revenue_old > 0, .data$revenue_abs_diff / .data$revenue_old, NA_real_),
      downloads_abs_diff = .data$downloads_fresh - .data$downloads_old,
      downloads_pct_diff = if_else(.data$downloads_old > 0, .data$downloads_abs_diff / .data$downloads_old, NA_real_)
    )

  comparison_summary <- bind_rows(
    old_sales_norm %>%
      summarise(
        source = "old_december_rds",
        rows = n(),
        apps = n_distinct(.data$unified_app_id),
        min_month = min(.data$month),
        max_month = max(.data$month),
        revenue = sum(.data$revenue, na.rm = TRUE),
        downloads = sum(.data$downloads, na.rm = TRUE)
      ),
    fresh_sales_norm %>%
      filter(.data$month <= max(old_sales_norm$month, na.rm = TRUE)) %>%
      summarise(
        source = "fresh_api_through_old_max_month",
        rows = n(),
        apps = n_distinct(.data$unified_app_id),
        min_month = min(.data$month),
        max_month = max(.data$month),
        revenue = sum(.data$revenue, na.rm = TRUE),
        downloads = sum(.data$downloads, na.rm = TRUE)
      ),
    fresh_sales_norm %>%
      summarise(
        source = "fresh_api_full_window",
        rows = n(),
        apps = n_distinct(.data$unified_app_id),
        min_month = min(.data$month),
        max_month = max(.data$month),
        revenue = sum(.data$revenue, na.rm = TRUE),
        downloads = sum(.data$downloads, na.rm = TRUE)
      )
  )
}

write_csv(old_sales_comparison, file.path(data_dir, "old_sales_row_comparison.csv"), na = "")
write_csv(comparison_summary, file.path(data_dir, "old_sales_summary_comparison.csv"), na = "")

display_years <- sort(2023:latest_year, decreasing = TRUE)
table_display <- portfolio %>%
  mutate(
    rank_display = if_else(is.na(.data$rank), "", as.character(.data$rank)),
    subgenre_display = coalesce(.data$subgenre, ""),
    subgenre_rank_display = if_else(is.na(.data$subgenre_rank), "", as.character(.data$subgenre_rank))
  ) %>%
  select(
    rank_display,
    app_name,
    subgenre_display,
    subgenre_rank_display,
    revenue_yoy,
    all_of(paste0("revenue_", display_years)),
    downloads_yoy,
    all_of(paste0("downloads_", display_years)),
    mau_yoy,
    all_of(paste0("mau_", display_years))
  )

gt_table <- table_display %>%
  gt() %>%
  tab_header(
    title = md("**Lilith Games Portfolio Scorecard**"),
    subtitle = glue("Worldwide unified mobile portfolio, {ytd_label} {latest_year} vs {previous_year}")
  ) %>%
  fmt(columns = starts_with("revenue_"), fns = format_money_short) %>%
  fmt(columns = c(starts_with("downloads_"), starts_with("mau_")), fns = format_count_short) %>%
  fmt(columns = ends_with("_yoy"), fns = format_yoy) %>%
  cols_label(
    rank_display = "#",
    app_name = "Game",
    subgenre_display = "Subgenre",
    subgenre_rank_display = "Rank",
    revenue_yoy = "YoY",
    downloads_yoy = "YoY",
    mau_yoy = "YoY"
  ) %>%
  tab_spanner(label = glue("Revenue ({ytd_label})"), columns = c(revenue_yoy, all_of(paste0("revenue_", display_years)))) %>%
  tab_spanner(label = glue("Downloads ({ytd_label})"), columns = c(downloads_yoy, all_of(paste0("downloads_", display_years)))) %>%
  tab_spanner(label = glue("Avg MAU ({ytd_label})"), columns = c(mau_yoy, all_of(paste0("mau_", display_years)))) %>%
  tab_source_note(glue("Source: Sensor Tower API, WW, unified iOS + Android; refreshed through {latest_complete_month_end}.")) %>%
  tab_source_note("Roster is the audited December Lilith app list; validation compares fresh API data against the old saved RDS on overlapping app-month rows.") %>%
  sub_missing(columns = everything(), missing_text = "-") %>%
  opt_table_font(font = list(google_font(name = "League Spartan"), default_fonts())) %>%
  tab_options(
    table.background.color = "#FFFFFF",
    table.border.top.style = "solid",
    table.border.top.width = px(3),
    table.border.top.color = "#1a1a1a",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "#1a1a1a",
    heading.background.color = "#FFFFFF",
    heading.title.font.size = px(30),
    heading.title.font.weight = "bold",
    heading.subtitle.font.size = px(15),
    heading.subtitle.font.weight = "normal",
    heading.border.bottom.style = "solid",
    heading.border.bottom.width = px(2),
    heading.border.bottom.color = "#1a1a1a",
    column_labels.background.color = "#f5f5f5",
    column_labels.font.weight = "bold",
    column_labels.font.size = px(12),
    column_labels.border.top.style = "solid",
    column_labels.border.top.width = px(2),
    column_labels.border.top.color = "#1a1a1a",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "#d0d0d0",
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#fafafa",
    table.font.size = px(11),
    data_row.padding = px(5),
    source_notes.font.size = px(10),
    source_notes.background.color = "#f5f5f5"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(13)),
      cell_fill(color = "#e8e8e8"),
      cell_borders(sides = c("top", "bottom"), color = "#1a1a1a", weight = px(2))
    ),
    locations = cells_body(rows = 1)
  ) %>%
  data_color(
    columns = ends_with("_yoy"),
    fn = yoy_fill
  )

for (year_value in display_years) {
  gt_table <- gt_table %>%
    cols_label(
      !!paste0("revenue_", year_value) := as.character(year_value),
      !!paste0("downloads_", year_value) := as.character(year_value),
      !!paste0("mau_", year_value) := as.character(year_value)
    )
}

output_png <- file.path(output_dir, "lilith_portfolio_table.png")
output_html <- file.path(output_dir, "lilith_portfolio_table.html")
gtsave(gt_table, output_html)
gtsave(gt_table, output_png, vwidth = 2200, vheight = 1050)
file.copy(output_png, file.path(preview_dir, "lilith_portfolio_table.png"), overwrite = TRUE)

portfolio_total_row <- portfolio %>% filter(.data$app_name == "PORTFOLIO TOTAL")
single_app_yoy <- portfolio %>%
  filter(.data$app_name != "PORTFOLIO TOTAL", !is.na(.data$revenue_yoy)) %>%
  filter(.data[[paste0("revenue_", previous_year)]] >= 100000) %>%
  summarise(max_abs_revenue_yoy = max(abs(.data$revenue_yoy), na.rm = TRUE)) %>%
  pull(.data$max_abs_revenue_yoy)
if (length(single_app_yoy) == 0 || is.infinite(single_app_yoy)) single_app_yoy <- NA_real_

overlap_revenue_ratio <- NA_real_
if (nrow(old_sales_comparison) > 0) {
  old_overlap_total <- sum(old_sales_comparison$revenue_old, na.rm = TRUE)
  fresh_overlap_total <- sum(old_sales_comparison$revenue_fresh, na.rm = TRUE)
  overlap_revenue_ratio <- if_else(old_overlap_total > 0, fresh_overlap_total / old_overlap_total, NA_real_)
}

checks <- bind_rows(
  make_check("auth_token_present", nzchar(Sys.getenv("SENSORTOWER_AUTH_TOKEN")), "Sensor Tower token loaded from environment/secrets."),
  make_check("baseline_roster_available", length(baseline_app_ids) > 0, glue("baseline_apps={length(baseline_app_ids)}")),
  make_check("publisher_apps_probe_recorded", TRUE, glue("probe_status={if_else(nrow(publisher_probe) > 0, 'available', 'blocked')}; error={publisher_probe_error}")),
  make_check("app_mapping_rows_complete", nrow(app_mapping) >= length(baseline_app_ids), glue("mapping_rows={nrow(app_mapping)}")),
  make_check("sales_rows_positive", nrow(sales_data) > 0, glue("sales_rows={nrow(sales_data)}")),
  make_check("mau_rows_positive", nrow(mau_data) > 0, glue("mau_rows={nrow(mau_data)}")),
  make_check("sales_latest_month_current", max(floor_date(as.Date(sales_data$date), "month"), na.rm = TRUE) >= latest_complete_month, glue("latest_sales_month={max(floor_date(as.Date(sales_data$date), 'month'), na.rm = TRUE)}; expected={latest_complete_month}")),
  make_check("mau_latest_month_current", max(floor_date(as.Date(mau_data$date), "month"), na.rm = TRUE) >= latest_complete_month, glue("latest_mau_month={max(floor_date(as.Date(mau_data$date), 'month'), na.rm = TRUE)}; expected={latest_complete_month}")),
  make_check("sales_no_duplicate_grain", sales_data %>% count(.data$unified_app_id, .data$country, .data$date) %>% filter(.data$n > 1) %>% nrow() == 0, "grain=unified_app_id/country/date"),
  make_check("mau_no_duplicate_grain", mau_data %>% count(.data$original_id, .data$country, .data$date, .data$metric) %>% filter(.data$n > 1) %>% nrow() == 0, "grain=original_id/country/date/metric"),
  make_check("sales_nonnegative", all(sales_data$revenue >= 0, na.rm = TRUE) && all(sales_data$downloads >= 0, na.rm = TRUE), "revenue and downloads are nonnegative"),
  make_check("mau_nonnegative", all(mau_data$value >= 0, na.rm = TRUE), "mau values are nonnegative"),
  make_check("summary_rows_positive", nrow(portfolio) > 1, glue("summary_rows={nrow(portfolio)}")),
  make_check("portfolio_total_positive", nrow(portfolio_total_row) == 1 && portfolio_total_row[[paste0("revenue_", latest_year)]] > 0 && portfolio_total_row[[paste0("downloads_", latest_year)]] > 0 && portfolio_total_row[[paste0("mau_", latest_year)]] > 0, glue("latest_revenue={if (nrow(portfolio_total_row) == 1) portfolio_total_row[[paste0('revenue_', latest_year)]] else NA}; latest_downloads={if (nrow(portfolio_total_row) == 1) portfolio_total_row[[paste0('downloads_', latest_year)]] else NA}; latest_mau={if (nrow(portfolio_total_row) == 1) portfolio_total_row[[paste0('mau_', latest_year)]] else NA}")),
  make_check("portfolio_yoy_sane", nrow(portfolio_total_row) == 1 && between(portfolio_total_row$revenue_yoy, -90, 300), glue("portfolio_revenue_yoy={if (nrow(portfolio_total_row) == 1) portfolio_total_row$revenue_yoy else NA}%")),
  make_check("single_app_yoy_sane", is.na(single_app_yoy) || single_app_yoy <= 5000, glue("max_abs_revenue_yoy_for_apps_with_prev_revenue_100k_plus={single_app_yoy}; high values are expected for low-base/newer titles")),
  make_check("old_baseline_available", !is.null(old_sales) && !is.null(old_mau) && !is.null(old_rank), glue("old_sales={nrow(old_sales %||% tibble())}; old_mau={nrow(old_mau %||% tibble())}; old_rank={nrow(old_rank %||% tibble())}")),
  make_check("old_sales_overlap_present", nrow(old_sales_comparison) > 0, glue("overlap_rows={nrow(old_sales_comparison)}")),
  make_check("old_overlap_revenue_ratio_sane", !is.na(overlap_revenue_ratio) && between(overlap_revenue_ratio, 0.5, 2.0), glue("fresh_over_old_overlap_revenue_ratio={round(overlap_revenue_ratio, 3)}")),
  make_check("chart_png_created", file.exists(output_png) && file.info(output_png)$size > 0, output_png),
  make_check("preview_png_created", file.exists(file.path(preview_dir, "lilith_portfolio_table.png")) && file.info(file.path(preview_dir, "lilith_portfolio_table.png"))$size > 0, file.path(preview_dir, "lilith_portfolio_table.png"))
)

write_csv(checks, file.path(data_dir, "validation_checks.csv"), na = "")
assert_no_failed_checks(checks)

message("\nSaved chart: ", output_png)
message("Saved preview: ", file.path(preview_dir, "lilith_portfolio_table.png"))
message("Saved validation: ", file.path(data_dir, "validation_checks.csv"))
message("\n=== Done ===")
