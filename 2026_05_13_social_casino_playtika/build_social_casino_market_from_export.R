#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(glue)
  library(httr2)
  library(lubridate)
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

theme_538 <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = 24, color = "#222222", hjust = 0),
      plot.subtitle = element_text(size = 12.5, color = "#4d4d4d", hjust = 0, margin = margin(b = 12)),
      plot.caption = element_text(size = 8.7, color = "#666666", hjust = 0, margin = margin(t = 14), lineheight = 1.08),
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
      plot.margin = margin(20, 98, 38, 18)
    )
}

format_money_short <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    abs(x) >= 1e9 ~ paste0("$", number(x / 1e9, accuracy = 0.1), "B"),
    abs(x) >= 1e6 ~ paste0("$", number(x / 1e6, accuracy = 1), "M"),
    abs(x) >= 1e3 ~ paste0("$", number(x / 1e3, accuracy = 1), "K"),
    TRUE ~ paste0("$", number(x, accuracy = 1))
  )
}

format_count_short <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    abs(x) >= 1e9 ~ paste0(number(x / 1e9, accuracy = 0.1), "B"),
    abs(x) >= 1e6 ~ paste0(number(x / 1e6, accuracy = 1), "M"),
    abs(x) >= 1e3 ~ paste0(number(x / 1e3, accuracy = 1), "K"),
    TRUE ~ number(x, accuracy = 1)
  )
}

make_check <- function(name, passed, detail) {
  tibble(
    check_name = name,
    check_passed = isTRUE(passed),
    detail = as.character(detail)
  )
}

fetch_bls_cpi_chunk <- function(start_year, end_year) {
  response <- request("https://api.bls.gov/publicAPI/v2/timeseries/data/CUUR0000SA0") |>
    req_url_query(startyear = start_year, endyear = end_year) |>
    req_retry(max_tries = 3) |>
    req_perform()

  payload <- resp_body_json(response, simplifyVector = FALSE)
  if (!identical(payload$status, "REQUEST_SUCCEEDED")) {
    message_payload <- payload$message
    if (is.null(message_payload)) {
      message_payload <- ""
    }
    message_text <- paste(unlist(message_payload), collapse = "; ")
    stop("BLS CPI request failed for ", start_year, "-", end_year, ": ", message_text)
  }

  series_data <- payload$Results$series[[1]]$data
  tibble(
    cpi_series_id = "CUUR0000SA0",
    year = as.integer(vapply(series_data, `[[`, character(1), "year")),
    period = vapply(series_data, `[[`, character(1), "period"),
    period_name = vapply(series_data, `[[`, character(1), "periodName"),
    cpi_value_raw = suppressWarnings(as.numeric(vapply(series_data, `[[`, character(1), "value")))
  ) |>
    filter(str_detect(.data$period, "^M\\d{2}$"), .data$period != "M13") |>
    mutate(
      month = as.integer(str_remove(.data$period, "^M")),
      date = as.Date(sprintf("%04d-%02d-01", .data$year, .data$month))
    ) |>
    select(
      "cpi_series_id",
      "date",
      "year",
      "month",
      "period",
      "period_name",
      "cpi_value_raw"
    )
}

fill_internal_cpi_gaps <- function(cpi_monthly) {
  cpi_monthly <- cpi_monthly |> arrange(.data$date)
  observed <- cpi_monthly |> filter(!is.na(.data$cpi_value_raw))
  if (nrow(observed) < 2) {
    stop("Not enough observed CPI values to interpolate internal gaps.")
  }

  interpolated_values <- approx(
    x = as.numeric(observed$date),
    y = observed$cpi_value_raw,
    xout = as.numeric(cpi_monthly$date),
    rule = 1
  )$y

  cpi_monthly |>
    mutate(
      cpi_value = if_else(is.na(.data$cpi_value_raw), interpolated_values, .data$cpi_value_raw),
      cpi_is_interpolated = is.na(.data$cpi_value_raw) & !is.na(.data$cpi_value)
    )
}

fetch_cpi_u_all_items <- function(start_year, end_year, cache_path) {
  chunk_starts <- seq(start_year, end_year, by = 10)
  chunk_ends <- pmin(chunk_starts + 9, end_year)

  cpi_monthly <- tryCatch(
    {
      bind_rows(Map(fetch_bls_cpi_chunk, chunk_starts, chunk_ends)) |>
        distinct(.data$date, .keep_all = TRUE) |>
        fill_internal_cpi_gaps() |>
        arrange(.data$date)
    },
    error = function(err) {
      if (file.exists(cache_path)) {
        warning("BLS CPI fetch failed; using cached CPI file: ", conditionMessage(err))
        return(read_csv(cache_path, show_col_types = FALSE) |> mutate(date = as.Date(.data$date)))
      }
      stop(err)
    }
  )

  write_csv(cpi_monthly, cache_path, na = "")
  cpi_monthly
}

assert_no_failed_checks <- function(checks) {
  failed <- checks |> filter(!.data$check_passed)
  if (nrow(failed) > 0) {
    print(failed, n = Inf)
    stop("Validation checks failed: ", paste(failed$check_name, collapse = ", "))
  }
}

build_market_chart <- function(
  market_monthly,
  output_path,
  latest_complete_month,
  subgenres,
  att_marker_date = as.Date("2021-06-01"),
  att_marker_label = "ATT reaches majority\niOS distribution\n(June 2021)"
) {
  chart_data <- market_monthly |>
    filter(.data$date <= latest_complete_month) |>
    select("date", total = "total_subgenre_revenue_usd", adjusted = "revenue_without_monopoly_go_usd") |>
    pivot_longer(cols = c("total", "adjusted"), names_to = "series_key", values_to = "value") |>
    mutate(
      series = recode(
        .data$series_key,
        total = "Social Casino Market",
        adjusted = "Excluding MONOPOLY GO!"
      ),
      series = factor(.data$series, levels = c("Social Casino Market", "Excluding MONOPOLY GO!"))
    )

  endpoints <- chart_data |>
    filter(!is.na(.data$value)) |>
    group_by(.data$series) |>
    arrange(.data$date, .by_group = TRUE) |>
    slice_tail(n = 1) |>
    ungroup() |>
    mutate(
      label = paste0(as.character(.data$series), "  ", format_money_short(.data$value)),
      label_x = .data$date + days(58),
      label_y = if_else(
        .data$series == "Excluding MONOPOLY GO!",
        .data$value - 34e6,
        .data$value + 8e6
      )
    )

  marker_label_y <- max(chart_data$value, na.rm = TRUE) -
    diff(range(chart_data$value, na.rm = TRUE)) * 0.08

  caption <- str_wrap(
    glue("Sub-genres used: {paste(subgenres, collapse = ', ')}."),
    width = 150
  )

  gg <- ggplot(chart_data, aes(x = .data$date, y = .data$value, color = .data$series, linetype = .data$series)) +
    geom_vline(
      xintercept = att_marker_date,
      color = "#555555",
      linetype = "dashed",
      linewidth = 0.55
    ) +
    annotate(
      "label",
      x = att_marker_date %m+% months(5),
      y = marker_label_y,
      label = att_marker_label,
      hjust = 0,
      size = 3.2,
      label.size = 0,
      fill = "white",
      color = "#333333"
    ) +
    geom_line(linewidth = 1.12, na.rm = TRUE) +
    geom_point(data = endpoints, size = 2.5, show.legend = FALSE) +
    geom_segment(
      data = endpoints,
      aes(xend = .data$label_x - days(10), yend = .data$label_y),
      color = "#b7b7b7",
      linewidth = 0.3,
      show.legend = FALSE
    ) +
    geom_text(
      data = endpoints,
      aes(x = .data$label_x, y = .data$label_y, label = .data$label),
      hjust = 0,
      show.legend = FALSE,
      size = 3.5
    ) +
    scale_color_manual(values = c("Social Casino Market" = "#3b6fb6", "Excluding MONOPOLY GO!" = "#d55e00")) +
    scale_linetype_manual(values = c("Social Casino Market" = "solid", "Excluding MONOPOLY GO!" = "22")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.18))) +
    scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale(), accuracy = 1), expand = expansion(mult = c(0.03, 0.12))) +
    coord_cartesian(clip = "off") +
    labs(
      title = "Social Casino Monthly Revenue (Inflation-Adjusted)",
      subtitle = "Worldwide iOS + Android Casino subgenre composite; dotted line subtracts MONOPOLY GO!",
      caption = caption
    ) +
    theme_538()

  ggsave(output_path, gg, width = 13.2, height = 7.8, dpi = 220, bg = "white")
  invisible(output_path)
}

build_downloads_chart <- function(
  market_monthly,
  output_path,
  latest_complete_month,
  subgenres,
  att_marker_date = as.Date("2021-06-01"),
  att_marker_label = "ATT reaches majority\niOS distribution\n(June 2021)"
) {
  chart_data <- market_monthly |>
    filter(.data$date <= latest_complete_month) |>
    select("date", total = "total_subgenre_downloads", adjusted = "downloads_without_monopoly_go") |>
    pivot_longer(cols = c("total", "adjusted"), names_to = "series_key", values_to = "value") |>
    mutate(
      series = recode(
        .data$series_key,
        total = "Social Casino Market",
        adjusted = "Excluding MONOPOLY GO!"
      ),
      series = factor(.data$series, levels = c("Social Casino Market", "Excluding MONOPOLY GO!"))
    )

  endpoints <- chart_data |>
    filter(!is.na(.data$value)) |>
    group_by(.data$series) |>
    arrange(.data$date, .by_group = TRUE) |>
    slice_tail(n = 1) |>
    ungroup() |>
    mutate(
      label = paste0(as.character(.data$series), "  ", format_count_short(.data$value)),
      label_x = .data$date + days(58),
      label_y = if_else(
        .data$series == "Excluding MONOPOLY GO!",
        .data$value - 3.2e6,
        .data$value + 2.8e6
      )
    )

  marker_label_y <- max(chart_data$value, na.rm = TRUE) -
    diff(range(chart_data$value, na.rm = TRUE)) * 0.08

  caption <- str_wrap(
    glue("Sub-genres used: {paste(subgenres, collapse = ', ')}."),
    width = 150
  )

  gg <- ggplot(chart_data, aes(x = .data$date, y = .data$value, color = .data$series, linetype = .data$series)) +
    geom_vline(
      xintercept = att_marker_date,
      color = "#555555",
      linetype = "dashed",
      linewidth = 0.55
    ) +
    annotate(
      "label",
      x = att_marker_date %m+% months(5),
      y = marker_label_y,
      label = att_marker_label,
      hjust = 0,
      size = 3.2,
      label.size = 0,
      fill = "white",
      color = "#333333"
    ) +
    geom_line(linewidth = 1.12, na.rm = TRUE) +
    geom_point(data = endpoints, size = 2.5, show.legend = FALSE) +
    geom_segment(
      data = endpoints,
      aes(xend = .data$label_x - days(10), yend = .data$label_y),
      color = "#b7b7b7",
      linewidth = 0.3,
      show.legend = FALSE
    ) +
    geom_text(
      data = endpoints,
      aes(x = .data$label_x, y = .data$label_y, label = .data$label),
      hjust = 0,
      show.legend = FALSE,
      size = 3.5
    ) +
    scale_color_manual(values = c("Social Casino Market" = "#3b6fb6", "Excluding MONOPOLY GO!" = "#d55e00")) +
    scale_linetype_manual(values = c("Social Casino Market" = "solid", "Excluding MONOPOLY GO!" = "22")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.18))) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale(), accuracy = 1), expand = expansion(mult = c(0.03, 0.12))) +
    coord_cartesian(clip = "off") +
    labs(
      title = "Social Casino Monthly Downloads",
      subtitle = "Worldwide iOS + Android Casino subgenre composite; dotted line subtracts MONOPOLY GO!",
      caption = caption
    ) +
    theme_538()

  ggsave(output_path, gg, width = 13.2, height = 7.8, dpi = 220, bg = "white")
  invisible(output_path)
}

script_dir <- get_script_dir()
data_dir <- ensure_dir(file.path(script_dir, "data"))
raw_dir <- ensure_dir(file.path(data_dir, "raw"))
output_dir <- ensure_dir(file.path(script_dir, "output"))

raw_export_path <- file.path(raw_dir, "social_casino_market_size_revenue_jan_2014_to_may_2026.csv")
if (!file.exists(raw_export_path)) {
  raw_export_path <- "/Users/phillip/Downloads/Market Size Revenue Jan 2014 to May 2026 (1).csv"
}
if (!file.exists(raw_export_path)) {
  stop("Market-size revenue export not found.")
}

monopoly_go_path <- file.path(data_dir, "monopoly_go_monthly_revenue_for_subgenre_merge.csv")
if (!file.exists(monopoly_go_path)) {
  stop("Missing MONOPOLY GO! merge file. Run fetch_monopoly_go_revenue.R first.")
}

market_raw <- read_tsv(
  raw_export_path,
  locale = locale(encoding = "UTF-16LE"),
  show_col_types = FALSE
) |>
  rename(
    game_iq_classes = "Game IQ: Classes",
    game_iq_genre = "Game IQ: Genre",
    game_iq_sub_genre = "Game IQ: Sub-Genre",
    date = "Date",
    country_region = "Country/Region",
    device = "Device",
    downloads = "Downloads",
    revenue_usd = "Revenue ($)",
    rpd_usd = "RPD ($)"
  ) |>
  mutate(
    date = as.Date(.data$date),
    downloads = as.numeric(.data$downloads),
    revenue_usd = as.numeric(.data$revenue_usd),
    rpd_usd = as.numeric(.data$rpd_usd)
  )

latest_complete_month <- floor_date(Sys.Date(), "month") - months(1)
latest_complete_month <- as.Date(format(latest_complete_month, "%Y-%m-01"))

cpi_monthly <- fetch_cpi_u_all_items(
  start_year = year(min(market_raw$date, na.rm = TRUE)),
  end_year = year(latest_complete_month),
  cache_path = file.path(data_dir, "cpi_u_all_items_monthly.csv")
)

available_base_dates <- cpi_monthly$date[
  cpi_monthly$date <= latest_complete_month &
    !is.na(cpi_monthly$cpi_value)
]
if (length(available_base_dates) == 0) {
  stop("No CPI values available through the latest complete month.")
}
inflation_base_month <- max(available_base_dates)
inflation_base_cpi <- cpi_monthly$cpi_value[cpi_monthly$date == inflation_base_month][[1]]

market_normalized <- market_raw |>
  arrange(.data$date, .data$game_iq_sub_genre, .data$country_region, .data$device)

market_monthly <- market_normalized |>
  group_by(.data$date) |>
  summarise(
    total_subgenre_revenue_usd_nominal = sum(.data$revenue_usd, na.rm = TRUE),
    total_subgenre_downloads = sum(.data$downloads, na.rm = TRUE),
    source_rows = n(),
    country_count = n_distinct(.data$country_region),
    device_count = n_distinct(.data$device),
    subgenre_count = n_distinct(.data$game_iq_sub_genre),
    .groups = "drop"
  ) |>
  left_join(
    cpi_monthly |>
      select("date", "cpi_series_id", "cpi_value_raw", "cpi_value", "cpi_is_interpolated"),
    by = "date"
  ) |>
  mutate(
    inflation_base_month = inflation_base_month,
    inflation_base_cpi = inflation_base_cpi,
    inflation_adjustment_factor = if_else(
      !is.na(.data$cpi_value) & .data$cpi_value > 0,
      .data$inflation_base_cpi / .data$cpi_value,
      NA_real_
    ),
    total_subgenre_revenue_real_usd = .data$total_subgenre_revenue_usd_nominal *
      .data$inflation_adjustment_factor,
    total_subgenre_revenue_usd = .data$total_subgenre_revenue_real_usd
  ) |>
  arrange(.data$date)

subgenre_audit <- market_normalized |>
  group_by(.data$game_iq_classes, .data$game_iq_genre, .data$game_iq_sub_genre) |>
  summarise(
    rows = n(),
    first_month = min(.data$date, na.rm = TRUE),
    last_month = max(.data$date, na.rm = TRUE),
    revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
    downloads = sum(.data$downloads, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$game_iq_sub_genre)

monopoly_go <- read_csv(monopoly_go_path, show_col_types = FALSE) |>
  mutate(date = as.Date(.data$date)) |>
  select(all_of(c("date", "monopoly_go_revenue_usd", "monopoly_go_downloads")))

market_with_adjustment_all <- market_monthly |>
  left_join(monopoly_go, by = "date") |>
  mutate(
    monopoly_go_revenue_usd_nominal = coalesce(.data$monopoly_go_revenue_usd, 0),
    monopoly_go_downloads = coalesce(.data$monopoly_go_downloads, 0),
    monopoly_go_adjustment_active = .data$monopoly_go_revenue_usd_nominal > 0 & .data$date >= min(monopoly_go$date[monopoly_go$monopoly_go_revenue_usd >= 5e6], na.rm = TRUE),
    monopoly_go_revenue_real_usd = .data$monopoly_go_revenue_usd_nominal * .data$inflation_adjustment_factor,
    monopoly_go_revenue_usd = .data$monopoly_go_revenue_real_usd,
    revenue_without_monopoly_go_usd_nominal = if_else(
      .data$monopoly_go_adjustment_active,
      .data$total_subgenre_revenue_usd_nominal - .data$monopoly_go_revenue_usd_nominal,
      NA_real_
    ),
    revenue_without_monopoly_go_real_usd = if_else(
      .data$monopoly_go_adjustment_active,
      .data$total_subgenre_revenue_real_usd - .data$monopoly_go_revenue_real_usd,
      NA_real_
    ),
    revenue_without_monopoly_go_usd = .data$revenue_without_monopoly_go_real_usd,
    downloads_without_monopoly_go = if_else(
      .data$monopoly_go_adjustment_active,
      .data$total_subgenre_downloads - .data$monopoly_go_downloads,
      NA_real_
    ),
    monopoly_go_revenue_share = if_else(
      .data$total_subgenre_revenue_usd_nominal > 0,
      .data$monopoly_go_revenue_usd_nominal / .data$total_subgenre_revenue_usd_nominal,
      NA_real_
    ),
    monopoly_go_download_share = if_else(
      .data$total_subgenre_downloads > 0,
      .data$monopoly_go_downloads / .data$total_subgenre_downloads,
      NA_real_
    ),
    is_latest_complete_month = .data$date == latest_complete_month
  ) |>
  arrange(.data$date)

market_with_adjustment <- market_with_adjustment_all |>
  filter(.data$date <= latest_complete_month)

subgenres <- sort(unique(market_normalized$game_iq_sub_genre))

market_complete <- market_with_adjustment

recalc <- market_normalized |>
  group_by(.data$date) |>
  summarise(
    revenue_check = sum(.data$revenue_usd, na.rm = TRUE),
    downloads_check = sum(.data$downloads, na.rm = TRUE),
    .groups = "drop"
  ) |>
  inner_join(market_monthly, by = "date") |>
  mutate(
    revenue_abs_error = abs(.data$revenue_check - .data$total_subgenre_revenue_usd_nominal),
    downloads_abs_error = abs(.data$downloads_check - .data$total_subgenre_downloads)
  )

inflation_formula_check <- market_complete |>
  mutate(real_revenue_abs_error = abs(
    .data$total_subgenre_revenue_real_usd -
      .data$total_subgenre_revenue_usd_nominal * .data$inflation_adjustment_factor
  ))

checks <- bind_rows(
  make_check(
    "raw_export_schema",
    all(c("game_iq_classes", "game_iq_genre", "game_iq_sub_genre", "date", "country_region", "device", "downloads", "revenue_usd") %in% names(market_normalized)),
    paste("Columns:", paste(names(market_normalized), collapse = ", "))
  ),
  make_check(
    "scope_is_casino_genre",
    all(market_normalized$game_iq_classes == "Casino") && all(market_normalized$game_iq_genre == "Casino"),
    "All export rows are Game IQ Classes = Casino and Game IQ Genre = Casino."
  ),
  make_check(
    "subgenre_count_matches_export",
    length(subgenres) == 13,
    paste("Subgenres:", paste(subgenres, collapse = ", "))
  ),
  make_check(
    "date_range_has_expected_export_window",
    min(market_normalized$date, na.rm = TRUE) == as.Date("2014-01-01") && max(market_normalized$date, na.rm = TRUE) == as.Date("2026-05-01"),
    paste("Raw export date range:", min(market_normalized$date, na.rm = TRUE), "to", max(market_normalized$date, na.rm = TRUE))
  ),
  make_check(
    "latest_complete_month_is_april_2026",
    latest_complete_month == as.Date("2026-04-01") && max(market_complete$date, na.rm = TRUE) == as.Date("2026-04-01"),
    paste("Latest complete plotted month:", latest_complete_month)
  ),
  make_check(
    "monthly_revenue_reconciles_to_raw_rows",
    max(recalc$revenue_abs_error, na.rm = TRUE) < 1e-6,
    paste("Max revenue reconciliation error:", max(recalc$revenue_abs_error, na.rm = TRUE))
  ),
  make_check(
    "monthly_downloads_reconcile_to_raw_rows",
    max(recalc$downloads_abs_error, na.rm = TRUE) < 1e-6,
    paste("Max downloads reconciliation error:", max(recalc$downloads_abs_error, na.rm = TRUE))
  ),
  make_check(
    "cpi_series_available_through_latest_complete_month",
    inflation_base_month == latest_complete_month,
    paste(
      "Inflation base month:",
      inflation_base_month,
      "base CPI:",
      inflation_base_cpi,
      "interpolated CPI months:",
      paste(cpi_monthly$date[cpi_monthly$cpi_is_interpolated], collapse = ", ")
    )
  ),
  make_check(
    "inflation_adjustment_formula_reconciles",
    max(inflation_formula_check$real_revenue_abs_error, na.rm = TRUE) < 1e-6,
    paste("Max real revenue formula error:", max(inflation_formula_check$real_revenue_abs_error, na.rm = TRUE))
  ),
  make_check(
    "latest_month_inflation_factor_is_one",
    abs(market_complete$inflation_adjustment_factor[market_complete$date == latest_complete_month] - 1) < 1e-9,
    paste("Latest complete month inflation factor:", market_complete$inflation_adjustment_factor[market_complete$date == latest_complete_month])
  ),
  make_check(
    "monopoly_go_joined",
    any(market_with_adjustment_all$monopoly_go_revenue_usd > 5e6, na.rm = TRUE),
    paste("Max MONOPOLY GO! monthly real revenue:", format_money_short(max(market_with_adjustment_all$monopoly_go_revenue_usd, na.rm = TRUE)))
  ),
  make_check(
    "adjusted_revenue_never_exceeds_total",
    all(market_complete$revenue_without_monopoly_go_usd <= market_complete$total_subgenre_revenue_usd + 1e-6, na.rm = TRUE),
    "Adjusted revenue never exceeds total market revenue."
  ),
  make_check(
    "adjusted_downloads_never_exceed_total",
    all(market_complete$downloads_without_monopoly_go <= market_complete$total_subgenre_downloads + 1e-6, na.rm = TRUE),
    "Adjusted downloads never exceed total downloads."
  ),
  make_check(
    "partial_months_removed_from_primary_outputs",
    any(market_with_adjustment_all$date > latest_complete_month) &&
      all(market_with_adjustment$date <= latest_complete_month),
    paste("Primary monthly outputs end at", max(market_with_adjustment$date, na.rm = TRUE))
  )
)

write_csv(market_normalized, file.path(data_dir, "social_casino_market_size_revenue_export_normalized.csv"), na = "")
write_csv(subgenre_audit, file.path(data_dir, "social_casino_market_size_revenue_export_subgenre_audit.csv"), na = "")
write_csv(cpi_monthly, file.path(data_dir, "cpi_u_all_items_monthly.csv"), na = "")
write_csv(market_with_adjustment, file.path(data_dir, "social_casino_market_monthly_with_without_monopoly_go_from_export.csv"), na = "")
write_csv(market_with_adjustment, file.path(data_dir, "social_casino_monthly_market.csv"), na = "")
write_csv(checks, file.path(data_dir, "social_casino_market_export_validation_checks.csv"), na = "")
assert_no_failed_checks(checks)

chart_path <- file.path(output_dir, "social_casino_market_revenue_with_without_monopoly_go_from_export_538.png")
standard_chart_path <- file.path(output_dir, "social_casino_monthly_revenue_with_without_monopoly_go_538.png")
downloads_chart_path <- file.path(output_dir, "social_casino_market_downloads_with_without_monopoly_go_from_export_538.png")
standard_downloads_chart_path <- file.path(output_dir, "social_casino_monthly_downloads_with_without_monopoly_go_538.png")
build_market_chart(
  market_monthly = market_with_adjustment,
  output_path = chart_path,
  latest_complete_month = latest_complete_month,
  subgenres = subgenres
)
file.copy(chart_path, standard_chart_path, overwrite = TRUE)
build_downloads_chart(
  market_monthly = market_with_adjustment,
  output_path = downloads_chart_path,
  latest_complete_month = latest_complete_month,
  subgenres = subgenres
)
file.copy(downloads_chart_path, standard_downloads_chart_path, overwrite = TRUE)

message("\nOutputs written:")
message("  ", file.path(data_dir, "social_casino_market_size_revenue_export_normalized.csv"))
message("  ", file.path(data_dir, "social_casino_market_size_revenue_export_subgenre_audit.csv"))
message("  ", file.path(data_dir, "cpi_u_all_items_monthly.csv"))
message("  ", file.path(data_dir, "social_casino_market_monthly_with_without_monopoly_go_from_export.csv"))
message("  ", file.path(data_dir, "social_casino_monthly_market.csv"))
message("  ", file.path(data_dir, "social_casino_market_export_validation_checks.csv"))
message("  ", chart_path)
message("  ", standard_chart_path)
message("  ", downloads_chart_path)
message("  ", standard_downloads_chart_path)
