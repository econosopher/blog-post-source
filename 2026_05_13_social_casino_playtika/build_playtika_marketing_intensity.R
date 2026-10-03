#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(glue)
  library(httr2)
  library(jsonlite)
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
    normalizePath(getwd(), mustWork = TRUE)
  } else {
    dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
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
      plot.caption = element_text(size = 8.8, color = "#666666", hjust = 0, margin = margin(t = 14), lineheight = 1.08),
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
      strip.text = element_text(face = "bold", color = "#222222", size = 11, hjust = 0),
      strip.background = element_blank(),
      panel.spacing.y = unit(1.15, "lines"),
      plot.margin = margin(20, 96, 32, 18)
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

build_sec_archive_url <- function(accession_number) {
  if_else(
    is.na(accession_number) | accession_number == "",
    NA_character_,
    paste0(
      "https://www.sec.gov/Archives/edgar/data/1828016/",
      str_replace_all(accession_number, "-", ""),
      "/"
    )
  )
}

fetch_sec_companyfacts <- function(cache_path, refresh = FALSE) {
  if (file.exists(cache_path) && !isTRUE(refresh)) {
    return(fromJSON(cache_path, simplifyVector = FALSE))
  }

  user_agent <- Sys.getenv("SEC_USER_AGENT")
  if (!nzchar(user_agent)) {
    user_agent <- "pblack@gameeconomistconsulting.com Playtika marketing intensity research"
  }

  response <- request("https://data.sec.gov/api/xbrl/companyfacts/CIK0001828016.json") |>
    req_headers("User-Agent" = user_agent) |>
    req_retry(max_tries = 3) |>
    req_perform()

  body <- resp_body_string(response)
  ensure_dir(dirname(cache_path))
  writeLines(body, cache_path, useBytes = TRUE)
  fromJSON(body, simplifyVector = FALSE)
}

extract_usd_facts <- function(companyfacts, tag) {
  tag_facts <- companyfacts$facts[["us-gaap"]][[tag]]$units[["USD"]]
  if (is.null(tag_facts)) {
    return(tibble())
  }

  map_dfr(tag_facts, function(x) {
    tibble(
      tag = tag,
      start = as.Date(x$start %||% NA_character_),
      end = as.Date(x$end %||% NA_character_),
      value = suppressWarnings(as.numeric(x$val %||% NA_real_)),
      accession_number = as.character(x$accn %||% NA_character_),
      fiscal_year = suppressWarnings(as.integer(x$fy %||% NA_integer_)),
      fiscal_period = as.character(x$fp %||% NA_character_),
      form = as.character(x$form %||% NA_character_),
      filed = as.Date(x$filed %||% NA_character_),
      frame = as.character(x$frame %||% NA_character_)
    )
  }) |>
    mutate(source_url = build_sec_archive_url(.data$accession_number))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

select_annual_facts <- function(facts_long, min_year = 2018) {
  facts_long |>
    mutate(
      frame_year = suppressWarnings(as.integer(str_match(.data$frame, "^CY(\\d{4})$")[, 2])),
      period_year = coalesce(.data$frame_year, year(.data$end)),
      exact_calendar_frame = .data$frame == paste0("CY", .data$period_year),
      calendar_year_end = month(.data$end) == 12 & mday(.data$end) == 31
    ) |>
    filter(
      .data$form == "10-K",
      .data$fiscal_period == "FY",
      .data$calendar_year_end,
      .data$period_year >= min_year,
      !is.na(.data$value)
    ) |>
    group_by(.data$tag, .data$period_year) |>
    arrange(desc(.data$exact_calendar_frame), desc(.data$filed), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    transmute(
      tag,
      period_type = "annual",
      fiscal_year = .data$period_year,
      fiscal_quarter = NA_integer_,
      period_id = as.character(.data$period_year),
      period_start = as.Date(sprintf("%04d-01-01", .data$period_year)),
      period_end = as.Date(sprintf("%04d-12-31", .data$period_year)),
      value,
      source_form = form,
      source_filed = filed,
      source_url,
      source_frame = frame
    )
}

select_direct_quarter_facts <- function(facts_long, min_year = 2020) {
  facts_long |>
    mutate(
      frame_year = suppressWarnings(as.integer(str_match(.data$frame, "^CY(\\d{4})Q([1-4])$")[, 2])),
      frame_quarter = suppressWarnings(as.integer(str_match(.data$frame, "^CY(\\d{4})Q([1-4])$")[, 3])),
      exact_quarter_frame = !is.na(.data$frame_year) & !is.na(.data$frame_quarter)
    ) |>
    filter(
      .data$form == "10-Q",
      .data$exact_quarter_frame,
      .data$frame_year >= min_year,
      !is.na(.data$value)
    ) |>
    group_by(.data$tag, .data$frame_year, .data$frame_quarter) |>
    arrange(desc(.data$filed), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    transmute(
      tag,
      period_type = "quarter",
      fiscal_year = .data$frame_year,
      fiscal_quarter = .data$frame_quarter,
      period_id = paste0(.data$frame_year, "-Q", .data$frame_quarter),
      period_start = floor_date(.data$end, "quarter"),
      period_end = .data$end,
      value,
      source_form = form,
      source_filed = filed,
      source_url,
      source_frame = frame,
      value_derivation = "direct_sec_quarter_frame"
    )
}

derive_fourth_quarter_facts <- function(annual_facts, quarter_facts) {
  eligible_tags <- c(
    "RevenueFromContractWithCustomerExcludingAssessedTax",
    "SellingAndMarketingExpense"
  )

  quarter_sums <- quarter_facts |>
    filter(.data$tag %in% eligible_tags, .data$fiscal_quarter %in% 1:3) |>
    group_by(.data$tag, .data$fiscal_year) |>
    summarise(
      first_three_quarter_value = sum(.data$value, na.rm = TRUE),
      first_three_quarter_count = n_distinct(.data$fiscal_quarter),
      .groups = "drop"
    ) |>
    filter(.data$first_three_quarter_count == 3)

  annual_facts |>
    filter(.data$tag %in% eligible_tags) |>
    inner_join(quarter_sums, by = c("tag", "fiscal_year")) |>
    transmute(
      tag,
      period_type = "quarter",
      fiscal_year,
      fiscal_quarter = 4L,
      period_id = paste0(.data$fiscal_year, "-Q4"),
      period_start = as.Date(sprintf("%04d-10-01", .data$fiscal_year)),
      period_end = as.Date(sprintf("%04d-12-31", .data$fiscal_year)),
      value = .data$value - .data$first_three_quarter_value,
      source_form,
      source_filed,
      source_url,
      source_frame,
      value_derivation = "annual_minus_q1_q2_q3"
    ) |>
    filter(!is.na(.data$value), .data$value >= 0)
}

rename_metric_columns <- function(data) {
  data |>
    rename(
      gaap_revenue = "RevenueFromContractWithCustomerExcludingAssessedTax",
      sales_marketing_expense = "SellingAndMarketingExpense",
      advertising_expense = "AdvertisingExpense"
    )
}

wide_period_facts <- function(period_facts) {
  value_wide <- period_facts |>
    select("period_type", "fiscal_year", "fiscal_quarter", "period_id", "period_start", "period_end", "tag", "value") |>
    pivot_wider(names_from = "tag", values_from = "value") |>
    rename_metric_columns()

  source_wide <- period_facts |>
    select("period_type", "fiscal_year", "fiscal_quarter", "period_id", "tag", "source_url", "source_form", "source_filed", "source_frame") |>
    pivot_wider(
      names_from = "tag",
      values_from = c("source_url", "source_form", "source_filed", "source_frame"),
      names_glue = "{.value}_{tag}"
    ) |>
    rename(
      revenue_source_url = "source_url_RevenueFromContractWithCustomerExcludingAssessedTax",
      sales_marketing_source_url = "source_url_SellingAndMarketingExpense",
      advertising_source_url = "source_url_AdvertisingExpense",
      revenue_source_form = "source_form_RevenueFromContractWithCustomerExcludingAssessedTax",
      sales_marketing_source_form = "source_form_SellingAndMarketingExpense",
      advertising_source_form = "source_form_AdvertisingExpense",
      revenue_source_filed = "source_filed_RevenueFromContractWithCustomerExcludingAssessedTax",
      sales_marketing_source_filed = "source_filed_SellingAndMarketingExpense",
      advertising_source_filed = "source_filed_AdvertisingExpense",
      revenue_source_frame = "source_frame_RevenueFromContractWithCustomerExcludingAssessedTax",
      sales_marketing_source_frame = "source_frame_SellingAndMarketingExpense",
      advertising_source_frame = "source_frame_AdvertisingExpense"
    )

  value_wide |>
    left_join(source_wide, by = c("period_type", "fiscal_year", "fiscal_quarter", "period_id")) |>
    arrange(.data$period_type, .data$period_end) |>
    mutate(
      sales_marketing_share_of_revenue = .data$sales_marketing_expense / .data$gaap_revenue,
      advertising_share_of_revenue = .data$advertising_expense / .data$gaap_revenue,
      revenue_denominator_label = "GAAP revenue",
      source_type = "SEC XBRL companyfacts",
      acquisition_metric_caveat = "Not true CAC; Playtika does not disclose paid acquired users or paid installs."
    )
}

build_marketing_intensity_panel <- function(companyfacts) {
  tags <- c(
    "RevenueFromContractWithCustomerExcludingAssessedTax",
    "SellingAndMarketingExpense",
    "AdvertisingExpense"
  )

  facts_long <- map_dfr(tags, \(tag) extract_usd_facts(companyfacts, tag))
  annual_facts <- select_annual_facts(facts_long)
  direct_quarter_facts <- select_direct_quarter_facts(facts_long)
  derived_q4_facts <- derive_fourth_quarter_facts(annual_facts, direct_quarter_facts)

  bind_rows(
    annual_facts,
    direct_quarter_facts,
    derived_q4_facts
  ) |>
    wide_period_facts() |>
    arrange(.data$period_start)
}

read_playtika_sensor_tower_monthly <- function(project_dir) {
  path <- file.path(project_dir, "data", "playtika_title_monthly_metrics.csv")
  if (!file.exists(path)) {
    stop("Missing Sensor Tower monthly Playtika file: ", path)
  }

  read_csv(path, show_col_types = FALSE) |>
    mutate(date = as.Date(.data$date)) |>
    group_by(.data$date) |>
    summarise(
      sensor_tower_portfolio_revenue = sum(.data$revenue_usd, na.rm = TRUE),
      sensor_tower_downloads = sum(.data$downloads, na.rm = TRUE),
      sensor_tower_title_count = n_distinct(.data$unified_app_id),
      .groups = "drop"
    )
}

build_sensor_tower_periods <- function(sensor_tower_monthly) {
  annual <- sensor_tower_monthly |>
    mutate(fiscal_year = year(.data$date)) |>
    group_by(.data$fiscal_year) |>
    summarise(
      period_type = "annual",
      fiscal_quarter = NA_integer_,
      period_id = as.character(.data$fiscal_year[[1]]),
      period_start = as.Date(sprintf("%04d-01-01", .data$fiscal_year[[1]])),
      period_end = as.Date(sprintf("%04d-12-31", .data$fiscal_year[[1]])),
      observed_months = n_distinct(.data$date),
      sensor_tower_portfolio_revenue = sum(.data$sensor_tower_portfolio_revenue, na.rm = TRUE),
      sensor_tower_downloads = sum(.data$sensor_tower_downloads, na.rm = TRUE),
      sensor_tower_title_count = max(.data$sensor_tower_title_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(.data$observed_months == 12)

  quarters <- sensor_tower_monthly |>
    mutate(
      fiscal_year = year(.data$date),
      fiscal_quarter = quarter(.data$date)
    ) |>
    group_by(.data$fiscal_year, .data$fiscal_quarter) |>
    summarise(
      period_type = "quarter",
      period_id = paste0(.data$fiscal_year[[1]], "-Q", .data$fiscal_quarter[[1]]),
      period_start = floor_date(min(.data$date), "quarter"),
      period_end = ceiling_date(max(.data$date), "month") - days(1),
      observed_months = n_distinct(.data$date),
      sensor_tower_portfolio_revenue = sum(.data$sensor_tower_portfolio_revenue, na.rm = TRUE),
      sensor_tower_downloads = sum(.data$sensor_tower_downloads, na.rm = TRUE),
      sensor_tower_title_count = max(.data$sensor_tower_title_count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(.data$observed_months == 3)

  bind_rows(annual, quarters) |>
    mutate(
      st_denominator_label = "Sensor Tower gross consumer spend proxy",
      st_source_label = "Sensor Tower Playtika publisher portfolio, WW unified iOS + Android",
      proxy_caveat = "Sensor Tower denominator is a third-party gross consumer spend proxy, not company-reported bookings."
    )
}

build_acquisition_proxy_panel <- function(marketing_intensity, sensor_tower_periods) {
  marketing_intensity |>
    left_join(
      sensor_tower_periods,
      by = c("period_type", "fiscal_year", "fiscal_quarter", "period_id", "period_start", "period_end")
    ) |>
    filter(!is.na(.data$sensor_tower_portfolio_revenue)) |>
    mutate(
      sales_marketing_share_of_st_proxy_bookings = .data$sales_marketing_expense / .data$sensor_tower_portfolio_revenue,
      advertising_share_of_st_proxy_bookings = .data$advertising_expense / .data$sensor_tower_portfolio_revenue,
      implied_cost_per_download = .data$sales_marketing_expense / .data$sensor_tower_downloads,
      implied_ad_cost_per_download = .data$advertising_expense / .data$sensor_tower_downloads
    )
}

build_marketing_share_chart_data <- function(marketing_intensity) {
  marketing_intensity |>
    filter(.data$period_type == "quarter", !is.na(.data$sales_marketing_share_of_revenue)) |>
    transmute(
      period_type,
      period_end,
      period_id,
      panel = "Quarterly Filing Ratio",
      series = "Sales & Marketing / Revenue",
      value = .data$sales_marketing_share_of_revenue
    ) |>
    mutate(
      panel = factor(.data$panel, levels = "Quarterly Filing Ratio"),
      series = factor(.data$series, levels = "Sales & Marketing / Revenue")
    )
}

render_marketing_share_chart <- function(marketing_intensity, output_path) {
  chart_data <- build_marketing_share_chart_data(marketing_intensity)

  endpoints <- chart_data |>
    group_by(.data$panel, .data$series) |>
    filter(.data$period_end == max(.data$period_end, na.rm = TRUE)) |>
    ungroup() |>
    mutate(label = paste0(.data$series, " ", percent(.data$value, accuracy = 0.1)))

  plot <- ggplot(chart_data, aes(x = .data$period_end, y = .data$value, color = .data$series, linetype = .data$series)) +
    geom_line(linewidth = 1.05, na.rm = TRUE) +
    geom_point(size = 2.1, na.rm = TRUE) +
    geom_text_repel(
      data = endpoints,
      aes(label = .data$label),
      direction = "y",
      nudge_x = 90,
      hjust = 0,
      size = 3.4,
      segment.color = "#aaaaaa",
      min.segment.length = 0,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    facet_wrap(~panel, ncol = 1, scales = "free_x") +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.10))) +
    scale_color_manual(values = c("Sales & Marketing / Revenue" = "#222222")) +
    scale_linetype_manual(values = c("Sales & Marketing / Revenue" = "solid")) +
    labs(
      title = "Playtika Quarterly Marketing Intensity Climbed After SuperPlay",
      subtitle = "Latest quarterly sales and marketing expense was 48.4% of GAAP revenue in Q1 2026.",
      caption = "Source: SEC XBRL companyfacts for Playtika Holding Corp. | Sales and marketing includes advertising, user acquisition, personnel, overhead, depreciation, and amortization. This is not true CAC."
    ) +
    theme_538()

  ggsave(output_path, plot, width = 13.5, height = 8.4, dpi = 220, bg = "white")
  invisible(output_path)
}

build_acquisition_proxy_chart_data <- function(acquisition_proxy) {
  revenue_comparison <- acquisition_proxy |>
    filter(.data$period_type == "quarter") |>
    select(
      "period_end",
      "period_id",
      playtika_reported_revenue = "gaap_revenue",
      sensor_tower_portfolio_revenue = "sensor_tower_portfolio_revenue"
    ) |>
    pivot_longer(
      c("playtika_reported_revenue", "sensor_tower_portfolio_revenue"),
      names_to = "series_key",
      values_to = "value"
    ) |>
    filter(!is.na(.data$value)) |>
    mutate(
      metric = "Revenue Comparison",
      series = recode(
        .data$series_key,
        playtika_reported_revenue = "Playtika Reported Revenue",
        sensor_tower_portfolio_revenue = "Sensor Tower Portfolio Revenue"
      ),
      label_value = format_money_short(.data$value)
    )

  cost_per_download <- acquisition_proxy |>
    filter(.data$period_type == "quarter") |>
    transmute(
      period_end,
      period_id,
      metric = "Implied Sales & Marketing Cost Per Download",
      series = "Implied Sales & Marketing Cost Per Download",
      value = .data$implied_cost_per_download,
      label_value = dollar(.data$value, accuracy = 0.01)
    )

  bind_rows(revenue_comparison, cost_per_download) |>
    mutate(
      metric = factor(
        .data$metric,
        levels = c("Revenue Comparison", "Implied Sales & Marketing Cost Per Download")
      ),
      series = factor(
        .data$series,
        levels = c(
          "Playtika Reported Revenue",
          "Sensor Tower Portfolio Revenue",
          "Implied Sales & Marketing Cost Per Download"
        )
      )
    )
}

render_acquisition_proxy_chart <- function(acquisition_proxy, output_path) {
  chart_data <- build_acquisition_proxy_chart_data(acquisition_proxy)

  endpoints <- chart_data |>
    group_by(.data$metric, .data$series) |>
    filter(.data$period_end == max(.data$period_end, na.rm = TRUE)) |>
    ungroup() |>
    mutate(label = if_else(
      .data$metric == "Revenue Comparison",
      paste0(.data$series, ": ", .data$label_value),
      paste0("Q1 2026: ", .data$label_value)
    ))

  plot <- ggplot(chart_data, aes(x = .data$period_end, y = .data$value, color = .data$series, linetype = .data$series)) +
    geom_line(linewidth = 1.05, na.rm = TRUE) +
    geom_point(size = 2.1, na.rm = TRUE) +
    geom_text_repel(
      data = endpoints,
      aes(label = .data$label),
      direction = "y",
      nudge_x = 90,
      hjust = 0,
      size = 3.4,
      color = "#222222",
      segment.color = "#aaaaaa",
      min.segment.length = 0,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    facet_wrap(~metric, ncol = 1, scales = "free_y") +
    scale_x_date(
      date_breaks = "6 months",
      labels = \(x) paste0(year(x), " Q", quarter(x)),
      expand = expansion(mult = c(0.01, 0.16))
    ) +
    scale_y_continuous(
      labels = function(x) {
        if (max(abs(x), na.rm = TRUE) > 1e6) {
          dollar(x / 1e6, accuracy = 1, suffix = "M")
        } else {
          dollar(x, accuracy = 0.01)
        }
      },
      limits = c(0, NA)
    ) +
    scale_color_manual(values = c(
      "Playtika Reported Revenue" = "#222222",
      "Sensor Tower Portfolio Revenue" = "#c44e52",
      "Implied Sales & Marketing Cost Per Download" = "#222222"
    )) +
    scale_linetype_manual(values = c(
      "Playtika Reported Revenue" = "solid",
      "Sensor Tower Portfolio Revenue" = "22",
      "Implied Sales & Marketing Cost Per Download" = "solid"
    )) +
    labs(
      title = "Playtika Revenue Diverges From Sensor Tower Gross Spend",
      subtitle = "Reported GAAP revenue is compared with Sensor Tower portfolio revenue; cost per download keeps the blended S&M proxy.",
      caption = "Source: SEC XBRL companyfacts; Sensor Tower via SensorTowerR.\nSensor Tower gross spend is a third-party proxy, not company-reported bookings; cost/download is blended portfolio S&M divided by all observed downloads."
    ) +
    theme_538()

  ggsave(output_path, plot, width = 13.5, height = 8.2, dpi = 220, bg = "white")
  invisible(output_path)
}

build_validation_checks <- function(marketing_intensity, acquisition_proxy, output_paths) {
  q1_2026 <- marketing_intensity |>
    filter(.data$period_id == "2026-Q1", .data$period_type == "quarter")

  annual_expected <- tibble(
    fiscal_year = 2020:2025,
    expected_share = c(
      502.0 / 2371.5,
      581.7 / 2583.0,
      603.7 / 2615.5,
      585.7 / 2567.0,
      705.0 / 2549.3,
      949.8 / 2755.4
    )
  )

  annual_actual <- marketing_intensity |>
    filter(.data$period_type == "annual", .data$fiscal_year %in% annual_expected$fiscal_year) |>
    select("fiscal_year", "sales_marketing_share_of_revenue") |>
    inner_join(annual_expected, by = "fiscal_year") |>
    mutate(abs_error = abs(.data$sales_marketing_share_of_revenue - .data$expected_share))

  ratio_rows <- marketing_intensity |>
    filter(!is.na(.data$sales_marketing_share_of_revenue))

  proxy_rows <- acquisition_proxy |>
    filter(!is.na(.data$sensor_tower_portfolio_revenue))
  marketing_chart_data <- build_marketing_share_chart_data(marketing_intensity)
  acquisition_chart_data <- build_acquisition_proxy_chart_data(acquisition_proxy)

  bind_rows(
    make_check(
      "q1_2026_ratio_reconciles",
      nrow(q1_2026) == 1 &&
        abs(q1_2026$gaap_revenue[[1]] - 744.7e6) < 1 &&
        abs(q1_2026$sales_marketing_expense[[1]] - 360.6e6) < 1 &&
        abs(q1_2026$sales_marketing_share_of_revenue[[1]] - 360.6 / 744.7) < 0.0001,
      "Q1 2026 ratio reconciles to 360.6 / 744.7 = 48.4%."
    ),
    make_check(
      "annual_playtika_matches_may_3_panel",
      nrow(annual_actual) == nrow(annual_expected) && max(annual_actual$abs_error, na.rm = TRUE) < 0.0001,
      "Annual 2020-2025 sales and marketing / revenue ratios match the existing May 3 Playtika benchmark rows."
    ),
    make_check(
      "ratio_rows_have_valid_sources_and_denominators",
      nrow(ratio_rows) > 0 &&
        all(!is.na(ratio_rows$gaap_revenue) & ratio_rows$gaap_revenue > 0) &&
        all(!is.na(ratio_rows$sales_marketing_expense) & ratio_rows$sales_marketing_expense >= 0) &&
        all(!is.na(ratio_rows$sales_marketing_source_url) & ratio_rows$sales_marketing_source_url != "") &&
        all(ratio_rows$revenue_denominator_label == "GAAP revenue"),
      "Every SEC sales and marketing ratio row has a positive revenue denominator, nonnegative numerator, source URL, and GAAP revenue label."
    ),
    make_check(
      "sensor_tower_proxy_rows_labeled",
      nrow(proxy_rows) > 0 &&
        all(proxy_rows$st_denominator_label == "Sensor Tower gross consumer spend proxy") &&
        all(str_detect(proxy_rows$proxy_caveat, fixed("not company-reported bookings"))),
      "Every Sensor Tower denominator row is labeled as a gross consumer spend proxy, not company-reported bookings."
    ),
    make_check(
      "acquisition_proxy_ratios_finite",
      nrow(proxy_rows) > 0 &&
        all(is.finite(proxy_rows$sales_marketing_share_of_st_proxy_bookings)) &&
        all(is.finite(proxy_rows$implied_cost_per_download)),
      "Sales and marketing / Sensor Tower proxy bookings and implied cost per download are finite for all joined proxy rows."
    ),
    make_check(
      "marketing_share_chart_is_quarterly_only",
      nrow(marketing_chart_data) > 0 &&
        all(marketing_chart_data$period_type == "quarter") &&
        !any(marketing_chart_data$panel == "Annual Filing Ratios"),
      "Marketing share chart data contains quarterly filing ratios only; annual filing-ratio panel is excluded."
    ),
    make_check(
      "acquisition_proxy_chart_compares_revenue_and_cost_per_download",
      all(c(
        "Playtika Reported Revenue",
        "Sensor Tower Portfolio Revenue",
        "Implied Sales & Marketing Cost Per Download"
      ) %in% unique(as.character(acquisition_chart_data$series))) &&
        all(acquisition_chart_data$value > 0, na.rm = TRUE),
      "Acquisition proxy chart compares Playtika reported revenue with Sensor Tower portfolio revenue and keeps implied S&M cost per download."
    ),
    make_check(
      "chart_outputs_exist",
      all(file.exists(output_paths)) && all(file.size(output_paths) > 0),
      paste("Rendered chart outputs:", paste(basename(output_paths), collapse = ", "))
    )
  )
}

build_playtika_marketing_intensity <- function(project_dir = get_script_dir(),
                                               write_outputs = TRUE,
                                               render_charts = TRUE,
                                               refresh_sec = FALSE) {
  data_dir <- ensure_dir(file.path(project_dir, "data"))
  cache_dir <- ensure_dir(file.path(data_dir, "cache"))
  output_dir <- ensure_dir(file.path(project_dir, "output"))

  sec_cache_path <- file.path(cache_dir, "playtika_sec_companyfacts.json")
  companyfacts <- fetch_sec_companyfacts(sec_cache_path, refresh = refresh_sec)
  marketing_intensity <- build_marketing_intensity_panel(companyfacts)

  sensor_tower_monthly <- read_playtika_sensor_tower_monthly(project_dir)
  sensor_tower_periods <- build_sensor_tower_periods(sensor_tower_monthly)
  acquisition_proxy <- build_acquisition_proxy_panel(marketing_intensity, sensor_tower_periods)

  chart_paths <- c(
    file.path(output_dir, "playtika_marketing_share_time_series_538.png"),
    file.path(output_dir, "playtika_implied_acquisition_cost_proxy_538.png")
  )

  if (isTRUE(write_outputs)) {
    write_csv(marketing_intensity, file.path(data_dir, "playtika_marketing_intensity_time_series.csv"), na = "")
    write_csv(acquisition_proxy, file.path(data_dir, "playtika_acquisition_proxy_time_series.csv"), na = "")
  }

  if (isTRUE(render_charts)) {
    render_marketing_share_chart(marketing_intensity, chart_paths[[1]])
    render_acquisition_proxy_chart(acquisition_proxy, chart_paths[[2]])
  }

  validation_checks <- build_validation_checks(
    marketing_intensity = marketing_intensity,
    acquisition_proxy = acquisition_proxy,
    output_paths = if (isTRUE(render_charts)) chart_paths else character()
  )

  if (!isTRUE(render_charts)) {
    validation_checks <- validation_checks |>
      filter(.data$check_name != "chart_outputs_exist")
  }

  if (isTRUE(write_outputs)) {
    write_csv(validation_checks, file.path(data_dir, "playtika_marketing_intensity_validation_checks.csv"), na = "")
  }

  assert_no_failed_checks(validation_checks)

  list(
    marketing_intensity = marketing_intensity,
    acquisition_proxy = acquisition_proxy,
    validation_checks = validation_checks,
    chart_paths = chart_paths
  )
}

if (sys.nframe() == 0) {
  outputs <- build_playtika_marketing_intensity()

  preview_dir <- ensure_dir("/tmp/codex_preview/social_casino_playtika")
  preview_paths <- file.path(preview_dir, basename(outputs$chart_paths))
  file.copy(outputs$chart_paths, preview_paths, overwrite = TRUE)

  message("\nOutputs written:")
  message("  ", file.path(get_script_dir(), "data", "playtika_marketing_intensity_time_series.csv"))
  message("  ", file.path(get_script_dir(), "data", "playtika_acquisition_proxy_time_series.csv"))
  message("  ", file.path(get_script_dir(), "data", "playtika_marketing_intensity_validation_checks.csv"))
  message("  ", outputs$chart_paths[[1]])
  message("  ", outputs$chart_paths[[2]])
  message("Preview copies:")
  walk(preview_paths, \(path) message("  ", path))
}
