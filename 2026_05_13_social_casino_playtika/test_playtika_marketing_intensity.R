#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(testthat)
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

project_dir <- get_script_dir()
script_path <- file.path(project_dir, "build_playtika_marketing_intensity.R")

source(script_path)

outputs <- build_playtika_marketing_intensity(
  project_dir = project_dir,
  write_outputs = FALSE,
  render_charts = FALSE
)

test_that("latest Q1 2026 Playtika sales and marketing share reconciles to the filing", {
  q1_2026 <- outputs$marketing_intensity |>
    filter(.data$period_id == "2026-Q1", .data$period_type == "quarter")

  expect_equal(nrow(q1_2026), 1)
  expect_equal(q1_2026$gaap_revenue[[1]], 744.7e6, tolerance = 1)
  expect_equal(q1_2026$sales_marketing_expense[[1]], 360.6e6, tolerance = 1)
  expect_equal(q1_2026$sales_marketing_share_of_revenue[[1]], 360.6 / 744.7, tolerance = 0.0001)
})

test_that("annual Playtika sales and marketing share matches the May 3 benchmark panel", {
  expected <- tibble(
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

  actual <- outputs$marketing_intensity |>
    filter(.data$period_type == "annual", .data$fiscal_year %in% expected$fiscal_year) |>
    select("fiscal_year", "sales_marketing_share_of_revenue") |>
    inner_join(expected, by = "fiscal_year")

  expect_equal(nrow(actual), nrow(expected))
  expect_equal(actual$sales_marketing_share_of_revenue, actual$expected_share, tolerance = 0.0001)
})

test_that("Sensor Tower acquisition proxy rows are explicitly labeled as third-party proxies", {
  proxy_rows <- outputs$acquisition_proxy |>
    filter(!is.na(.data$sensor_tower_portfolio_revenue))

  expect_gt(nrow(proxy_rows), 0)
  expect_true(all(proxy_rows$st_denominator_label == "Sensor Tower gross consumer spend proxy"))
  expect_true(all(grepl("not company-reported bookings", proxy_rows$proxy_caveat, fixed = TRUE)))
  expect_false(any(proxy_rows$st_denominator_label == "Bookings"))
})

test_that("marketing share chart data is quarterly only", {
  chart_data <- build_marketing_share_chart_data(outputs$marketing_intensity)

  expect_gt(nrow(chart_data), 0)
  expect_true(all(chart_data$period_type == "quarter"))
  expect_false(any(chart_data$panel == "Annual Filing Ratios"))
})

test_that("acquisition proxy chart compares Playtika and Sensor Tower revenue and keeps cost per download", {
  chart_data <- build_acquisition_proxy_chart_data(outputs$acquisition_proxy)

  expect_true(all(c(
    "Playtika Reported Revenue",
    "Sensor Tower Portfolio Revenue",
    "Implied Sales & Marketing Cost Per Download"
  ) %in% unique(chart_data$series)))

  revenue_rows <- chart_data |>
    filter(.data$metric == "Revenue Comparison")

  expect_gt(nrow(revenue_rows), 0)
  expect_true(all(revenue_rows$value > 0))
  expect_true(all(revenue_rows$series[revenue_rows$series == "Sensor Tower Portfolio Revenue"] != "Bookings"))
})
