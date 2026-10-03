#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(devtools)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(glue)
  library(gt)
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
      plot.margin = margin(20, 72, 28, 18)
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

slugify <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", "and") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

first_existing_column <- function(data, candidates, default = NA_character_) {
  existing <- intersect(candidates, names(data))
  if (length(existing) == 0) {
    rep(default, nrow(data))
  } else {
    data[[existing[[1]]]]
  }
}

pluck_scalar <- function(x, name, default = NA_character_) {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  value[[1]]
}

pluck_numeric <- function(x, name) {
  suppressWarnings(as.numeric(pluck_scalar(x, name, default = NA_real_)))
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

read_cache <- function(path, required_cols = character()) {
  if (!file.exists(path)) {
    return(NULL)
  }

  cached <- read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(required_cols, names(cached))
  if (length(missing_cols) > 0) {
    message("Ignoring cache with missing columns: ", path)
    return(NULL)
  }

  cached
}

fetch_app_metrics_cached <- function(app_ids, date_from, date_to, cache_path, label) {
  required_cols <- c("app_id", "os", "country", "date", "revenue", "downloads")
  cached <- read_cache(cache_path, required_cols)
  cache_end_month <- floor_date(date_to, "month")
  if (!is.null(cached)) {
    cached <- cached |> mutate(date = as.Date(.data$date))
    if (nrow(cached) > 0 && max(cached$date, na.rm = TRUE) >= cache_end_month) {
      message("Using cached metrics for ", label, ": ", cache_path)
      return(cached |> filter(.data$date >= date_from, .data$date <= date_to))
    }
  }

  ids <- unique(na.omit(as.character(app_ids)))
  message(glue("Fetching {label}: {length(ids)} apps from {date_from} to {date_to}"))

  rows <- imap_dfr(ids, function(app_id, idx) {
    message(glue("  {label} app {idx}/{length(ids)}: {app_id}"))
    out <- tryCatch(
      {
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
        )
      },
      error = function(e) {
        message("    Failed app ", app_id, ": ", conditionMessage(e))
        tibble()
      }
    )
    Sys.sleep(0.2)
    out
  })

  rows <- rows |>
    mutate(
      app_id = as.character(.data$app_id),
      os = as.character(.data$os),
      country = as.character(.data$country),
      date = as.Date(.data$date),
      revenue = as.numeric(.data$revenue),
      downloads = as.numeric(.data$downloads)
    ) |>
    arrange(.data$app_id, .data$date, .data$country)

  write_csv(rows, cache_path, na = "")
  rows
}

fetch_unified_sales_metrics_cached <- function(app_ids, date_from, date_to, cache_path, label, chunk_size = 50) {
  required_cols <- c("date", "country", "unified_app_id", "revenue", "downloads")
  cached <- read_cache(cache_path, required_cols)
  ids <- unique(na.omit(as.character(app_ids)))
  id_cache_path <- paste0(cache_path, ".ids")
  cached_requested_ids <- if (file.exists(id_cache_path)) read_lines(id_cache_path) else character()
  cache_end_month <- floor_date(date_to, "month")

  if (!is.null(cached)) {
    cached <- cached |>
      mutate(
        date = as.Date(.data$date),
        unified_app_id = as.character(.data$unified_app_id)
      )
    cached_ids <- unique(cached$unified_app_id)
    if (
      nrow(cached) > 0 &&
        all(ids %in% cached_requested_ids) &&
        min(cached$date, na.rm = TRUE) <= date_from &&
        max(cached$date, na.rm = TRUE) >= cache_end_month
    ) {
      message("Using cached unified sales metrics for ", label, ": ", cache_path)
      return(cached |> filter(.data$date >= date_from, .data$date <= date_to, .data$unified_app_id %in% ids))
    }
  }

  id_chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
  segment_starts <- seq(floor_date(date_from, "year"), floor_date(date_to, "year"), by = "1 year")
  segments <- tibble(
    segment_start = pmax(segment_starts, date_from),
    segment_end = pmin(ceiling_date(segment_starts, "year") - days(1), date_to)
  )

  message(glue("Fetching {label} with unified sales endpoint: {length(ids)} apps, {length(id_chunks)} chunks"))
  rows <- imap_dfr(id_chunks, function(chunk_ids, chunk_idx) {
    map_dfr(seq_len(nrow(segments)), function(segment_idx) {
      segment_start <- segments$segment_start[[segment_idx]]
      segment_end <- segments$segment_end[[segment_idx]]
      message(glue("  {label} chunk {chunk_idx}/{length(id_chunks)}: {segment_start} to {segment_end}"))
      st_unified_sales_report_impl(
        unified_app_id = chunk_ids,
        countries = "WW",
        start_date = segment_start,
        end_date = segment_end,
        date_granularity = "monthly",
        auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
        verbose = FALSE
      )
    })
  }) |>
    mutate(
      date = as.Date(.data$date),
      country = as.character(.data$country),
      unified_app_id = as.character(.data$unified_app_id),
      revenue = as.numeric(.data$revenue),
      downloads = as.numeric(.data$downloads)
    ) |>
    arrange(.data$unified_app_id, .data$date, .data$country)

  write_csv(rows, cache_path, na = "")
  write_lines(ids, id_cache_path)
  rows
}

fetch_social_casino_market_country_cached <- function(date_from, date_to, cache_path) {
  required_cols <- c("date", "os", "country_code", "category_id", "revenue_usd", "downloads")
  cached <- read_cache(cache_path, required_cols)
  cache_end_month <- floor_date(date_to, "month")
  if (!is.null(cached)) {
    cached <- cached |> mutate(date = as.Date(.data$date))
    if (nrow(cached) > 0 && min(cached$date, na.rm = TRUE) <= date_from && max(cached$date, na.rm = TRUE) >= cache_end_month) {
      message("Using cached aggregate social casino market data: ", cache_path)
      return(cached |> filter(.data$date >= date_from, .data$date <= date_to))
    }
  }

  platform_categories <- tribble(
    ~os, ~category_id,
    "ios", "7006",
    "android", "game_casino"
  )

  segment_starts <- seq(floor_date(date_from, "year"), floor_date(date_to, "year"), by = "1 year")
  segments <- tibble(
    segment_start = pmax(segment_starts, date_from),
    segment_end = pmin(ceiling_date(segment_starts, "year") - days(1), date_to)
  )

  normalize_game_summary <- function(raw, os, category_id) {
    raw_tbl <- as_tibble(raw)
    if (nrow(raw_tbl) == 0) {
      return(tibble())
    }

    if (os == "ios") {
      downloads <- suppressWarnings(as.numeric(raw_tbl$iu)) + suppressWarnings(as.numeric(raw_tbl$au))
      revenue_usd <- (suppressWarnings(as.numeric(raw_tbl$ir)) + suppressWarnings(as.numeric(raw_tbl$ar))) / 100
    } else {
      downloads <- suppressWarnings(as.numeric(raw_tbl$u))
      revenue_usd <- suppressWarnings(as.numeric(raw_tbl$r)) / 100
    }

    tibble(
      date = as.Date(substr(raw_tbl$d, 1, 10)),
      os = os,
      country_code = as.character(raw_tbl$cc),
      category_id = category_id,
      source_endpoint = "games_breakdown",
      revenue_usd = revenue_usd,
      downloads = downloads
    ) |>
      filter(!is.na(.data$date), !is.na(.data$country_code), .data$country_code != "")
  }

  message(glue("Fetching aggregate social casino market via games_breakdown: {date_from} to {date_to}"))
  rows <- pmap_dfr(platform_categories, function(os, category_id) {
    map_dfr(seq_len(nrow(segments)), function(idx) {
      segment_start <- segments$segment_start[[idx]]
      segment_end <- segments$segment_end[[idx]]
      message(glue("  {os} {category_id}: {segment_start} to {segment_end}"))
      raw <- st_game_summary(
        categories = category_id,
        countries = "WW",
        os = os,
        date_granularity = "monthly",
        start_date = segment_start,
        end_date = segment_end,
        auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
        enrich_response = FALSE
      )
      normalize_game_summary(raw, os, category_id)
    })
  }) |>
    arrange(.data$date, .data$os, .data$country_code)

  write_csv(rows, cache_path, na = "")
  rows
}

create_social_casino_subgenre_filter <- function(subgenres, output_path) {
  filter_id <- st_filter(
    custom_fields = list("Game Sub-genre" = subgenres),
    auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  )

  filter_audit <- tibble(
    filter_scope = "social_casino_game_subgenre_composite",
    game_subgenre = subgenres,
    filter_id = as.character(filter_id)
  )

  write_csv(filter_audit, output_path, na = "")
  filter_id
}

parse_comparison_page <- function(response_text, query_date, offset, limit, subgenre_filter_id) {
  parsed <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
  if (length(parsed) == 0) {
    return(tibble())
  }

  rows <- imap_dfr(parsed, function(item, idx) {
    row_date <- as.Date(substr(as.character(pluck_scalar(item, "date", default = as.character(query_date))), 1, 10))
    tibble(
      date = row_date,
      rank = offset + idx,
      unified_app_id = as.character(pluck_scalar(item, "app_id")),
      revenue_usd = pluck_numeric(item, "revenue_absolute") / 100,
      downloads = pluck_numeric(item, "units_absolute"),
      revenue_delta_usd = pluck_numeric(item, "revenue_delta") / 100,
      downloads_delta = pluck_numeric(item, "units_delta"),
      source_endpoint = "sales_report_estimates_comparison_attributes",
      source_measure = "revenue",
      custom_fields_filter_id = as.character(subgenre_filter_id),
      custom_tags_mode = "include_unified_apps",
      query_date = as.Date(query_date),
      page_offset = offset,
      page_limit = limit
    )
  })

  rows |>
    filter(!is.na(.data$date), !is.na(.data$unified_app_id), .data$unified_app_id != "")
}

fetch_social_casino_subgenre_revenue_cached <- function(date_from,
                                                        date_to,
                                                        cache_path,
                                                        page_audit_path,
                                                        subgenre_filter_id,
                                                        subgenres,
                                                        limit = 2000L,
                                                        max_pages = 12L) {
  stop(
    "Do not use paginated top-app custom-filter rows as the social casino subgenre market denominator. ",
    "Export the true market-level subgenre revenue CSV from Sensor Tower and join it to ",
    "data/monopoly_go_monthly_revenue_for_subgenre_merge.csv instead."
  )

  required_cols <- c("date", "rank", "unified_app_id", "revenue_usd", "source_endpoint", "page_offset")
  cached <- read_cache(cache_path, required_cols)
  audit_cached <- read_cache(page_audit_path, c("date", "page_offset", "terminal_page"))

  month_starts <- seq(floor_date(date_from, "month"), floor_date(date_to, "month"), by = "1 month")
  month_starts <- as.Date(month_starts)
  completed_months <- if (!is.null(audit_cached) && nrow(audit_cached) > 0) {
    audit_cached |>
      mutate(date = as.Date(.data$date)) |>
      filter(.data$terminal_page) |>
      distinct(.data$date) |>
      pull(.data$date)
  } else {
    as.Date(character())
  }

  rows <- if (!is.null(cached)) {
    cached |>
      mutate(
        date = as.Date(.data$date),
        query_date = as.Date(.data$query_date)
      )
  } else {
    tibble()
  }

  page_audit <- if (!is.null(audit_cached)) {
    audit_cached |>
      mutate(
        date = as.Date(.data$date),
        query_date = as.Date(.data$query_date)
      )
  } else {
    tibble()
  }

  missing_months <- setdiff(month_starts, completed_months)
  if (length(missing_months) == 0 && nrow(rows) > 0) {
    message("Using cached social casino subgenre revenue rows: ", cache_path)
    return(rows |> filter(.data$date >= date_from, .data$date <= date_to))
  }

  message(glue(
    "Fetching social casino subgenre composite revenue via paginated custom filter: {length(missing_months)} missing months"
  ))

  endpoint_url <- "https://api.sensortower.com/v1/unified/sales_report_estimates_comparison_attributes"

  for (month_start in missing_months) {
    month_start <- as.Date(month_start, origin = "1970-01-01")
    month_end <- ceiling_date(month_start, "month") - days(1)
    message(glue("  Subgenre composite month {format(month_start, '%Y-%m')}"))

    month_rows <- tibble()
    month_audit <- tibble()

    for (page_idx in seq_len(max_pages)) {
      offset <- (page_idx - 1L) * limit
      resp <- request(endpoint_url) |>
        req_url_query(
          auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
          comparison_attribute = "absolute",
          time_range = "month",
          measure = "revenue",
          date = as.character(month_end),
          category = 0,
          regions = "WW",
          limit = limit,
          offset = offset,
          device_type = "total",
          custom_fields_filter_id = as.character(subgenre_filter_id),
          custom_tags_mode = "include_unified_apps"
        ) |>
        req_headers(Accept = "application/json") |>
        req_timeout(120) |>
        req_perform()

      response_text <- resp_body_string(resp)
      page_rows <- parse_comparison_page(
        response_text = response_text,
        query_date = month_end,
        offset = offset,
        limit = limit,
        subgenre_filter_id = subgenre_filter_id
      )

      page_max_revenue <- if (nrow(page_rows) > 0) max(page_rows$revenue_usd, na.rm = TRUE) else 0
      page_revenue <- if (nrow(page_rows) > 0) sum(page_rows$revenue_usd, na.rm = TRUE) else 0
      terminal_page <- nrow(page_rows) < limit || page_max_revenue <= 0

      month_rows <- bind_rows(month_rows, page_rows)
      month_audit <- bind_rows(
        month_audit,
        tibble(
          date = month_start,
          query_date = month_end,
          page_offset = offset,
          page_limit = limit,
          page_rows = nrow(page_rows),
          page_revenue_usd = page_revenue,
          page_max_revenue_usd = page_max_revenue,
          terminal_page = terminal_page,
          source_endpoint = "sales_report_estimates_comparison_attributes",
          source_measure = "revenue",
          custom_fields_filter_id = as.character(subgenre_filter_id)
        )
      )

      message(glue(
        "    offset {offset}: {nrow(page_rows)} rows, {format_money_short(page_revenue)} revenue, max row {format_money_short(page_max_revenue)}"
      ))

      Sys.sleep(0.1)
      if (terminal_page) break
    }

    if (!any(month_audit$terminal_page)) {
      stop("Pagination did not reach a zero-revenue or short terminal page for ", month_start)
    }

    rows <- bind_rows(rows, month_rows) |>
      distinct(.data$date, .data$rank, .data$unified_app_id, .keep_all = TRUE) |>
      arrange(.data$date, .data$rank)
    page_audit <- bind_rows(page_audit, month_audit) |>
      distinct(.data$date, .data$page_offset, .keep_all = TRUE) |>
      arrange(.data$date, .data$page_offset)

    write_csv(rows, cache_path, na = "")
    write_csv(page_audit, page_audit_path, na = "")
  }

  rows |> filter(.data$date >= date_from, .data$date <= date_to)
}

fetch_casino_roster <- function(latest_complete_month_end, cache_path) {
  cached <- read_cache(cache_path, c("rank", "unified_app_id", "unified_app_name"))
  if (!is.null(cached) && nrow(cached) > 0) {
    message("Using cached casino roster: ", cache_path)
    return(cached)
  }

  casino_filter <- st_filter(
    genre = "Casino",
    auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  )

  message("Fetching current Sensor Tower casino roster with filter ", as.character(casino_filter))
  roster_raw <- st_rankings(
    entity = "app",
    os = "unified",
    country = "WW",
    date = latest_complete_month_end,
    limit = 1500,
    filter = casino_filter,
    auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN")
  )

  roster <- roster_raw |>
    as_tibble() |>
    transmute(
      rank = as.integer(.data$rank),
      unified_app_id = as.character(.data$id),
      unified_app_name = as.character(.data$name),
      game_genre = first_existing_column(roster_raw, c("aggregate_tags.Game Genre", "custom_tags.Game Genre")),
      game_subgenre = first_existing_column(roster_raw, c("aggregate_tags.Game Sub-genre", "custom_tags.Game Sub-genre")),
      publisher_name = first_existing_column(roster_raw, c("unified_publisher_name", "publisher_name")),
      ranking_revenue_usd = suppressWarnings(as.numeric(first_existing_column(roster_raw, c("revenue"), default = NA_real_))),
      filter_id = as.character(casino_filter),
      ranking_date = as.Date(latest_complete_month_end)
    ) |>
    filter(!is.na(.data$unified_app_id), .data$unified_app_id != "") |>
    distinct(.data$unified_app_id, .keep_all = TRUE) |>
    arrange(.data$rank)

  write_csv(roster, cache_path, na = "")
  roster
}

fetch_playtika_roster <- function(cache_path) {
  cached <- read_cache(cache_path, c("unified_app_id", "unified_app_name"))
  if (!is.null(cached) && nrow(cached) > 0) {
    message("Using cached Playtika roster: ", cache_path)
    return(cached)
  }

  playtika_id <- "5614c22f3f07e25d290063a9"
  message("Fetching Playtika publisher portfolio: ", playtika_id)
  apps_raw <- st_publisher_apps(
    unified_id = playtika_id,
    aggregate_related = FALSE,
    auth_token = Sys.getenv("SENSORTOWER_AUTH_TOKEN"),
    verbose = FALSE
  ) |>
    as_tibble()

  apps <- apps_raw |>
    transmute(
      unified_app_id = as.character(first_existing_column(apps_raw, c("unified_app_id", "app_id", "id"))),
      unified_app_name = as.character(first_existing_column(apps_raw, c("unified_app_name", "app_name", "name"))),
      selected_publisher_id = playtika_id,
      selected_publisher_name = "Playtika"
    ) |>
    filter(!is.na(.data$unified_app_id), .data$unified_app_id != "") |>
    distinct(.data$unified_app_id, .keep_all = TRUE) |>
    arrange(.data$unified_app_name)

  write_csv(apps, cache_path, na = "")
  apps
}

build_market_monthly <- function(platform_country_monthly, monopoly_go_monthly, monopoly_go_adjustment_start_date) {
  market <- platform_country_monthly |>
    group_by(.data$date) |>
    summarise(
      total_revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
      total_downloads = sum(.data$downloads, na.rm = TRUE),
      market_source_rows = n(),
      .groups = "drop"
    )

  monopoly <- monopoly_go_monthly |>
    group_by(.data$date) |>
    summarise(
      monopoly_go_revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
      monopoly_go_downloads = sum(.data$downloads, na.rm = TRUE),
      .groups = "drop"
    )

  market |>
    left_join(monopoly, by = "date") |>
    mutate(
      monopoly_go_revenue_usd = coalesce(.data$monopoly_go_revenue_usd, 0),
      monopoly_go_downloads = coalesce(.data$monopoly_go_downloads, 0),
      monopoly_go_adjustment_active = .data$date >= monopoly_go_adjustment_start_date,
      revenue_without_monopoly_go_usd = if_else(
        .data$monopoly_go_adjustment_active,
        .data$total_revenue_usd - .data$monopoly_go_revenue_usd,
        NA_real_
      ),
      downloads_without_monopoly_go = if_else(
        .data$monopoly_go_adjustment_active,
        .data$total_downloads - .data$monopoly_go_downloads,
        NA_real_
      )
    ) |>
    arrange(.data$date)
}

build_market_monthly_from_subgenre_rows <- function(subgenre_revenue_rows,
                                                    monopoly_go_id,
                                                    monopoly_go_adjustment_start_date) {
  market <- subgenre_revenue_rows |>
    group_by(.data$date) |>
    summarise(
      total_revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
      market_source_rows = n(),
      positive_revenue_source_rows = sum(.data$revenue_usd > 0, na.rm = TRUE),
      source_page_count = n_distinct(.data$page_offset),
      .groups = "drop"
    )

  monopoly <- subgenre_revenue_rows |>
    filter(.data$unified_app_id == monopoly_go_id) |>
    group_by(.data$date) |>
    summarise(
      monopoly_go_revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
      monopoly_go_downloads_in_revenue_ranked_rows = sum(.data$downloads, na.rm = TRUE),
      .groups = "drop"
    )

  market |>
    left_join(monopoly, by = "date") |>
    mutate(
      monopoly_go_revenue_usd = coalesce(.data$monopoly_go_revenue_usd, 0),
      monopoly_go_downloads_in_revenue_ranked_rows = coalesce(.data$monopoly_go_downloads_in_revenue_ranked_rows, 0),
      monopoly_go_adjustment_active = .data$date >= monopoly_go_adjustment_start_date,
      revenue_without_monopoly_go_usd = if_else(
        .data$monopoly_go_adjustment_active,
        .data$total_revenue_usd - .data$monopoly_go_revenue_usd,
        NA_real_
      )
    ) |>
    arrange(.data$date)
}

build_market_chart <- function(market_monthly,
                               metric,
                               output_path,
                               latest_complete_month_end,
                               monopoly_go_adjustment_start_date,
                               subgenres = NULL) {
  if (metric == "revenue") {
    chart_data <- market_monthly |>
      select("date", total = "total_revenue_usd", adjusted = "revenue_without_monopoly_go_usd") |>
      pivot_longer(cols = c("total", "adjusted"), names_to = "series_key", values_to = "value")
    title <- "Social Casino Monthly Revenue"
    y_labels <- label_dollar(scale_cut = cut_short_scale(), accuracy = 1)
    endpoint_formatter <- format_money_short
  } else {
    chart_data <- market_monthly |>
      select("date", total = "total_downloads", adjusted = "downloads_without_monopoly_go") |>
      pivot_longer(cols = c("total", "adjusted"), names_to = "series_key", values_to = "value")
    title <- "Social Casino Monthly Downloads"
    y_labels <- label_number(scale_cut = cut_short_scale(), accuracy = 1)
    endpoint_formatter <- format_count_short
  }

  chart_data <- chart_data |>
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
    mutate(label = paste0(as.character(.data$series), "  ", endpoint_formatter(.data$value)))

  gg <- ggplot(chart_data, aes(x = .data$date, y = .data$value, color = .data$series, linetype = .data$series)) +
    geom_line(linewidth = 1.05, na.rm = TRUE) +
    geom_point(data = endpoints, size = 2.4, show.legend = FALSE) +
    ggrepel::geom_text_repel(
      data = endpoints,
      aes(label = .data$label),
      direction = "y",
      nudge_x = 45,
      hjust = 0,
      segment.color = "#b7b7b7",
      segment.size = 0.3,
      min.segment.length = 0,
      box.padding = 0.35,
      point.padding = 0.25,
      seed = 13,
      show.legend = FALSE,
      size = 3.5
    ) +
    scale_color_manual(values = c("Social Casino Market" = "#3b6fb6", "Excluding MONOPOLY GO!" = "#d55e00")) +
    scale_linetype_manual(values = c("Social Casino Market" = "solid", "Excluding MONOPOLY GO!" = "11")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.18))) +
    scale_y_continuous(labels = y_labels, expand = expansion(mult = c(0.03, 0.12))) +
    labs(
      title = title,
      subtitle = glue(
        "Worldwide iOS + Android monthly Sensor Tower estimates; dotted line subtracts MONOPOLY GO! from {format(monopoly_go_adjustment_start_date, '%B %Y')} onward"
      ),
      caption = glue(
        "{str_wrap(glue('Source: Sensor Tower via SensorTowerR /v1/unified/sales_report_estimates_comparison_attributes, using a composite Game Sub-genre custom filter and paginating until the revenue tail is zero. Subgenres used: {paste(subgenres, collapse = \", \")}. Latest complete month: {format(latest_complete_month_end, \"%B %Y\")}'), width = 145)}"
      )
    ) +
    theme_538()

  ggsave(output_path, gg, width = 13.2, height = 7.4, dpi = 220, bg = "white")
  invisible(output_path)
}

build_playtika_table_data <- function(playtika_monthly, playtika_roster, casino_roster, latest_complete_month_end) {
  run_rate_month_count <- month(latest_complete_month_end)
  years <- 2023:year(latest_complete_month_end)

  ytd <- playtika_monthly |>
    mutate(year = year(.data$date), month = month(.data$date)) |>
    filter(.data$year %in% years, .data$month <= run_rate_month_count) |>
    group_by(.data$unified_app_id, .data$year) |>
    summarise(
      revenue_ytd_usd = sum(.data$revenue_usd, na.rm = TRUE),
      downloads_ytd = sum(.data$downloads, na.rm = TRUE),
      observed_months = n_distinct(.data$month[.data$revenue_usd > 0 | .data$downloads > 0]),
      .groups = "drop"
    ) |>
    mutate(
      revenue_run_rate_usd = .data$revenue_ytd_usd / run_rate_month_count * 12,
      downloads_run_rate = .data$downloads_ytd / run_rate_month_count * 12
    )

  revenue_wide <- ytd |>
    select(.data$unified_app_id, .data$year, .data$revenue_run_rate_usd) |>
    pivot_wider(names_from = .data$year, values_from = .data$revenue_run_rate_usd, names_prefix = "revenue_run_rate_", values_fill = 0)

  downloads_wide <- ytd |>
    select(.data$unified_app_id, .data$year, .data$downloads_run_rate) |>
    pivot_wider(names_from = .data$year, values_from = .data$downloads_run_rate, names_prefix = "downloads_run_rate_", values_fill = 0)

  observed_wide <- ytd |>
    select(.data$unified_app_id, .data$year, .data$observed_months) |>
    pivot_wider(names_from = .data$year, values_from = .data$observed_months, names_prefix = "observed_months_", values_fill = 0)

  tags <- casino_roster |>
    select(.data$unified_app_id, market_rank = .data$rank, .data$game_genre, .data$game_subgenre)

  table_data <- playtika_roster |>
    left_join(tags, by = "unified_app_id") |>
    left_join(revenue_wide, by = "unified_app_id") |>
    left_join(downloads_wide, by = "unified_app_id") |>
    left_join(observed_wide, by = "unified_app_id") |>
    mutate(
      game_subgenre = if_else(is.na(.data$game_subgenre) | .data$game_subgenre == "", "Other / Unranked", .data$game_subgenre)
    )

  for (yr in years) {
    for (prefix in c("revenue_run_rate_", "downloads_run_rate_", "observed_months_")) {
      col <- paste0(prefix, yr)
      if (!col %in% names(table_data)) table_data[[col]] <- 0
    }
  }

  latest_year <- max(years)
  previous_year <- latest_year - 1
  latest_rev <- paste0("revenue_run_rate_", latest_year)
  prev_rev <- paste0("revenue_run_rate_", previous_year)
  latest_dl <- paste0("downloads_run_rate_", latest_year)
  prev_dl <- paste0("downloads_run_rate_", previous_year)

  table_data <- table_data |>
    mutate(
      revenue_growth_latest_yoy = if_else(.data[[prev_rev]] > 0, (.data[[latest_rev]] - .data[[prev_rev]]) / .data[[prev_rev]], NA_real_),
      downloads_growth_latest_yoy = if_else(.data[[prev_dl]] > 0, (.data[[latest_dl]] - .data[[prev_dl]]) / .data[[prev_dl]], NA_real_)
    ) |>
    filter(if_any(starts_with("revenue_run_rate_"), ~ .x > 0) | if_any(starts_with("downloads_run_rate_"), ~ .x > 0)) |>
    arrange(desc(.data[[latest_rev]]), .data$unified_app_name) |>
    mutate(rank = row_number())

  total_row <- table_data |>
    summarise(
      across(starts_with("revenue_run_rate_"), ~ sum(.x, na.rm = TRUE)),
      across(starts_with("downloads_run_rate_"), ~ sum(.x, na.rm = TRUE)),
      across(starts_with("observed_months_"), ~ run_rate_month_count),
      revenue_growth_latest_yoy = (sum(.data[[latest_rev]], na.rm = TRUE) - sum(.data[[prev_rev]], na.rm = TRUE)) / sum(.data[[prev_rev]], na.rm = TRUE),
      downloads_growth_latest_yoy = (sum(.data[[latest_dl]], na.rm = TRUE) - sum(.data[[prev_dl]], na.rm = TRUE)) / sum(.data[[prev_dl]], na.rm = TRUE)
    ) |>
    mutate(
      unified_app_id = "portfolio_total",
      unified_app_name = "Portfolio Total",
      selected_publisher_id = "5614c22f3f07e25d290063a9",
      selected_publisher_name = "Playtika",
      market_rank = NA_integer_,
      game_genre = NA_character_,
      game_subgenre = "",
      rank = NA_integer_
    )

  bind_rows(total_row, table_data) |>
    select(
      .data$rank, .data$unified_app_id, .data$unified_app_name, .data$game_subgenre,
      starts_with("revenue_run_rate_"),
      .data$revenue_growth_latest_yoy,
      starts_with("downloads_run_rate_"),
      .data$downloads_growth_latest_yoy,
      starts_with("observed_months_"),
      .data$market_rank
    )
}

filter_playtika_display_table <- function(table_data, latest_complete_month_end, top_n = 20, min_latest_revenue_usd = 15e6) {
  latest_rev <- paste0("revenue_run_rate_", year(latest_complete_month_end))

  portfolio_row <- table_data |>
    filter(.data$unified_app_id == "portfolio_total")

  visible_titles <- table_data |>
    filter(.data$unified_app_id != "portfolio_total") |>
    arrange(.data$rank) |>
    slice_head(n = top_n) |>
    filter(.data[[latest_rev]] >= min_latest_revenue_usd)

  bind_rows(portfolio_row, visible_titles)
}

render_playtika_gt <- function(table_data,
                               output_path,
                               latest_complete_month_end,
                               displayed_title_count,
                               source_title_count,
                               top_n = 20,
                               min_latest_revenue_usd = 15e6) {
  years <- sort(as.integer(str_remove(grep("^revenue_run_rate_", names(table_data), value = TRUE), "^revenue_run_rate_")), decreasing = TRUE)
  run_rate_month_count <- month(latest_complete_month_end)
  latest_year <- max(years)
  previous_year <- latest_year - 1

  revenue_cols <- paste0("revenue_run_rate_", years)
  downloads_cols <- paste0("downloads_run_rate_", years)

  display_data <- table_data |>
    select(
      .data$rank,
      game = .data$unified_app_name,
      subgenre = .data$game_subgenre,
      all_of(revenue_cols),
      .data$revenue_growth_latest_yoy,
      all_of(downloads_cols),
      .data$downloads_growth_latest_yoy
    )

  labels <- c(
    list(
      rank = "#",
      game = "Game",
      subgenre = "Subgenre",
      revenue_growth_latest_yoy = glue("Rev. {str_sub(as.character(previous_year), 3, 4)}-{str_sub(as.character(latest_year), 3, 4)}"),
      downloads_growth_latest_yoy = glue("Dl. {str_sub(as.character(previous_year), 3, 4)}-{str_sub(as.character(latest_year), 3, 4)}")
    ),
    setNames(as.list(as.character(years)), revenue_cols),
    setNames(as.list(as.character(years)), downloads_cols)
  )

  growth_fill <- function(x) {
    capped <- pmax(pmin(as.numeric(x), 1), -1)
    scales::col_numeric(
      palette = c("#c0392b", "#fff7ec", "#2e7d32"),
      domain = c(-1, 1),
      na.color = "#ffffff"
    )(capped)
  }

  table_gt <- display_data |>
    gt() |>
    tab_header(
      title = "Playtika Portfolio YTD Run-Rate Scorecard",
      subtitle = glue(
        "Portfolio total includes all {source_title_count} apps; displayed titles are top {top_n} by {latest_year} revenue run rate and >= {format_money_short(min_latest_revenue_usd)}"
      )
    ) |>
    tab_spanner(label = "Revenue Run Rate", columns = all_of(revenue_cols)) |>
    tab_spanner(label = "Downloads Run Rate", columns = all_of(downloads_cols)) |>
    cols_label(.list = labels) |>
    fmt(
      columns = all_of(revenue_cols),
      fns = function(x) {
        ifelse(is.na(x) | x == 0, "-",
          ifelse(x >= 1e9, paste0("$", format(round(x / 1e9, 1), nsmall = 1), "B"),
            ifelse(x >= 1e6, paste0("$", round(x / 1e6), "M"), paste0("$", round(x / 1e3), "K"))
          )
        )
      }
    ) |>
    fmt_number(columns = all_of(downloads_cols), decimals = 0, suffixing = TRUE) |>
    fmt_percent(columns = c("revenue_growth_latest_yoy", "downloads_growth_latest_yoy"), decimals = 0) |>
    data_color(
      columns = c("revenue_growth_latest_yoy", "downloads_growth_latest_yoy"),
      fn = growth_fill
    ) |>
    sub_missing(columns = everything(), missing_text = "-") |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(rows = game == "Portfolio Total")
    ) |>
    tab_style(
      style = cell_fill(color = "#f0f0f0"),
      locations = cells_body(
        columns = c("rank", "game", "subgenre", all_of(revenue_cols), all_of(downloads_cols)),
        rows = game == "Portfolio Total"
      )
    ) |>
    tab_options(
      table.font.names = "Helvetica",
      table.font.size = px(13),
      column_labels.font.size = px(12),
      heading.title.font.size = px(24),
      heading.subtitle.font.size = px(14),
      data_row.padding = px(5),
      source_notes.font.size = px(11)
    ) |>
    tab_source_note(
      source_note = glue(
        "Source: Sensor Tower via SensorTowerR | Publisher boundary: Playtika (5614c22f3f07e25d290063a9) | Run rate = Jan-{format(latest_complete_month_end, '%b')} YTD / {run_rate_month_count} * 12 | Visible title rows: {displayed_title_count}"
      )
    )

  gtsave(table_gt, output_path, vwidth = 1900, vheight = 980)
  invisible(output_path)
}

script_dir <- get_script_dir()
data_dir <- ensure_dir(file.path(script_dir, "data"))
cache_dir <- ensure_dir(file.path(data_dir, "cache"))
output_dir <- ensure_dir(file.path(script_dir, "output"))
unlink(file.path(output_dir, "social_casino_monthly_downloads_with_without_monopoly_go_538.png"))

load_sensortower_token()
load_sensortower_package()

analysis_date <- Sys.Date()
latest_complete_month_end <- floor_date(analysis_date, "month") - days(1)
market_start_date <- as.Date("2012-01-01")
playtika_start_date <- as.Date("2023-01-01")
monopoly_go_id <- "62be6a5fbab10c69c5a0a42a"
monopoly_go_launch_revenue_threshold_usd <- 5e6
monopoly_go_launch_download_threshold <- 5e6
playtika_display_top_n <- 20L
playtika_display_min_latest_revenue_usd <- 15e6
social_casino_subgenres <- c(
  "Slots",
  "Poker",
  "Casino Cards",
  "Mahjong",
  "Bingo",
  "Other Casino",
  "Fish Shooting",
  "Virtual Casino",
  "Landlord",
  "Coin Looters",
  "Pachinko",
  "Okey",
  "Coin Pusher",
  "Dominoes",
  "Teen Patti"
)
subgenre_market_page_limit <- 2000L

casino_roster_path <- file.path(data_dir, "social_casino_roster_audit.csv")
monopoly_go_metrics_cache <- file.path(cache_dir, "monopoly_go_monthly_metrics.csv")
subgenre_filter_audit_path <- file.path(data_dir, "social_casino_subgenre_filter_ids.csv")
subgenre_revenue_cache <- file.path(cache_dir, "social_casino_subgenre_revenue_rows.csv")
subgenre_page_audit_cache <- file.path(cache_dir, "social_casino_subgenre_revenue_page_audit.csv")
playtika_roster_path <- file.path(data_dir, "playtika_roster_audit.csv")
playtika_metrics_cache <- file.path(cache_dir, "playtika_title_monthly_metrics.csv")

casino_roster <- fetch_casino_roster(latest_complete_month_end, casino_roster_path)
if (!monopoly_go_id %in% casino_roster$unified_app_id) {
  monopoly_lookup <- st_apps(query = "MONOPOLY GO", os = "unified", limit = 10) |>
    as_tibble() |>
    filter(.data$unified_app_id == monopoly_go_id)
  if (nrow(monopoly_lookup) == 0) {
    stop("Could not verify MONOPOLY GO! unified app ID in Sensor Tower search.")
  }
  casino_roster <- bind_rows(
    casino_roster,
    tibble(
      rank = NA_integer_,
      unified_app_id = monopoly_go_id,
      unified_app_name = "MONOPOLY GO!",
      game_genre = "Casino",
      game_subgenre = "Coin Looters",
      publisher_name = NA_character_,
      ranking_revenue_usd = NA_real_,
      filter_id = unique(casino_roster$filter_id)[1],
      ranking_date = latest_complete_month_end
    )
  ) |>
    distinct(.data$unified_app_id, .keep_all = TRUE)
  write_csv(casino_roster, casino_roster_path, na = "")
}

social_casino_subgenre_filter <- create_social_casino_subgenre_filter(
  subgenres = social_casino_subgenres,
  output_path = subgenre_filter_audit_path
)

social_casino_subgenre_revenue_rows <- fetch_social_casino_subgenre_revenue_cached(
  date_from = market_start_date,
  date_to = latest_complete_month_end,
  cache_path = subgenre_revenue_cache,
  page_audit_path = subgenre_page_audit_cache,
  subgenre_filter_id = social_casino_subgenre_filter,
  subgenres = social_casino_subgenres,
  limit = subgenre_market_page_limit
)
write_csv(social_casino_subgenre_revenue_rows, file.path(data_dir, "social_casino_subgenre_revenue_rows.csv"), na = "")

social_casino_subgenre_page_audit <- read_csv(subgenre_page_audit_cache, show_col_types = FALSE) |>
  mutate(
    date = as.Date(.data$date),
    query_date = as.Date(.data$query_date)
  )
write_csv(social_casino_subgenre_page_audit, file.path(data_dir, "social_casino_subgenre_revenue_page_audit.csv"), na = "")

monopoly_go_metrics <- fetch_app_metrics_cached(
  app_ids = monopoly_go_id,
  date_from = market_start_date,
  date_to = latest_complete_month_end,
  cache_path = monopoly_go_metrics_cache,
  label = "MONOPOLY GO! exclusion"
)

monopoly_go_monthly <- monopoly_go_metrics |>
  transmute(
    date = as.Date(.data$date),
    unified_app_id = as.character(.data$app_id),
    revenue_usd = coalesce(as.numeric(.data$revenue), 0),
    downloads = coalesce(as.numeric(.data$downloads), 0)
  ) |>
  group_by(.data$date, .data$unified_app_id) |>
  summarise(
    revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
    downloads = sum(.data$downloads, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(monopoly_go_monthly, file.path(data_dir, "monopoly_go_monthly_metrics.csv"), na = "")

monopoly_go_composite_monthly <- social_casino_subgenre_revenue_rows |>
  filter(.data$unified_app_id == monopoly_go_id) |>
  group_by(.data$date, .data$unified_app_id) |>
  summarise(
    revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
    downloads = sum(.data$downloads, na.rm = TRUE),
    .groups = "drop"
  )

monopoly_go_adjustment_start_date <- monopoly_go_composite_monthly |>
  filter(
    .data$revenue_usd >= monopoly_go_launch_revenue_threshold_usd,
    .data$downloads >= monopoly_go_launch_download_threshold
  ) |>
  summarise(date = min(.data$date, na.rm = TRUE)) |>
  pull(.data$date)

if (length(monopoly_go_adjustment_start_date) == 0 || is.na(monopoly_go_adjustment_start_date)) {
  stop("Could not identify a material MONOPOLY GO! launch month for the adjusted market series.")
}

social_casino_monthly_market <- build_market_monthly_from_subgenre_rows(
  subgenre_revenue_rows = social_casino_subgenre_revenue_rows,
  monopoly_go_id = monopoly_go_id,
  monopoly_go_adjustment_start_date = monopoly_go_adjustment_start_date
)
write_csv(social_casino_monthly_market, file.path(data_dir, "social_casino_monthly_market.csv"), na = "")
write_csv(social_casino_monthly_market, file.path(data_dir, "social_casino_subgenre_monthly_market.csv"), na = "")

monopoly_go_impact_audit <- social_casino_monthly_market |>
  mutate(
    monopoly_go_revenue_share = if_else(.data$total_revenue_usd > 0, .data$monopoly_go_revenue_usd / .data$total_revenue_usd, NA_real_)
  )

monopoly_go_impact_summary <- tibble(
  metric = c(
    "adjustment_start_date",
    "latest_month",
    "latest_revenue_share",
    "peak_revenue_share",
    "peak_revenue_share_month",
    "subgenre_composite_filter_id"
  ),
  value = c(
    as.character(monopoly_go_adjustment_start_date),
    as.character(max(monopoly_go_impact_audit$date, na.rm = TRUE)),
    percent(tail(na.omit(monopoly_go_impact_audit$monopoly_go_revenue_share), 1), accuracy = 0.1),
    percent(max(monopoly_go_impact_audit$monopoly_go_revenue_share, na.rm = TRUE), accuracy = 0.1),
    as.character(monopoly_go_impact_audit$date[which.max(monopoly_go_impact_audit$monopoly_go_revenue_share)]),
    as.character(social_casino_subgenre_filter)
  )
)
write_csv(monopoly_go_impact_audit, file.path(data_dir, "monopoly_go_impact_monthly_audit.csv"), na = "")
write_csv(monopoly_go_impact_summary, file.path(data_dir, "monopoly_go_impact_summary.csv"), na = "")

build_market_chart(
  social_casino_monthly_market,
  metric = "revenue",
  output_path = file.path(output_dir, "social_casino_monthly_revenue_with_without_monopoly_go_538.png"),
  latest_complete_month_end = latest_complete_month_end,
  monopoly_go_adjustment_start_date = monopoly_go_adjustment_start_date,
  subgenres = social_casino_subgenres
)

playtika_roster <- fetch_playtika_roster(playtika_roster_path)
playtika_metrics <- fetch_unified_sales_metrics_cached(
  app_ids = playtika_roster$unified_app_id,
  date_from = playtika_start_date,
  date_to = latest_complete_month_end,
  cache_path = playtika_metrics_cache,
  label = "Playtika portfolio"
)

playtika_title_monthly <- playtika_metrics |>
  left_join(playtika_roster |> select(.data$unified_app_id, .data$unified_app_name), by = "unified_app_id") |>
  transmute(
    date = as.Date(.data$date),
    unified_app_id = as.character(.data$unified_app_id),
    unified_app_name = .data$unified_app_name,
    revenue_usd = coalesce(as.numeric(.data$revenue), 0),
    downloads = coalesce(as.numeric(.data$downloads), 0)
  ) |>
  group_by(.data$date, .data$unified_app_id, .data$unified_app_name) |>
  summarise(
    revenue_usd = sum(.data$revenue_usd, na.rm = TRUE),
    downloads = sum(.data$downloads, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$date, .data$unified_app_name)

write_csv(playtika_title_monthly, file.path(data_dir, "playtika_title_monthly_metrics.csv"), na = "")

playtika_table_data <- build_playtika_table_data(
  playtika_monthly = playtika_title_monthly,
  playtika_roster = playtika_roster,
  casino_roster = casino_roster,
  latest_complete_month_end = latest_complete_month_end
)
write_csv(playtika_table_data, file.path(data_dir, "playtika_portfolio_ytd_run_rate_full_table_data.csv"), na = "")

playtika_table_display_data <- filter_playtika_display_table(
  table_data = playtika_table_data,
  latest_complete_month_end = latest_complete_month_end,
  top_n = playtika_display_top_n,
  min_latest_revenue_usd = playtika_display_min_latest_revenue_usd
)
write_csv(playtika_table_display_data, file.path(data_dir, "playtika_portfolio_ytd_run_rate_table_data.csv"), na = "")

render_playtika_gt(
  table_data = playtika_table_display_data,
  output_path = file.path(output_dir, "playtika_portfolio_ytd_run_rate_table.png"),
  latest_complete_month_end = latest_complete_month_end,
  displayed_title_count = nrow(playtika_table_display_data |> filter(.data$unified_app_id != "portfolio_total")),
  source_title_count = nrow(playtika_table_data |> filter(.data$unified_app_id != "portfolio_total")),
  top_n = playtika_display_top_n,
  min_latest_revenue_usd = playtika_display_min_latest_revenue_usd
)

market_recalc <- social_casino_subgenre_revenue_rows |>
  group_by(.data$date) |>
  summarise(
    total_revenue_usd_check = sum(.data$revenue_usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  inner_join(social_casino_monthly_market, by = "date") |>
  mutate(
    revenue_abs_error = abs(.data$total_revenue_usd_check - .data$total_revenue_usd)
  )

subgenre_page_tail_check <- social_casino_subgenre_page_audit |>
  group_by(.data$date) |>
  arrange(.data$page_offset, .by_group = TRUE) |>
  slice_tail(n = 1) |>
  ungroup() |>
  mutate(page_is_complete_tail = .data$page_rows < .data$page_limit | .data$page_max_revenue_usd <= 0)

monopoly_go_endpoint_compare <- monopoly_go_composite_monthly |>
  select(.data$date, composite_revenue_usd = .data$revenue_usd) |>
  inner_join(
    monopoly_go_monthly |> select(.data$date, app_metric_revenue_usd = .data$revenue_usd),
    by = "date"
  ) |>
  mutate(revenue_abs_error = abs(.data$composite_revenue_usd - .data$app_metric_revenue_usd))

playtika_total_check <- playtika_table_data |>
  filter(.data$unified_app_id != "portfolio_total") |>
  summarise(across(starts_with("revenue_run_rate_"), ~ sum(.x, na.rm = TRUE))) |>
  bind_cols(
    playtika_table_data |>
      filter(.data$unified_app_id == "portfolio_total") |>
      select(starts_with("revenue_run_rate_")) |>
      rename_with(\(x) paste0(x, "_total"))
  )

revenue_cols <- grep("^revenue_run_rate_\\d{4}$", names(playtika_table_data), value = TRUE)
playtika_total_max_error <- max(abs(as.numeric(playtika_total_check[revenue_cols]) - as.numeric(playtika_total_check[paste0(revenue_cols, "_total")])), na.rm = TRUE)
observed_month_cols <- grep("^observed_months_\\d{4}$", names(playtika_table_data), value = TRUE)
portfolio_observed_months <- playtika_table_data |>
  filter(.data$unified_app_id == "portfolio_total") |>
  select(all_of(observed_month_cols)) |>
  unlist(use.names = FALSE)
latest_playtika_revenue_col <- paste0("revenue_run_rate_", year(latest_complete_month_end))
visible_playtika_titles <- playtika_table_display_data |>
  filter(.data$unified_app_id != "portfolio_total")
latest_monopoly_go_impact <- monopoly_go_impact_audit |>
  arrange(.data$date) |>
  slice_tail(n = 1)

validation_checks <- bind_rows(
  make_check(
    "casino_roster_nonempty",
    nrow(casino_roster) > 100,
    glue("Casino roster has {nrow(casino_roster)} unique unified apps.")
  ),
  make_check(
    "top_casino_roster_genre_is_casino",
    {
      sampled_genres <- casino_roster |> arrange(.data$rank) |> slice_head(n = min(50, nrow(casino_roster))) |> pull(.data$game_genre)
      sampled_genres <- sampled_genres[!is.na(sampled_genres) & sampled_genres != ""]
      length(sampled_genres) > 0 && all(sampled_genres == "Casino")
    },
    "Top sampled casino roster rows carry Game Genre = Casino."
  ),
  make_check(
    "subgenre_composite_filter_contains_expected_subgenres",
    {
      filter_audit <- read_csv(subgenre_filter_audit_path, show_col_types = FALSE)
      setequal(filter_audit$game_subgenre, social_casino_subgenres) &&
        n_distinct(filter_audit$filter_id) == 1
    },
    glue("Composite custom filter {as.character(social_casino_subgenre_filter)} covers {length(social_casino_subgenres)} declared Game Sub-genre values.")
  ),
  make_check(
    "market_uses_subgenre_custom_filter_endpoint",
    all(social_casino_subgenre_revenue_rows$source_endpoint == "sales_report_estimates_comparison_attributes") &&
      all(social_casino_subgenre_revenue_rows$custom_fields_filter_id == as.character(social_casino_subgenre_filter)),
    "Market denominator comes from the paginated Sensor Tower market-analysis endpoint with one composite Game Sub-genre custom filter."
  ),
  make_check(
    "subgenre_market_pages_reach_complete_tail",
    all(subgenre_page_tail_check$page_is_complete_tail) &&
      n_distinct(subgenre_page_tail_check$date) == length(seq(floor_date(market_start_date, "month"), floor_date(latest_complete_month_end, "month"), by = "1 month")),
    glue("Every monthly revenue pull ends on a short page or zero-revenue page; max terminal-page revenue is {format_money_short(max(subgenre_page_tail_check$page_max_revenue_usd, na.rm = TRUE))}.")
  ),
  make_check(
    "monopoly_go_present_before_exclusion",
    monopoly_go_id %in% monopoly_go_composite_monthly$unified_app_id && sum(monopoly_go_composite_monthly$revenue_usd, na.rm = TRUE) > 0,
    glue("MONOPOLY GO! ID {monopoly_go_id} is present inside the subgenre composite before exclusion.")
  ),
  make_check(
    "monopoly_go_composite_reconciles_to_app_metrics",
    nrow(monopoly_go_endpoint_compare) > 0 && max(monopoly_go_endpoint_compare$revenue_abs_error, na.rm = TRUE) < 1,
    glue("Max MONOPOLY GO! revenue difference between composite row and direct app metrics is {format_money_short(max(monopoly_go_endpoint_compare$revenue_abs_error, na.rm = TRUE))}.")
  ),
  make_check(
    "adjusted_market_never_exceeds_total",
    all(social_casino_monthly_market$revenue_without_monopoly_go_usd <= social_casino_monthly_market$total_revenue_usd + 1e-6, na.rm = TRUE),
    "Adjusted revenue totals are less than or equal to full market revenue totals for every month."
  ),
  make_check(
    "adjusted_series_starts_at_monopoly_go_launch_ramp",
    min(social_casino_monthly_market$date[!is.na(social_casino_monthly_market$revenue_without_monopoly_go_usd)], na.rm = TRUE) == monopoly_go_adjustment_start_date,
    glue("Adjusted series starts {format(monopoly_go_adjustment_start_date, '%Y-%m-%d')}, first month with MONOPOLY GO! revenue >= {format_money_short(monopoly_go_launch_revenue_threshold_usd)} and downloads >= {format_count_short(monopoly_go_launch_download_threshold)}.")
  ),
  make_check(
    "monopoly_go_impact_is_material",
    latest_monopoly_go_impact$monopoly_go_revenue_share[[1]] > 0.10 &&
      max(monopoly_go_impact_audit$monopoly_go_revenue_share, na.rm = TRUE) > 0.25,
    glue(
      "MONOPOLY GO! latest revenue share is {percent(latest_monopoly_go_impact$monopoly_go_revenue_share[[1]], accuracy = 0.1)}; peak share is {percent(max(monopoly_go_impact_audit$monopoly_go_revenue_share, na.rm = TRUE), accuracy = 0.1)}."
    )
  ),
  make_check(
    "monthly_market_totals_reconcile",
    max(market_recalc$revenue_abs_error, na.rm = TRUE) < 1e-6,
    glue("Max revenue error versus summed paginated subgenre rows {number(max(market_recalc$revenue_abs_error, na.rm = TRUE), accuracy = 0.000001)}.")
  ),
  make_check(
    "latest_complete_month_explicit",
    max(social_casino_monthly_market$date, na.rm = TRUE) == as.Date(format(latest_complete_month_end, "%Y-%m-01")),
    glue("Latest complete month is {format(latest_complete_month_end, '%Y-%m-%d')}; monthly row date is {format(max(social_casino_monthly_market$date, na.rm = TRUE), '%Y-%m-%d')}.")
  ),
  make_check(
    "playtika_portfolio_total_equals_titles",
    is.finite(playtika_total_max_error) && playtika_total_max_error < 1e-6,
    glue("Max Playtika portfolio revenue run-rate reconciliation error: {number(playtika_total_max_error, accuracy = 0.000001)}.")
  ),
  make_check(
    "playtika_ytd_run_rate_months_identical",
    all(portfolio_observed_months == month(latest_complete_month_end)),
    glue("Displayed run-rate years all use Jan-{format(latest_complete_month_end, '%b')} ({month(latest_complete_month_end)} months).")
  ),
  make_check(
    "playtika_display_keeps_portfolio_total",
    playtika_table_display_data$unified_app_id[[1]] == "portfolio_total",
    "Displayed Playtika table keeps the full portfolio total row first."
  ),
  make_check(
    "playtika_display_filters_top20_and_min_revenue",
    nrow(visible_playtika_titles) <= playtika_display_top_n &&
      all(visible_playtika_titles[[latest_playtika_revenue_col]] >= playtika_display_min_latest_revenue_usd),
    glue("Displayed {nrow(visible_playtika_titles)} title rows; each has {year(latest_complete_month_end)} revenue run rate >= {format_money_short(playtika_display_min_latest_revenue_usd)}.")
  )
)

write_csv(validation_checks, file.path(data_dir, "validation_checks.csv"), na = "")
assert_no_failed_checks(validation_checks)

manifest <- tibble(
  key = c(
    "analysis_date",
    "latest_complete_month_end",
    "market_start_date",
    "playtika_start_date",
    "market_scope",
    "market_endpoint",
    "social_casino_subgenre_filter_id",
    "social_casino_subgenres",
    "subgenre_pagination_rule",
    "monopoly_go_unified_app_id",
    "monopoly_go_adjusted_series_start",
    "monopoly_go_adjusted_series_start_rule",
    "playtika_publisher_id",
    "playtika_display_rule"
  ),
  value = c(
    as.character(analysis_date),
    as.character(latest_complete_month_end),
    as.character(market_start_date),
    as.character(playtika_start_date),
    "Worldwide unified iOS + Android",
    "Sensor Tower /v1/unified/sales_report_estimates_comparison_attributes with one composite Game Sub-genre custom filter",
    as.character(social_casino_subgenre_filter),
    paste(social_casino_subgenres, collapse = ", "),
    glue("limit={subgenre_market_page_limit}, offset pagination continues until the monthly page is short or max page revenue is zero"),
    monopoly_go_id,
    as.character(monopoly_go_adjustment_start_date),
    glue("First month with MONOPOLY GO! revenue >= {format_money_short(monopoly_go_launch_revenue_threshold_usd)} and downloads >= {format_count_short(monopoly_go_launch_download_threshold)}"),
    "5614c22f3f07e25d290063a9",
    glue("Portfolio total plus top {playtika_display_top_n} titles by {year(latest_complete_month_end)} revenue run rate, excluding title rows below {format_money_short(playtika_display_min_latest_revenue_usd)}")
  )
)
write_csv(manifest, file.path(data_dir, "source_manifest.csv"), na = "")

message("\nOutputs written:")
message("  ", file.path(data_dir, "social_casino_monthly_market.csv"))
message("  ", file.path(data_dir, "social_casino_subgenre_revenue_rows.csv"))
message("  ", file.path(data_dir, "social_casino_subgenre_revenue_page_audit.csv"))
message("  ", file.path(data_dir, "social_casino_roster_audit.csv"))
message("  ", file.path(output_dir, "social_casino_monthly_revenue_with_without_monopoly_go_538.png"))
message("  ", file.path(data_dir, "playtika_portfolio_ytd_run_rate_table_data.csv"))
message("  ", file.path(data_dir, "playtika_portfolio_ytd_run_rate_full_table_data.csv"))
message("  ", file.path(data_dir, "monopoly_go_impact_summary.csv"))
message("  ", file.path(output_dir, "playtika_portfolio_ytd_run_rate_table.png"))
message("  ", file.path(data_dir, "validation_checks.csv"))
