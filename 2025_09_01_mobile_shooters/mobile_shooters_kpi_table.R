#!/usr/bin/env Rscript

# Mobile Shooters KPI Tables (CN and US)
# Replicates the Word Puzzle / Lilith GT table style using a Sensor Tower
# custom filter URL. Creates two tables: one for China (CN) and one for US.

suppressPackageStartupMessages({
  library(pacman)
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::load_all("../../sensortowerR")
  p_load(dplyr, gt, gtExtras, scales, glue, stringr, readr, httr)
})

# Optional: load DOF GT theme if available (non-fatal if missing)
tryCatch({
  if (file.exists("../../dof-theme/dof_gt_theme.R")) {
    source("../../dof-theme/dof_gt_theme.R")
  } else if (file.exists("../../dof_theme/dof_gt_theme.R")) {
    source("../../dof_theme/dof_gt_theme.R")
  }
}, error = function(e) {
  message("Note: DOF theme not loaded")
})

message("=== Generating Mobile Shooters KPI Tables (CN, US) ===\n")

# Fail loudly if API key is missing
api_token <- Sys.getenv("SENSORTOWER_AUTH_TOKEN", unset = "")
if (identical(api_token, "")) {
  stop(
    "Missing SENSORTOWER_AUTH_TOKEN environment variable.\n",
    "Set it in your shell or in the repo .env (../.env), ",
    "then restart your R session."
  )
}


# Provided Sensor Tower URLs (from web UI)
url_cn <- "https://app.sensortower.com/market-analysis/top-apps?metric=activeUsers&os=unified&custom_fields_filter_id=6009cf3a241bc16eb88e80cd&uai=6838fcfb0c4851bac2097fb0&saa=com.netease.g108na&saa=com.netease.g108hmt&sia=1593326563&sia=6744052056&edit=1&granularity=weekly&start_date=2025-08-02&end_date=2025-08-31&duration=P30D&measure=DAU&comparison_attribute=absolute&category=0&page=1&page_size=25&custom_fields_filter_mode=include_unified_apps&period=day&device=iphone&device=ipad&country=CN"
url_us <- "https://app.sensortower.com/market-analysis/top-apps?metric=activeUsers&os=unified&custom_fields_filter_id=6009cf3a241bc16eb88e80cd&uai=6838fcfb0c4851bac2097fb0&saa=com.netease.g108na&saa=com.netease.g108hmt&sia=1593326563&sia=6744052056&edit=1&granularity=weekly&start_date=2025-08-02&end_date=2025-08-31&duration=P30D&measure=DAU&comparison_attribute=absolute&category=0&page=1&page_size=25&custom_fields_filter_mode=include_unified_apps&period=day&country=US&device=iphone&device=ipad"

# Ensure output directory exists
if (!dir.exists("output")) dir.create("output", recursive = TRUE, showWarnings = FALSE)

# Helper: safely pick a column with fallbacks
pick_col_strict <- function(df, primary) {
  if (!(primary %in% names(df))) return(rep(NA_real_, nrow(df)))
  df[[primary]]
}

# Helper: decode strings like "<e4><b8><80>..." (hex bytes) into UTF-8 text
decode_bracket_hex <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    if (!grepl("^(<([0-9A-Fa-f]{2})>)+$", s, perl = TRUE)) return(s)
    hex <- unlist(regmatches(s, gregexpr("[0-9A-Fa-f]{2}", s, perl = TRUE)))
    if (length(hex) == 0) return(s)
    raw <- as.raw(strtoi(hex, base = 16L))
    # Convert raw UTF-8 bytes to character
    out <- tryCatch(rawToChar(raw), error = function(e) s)
    out <- tryCatch(enc2utf8(out), error = function(e) out)
    out
  }, character(1), USE.NAMES = FALSE)
}

# Helper: build KPI data frame from top charts response
build_kpi_data <- function(data, region_code) {
  if (is.null(region_code) || is.na(region_code) || !nzchar(region_code)) region_code <- "US"
  region_code <- toupper(region_code)
  reg <- tolower(region_code)

  # DAU: prefer the active-users absolute value from the DAU endpoint
  dau_vals <- pick_col_strict(data, "users_absolute")

  # MAU: prefer region-specific if available, else WW
  mau_vals <- pick_col_strict(data, paste0("mau_month_", reg))

  # Downloads/Revenue from custom tags (single call, no extra endpoints)
  # Prefer region-specific custom tags (e.g., "Last 30 Days Downloads (US)")
  # then fall back to WW. If not present in tags, try enriched 30d columns.
  extract_tag <- function(ent_df, label) {
    if (is.null(ent_df) || !is.data.frame(ent_df)) return(NA_real_)
    nm <- paste0("custom_tags.", label)
    if (nm %in% names(ent_df)) {
      val <- suppressWarnings(as.numeric(gsub("[$,]", "", ent_df[[nm]])))
      return(ifelse(length(val) > 0, val, NA_real_))
    }
    NA_real_
  }
  extract_tag_top <- function(df, label) {
    nm <- paste0("custom_tags.", label)
    if (nm %in% names(df)) {
      suppressWarnings(as.numeric(gsub("[$,]", "", df[[nm]])))
    } else rep(NA_real_, nrow(df))
  }

  # Pull from entities list-col rowwise
  ents <- if ("entities" %in% names(data)) data$entities else vector("list", nrow(data))
  reg_upper <- toupper(reg)
  dl_reg_lbl <- paste0("Last 30 Days Downloads (", reg_upper, ")")
  dl_ww_lbl  <- "Last 30 Days Downloads (WW)"
  rev_reg_lbl <- paste0("Last 30 Days Revenue (", reg_upper, ")")
  rev_ww_lbl  <- "Last 30 Days Revenue (WW)"

  if ("entities" %in% names(data)) {
    downloads_vals <- vapply(ents, function(e) {
      v <- extract_tag(e, dl_reg_lbl)
      if (is.na(v)) v <- extract_tag(e, dl_ww_lbl)
      v
    }, numeric(1))
    revenue_vals <- vapply(ents, function(e) {
      v <- extract_tag(e, rev_reg_lbl)
      if (is.na(v)) v <- extract_tag(e, rev_ww_lbl)
      v
    }, numeric(1))
  } else {
    downloads_vals <- extract_tag_top(data, dl_reg_lbl)
    if (all(is.na(downloads_vals))) downloads_vals <- extract_tag_top(data, dl_ww_lbl)
    revenue_vals <- extract_tag_top(data, rev_reg_lbl)
    if (all(is.na(revenue_vals))) revenue_vals <- extract_tag_top(data, rev_ww_lbl)
  }

  # MAU from region-specific column or custom tag fallback
  mau_vals <- pick_col_strict(data, paste0("mau_month_", reg))
  if (all(is.na(mau_vals))) {
    mau_reg_lbl <- paste0("Last Month Average MAU (", reg_upper, ")")
    mau_ww_lbl  <- "Last Month Average MAU (WW)"
    if ("entities" %in% names(data)) {
      mau_vals <- vapply(ents, function(e) {
        v <- extract_tag(e, mau_reg_lbl)
        if (is.na(v)) v <- extract_tag(e, mau_ww_lbl)
        v
      }, numeric(1))
    } else {
      mau_vals <- extract_tag_top(data, mau_reg_lbl)
      if (all(is.na(mau_vals))) mau_vals <- extract_tag_top(data, mau_ww_lbl)
    }
  }

  # Retention: prefer region-specific, then US, then WW
  d1_vals  <- pick_col_strict(data, paste0("retention_1d_",  reg))
  d7_vals  <- pick_col_strict(data, paste0("retention_7d_",  reg))
  d30_vals <- pick_col_strict(data, paste0("retention_30d_", reg))

  # Pick app name
  extract_tag_chr <- function(ent_df, label) {
    if (is.null(ent_df) || !is.data.frame(ent_df)) return(NA_character_)
    nm <- paste0("custom_tags.", label)
    if (nm %in% names(ent_df)) {
      v <- as.character(ent_df[[nm]])
      return(ifelse(length(v) > 0, v, NA_character_))
    }
    NA_character_
  }
  app_name <- if ("unified_app_name" %in% names(data)) data$unified_app_name
              else if ("app_name" %in% names(data)) data$app_name
              else if ("app.name" %in% names(data)) data$app.name
              else if ("entities" %in% names(data)) vapply(ents, function(e) extract_tag_chr(e, "unified_product_name"), character(1))
              else if ("custom_tags.unified_product_name" %in% names(data)) data[["custom_tags.unified_product_name"]]
              else rep(NA_character_, nrow(data))

  uid <- if ("unified_app_id" %in% names(data)) data$unified_app_id else
           if ("app_id" %in% names(data)) data$app_id else
             rep(NA_character_, nrow(data))

  out <- dplyr::tibble(
    rank = dplyr::row_number(dplyr::desc(dau_vals)),
    unified_app_id = uid,
    app_name = app_name,
    dau = as.numeric(dau_vals),
    mau = as.numeric(mau_vals),
    dau_mau_ratio = ifelse(!is.na(dau_vals) & !is.na(mau_vals) & mau_vals > 0, dau_vals / mau_vals, NA_real_),
    downloads = as.numeric(downloads_vals),
    revenue = as.numeric(revenue_vals),
    d1 = as.numeric(d1_vals),
    d7 = as.numeric(d7_vals),
    d30 = as.numeric(d30_vals)
  )

  # Re-rank after constructing dau and keep only the top 10
  out <- out %>%
    dplyr::arrange(dplyr::desc(dau)) %>%
    dplyr::mutate(
      rank = dplyr::row_number()
    ) %>%
    dplyr::slice_head(n = 10)

  # Add DAU share within listed titles
  total_dau <- sum(out$dau, na.rm = TRUE)
  out <- out %>% dplyr::mutate(share = ifelse(!is.na(dau) & total_dau > 0, dau / total_dau, NA_real_))
  out
}

# Helper: build the GT table with consistent styling
make_gt_table <- function(kpi_data, title, subtitle, region_code) {
  # Determine which optional columns have any data
  has_perf <- any(!is.na(kpi_data$downloads)) || any(!is.na(kpi_data$revenue))
  has_ret  <- any(!is.na(kpi_data$d1)) || any(!is.na(kpi_data$d7)) || any(!is.na(kpi_data$d30))
  if (tolower(region_code) == "cn") has_ret <- FALSE
  has_ratio <- any(!is.na(kpi_data$dau_mau_ratio))

  gt_tbl <- kpi_data %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(title = title, subtitle = subtitle) %>%
    cols_label(
      rank = "#",
      app_name = "Game",
      dau = "DAU",
      mau = "MAU",
      dau_mau_ratio = "Ratio",
      share = "Share",
      downloads = "Downloads",
      revenue = "Revenue",
      d1 = "D1",
      d7 = "D7",
      d30 = "D30"
    ) %>%
    tab_spanner(label = "Active Users", columns = c(dau, mau, dau_mau_ratio, share)) %>%
    {
      if (has_perf) tab_spanner(., label = "Performance", columns = c(downloads, revenue)) else .
    } %>%
    {
      if (has_ret) tab_spanner(., label = "Retention %", columns = c(d1, d7, d30)) else .
    } %>%
    # Use GT built-ins for compact rounding
    fmt_number(columns = c(dau, mau), suffixing = TRUE, decimals = 0) %>%
    { if (has_perf) fmt_number(., columns = downloads, suffixing = TRUE, decimals = 0) else . } %>%
    { if (has_perf) fmt_currency(., columns = revenue, suffixing = TRUE, decimals = 0) else . } %>%
    fmt_percent(columns = c(dau_mau_ratio, d1, d7, d30), decimals = 0) %>%
    fmt_percent(columns = share, decimals = 0) %>%
    # Render true em-dash via HTML entity to avoid byte-code artifacts
    sub_missing(missing_text = gt::html("&mdash;")) %>%
    # Heatmaps for ratio and retention (only if any values present)
    { if (has_ratio) data_color(., columns = dau_mau_ratio, method = "numeric",
               palette = c("#d73027", "#fee08b", "#1a9850"), domain = NULL) else . } %>%
    {
      if (has_ret) data_color(., columns = d1, method = "numeric",
                              palette = c("#d73027", "#fee08b", "#1a9850"), domain = NULL) else .
    } %>%
    {
      if (has_ret) data_color(., columns = d7, method = "numeric",
                              palette = c("#d73027", "#fee08b", "#1a9850"), domain = NULL) else .
    } %>%
    {
      if (has_ret) data_color(., columns = d30, method = "numeric",
                              palette = c("#d73027", "#fee08b", "#1a9850"), domain = NULL) else .
    } %>%
    # Add footnote clarifying share definition
    tab_source_note("Share = DAU share among titles listed (top 10)") %>%
    tab_source_note("Source: Sensor Tower") %>%
    tab_options(table.font.size = px(12), data_row.padding = px(5))

  # Prefer a CJK-capable font so Chinese names render properly
  gt_tbl <- gt_tbl %>%
    # Prefer system CJK fonts to ensure glyphs render in headless Chrome
    opt_table_font(font = list(
      "PingFang SC",            # macOS Simplified Chinese system font
      "Hiragino Sans GB",       # macOS/JP CJK font
      "Heiti SC",               # Older macOS CJK font
      "Noto Sans CJK SC",       # Installed Noto CJK family if present
      "Arial Unicode MS",       # Broad Unicode coverage
      default_fonts()
    ))

  # Hide retention columns for CN explicitly
  if (tolower(region_code) == "cn") {
    gt_tbl <- gt_tbl %>% cols_hide(columns = c(d1, d7, d30))
  }

  gt_tbl
}

# Fetch, prepare, and render for a given URL
build_and_save <- function(url, region_label_override = NULL) {
  params <- st_parse_web_url(url, verbose = FALSE)

  # Ensure required params for unified + custom filter
  params$enrich_response <- TRUE
  params$deduplicate_apps <- TRUE
  # Always limit to top 10
  params$limit <- 10

  # Derive region label
  region_code <- toupper(strsplit(params$regions, ",")[[1]][1])
  if (!is.null(region_label_override)) region_code <- toupper(region_label_override)

  # Execute API call (force iOS-only for CN)
  message(glue::glue("Fetching data for region: {region_code} ..."))
  if (toupper(region_code) == "CN") {
    params$os <- "ios"
    params$custom_tags_mode <- NULL
  }
  top_apps <- tryCatch({
    do.call(st_top_charts, params)
  }, error = function(e) {
    if (grepl("RETENTION", e$message, ignore.case = TRUE)) {
      message("Retention not available for region; retrying with iOS-only to bypass gating...")
      params_fallback <- params
      params_fallback$os <- "ios"
      # unified-only param not needed for iOS
      params_fallback$custom_tags_mode <- NULL
      tryCatch(do.call(st_top_charts, params_fallback), error = function(e2) stop(e2))
    } else stop(e)
  })

  message(glue::glue("  -> {nrow(top_apps)} apps returned"))

  # Package enforces region-only; no script-level fallbacks

  # Build KPI data
  kpi <- build_kpi_data(top_apps, region_code)

  # If app names missing (common for iOS CN), resolve via a single app-details query
  if (toupper(region_code) == "CN") {
    if (all(is.na(kpi$app_name)) || any(is.na(kpi$app_name))) {
      ios_ids <- if ("app_id" %in% names(top_apps)) top_apps$app_id else NULL
      if (!is.null(ios_ids) && length(ios_ids) > 0) {
        details <- tryCatch({ st_app_details(app_ids = as.character(ios_ids), os = "ios") }, error = function(e) NULL)
        if (!is.null(details) && nrow(details) > 0 && "app_name" %in% names(details)) {
          name_map <- details %>% dplyr::select(ios_id = app_id, resolved_name = app_name) %>% dplyr::distinct()
          # Join by the iOS app_id used in this CN run (stored in unified_app_id)
          kpi <- kpi %>%
            dplyr::mutate(ios_id = as.character(unified_app_id)) %>%
            dplyr::left_join(name_map, by = "ios_id") %>%
            dplyr::mutate(app_name = dplyr::coalesce(app_name, resolved_name)) %>%
            dplyr::select(-dplyr::any_of(c("ios_id", "resolved_name")))
        }
      }
    }
    # Ensure app names render as valid UTF-8 (avoid hex byte escapes)
    # Fix hex-bracket escaped names and ensure valid UTF-8
    kpi$app_name <- decode_bracket_hex(kpi$app_name)
    kpi$app_name <- tryCatch(enc2utf8(kpi$app_name), error = function(e) kpi$app_name)
    try(Encoding(kpi$app_name) <- "UTF-8", silent = TRUE)
  }

  # No additional API calls — downloads/revenue remain from custom tags only

  # Titles
  date_label <- "Aug 2 - Aug 31, 2025"
  title <- "TOP MOBILE SHOOTERS"
  subtitle <- glue::glue("By Daily Active Users | {date_label} | {region_code} Market")

  # Build table
  # Ensure we don't expose internal join key in the table
  gt_tbl <- make_gt_table(dplyr::select(kpi, -dplyr::any_of("unified_app_id")), title, subtitle, tolower(region_code))

  # Save outputs
  png_path <- glue::glue("output/mobile_shooters_{tolower(region_code)}.png")
  csv_path <- glue::glue("output/mobile_shooters_{tolower(region_code)}_data.csv")
  gtsave(gt_tbl, png_path, vwidth = 1400, vheight = 700)
  readr::write_csv(dplyr::select(kpi, -dplyr::any_of("unified_app_id")), csv_path)
  message(glue::glue("✓ Saved: {png_path}"))
  message(glue::glue("✓ Saved: {csv_path}"))
}

# Main execution: skip CN cleanly if region metrics unavailable
build_and_save(url_us, "US")
try({ build_and_save(url_cn, "CN") })

message("\nAll done!")
