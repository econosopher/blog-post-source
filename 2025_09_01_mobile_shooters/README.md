Mobile Shooters KPI Tables

Generates two GT tables for “Mobile Shooters” using Sensor Tower custom filter URLs, modeled after the Puzzle/Lilith tables. No fallbacks are applied; metrics are tied to the requested region.

What it does
- Parses the provided Sensor Tower web URLs into API parameters.
- Calls `st_top_charts()` with enrichment and deduplication enabled.
- Builds a FiveThirtyEight-styled GT table with these columns (no demographics):
  - Active Users: DAU (region), MAU (region only), Ratio
  - Performance: Downloads (region if available), Revenue (region if available)
  - Retention: D1, D7, D30 (region only)
  - Subtitles are moved into footnotes for clarity
- Saves outputs in `output/`:
  - `mobile_shooters_us.png` and `mobile_shooters_us_data.csv`
  - `mobile_shooters_cn.png` and `mobile_shooters_cn_data.csv`

Prerequisites
- R installed
- Packages: `sensortowerR`, `gt`, `gtExtras`, `dplyr`, `scales`, `glue`, `stringr`, `readr`, `pacman`
- Sensor Tower API token in environment: `SENSORTOWER_AUTH_TOKEN`
- Optional: local `sensortowerR` dev tree at `../../sensortowerR` (script will fall back to installed package)

URLs used
- CN: `https://app.sensortower.com/market-analysis/top-apps?metric=activeUsers&os=unified&custom_fields_filter_id=6009cf3a241bc16eb88e80cd&uai=6838fcfb0c4851bac2097fb0&saa=com.netease.g108na&saa=com.netease.g108hmt&sia=1593326563&sia=6744052056&edit=1&granularity=weekly&start_date=2025-08-02&end_date=2025-08-31&duration=P30D&measure=DAU&comparison_attribute=absolute&category=0&page=1&page_size=25&custom_fields_filter_mode=include_unified_apps&period=day&device=iphone&device=ipad&country=CN`
- US: `https://app.sensortower.com/market-analysis/top-apps?metric=activeUsers&os=unified&custom_fields_filter_id=6009cf3a241bc16eb88e80cd&uai=6838fcfb0c4851bac2097fb0&saa=com.netease.g108na&saa=com.netease.g108hmt&sia=1593326563&sia=6744052056&edit=1&granularity=weekly&start_date=2025-08-02&end_date=2025-08-31&duration=P30D&measure=DAU&comparison_attribute=absolute&category=0&page=1&page_size=25&custom_fields_filter_mode=include_unified_apps&period=day&country=US&device=iphone&device=ipad`

Run
- From this directory:
  - `Rscript mobile_shooters_kpi_table.R`

Notes
- DAU uses `users_absolute` from the active users endpoint aligned to the requested region.
- MAU/Retention are included only if region-specific fields exist for the requested region; otherwise the region’s table is not generated.
- Performance metrics are included only if region-specific (no WW fallback). If missing, the Performance spanner is omitted.
- Subtitles are moved to footnotes.
- Output subtitle dates fixed to Aug 2–31, 2025 to match the URLs.
