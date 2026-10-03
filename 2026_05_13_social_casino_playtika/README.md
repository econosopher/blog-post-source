# Social Casino Market + Playtika YTD Run Rate

Current build:

```bash
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/build_social_casino_market_from_export.R
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/build_playtika_marketing_intensity.R
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/test_playtika_marketing_intensity.R
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/test_social_casino_workflow_contracts.R
```

MONOPOLY GO! revenue refresh only:

```bash
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/fetch_monopoly_go_revenue.R
```

Website/exported subgenre market rebuild:

```bash
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/build_social_casino_market_from_export.R
```

Playtika marketing intensity / acquisition proxy:

```bash
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/build_playtika_marketing_intensity.R
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/test_playtika_marketing_intensity.R
```

Legacy all-in-one script:

```bash
# Not the primary rebuild path. This script intentionally stops before the old
# paginated top-app custom-filter denominator path.
Rscript /Users/phillip/Documents/vibe_coding_projects/blog-post-source/2026_05_13_social_casino_playtika/build_social_casino_playtika.R
```

Current handoff:

- The subgenre market denominator should come from the Sensor Tower website or a
  true market-level/export surface, not from paginated top-app rows.
- The merge-ready MONOPOLY GO! file is
  `data/monopoly_go_monthly_revenue_for_subgenre_merge.csv`.
- Join the dropped-in subgenre market CSV on `date` or `month`, then compute
  `revenue_without_monopoly_go_usd = total_subgenre_revenue_usd -
  monopoly_go_revenue_usd`.
- The export-based market rebuild now uses inflation-adjusted dollars as the
  primary `*_usd` fields, with nominal Sensor Tower export values retained in
  explicit `*_usd_nominal` columns.
- The chart footnote should declare the subgenres used in the website/exported
  market-level denominator and should not include source-process wording,
  MONOPOLY GO! unified app IDs, or partial-month caveats.

Scope:

- Sensor Tower API via local `SensorTowerR`.
- Worldwide unified iOS + Android.
- Social casino market totals use Sensor Tower's aggregate `games_breakdown`
  endpoint: iOS `Games/Casino` (`7006`) plus Android `Casino` (`game_casino`).
- The current `genre = "Casino"` filter is used only for roster/taxonomy audit,
  not as the market denominator.
- Adjusted market line subtracts only `MONOPOLY GO!` and starts in April 2023,
  the first month with material revenue and downloads in Sensor Tower.
- The ex-MONOPOLY GO! revenue line was triple-checked with a fresh Sensor Tower
  pull for the launch/ramp and latest-month windows. The apparent 2023 decline is
  not a one-month April cliff: ex-MONOPOLY GO! revenue moves from $383M in March
  2023 to $358M in April 2023, then falls through late 2023 as MONOPOLY GO!
  ramps to more than half of observed casino category revenue.
- Export-based revenue outputs are inflation-adjusted to April 2026 U.S. dollars
  using BLS CPI-U All Items (`CUUR0000SA0`). The CPI file is
  `data/cpi_u_all_items_monthly.csv`; internal missing CPI months are flagged in
  `cpi_is_interpolated`.
- Social casino revenue and downloads charts mark June 2021 as the ATT
  majority-distribution date, matching the prior Eric Seufert prep chart.
- Playtika table uses the current Sensor Tower Playtika publisher boundary. The
  rendered scorecard keeps the full portfolio row, then shows top-20 current
  revenue run-rate titles excluding title rows below $15M.
- Playtika marketing intensity is not true CAC. It uses SEC sales and marketing
  and advertising facts over GAAP revenue, plus Sensor Tower Playtika portfolio
  revenue/downloads only as third-party acquisition-cost proxies. Sensor Tower
  denominators are explicitly labeled as not company-reported bookings.
- The marketing-share chart is quarterly-only; the annual filing-ratio panel is
  retained in the CSV for reconciliation but not rendered. The acquisition proxy
  chart compares Playtika reported GAAP revenue with Sensor Tower portfolio
  revenue and keeps the implied sales and marketing cost per download series.

Primary outputs:

- `data/social_casino_monthly_market.csv`
- `data/social_casino_market_monthly_with_without_monopoly_go_from_export.csv`
- `data/social_casino_roster_audit.csv`
- `data/social_casino_market_platform_country.csv`
- `data/monopoly_go_monthly_metrics.csv`
- `data/monopoly_go_impact_summary.csv`
- `data/social_casino_revenue_triple_check_fresh_pull.csv`
- `output/social_casino_monthly_revenue_with_without_monopoly_go_538.png`
- `output/social_casino_monthly_downloads_with_without_monopoly_go_538.png`
- `data/playtika_portfolio_ytd_run_rate_table_data.csv`
- `data/playtika_portfolio_ytd_run_rate_full_table_data.csv`
- `data/playtika_marketing_intensity_time_series.csv`
- `data/playtika_acquisition_proxy_time_series.csv`
- `data/playtika_marketing_intensity_validation_checks.csv`
- `output/playtika_portfolio_ytd_run_rate_table.png`
- `output/playtika_marketing_share_time_series_538.png`
- `output/playtika_implied_acquisition_cost_proxy_538.png`
- `data/validation_checks.csv`
