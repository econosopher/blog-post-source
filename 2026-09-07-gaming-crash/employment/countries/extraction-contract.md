# Country employment extraction contract

Cutoff: 2026-09-09. Observation target 2015-latest. One JSON per country under country/{ISO2}.json; agents own only assigned countries and raw/{ISO2}/. Do not change prior article or ASGC output. Use null for missing values, never 0 as missing. Sources publicly available by cutoff only. Source label year is not necessarily observation year. No invented annual data, no annual interpolation, no automatic FTE conversion or subgroup gross-up.

JSON structure (all arrays required; strings may be "unknown" when appropriate):
```
{
 "country":"Sweden", "iso2":"SE", "priority":"primary",
 "coverage_status":"usable_series|usable_snapshot|context_only|evidence_gap",
 "summary":"Concise evidence-based finding, limitations and search completeness.",
 "preferred_series_ids":["SE_domestic"],
 "search_log":[{"route":"statistical_agency|industry_association|local_language", "query":"...", "url":"...", "outcome":"..."}],
 "sources":[{"source_id":"SE_gdi2025","title":"...","publisher":"...","url":"...","publication_date":"2025-12-08 or null","retrieved_date":"2026-09-09","source_type":"government|industry_association|research_partner|secondary","local_file":"raw/SE/gdi2025.pdf or null","locator":"p18, methodology pp60-61","access_status":"full_source|public_summary|secondary_only|blocked","notes":"..."}],
 "series":[{
 "series_id":"SE_domestic","label":"Domestic studio full-time positions","measure":"employment_stock|contractor_share|nationality_share|contractor_count|nationality_count|other",
 "unit":"people|FTE|annual_average_full_time_positions|jobs|ILU|percent",
 "industry_scope":"...", "occupation_scope":"...", "geography_basis":"domestic_workplace|domestic_residence|company_worldwide|mixed|unknown",
 "geography_note":"...", "worker_status":"Employees/contractors/founders etc", "contractor_treatment":"...", "nationality_definition":"...",
 "method":"...", "population_basis":"national_estimate|administrative_population|survey_sample|company_panel|unknown",
 "sample_size":"...", "comparability_group":"...",
 "chart_eligible":true,"trend_eligible":true,"index_2019_eligible":false,
 "comparison_note":"Why eligible or excluded; breaks and adjustments",
 "source_native_term":"...","term_translation":"..."
 }],
 "observations":[{
 "observation_id":"SE_domestic_2024_gdi2025","series_id":"SE_domestic","source_id":"SE_gdi2025",
 "observation_period":"2024 annual average","year":2024,"value":9130,"value_low":null,"value_high":null,
 "status":"reported|estimated|derived|forecast", "preferred":true,
 "subgroup":"all","denominator":"...","source_locator":"p18 table",
 "publication_vintage":"2025 edition","method_break":false,"break_note":"...","derivation":"...","notes":"..."
 }],
 "limitations":["..."],
 "review_checks":["Arithmetic checked...","PDF table visually checked..."],
 "research_complete":true
}
```

All observations must cite inspected content, not just search snippets. Keep original and revised vintages, mark preferred explicitly. Use separate series IDs for definitions not safely joined. Exact source-reported year null if unresolved (e.g., undated employment estimate in Japan report); record edition separately. For ranges value=null, low/high supplied, no midpoint. Percent denominator must distinguish respondents vs national workforce. No multiplying survey shares into unrelated national totals. Prefer domestic core developer/publisher series; specialist services and broad impact/workforce separately labeled. Foreign citizenship, foreign ownership and staff abroad distinct. Record nationality and contractor percentages whenever verified, even when sample-only and not country comparable. Every country, including usable ones, gets 3 search routes; evidence gap only after all routes checked. Public-summary facts are allowed, mark limited detail. No purchases/outreach. Save primary public snapshots where feasible; minimum: extract plus direct source locator if a large PDF is impractical. Do not copy private correspondence.

Per-country source memo can live in JSON notes; root will render Markdown and CSV. Country-chart source notes use series definition. Defaults for index eligibility: only real 2019 baseline, stable domestic definition through plotted observations, no unresolved methodology break; choose false if unsure. Any later source revised cohort must be documented. Discrete snapshots do not become annual growth rates. regional and national totals must not be mixed.
