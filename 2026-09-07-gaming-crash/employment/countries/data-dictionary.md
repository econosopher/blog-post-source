# Data dictionary

This package records published employment evidence available by 9 September 2026. It covers observations from 2015 onward, plus undated estimates whose source does not establish an employment year. It does not estimate missing years or a world total.

## Tables and joins

| File | One row represents | Join / key |
| --- | --- | --- |
| observations.csv | One value, interval or one-sided bound from one source vintage | observation_id; links through series_id and source_id |
| source_definitions.csv | One distinct measurement definition | series_id |
| citations.csv | One inspected source within a country packet | source_id |
| comparison_eligibility.csv | A series-level assessment of chart and comparison use | series_id |
| country_coverage.csv | Coverage and evidence limits for one country | iso2 |
| search_log.csv | A statistical-agency, association or local-language source check | iso2 + route + query |
| indexed_2019.csv | An eligible native observation divided by its verified 2019 baseline | observation_id + series_id |
| plotted_values.csv | One mark in one exported chart | chart_id + observation_id |
| chart_manifest.csv | One PNG/SVG pair | chart_id |

CSV files use UTF-8 with a byte-order mark for spreadsheet compatibility. Empty cells mean unknown or not applicable; they never mean zero. Booleans are True/False. Lists and dictionaries are JSON text when stored in a CSV cell. The country JSON files preserve the complete extraction, search record and review notes.

## Observation fields

| Field | Meaning |
| --- | --- |
| iso2, country | Country label. It does not establish the geographical perimeter of the observation. |
| observation_id | Stable extraction identifier linking a plotted mark to evidence. |
| series_id | Definition governing the observation, including industry, geography and worker status. |
| source_id | Citation for this value and vintage. |
| observation_period | Source-specific fiscal year, survey period, point-in-time date, annual average or explicit undated label. This controls interpretation. |
| year | Sorting/plotting year of the observation. Fiscal-year end is used where applicable. Blank if the source does not establish an employment year; report edition is not substituted. |
| value | Reported central/point value. Blank for a range without a central estimate. |
| value_low, value_high | Interval endpoints or a one-sided bound. A value plus two bounds may be a published confidence interval. Interval type and strictness follow the source notes. No midpoint is created. |
| value_qualifier | Optional qualifier such as approximately, nearly, more_than, at_least, up_to or almost, retained in point, range and bound labels. Spaces and underscores are accepted. Strict bounds display as > or <. Source notes remain authoritative. |
| display_period | Optional concise chart label, such as H1 2025 estimate or 2025-26 survey. It never replaces the full observation_period or changes year. |
| status | reported: explicit source value; estimated: source/model/survey population estimate; derived: arithmetic stated in derivation; forecast: expected future outcome. This is an assessment of the value, separate from publisher type. |
| preferred | Selected vintage within this definition. False retains an older revision, competing unresolved figure or contextual alternative. True does not mean internationally comparable. |
| subgroup | All covered workers or a named category, such as freelancers, non-citizens or staff abroad. |
| denominator | Exact population or sample to which a count/share refers, including known response counts. Percentages must not be multiplied into a different workforce total. |
| source_locator | Page, table, chart or paragraph where the value was inspected. Printed page and PDF page are distinguished when they differ. |
| publication_vintage | Report edition/source vintage. This is independent of the employment year. |
| method_break, break_note | Flag and explanation for a scope, unit, sample-frame, timing or methodology discontinuity. No trend line crosses a flagged point. |
| derivation | Transparent arithmetic for a derived value; no undocumented population adjustments. |
| notes | Qualifiers, conflicts, revisions, sample details and unresolved exclusions specific to this observation. |

## Definition fields

| Field | Meaning |
| --- | --- |
| label, source_native_term, term_translation | English series label, original measurement wording and explanation. |
| measure | Employment stock, contractor share/count, nationality or migration share/count, visa share/count, or another explicitly described measure. Visa observations use dedicated measure types. nationality_definition states the actual citizenship, origin or permit concept for every demographic series. |
| unit | Source-native unit. People, jobs, FTE, annual-average full-time positions and Canadian ILUs are different measures. Some sources use a mixed full-time-employment measure including FTEs and contractors. |
| industry_scope | Development, publishing, services and wider activities covered or excluded. Unknown exclusions stay unknown. |
| occupation_scope | Whether all company roles or only specified production/technical occupations are included. |
| geography_basis, geography_note | Domestic workplaces, residents, companies worldwide, mixed or unknown. Detail includes remote staff, overseas subsidiaries and local foreign-owned firms where verified. |
| worker_status, contractor_treatment | Treatment of payroll staff, contractors, freelancers, founders, agency staff, apprentices and interns where known. Missing status details do not imply exclusion. |
| nationality_definition | Citizenship, foreign origin, work permit, migration status or another explicitly reported concept. Foreign ownership and employment abroad do not establish nationality. |
| method, population_basis | Administrative population, national estimate, survey sample, company panel or unknown, with the source's collection/expansion method. A government publisher does not automatically make the value a census. |
| sample_size | Known respondents or covered establishments; observation-level notes may provide changing yearly counts. |
| comparability_group | Local definition group, not proof that all countries sharing a unit are comparable. |
| chart_eligible | Can be shown honestly as a source-native series/snapshot with its limitations. This includes clearly labelled samples, ranges and some context estimates. |
| trend_eligible | A line may connect consecutive observed years within the series, except at a declared break. Sparse points are never filled. |
| index_2019_eligible | Research assessment supports a 2019 index. The renderer additionally checks an actual positive 2019 value, at least three years, domestic geography and no unresolved break in the indexed period. |
| comparison_note | Reason for acceptance or exclusion, including remaining unit, scope and source limits. |

## Citation fields

source_id identifies the citation. title and publisher identify the authored source, including associations and research partners. url is the direct inspected public source; a public mirror is stated where used. publication_date is the verified full date, otherwise blank with known month/year in notes. retrieved_date records this research's access date. source_type distinguishes government, industry association, research partner and secondary publication. local_file points to the original snapshot or verified extract where available. locator identifies the relevant section. access_status distinguishes full source, public summary, secondary-only evidence and blocked access. notes explains publication-date ambiguity, mirrors and extraction limits.

## Comparison rules

The comparison target is people working domestically in game development and publishing, irrespective of citizenship or employer ownership. Source-native series are retained even when they do not meet that target. Specialist services, venues, hardware, gambling and economic-impact employment are kept separate where possible.

The index is `native value / verified 2019 value * 100`. It measures each source's relative change and does not harmonize country coverage. No contractor total is added to an employee total unless a source explicitly establishes non-overlap. No headcount is converted to FTE without a documented source method. Stock changes are not measured hiring, layoffs or migration.

The gallery excludes forecasts and non-preferred vintages. Unplotted context remains in the CSVs and country notes. An evidence gap means this search did not establish a usable estimate; it does not claim that no estimate exists anywhere.
