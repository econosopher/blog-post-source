# United Kingdom archive extension

This bounded pass produces seven proposed observations and no accepted changes. It found exact, public, source-native values for the existing DCMS jobs series in **2011-2014** and for TIGA's separate FTE and people series at **November 2017**. It also records a separately scoped Ukie/Screen Business **2016 direct-company FTE** benchmark.

| Proposed series | New years | Value(s) | What it measures | Decision boundary |
| --- | ---: | ---: | --- | --- |
| `GB_dcms_jobs` | 2011-14 | 13,000; 15,000; 19,000; 24,000 | SIC-defined computer-games jobs | Government estimates; small samples, and verify against the current 2026 ODS before integration. |
| `GB_tiga_fte` | 2017 | 13,277 | Creative staff in studios, full-time and full-time equivalent | Pair it only with TIGA FTE endpoints. |
| `GB_tiga_people` | 2017 | 15,851 | Total development workforce including contractors | Headcount, not FTE; do not add to the FTE series. |
| `GB_ukie_direct_company_fte_2016` | 2016 | 16,140 | Direct FTE roles at games companies | Separate singleton; it is not Ukie's 47,620 supported FTE total. |

## Decisive evidence

- DCMS's 2016 **Annex C, Table 16** explicitly gives 13,000 (2011), 15,000 (2012), 19,000 (2013), 24,000 (2014), and 20,000 (2015). It identifies SIC 58.21 and 62.01/1 and cautions that variation can partly come from small sample sizes. The downloaded source is `dcms-focus-employment-2016.pdf`; an extract is saved as `dcms-focus-employment-2016.txt`.
- TIGA's 2019 release says that, from November 2017 to November 2018, creative staff rose from 13,277 to almost 14,353 FTE and the total development workforce including contractors rose from 15,851 to 16,532. It separately reports indirect jobs supported, which are excluded from the proposals. The official API response is saved as `tiga-2018-wp-api.json` because the rendered page returned 502 during this pass.
- Ukie's public Think Global, Create Local page describes 16,140 direct FTE roles using 2016 underlying data. Its 47,620 FTE figure is explicitly an industry-supported total, so it is excluded from direct-employment proposals.

## Method and comparability gaps

- The 2016 DCMS release predates the packet's 2026 DCMS ODS. The proposed 2011-14 values need a revision check before acceptance; they should retain the existing `GB_dcms_jobs` uncertainty and non-headline trend treatment.
- The TIGA 2019 page calls its work an extensive survey, but the full report is commercial. The existing packet's TIGA-series definition must govern worker-status and scope; this extension adds only the explicit endpoints.
- The Ukie 16,140 direct-FTE benchmark has no contractor treatment, detailed occupation scope, or repeat observation in the inspected public page. Its original BFI methodology should be retrieved before any broader use.
- Ukie's January 2025 fact sheet says 26,000 people are employed directly, but it does not state the source, reference period, contractor treatment, or methodology. It is logged but deliberately not proposed.

## Forward conclusion

The most recent primary values already in the packet remain DCMS's 2025 annual average and TIGA's September 2025 snapshot, published in 2026. No public, game-only 2026 reference-year employment value was found in this bounded sweep. No paywalled TIGA report values were inferred.

Main review accepted two TIGA observations; see [acceptance](acceptance.md). Other proposals remain unaccepted.


## Explicit within-series review

Approve the two accepted November 2017 TIGA endpoints as additions to their existing source-native series. Do not combine, add, subtract, or switch between GB_tiga_fte and GB_tiga_people. The former prorates freelancer work into FTE; the latter counts contractors as people. Do not append either endpoint to GB_dcms_jobs. DCMS is APS annual-average filled jobs under SIC 58.21 and 62.011, including first and second jobs and self-employment, with a different geography and uncertainty profile. Do not use UK indirect jobs supported (24,274 in 2017) as direct employment, and do not join the endpoints to the Ukie 2016 direct-company FTE or its 47,620 supported-FTE total. Do not treat TIGA November snapshot years as calendar-year annual averages or as a 2019 index baseline.
See trend-review.json for exact source locators and permitted joins.
