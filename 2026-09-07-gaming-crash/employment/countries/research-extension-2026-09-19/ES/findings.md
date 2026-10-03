# Spain archive extension: DEV, AEVI and government cross-check

## Proposed additions

Add two direct-employment estimates to `ES_direct`:

| Year | People | Decisive primary locator |
| --- | ---: | --- |
| 2013 | 2,630 | DEV *Libro Blanco 2023*, PDF p. 31 / printed p. 31, Figura 16 |
| 2014 | 3,376 | DEV *Libro Blanco 2023*, PDF p. 31 / printed p. 31, Figura 16 |

The primary PDF is retained in `raw/DEV_Libro_Blanco_2023.pdf`; its text extract is beside it. Figura 16 is headed *Evolución del empleo en el videojuego español (2013-2022)*. Its 2015-2022 values exactly match the accepted direct series. This makes the two earlier values the strongest recoverable additions, with no interpolation or reverse calculation.

## Direct employment is distinct from adjacent counts

The same printed p. 31 says 2022 had 9,261 direct jobs and separately states that direct jobs plus freelance workers and other indirect jobs totalled 14,108. The 2025 DEV book likewise reports 10,508 direct jobs in 2024 and 2,330 **external freelancers** on PDF p. 35 / printed p. 35. The proposed 2013-14 rows are therefore direct employment only. They do not contain freelancers, indirect jobs, tax-policy model outputs, or forecasts.

The 2023 book's adjacent chart (PDF p. 31 / printed p. 31, Figura 17) forecasts 2023-2026; exclude it. The DEV 2024 book independently reports 10,259 direct jobs in 2023 on PDF p. 29 / printed p. 29, Figura 16, then forecasts 2024-2027. The 2025 book's printed p. 35 records actual 2020-2024, then forecasts 2025-2028. No 2025 actual employment count was recovered.

## Definition and revision check

All additions use the existing `ES_direct` definition: DEV's national estimate of **direct employment in Spanish game development**. The historical figures are from one retrospective chart and agree exactly with its accepted overlap, so they are safe additions to that series. This is still an association estimate, not a government administrative headcount.

Method continuity cannot be proved from the chart. The 2024 edition reports a late-2024 CAWI survey of 292 studies from a DEV-estimated universe of 495, obtained through DEV databases/channels, with a stated ±3.68% finite-population error (PDF p. 89 / printed p. 89). The 2025 edition reports N=283 and universe=500 using the same general channels (PDF p. 86 / printed p. 86). The 2024 report explicitly says participation rose 60% from earlier editions (PDF p. 22 / printed p. 22). None of the inspected editions publishes a historical revision or says that the 2013-2024 values were recalculated to a single fixed sampling frame. Treat this as a series-level comparability caveat, not a documented break.

## AEVI and government are corroboration/context, not replacements

AEVI's 2024 yearbook says the Spanish game industry employed **more than 11,000** people directly (PDF p. 3 / printed p. 3), but provides neither an exact extractable count nor a definition that demonstrates equivalence to DEV's development-sector direct employment. It is broader/rounded context and must not overwrite DEV's 10,508.

The Ministry of Culture's December 2021 release repeats the DEV/Libro Blanco 2019 value of 7,320 direct professionals. Its July 2015 release says employment increased 28% in 2014 but has no absolute count. Both support provenance but do not supply new independently defined rows.

## Forward and backward limit

The latest actual data recovered is DEV's **2024** direct-employment estimate, published in the 2025 book in July 2026. No DEV 2026/2025-reference-year actual report was found. Backward, 2013 is the earliest value in the recovered continuous DEV chart. Recover the original 2014/2015 Libro Blanco PDFs only if a pre-2013 history or contemporaneous-method audit is required.


### Plot-selection review, 19 September

DEV retrospective Figure 16 matches all overlapping 2015-2022 original observations. Extend direct-employment series to 2013; changing annual survey coverage remains a caveat. Exclude forecasts, freelancers and indirect jobs.
