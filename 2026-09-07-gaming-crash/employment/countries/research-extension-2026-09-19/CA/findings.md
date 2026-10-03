# Canada archive extension

Two accepted additions to the existing administrative ILU series: **2013: 27,612; 2014: 28,616**. They extend the already recorded 2015–2022 observations. [Statistics Canada, Table 3](https://www150.statcan.gc.ca/n1/pub/36-28-0001/2025008/article/00001-eng.htm).

ILUs allocate a worker across employers using wages, without directly measuring hours. They are neither FTEs nor snapshot headcounts. Coverage excludes territories. The games-specific classification starts in 2012, limiting backward extension. Table 3 increases from 58,279 in 2021 to 59,689 in 2022; adjacent prose incorrectly says employment fell.

A [2026 AI study](https://www150.statcan.gc.ca/n1/pub/36-28-0001/2026003/article/00003-eng.htm) was checked as a forward lead. Its selected linked sample and broader-sector employment discussion do not extend this series. No 2023 onward game-only administrative total was verified in this pass.

ESAC remains a separate survey-based FTE series. A failed resources-page access is logged, not treated as missing evidence. Accepted rows: `accepted-observations.json`; visited routes and assessments: `source-visits.json`.


## Explicit within-series review

Prepend the 2013 and 2014 observations to the existing 2015-2022 CA_statcan_ilu annual series. Do not merge the ILUs with CA_esac_legacy, CA_esac_revised, or CA_esac_2015_undated as one employment trend or index. Do not convert, relabel, or compare the ILU totals as FTEs or point-in-time headcount. Do not combine the values with CA_contract_share or CA_immigration_share to estimate national contractor or citizenship counts.
Source narrative says employment fell2021 to2022, while Table3 Total rises58279 to59689. Retain explicit table values; do not repeat narrative decline.
Full review: trend-review.json.


## New quarterly source family

Recovered57 current-vintage quarterly interactive-media jobs observations,2012Q1-2026Q1, from Statistics Canada table36-10-0652-01. Scope is games and related digital edutainment, product perspective; jobs are not ILUs/FTE/unique people. Official note identifies2016Q1 statistical break, so no connecting line crosses it. See [quarterly companion](quarterly-culture-followup/README.md). Current SEPH metadata lacks separate game-industry codes and cannot extend ILUs.


## Annual culture-account followup, 19 September 2026

Recovered 15 annual interactive-media jobs observations, 2010-2024, from table 36-10-0452-01 (vector v120591204). All 13 overlapping years reconcile to the existing quarterly companion means within 0.25 jobs. Same linked estimation family, not independent evidence. Keep separate 2010-2015 and 2016-2024 segments. See [annual companion](annual-culture-followup/README.md). Reviewed CPA/LFS/SEPH benchmarking; game-specific allocation ratios remain unresolved.
