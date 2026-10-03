# Denmark: archival and forward check

The extension adds an earlier authoritative producer-study vintage: **486 FTE in 2008 and 552 FTE in 2009**. The 2010 *Danske Indholdsproducenter* report counts employees at selected core computer-game producers, converts them to FTE, and identifies 72 developers in 2009. It uses a product-based company selection plus Statistics Denmark data. It excludes self-employed freelancers and support firms.

The report explicitly says reliable historical reconstruction was not feasible because the selected core-company population differs materially from broad Statistics Denmark industry-code pulls. That warning is borne out by later editions. A 2022 producer background report reprints **739, 817, 877, 903 and 979 FTE for 2016-2020**, yet those overlap with and differ from the 2021 vintage (706, 774, 822, 847 for 2016-2019) and the 2024 vintage (737, 802, 833 for 2017-2019). The 2022 report does not document its underlying population, so its rows are proposed only as contextual observations.

No single revised historical series supports 2008-2022 continuity. Preserve the 2008-2009 source as `DK_core_game_producers_v2009`; retain existing 2015-2017, 2016-2019, 2017-2022 and the proposed 2016-2020 rows as separate report vintages. Do not bridge their growth rates.

Forward check through 19 September 2026: no authoritative post-2022 games payroll FTE was verified. Producentforeningen's 2025/2026 material is newer, but the base packet decoded its Tableau workbook and found only film, TV and advertising. The newer release cannot be treated as evidence that games FTE fell, held steady or ceased to be measured.

The best intervening lead is *Det Interaktive Danmark i tal 2015*, whose publisher-search extract reports 735 game FTE in 2014 and 770 in 2015 and describes a 2009-2015 history. Its primary PDF currently returns HTTP 503, so this pass does not promote those values.

## Bounded within-vintage comparison review

Approved only the accepted years as a separate source-native comparison segment after main read the original source table and methods. Source-native2008/2009 core game-producer payroll FTE pair. Selected company population excludes freelancers/support firms. No joins to later report vintages. [Review](trend-review.json).
