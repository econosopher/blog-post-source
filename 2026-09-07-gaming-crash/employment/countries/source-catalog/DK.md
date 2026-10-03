# Denmark: source catalog

Original 9 September assessment: Domestic game-production payroll FTE is verified for 2017–2022 in the 2024 edition. Earlier 2015–2017 and 2016–2019 vintages are retained separately because the population was redefined. The June 2026 Tableau workbook was downloaded and decoded: it covers film, TV and advertising only, with no games segment.

Coverage ranges below include contextual and non-comparable series. A year span does not establish annual continuity.

[Latest deeper research](../research-extension-2026-09-19/DK/findings.md)

## Danske Indholdsproducenter 2024

[Producentforeningen / Statistics Denmark](https://gamesdenmark.dk/wp-content/uploads/2024/09/Danske-Indholdsproducenter2024.pdf) · recorded check: 2026-09-09 · access: full_source

- **Employment years recorded:** 2017, 2018, 2019, 2020, 2021, 2022; undated observations: 0.
- **Units:** FTE.
- **Method:** Statistics Denmark registry linked to manually selected producer company population
- **Use / limits:** 2024 edition reconstructs five previous years. Company population revised in 2021; use within-edition history, not naive edition splicing.
- **Series eligible for a within-series trend:** DK_payroll_fte.
- **Archive status:** Original coverage imported; archive extension not yet reviewed in this pass.

## Danske Indholdsproducenter: interactive tables

[Producentforeningen](https://public.tableau.com/workbooks/DanskeIndholdsproducenter2025.twb) · recorded check: 2026-09-09 · access: full_source

- **Employment years recorded:** None dated; undated observations: 0.
- **Units:** No employment series accepted.
- **Method:** See source note; no accepted series
- **Use / limits:** Initial .twbx request 404; correct .twb endpoint returned packaged workbook. Extracts updated 24 June 2026. All three contain only FILM/TV/REKLAME, and Metode says these are the three included industries. Public landing updated 3 July 2026. No games employment can be read from this workbook because that segment is absent, not because access failed.
- **Series eligible for a within-series trend:** None flagged in original packet.
- **Archive status:** Original coverage imported; archive extension not yet reviewed in this pass.

## Danske Indholdsproducenter i tal 2018

[Producentforeningen / Statistics Denmark](https://pro-f.dk/sites/default/files/2021-10/Danske%20Indholdsproducenter%20i%20tal%202018_0_1.pdf) · recorded check: 2026-09-09 · access: full_source

- **Employment years recorded:** 2015, 2016, 2017; undated observations: 0.
- **Units:** FTE.
- **Method:** Statistics Denmark special extraction on selected company CVR numbers
- **Use / limits:** Separate historical vintage. Do not splice with 2021 or 2024 editions: population changed. 2017 old value 1,009 differs from 2024 vintage 737.
- **Series eligible for a within-series trend:** DK_payroll_fte_v2018.
- **Archive status:** Original coverage imported; archive extension not yet reviewed in this pass.

## Danske Indholdsproducenter Marts 2021

[Producentforeningen / HBS Economics / Statistics Denmark](https://producentforeningen.dk/sites/default/files/2021-08/Danske%20Indholdsproducenter%202021_0.pdf) · recorded check: 2026-09-09 · access: full_source

- **Employment years recorded:** 2016, 2017, 2018, 2019; undated observations: 0.
- **Units:** FTE.
- **Method:** Statistics Denmark business register, HBS Economics and manual industry selection; retrospective 2019 cohort
- **Use / limits:** 2016–2019 within-vintage history; not a year-by-year complete historical population. 2021 report expressly warns against edition comparison.
- **Series eligible for a within-series trend:** DK_payroll_fte_v2021.
- **Archive status:** Original coverage imported; archive extension not yet reviewed in this pass.

## Accepted extension observations

These supplement the original snapshot. Different series or revised vintages stay separate.

| Year | Value | Unit | Series | Source |
| --- | ---: | --- | --- | --- |
| 2008 | 486 | FTE | DK_core_game_producers_v2009 | [Original](https://pro-f.dk/sites/default/files/2021-10/Danske%20Indholdsproducenter%202009_1_1.pdf) |
| 2009 | 552 | FTE | DK_core_game_producers_v2009 | [Original](https://pro-f.dk/sites/default/files/2021-10/Danske%20Indholdsproducenter%202009_1_1.pdf) |

[Full metadata](../research-extension-2026-09-19/DK/accepted-observations.json)

## Places visited and search outcomes

- [site.dst.dk spilbranchen årsværk](https://www.dst.dk/) (2026-09-09, statistical_agency): Statistical source found through producer report; payroll FTE definitions inspected.
- [Games Denmark Danske Indholdsproducenter 2024](https://gamesdenmark.dk/) (2026-09-09, industry_association): Full report downloaded and read.
- [Danske Indholdsproducenter 2025 tal grafer beskæftigelse](https://www.producentforeningen.dk/analyse/danske-indholdsproducenter-2024-tal-og-grafer) (2026-09-09, local_language): Newer Tableau release fully downloaded and decoded through public .twb endpoint; no SPIL segment exists. Earlier 2018/2021 reports supply 2015/2016 vintages.
- [DanskeIndholdsproducenter 2025 public workbook download](https://public.tableau.com/workbooks/DanskeIndholdsproducenter2025.twb) (2026-09-09, industry_association): 200 response with 44,152 byte packaged workbook; decoded all threeHyperfiles and verified onlyFILM/TV/REKLAME segments. Older guessed 2024/2023 workbook endpoints 404.
- [Danske Indholdsproducenter - Film, TV og Computerspil i tal, 2009](https://pro-f.dk/sites/default/files/2021-10/Danske%20Indholdsproducenter%202009_1_1.pdf) (2026-09-19, archive extension): 2008-2009 | Product-based mapping of selected core companies, then Statistics Denmark extraction. Core is direct producers and rights holders; self-employed freelancers and support functions are excluded. The report explicitly warns that a reliable retrospective history was not possible at that point. | Accept as the earliest authoritative two-point FTE vintage, kept separate. It cannot establish continuity with later editions because the population is manually curated and later reports redefine it. | full PDF downloaded and text inspected | If an intervening primary edition is found, compare its named-company population and method before bridging 2009 to 2015.
- [Det Interaktive Danmark i tal 2015](https://www.visiondenmark.dk/wp-content/uploads/2019/07/Det_Interaktive_Danmark_i_tal_2015.pdf) (2026-09-19, archive extension): 2009-2015 | Search-result extract describes game and other interactive core producers, FTE conversion and a corrected company-count method. | Promising intervening seven-year series: its search extract reports 735 game FTE in 2014 and 770 in 2015. It is not proposed here because the primary PDF returned HTTP 503 after three retry attempts, so exact chart values and methodology were not independently inspected. | access failure: HTTP 503 from publisher host | Retrieve from the publisher archive or a library copy; preserve as its own vintage until company-population comparability is proven.
- [State of the nation i den danske spilbranche](https://producentforeningen.dk/sites/default/files/2022-09/Baggrundsrapport%20-%20State%20of%20the%20nation%20i%20den%20danske%20spilbranche.pdf) (2026-09-19, archive extension): 2016-2020 employment chart; published September 2022 | Background report reprints a five-year FTE chart and cites 'Danske Indholdsproducenter 2022'; it does not reproduce the underlying company-selection method. | Useful evidence of a distinct 2022 retrospective vintage (739, 817, 877, 903, 979), but it conflicts with both the 2021 and 2024 reconstructions. Keep all vintages; do not use this reprint to splice a continuous series. | full PDF downloaded and text inspected | Locate the full Danske Indholdsproducenter 2022 report, if archived, to establish its population definition.
- [Danske Indholdsproducenter 2025: Stilstand i branchen](https://www.producentforeningen.dk/aktuelt/danske-indholdsproducenter-2025-stilstand-i-branchen) (2026-09-19, archive extension): 2024 reference year; published 2025 and updated 2026 | Publisher says the annual analysis is based on a special Statistics Denmark extraction; release text identifies film and television, and its associated interactive workbook was already decoded in the base packet. | No newer games payroll FTE verified. The base packet's June 2026 workbook inspection found only FILM, TV and REKLAME segments. This newer release therefore cannot extend the game series beyond 2022. | publisher HTML inspected; games-segment absence verified in base packet workbook | Watch for a future Games Denmark or Producentforeningen release that explicitly restores SPIL tables and describes the revised company population.
