# Games-industry employment research

A source-backed research section for the gaming-industry employment comparison. Original checkpoint: **20 September 2026**; archive imported **3 October 2026**.

The research archive is included here and can be rebuilt from a normal Git checkout. It contains the **31-country source catalog, 329 original observations, 98 original sources, 84 reviewed additions, 79 country charts, the current regional overview and the Finland historical companion**. Source reviews retain their original dates; archive import and offline rebuild verified **3 October 2026**.

## Contents

- [Country research and 79-chart library](countries/README.md).
- [31-country source catalog](countries/source-catalog/README.md), [accepted additions](countries/source-catalog/accepted-additions.csv) and [research findings](countries/research-extension-2026-09-19/README.md).
- [Current regional chart and exact plotted data](countries/overview-2026-09-19/README.md).
- [Finland historical companion](countries/overview-2026-09-19/finland-survey-history.png).
- [Reproduction instructions](REPRODUCE.md) and [import verification](IMPORT-STATUS.md).

- [Verified observation checkpoint](data/verified-checkpoint.csv): country, reference year, value, unit, separate series ID and source URL.
- [Source register](data/sources.json): original publication URLs, report locators, publishers and scope notes.
- [Country findings](notes/country-findings.md): Turkey, United States, Japan and Finland; verified findings and unresolved gaps.
- [Gemini verification](notes/gemini-verification.md): what the research runs found and which claims failed checking.

## Interpretation

Rows are source-native observations, not one harmonized worldwide series. Units, geographic coverage and employer universes vary. Several U.S. rows are overlapping measures of the same workforce; **do not sum them**. Publication dates are not automatically workforce dates. Missing data are not zero.

The latest chart specification uses regional facets with independent logarithmic y-axis ranges, bubbles scaled by reported employment, and at least three dated points per country. Connections identify countries; they do not establish comparability across every method change. The chart and editable SVG are included in the regional overview.

The newly verified Turkey observation is **14,918 for 2024**, attributed to TOGED in the [EGDF/VGE report, p 58](https://www.egdf.eu/wp-content/uploads/2026/08/VGE_EGDF-report2024_0821.pdf). Its questionnaire (p 78) is FTE-oriented and assigns remote workers abroad to the employer-registration country. This is not a strict domestic-workplace count.

## Publication boundary

This repository includes authored research notes, numeric observations, source URLs, reproduction code and generated charts. Third-party reports remain linked at their publishers; downloaded PDFs, page captures, model payloads and local session files are excluded. Historical `local_file` metadata identifies excluded source captures and is not needed to rebuild the charts. Publishers retain rights in their source publications.
