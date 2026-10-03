# Independent review of CY, AU, IL, NZ and TR packets

Reviewed 9 September 2026 by the Asia extraction agent. This review used the saved original PDFs/HTML and the current country JSONs. It did not change root-owned country files. Numerical PDF charts for Cyprus, Turkey and Israel were also rendered and inspected; the renders are saved alongside this memo.

## Actionable corrections

1. **NZ: visa metrics are classified as nationality metrics.** `NZ_visa_current` and `NZ_visa_ever` have `measure="nationality_share"`; `NZ_visa_counts` has `measure="nationality_count"`. The observations are about work visas, including former visa holders in the 2020 series. The prose correctly distinguishes these concepts, but the machine-readable measure contradicts it and could place the observations in a foreign-nationality chart. Change these to `other`, or use explicit visa measure types if the schema/renderer supports them. Keep the current-versus-current-or-former split and all existing denominator notes. Evidence: NZGDA 2018 and 2021–2023 skills paragraphs; 2020 highlights explicitly combine current and previous visa holders. Saved files: `raw/NZ/survey2018.html`, `survey2020.html`, `survey2021.html`, `survey2022.html`, `survey2023.html`.

2. **IL: carry approximation into chart labels.** The two employee counts are already `status="estimated"` and their notes correctly call them approximate. Add `value_qualifier="approximately"` to both, using the same rendering convention as other approximate snapshots. The source uses approximately 200 companies and approximately 14,000 workers on p22; it presents 4,000 and 14,000 as rounded benchmark values. This is a presentation correction, not a change to either number.

3. **NZ 2019: avoid asserting a narrower visa category than the source.** The 2019 source says 14% are “on work visas”; it does not expressly specify employer-supported visas in that sentence. Add a note preserving this exact scope, or label the combined series “Staff on work visas” and retain the more specific work-supported wording in each observation. No change to the reported 14% is needed.

## Verified cases

### Australia

- The 2024 IGEA historical table on p6 gives FY2016 842; FY2017 928; FY2019 1,275; FY2020 1,245; FY2021 1,327; FY2022 2,104; FY2023 2,458; FY2024 2,465. Packet values match. The missing FY2018 remains missing.
- The 2025 infographic p1 gives 2,443 and explicitly includes full-time equivalents and contractors. The packet's source-native unit treatment is suitably cautious.
- The FY2025 FAQ p2 explicitly adds third-party employment estimates for the same nonresponding studios whose revenue was added. This is an employment-method break, not merely a revenue-method break. Keeping FY2025 in a separate series and disabling an annual decline calculation is correct.
- The FY2025 FAQ and infographic agree that data cover 1 July 2024–30 June 2025 and were collected October 2025–January 2026. The packet does not mistake release date for the employment year.
- ABS saved table gives 734 in 2015–16 and 2,225 in 2021–22. Its glossary includes working proprietors/partners and payroll contract workers. These are separate headcount observations and should remain separate from IGEA. No contractor/FTE conversion is warranted.

### New Zealand

- The unit distinctions are supported by original pages: 2015 and 2018 use fulltime/full-time wording; 2016 gives FTE; 2017 says professional game developers; 2019 says creative and hi-tech workers; 2021 and 2023 use FTE; 2022 says full-time employees. Later releases sometimes retroactively describe earlier values differently. The packet correctly preserves different source-native series and disables the 2019 index.
- The 2025 release explicitly reports 1,097 FTE in 2024 and 1,418 in 2025. These values are directly stated, not back-solved from the growth percentage.
- The 2024 source does contain both 97 and 98 visa-supported staff; its detailed 98 statement is as of May 2024. Keeping both nonpreferred is appropriate.
- The 2024 “Overseas Staff” section gives 31 FTE in Australia and 85 elsewhere overseas. The headline total's relationship to those numbers is not explicitly reconciled. The packet correctly does not automatically subtract them.
- The 2020 15%/111 statement includes former visa holders; separating it from the current-visa series is necessary and already done.

### Turkey

- The EGDF p57 chart has one employment observation: 10,777 in 2023. The 2019–2022 employment bars are N/A. The nearby 3,300 is 2023 turnover in million euros, not an earlier employee count. The packet is correct.
- EGDF pp74–76 support the stated developer/publisher scope. Page76 requests FTE of employees, entrepreneurs and in-house freelancers, includes direct remote workers in third countries, and excludes employees of foreign subsidiaries and foreign subcontractor companies.
- The packet correctly marks geography mixed and disables the national index/trend. Turkey's own implementation of the questionnaire is not independently documented, and the existing caveat appropriately preserves that uncertainty.

### Israel

- P22 gives 4,000 in 2017 and 14,000 in 2021. The packet's observation years match the chart.
- P29 covers publishing, supporting technologies/services and platforms. P32 uses companies including 888 in its industry calculations. These are not a clean video-game-only core workforce.
- The PDF does not establish a domestic-only employee reconciliation. Keeping geography unknown, showing only clearly labelled contextual snapshots, and disabling trend/index are defensible.
- No derived 73%-of-14,000 adjustment should be introduced: publishing share alone would not resolve overseas staff or gambling scope. The packet already avoids that.

### Cyprus

- Visually verified p15: 1,438 (2019), 1,631 (2020), 2,166 (2021), 3,105 (2022), 4,057 (2023), 4,320 (2024). All match the JSON.
- P15 explicitly defines the employment figures as people employed and paying taxes in Cyprus. The packet correctly uses domestic workplace geography rather than ownership or citizenship.
- P67 excludes iGaming from the intended industry scope. P68 and p70 describe the CYSTAT J58/J62 cross-check and the additional head-office code problem. These limitations are recorded in the packet.
- A within-source 2019 index is supportable from this one-vintage, explicitly domestic series; no contradictory break was found in the inspected source. Keep it labelled as the report's mapped/tax-employment series. Index eligibility should not imply a proven exhaustive census of every developer, complete contractor coverage, or identical scope to every other country.

## Review boundary

This was a bounded independent verification of the consequential scope, date, unit and comparability decisions. It did not independently obtain each association's raw respondent microdata or resolve omissions that the public sources themselves leave open.
