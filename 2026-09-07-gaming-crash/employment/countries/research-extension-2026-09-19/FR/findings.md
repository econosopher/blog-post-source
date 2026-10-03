# France archive extension - 19 September 2026

## Decisive addition: government/PwC broad industry series, 2010-2018

The government-hosted March 2021 PIPAME synthesis, prepared by PwC Strategy&, has a source-native annual chart on **printed p. 7 (PDF p. 7), Figure 3, “Emplois selon le type d’acteur de la filière (en nombre d’emplois, 2010-2018)”**. It supplies an exact annual employment series:

| Year | Persons |
| --- | ---: |
| 2010 | 6,201 |
| 2011 | 6,718 |
| 2012 | 7,178 |
| 2013 | 8,279 |
| 2014 | 8,863 |
| 2015 | 9,465 |
| 2016 | 9,900 |
| 2017 | 11,060 |
| 2018 | 11,936 |

**Exact evidence and definition.** The same printed page says the industry employed about 11,900 people in 2018, including nearly 5,000 directly tied to production and about 3,200 at publishers. The stacked chart labels the components: studios, publishers, distributors, and “other actors.” Its source is “base de données des acteurs (‘données BDD’), analyses PwC Strategy&.” Footnote 5 says the core categories are studios, publishers, and distributors established in France and present in the database. Printed p. 6 explains that the broader ecosystem additionally includes technology suppliers, service providers, and schools.

This is a **national broad-industry persons/employment** series. The report does not present it as FTE and does not state independent-worker treatment for the employment chart. It therefore cannot be joined to the accepted EY direct employee FTE series. It is proposed separately as `FR_dge_pwc_broad_industry_persons`.

Source: [DGE/PIPAME, *Etude sur l’industrie du Jeu Video en France - tissu economique et competitivite* (March 2021)](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf), printed p. 7 / PDF p. 7. Local evidence: `dge-pipame-2021.pdf`, `dge-pages/page-07.txt`, and `dge-page-hi-07.png`.

## Established current series and revision check

The later EY/We Are Creative *Panorama des ICC 2025* is the strongest recoverable current source, published December 2025 and covering 2019-2024. Its games-sector summary is **printed p. 54 (PDF p. 55)**: 6,674 ETP in 2019 and 9,944 ETP in 2024. That page defines the scope as publishers, developers, and technology suppliers; it excludes console/accessory manufacturers, local associations, and esports organisations. Crucially, it says jobs **do not include independent workers**, so total sector ETP is probably greater than shown.

Its **printed p. 56 (PDF p. 57)** gives the rounded annual FTE chart: 6.7k, 9.7k, 10.2k, 11.3k, 10.7k, and 9.9k for 2019-2024. This is consistent with the accepted packet. The methodology is at printed pp. 106-110 (PDF pp. 107-111): 2019-2023 are built by applying EY-selected NAF-code shares to INSEE ESANE and Diane accounts; 2024 is estimated from Diane account growth because ESANE 2024 was not published. Exact method text is printed p. 108 / PDF p. 109.

The two series overlap only in 2019-adjacent period and are definitionally incompatible: PwC counts broad-industry persons including distribution/other ecosystem actors; EY reports direct employee FTE in a different NAF-allocation scope and excludes independent workers. No revision relationship is stated. Do not construct a combined 2010-2024 trend.

## SNJV and SELL/government chain checks

The government study is explicitly a DGE/CNC publication produced with SELL and SNJV collaboration, conducted by PwC from September 2019 to October 2020. Its broad historical chart is database-based, separate from the report’s questionnaire: printed p. 7 gives the actor-database source; PDF footnote 6 says the online questionnaire was sent to nearly 300 studios and publishers in November 2019-February 2020 and received about 100 responses, representing about 40% of estimated studio/publisher employment.

SNJV’s 2018 Barometre is survey evidence only. **Printed p. 20 (PDF p. 10)** reports average studio FTE (22 in 2014, 27.2 in 2015, 30.8 in 2016, 25.7 in 2017, 27.7 at mid-2018) and forecasts 1,200-1,500 new jobs by 2019, including 650-850 in game development. These are respondent averages and a forecast, not national stock observations. The questionnaire ran 5 June-8 August 2018; printed p. 1 / PDF p. 1 states it was a CAWI questionnaire to member and non-member sector firms.

SNJV’s 2021 Barometre confirms the later design stayed survey-based: **printed p. 3 (PDF p. 1)** says it contacted 1,200 verified/qualified companies from 3 May to 30 June 2021 and had a 17% participation rate. Its source list gives 97 respondents for the mean-studio-FTE chart and 98 for contract composition (printed p. 30 / PDF p. 15). It has no national workforce total. No newer SNJV annual stock was recovered in this bounded pass; the accepted 2024 SNJV report remains sample contract-composition evidence only.

## Unresolved limits

- No safely compatible pre-2019 employee-FTE series was recovered. The new PwC sequence is robust for broad persons but remains unsuitable for an FTE trend with EY.
- PwC’s Figure 3 does not specify independent-worker treatment or whether workers are headcount versus an implicit full-year employment stock. Preserve the published unit as persons/employment.
- The 2024 EY estimate is the latest recovered national FTE endpoint; no 2025 or 2026 national employment-stock release was located. SNJV’s later material does not solve this because it reports samples, composition, or forecasts.
- The 2018 SNJV 2019 hiring values are forecasts and are intentionally excluded from `proposed-observations.json`.

## Within-source historical comparison review

The2010-2018 source-native series is now approved as a separate broad-ecosystem historical segment. Main inspected the original chart totals and p6 coverage. This does not approve a join to EY FTE or imply a fixed panel. [Review](trend-review.json). Printed totals remain unchanged, including2012/2016 component discrepancies.
