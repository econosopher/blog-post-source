# France: source catalog

Original 9 September assessment: EY reports direct employee FTE excluding independent workers: exact endpoints 6,674 in 2019 and 9,944 in 2024. The preferred 2019–2024 annual series uses the consistently rounded employment chart. Scope includes developers, publishers and technology suppliers. SNJV contract shares remain sample-only.

Coverage ranges below include contextual and non-comparable series. A year span does not establish annual continuity.

[Latest deeper research](../research-extension-2026-09-19/FR/findings.md)

## Panorama des Industries Culturelles et Créatives 2025

[EY / we are creative](https://www.artcena.fr/sites/default/files/medias/rapport-panorama-industries-culturelles-creatives-2025.pdf) · recorded check: 2026-09-09 · access: full_source

- **Employment years recorded:** 2019, 2020, 2021, 2022, 2023, 2024; undated observations: 0.
- **Units:** FTE.
- **Method:** INSEE ESANE, company accounts in Diane, URSSAF and EY sector allocation
- **Use / limits:** Domestic direct employee FTE. Games-specificp 54 exclusion of independent workers overrides the generic appendix discussion of non-salaried work. Preferred annual chart retains the consistent roundedp 56 series; exactp 54 endpoints 6674 and 9944 are separately preserved nonpreferred. No splice with older EY editions.
- **Series eligible for a within-series trend:** FR_ey_direct_fte.
- **Archive status:** Original coverage imported; archive extension not yet reviewed in this pass.

## Baromètre annuel du jeu vidéo 2024

[Syndicat National du Jeu Vidéo](https://snjv.org/wp-content/uploads/2025/11/Barometre-du-jeu-video-2024.pdf) · recorded check: 2026-09-09 · access: full_source

- **Employment years recorded:** 2022, 2024; undated observations: 0.
- **Units:** percent.
- **Method:** SNJV annual studio survey
- **Use / limits:** Survey contract composition only; never multiplied by EY national FTE.
- **Series eligible for a within-series trend:** None flagged in original packet.
- **Archive status:** Original coverage imported; archive extension not yet reviewed in this pass.

## Accepted extension observations

These supplement the original snapshot. Different series or revised vintages stay separate.

| Year | Value | Unit | Series | Source |
| --- | ---: | --- | --- | --- |
| 2010 | 6,201 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |
| 2011 | 6,718 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |
| 2012 | 7,178 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |
| 2013 | 8,279 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |
| 2014 | 8,863 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |
| 2015 | 9,465 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |
| 2016 | 9,900 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |
| 2017 | 11,060 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |
| 2018 | 11,936 | persons | FR_dge_pwc_broad_industry_persons | [Original](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) |

[Full metadata](../research-extension-2026-09-19/FR/accepted-observations.json)

## Places visited and search outcomes

- [site.insee.fr emploi jeux vidéo 2024](https://www.insee.fr/) (2026-09-09, statistical_agency): No clean complete national developer census found; EY report uses official statistical sources.
- [SNJV baromètre annuel jeu vidéo 2024 emploi](https://snjv.org/) (2026-09-09, industry_association): Full report inspected; survey contract mix found, no validated national workforce stock therein.
- [site.cnc.fr jeu vidéo emplois 2024](https://www.artcena.fr/sites/default/files/medias/rapport-panorama-industries-culturelles-creatives-2025.pdf) (2026-09-09, local_language): French-language search found EY 2025 national FTE series and methodology.
- [Etude sur l'industrie du Jeu Video en France - tissu economique et competitivite](https://www.entreprises.gouv.fr/files/files/Publications/2021/Dossiers-dge/industrie-du-jeu-video-tissu-economique-et-competitivite-mars-2021.pdf) (2026-09-19, archive extension): 2010-2018 | PwC Strategy& actor database. Figure 3 breaks employment into studios, publishers, distributors, and other ecosystem-supporting actors; the report separately identifies an online questionnaire used for some studio/editor indicators. | Primary government-hosted synthesis with a source-native annual chart. Strong addition for pre-2019 broad industry employment, but it measures persons rather than FTE and includes distributors and other actors. Keep as a separate series from EY. | full PDF downloaded and visually verified | Integration may add as a distinct non-spliced series after schema review; do not use it as a continuation of FR_ey_direct_fte.
- [Panorama des Industries Culturelles et Creatives 2025](https://www.artcena.fr/sites/default/files/medias/rapport-panorama-industries-culturelles-creatives-2025.pdf) (2026-09-19, archive extension): 2019-2024 | EY applies sector shares to INSEE ESANE and Diane company accounts for 2019-2023; 2024 uses Diane-derived 2023-2024 growth rates. Games scope is publishers, developers, and technology suppliers. | Confirmed current endpoint. Printed p. 54 says 9,944 ETP in 2024 and explicitly excludes independent workers; printed p. 56 gives the 2019-2024 rounded annual FTE series. Not revised beyond 2024. | full PDF downloaded and text/visual inspection complete | Retain existing EY series unchanged; do not splice with PwC persons series.
- [Barometre annuel du jeu video en France, edition 2021](https://snjv.org/wp-content/uploads/2021/09/Barometre-SNJV-2021.pdf) (2026-09-19, archive extension): 2014-2021 contextual respondent averages; 2021 survey | Online questionnaire 3 May-30 June 2021, 1,200 verified/qualified companies contacted, 17% participation. Employment charts report survey respondent studio averages and contract composition; source table names 97-98 respondents for employment charts. | Not a national employment stock. It validates that later SNJV barometers remain survey-based and should not be multiplied into a national total or used to bridge EY/PwC. | full PDF downloaded and inspected | No proposed national-employment observations.
- [Barometre annuel du jeu video en France, 2018](https://snjv.org/wp-content/uploads/2018/12/Barometre_JV_2018.pdf) (2026-09-19, archive extension): 2014-2018 contextual respondent averages; 2019 forecast | CAWI questionnaire, 5 June-8 August 2018, sent to SNJV members and non-members. Employment page reports mean studio FTE and a forecast of new jobs, not a national stock. | Not usable for national employment history. Printed p. 20 reports 2018 mid-year mean studio FTE and an estimate of 1,200-1,500 future jobs by 2019; the latter is a forecast, not an observation. | full PDF downloaded and inspected | No proposed national-employment observations; retain only sample metrics if needed.
