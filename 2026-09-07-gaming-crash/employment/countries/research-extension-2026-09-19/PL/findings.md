# Poland: employment archive extension

This bounded pass adds the missing primary **2020** workforce point: **9,710 people** working in game production. The original PARP/GIC report calls this a "tighter estimate" of **full-time employees**, while its infographic uses "people working in game production." The proposal keeps the existing `PL_core_workforce` identifier and unit `people`; it must not be relabelled FTE because the report does not give a conversion rule.

The report also restates **4,000 (2016)** and **6,000 (2018)** as historical estimates, then says they were slightly underestimated. They are proposed only in a separate `PL_pre2020_historical_estimates` vintage. Their original sources, sampling and exact scope were not recovered, so they cannot be joined to the 2020+ series or used for a growth rate.

## Scope and method

The 2020 report says its 9,710 covers studios and companies doing game production and global publishing. The industry frame includes external development, but excludes local-market distribution and services not strictly related to game production. It discusses QA and localisation providers separately, explaining that their employment was not consistently trackable. The report attributes the sector-size research to Game Industry Conference but does not state a sample size or questionnaire design in the inspected employment section.

The 2021 report independently identifies 2020's 9,710 as the first tight estimate reflected in statistical sources. It confirms the production scope includes development, external development and publishers, and excludes service-only localisation/player support. This supports 2020 as the earliest defensible point in the existing estimated domestic production-workforce series.

## Exact evidence

- 2020: printed p. 8 gives 9,710 people working in game production; printed pp. 28-29 calls the figure 9,710 full-time employees and gives the scope and historical-estimate caveat.
- 2021: printed p. 8 says no earlier data had been reflected in statistical sources before the 2020 report's 9,710 tight estimate.
- 2023 and 2025 remain the base packet's source-native current snapshots. The latest is 14,568 in 2025. The 2025 source says its studio-discovery/counting approach changed; it also allows employment data corrections as coverage improves.

`raw/GIofP_2020.pdf`, its text extraction and `raw/GIofP_2020_decisive_extract.txt` preserve the primary evidence. `proposed-observations.json` is additions only; no accepted or shared files were edited.

## Continuity and forward limit

No annual report was found for 2022 or 2024. Indie Games Polska's archive explicitly says the 2023 edition does not repeat exactly the 2020-2021 research scope. These are discrete report snapshots, not an annual census; do not interpolate the missing years.

No newer, source-native 2026 reference-year game-production employment observation was located by 19 September 2026. The existing 2025 point remains the forward endpoint pending a later PARP/GIC report.


### Plot-selection review, 19 September

2020 production/global-publishing full-time employee estimate. Changed scope/discovery in later editions prevents a join; people are not converted to FTE.


## KPT historical-source follow-up
Recovered 2017 and 2020 editions and inspected 2015 indexed methodology. These are distinct firm/wage surveys, with explicit contract and reference-date differences; no national workforce additions. See kpt-archive-followup/README.md.


## GIC2022 primary recovery
Recovered publisher-distributed magazine original:2022=14,200 people, now accepted marker-only. Article describes a15-month interval and confirms pre2020 estimates were not measured counts. See gic-2022-followup/README.md.
