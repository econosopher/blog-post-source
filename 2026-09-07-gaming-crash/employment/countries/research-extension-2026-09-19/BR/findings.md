# Brazil: Abragames and government employment archive extension

Research date: 2026-09-19. Scope: backwards and forwards trace of the public Abragames/government developer-employment archive. This folder is an extension only; it does not alter `country/BR.json`, its accepted observations, or the shared source catalog.

## Result

Two pre-2018 observations are suitable for review, but neither belongs in the existing `BR_developer_people` national-estimate series.

| Reference year | Value | Series | What is counted | Status |
| --- | ---: | --- | --- | --- |
| 2008 | 560 people | `BR_developer_employees_reported_2008` | professionals reported as employed by 42 companies producing game software | proposed, reported company-set employment; method boundary unresolved |
| 2014 | 1,133 people | `BR_developer_people_sample_2014` | 392 partners/founders plus 741 collaborators at 133 valid developer-company respondents | proposed, respondent workforce total |

The 2005 study supplies only an approximately 15-employee-per-company average and is deliberately **not** proposed as a workforce-stock observation.

## Definition and vintage trace

| Source | Publication / fieldwork | Workforce reference | Value and basis | Key definition limit |
| --- | --- | --- | --- | --- |
| Abragames 2005 | Fieldwork 21 Feb-3 Apr 2005; published 1 May | current during fieldwork, not separately labelled | approximately 15 employees per game company | average only; respondent denominator, founders and contractors not established |
| Abragames 2008 | July 2008 | current at publication | 560 professionals employed by 42 game-software companies | report thanks 32 respondents; link to 42-company figure not documented; founders/contractors unclear |
| BNDES/USP I Census | Fieldwork 9-31 Jan 2014; presented July | January 2014, not the 2013 revenue year | 1,133 total at 133 valid respondents: 392 partners + 741 collaborators | respondent sum, not a national extrapolation; freelancers and other chain roles are outside its stated developer focus |
| Ministry of Culture II Census | 2018 | 2018 | existing packet has 2,731 respondent people plus an extrapolated developer total | do not substitute respondent count for estimate |
| Abragames/Brazil Games survey | 2022 | 2022 | existing estimate: 12,441 people | formal/informal firm mapping and extrapolation; changed coverage |
| Abragames/Brazil Games survey | 2023 report, later English upload | 2023 | existing estimate: 13,225 people | formal/informal firm mapping and extrapolation; upload year is not reference year |
| Nordicity white paper | June 2024 | cites 2023 | repeats 13,225 | secondary repeat, not new measurement |

## What can and cannot be charted

- The proposed 2008 and 2014 values can be retained as historical, source-native observations only if their different series IDs and definitions travel with them.
- Do not make a 2008 -> 2014 -> 2018 -> 2022 -> 2023 trend. The 2008 scope is unresolved, the 2014 value is a respondent sum, and 2018 onward separately includes sample and extrapolated national measures.
- Do not compute a national 2005 total by multiplying the 15-person average by the reported 55 active developers. The average is approximate and the report does not establish that the two bases match.
- Do not use 2013 for the 2014 Census workforce value. It is the revenue reference year elsewhere in the report; employment was gathered in the January 2014 fieldwork.
- Do not turn 13,225 into a 2024 observation. The 2024 Nordicity document attributes it to Abragames 2023 and gives no new fieldwork or estimate.

## Archive limit and next source-native check

The live Abragames archive lists the 2023 survey and 2024 policy materials, but no post-2023 full workforce survey. Brazil lacks a verified game-only annual administrative employment series because the relevant IBGE software classification is broader than games. Before any current-year claim, recheck Abragames/Brazil Games for a newly released full survey and read its methodology, reference period and worker-status treatment before extending the national-estimate series.

`primary-evidence.md` contains the original report locators; `source-visits.json` records the audit trail and access status.

## Main-agent acceptance

Both observations accepted after downloading and checking the original PDFs. They remain separate source-native series and are not eligible for an inferred national trend. Originals and text extracts are retained in `raw/`.

## Explicit plot-selection decision

The2008 and2014 records remain source-verified context but are withheld from the representative regional chart because their company-set/respondent coverage differs from the later extrapolated national series. [Review](trend-review.json).


## Forward check: Ministry of Culture 2026

The February 2026 government policy report still refers to Abragames 2023 for national industry data. No fresh national workforce observation was located. Its classification discussion explains the limits of broad software administrative data, but does not verify current implementation of a games-specific CNAE. See [review](minc-2026-followup/README.md) and saved original. No chart addition.


## Administrative classification follow-up

Original IBGE notes confirm games software shares CNAE6203-1/00 with other software. A total for that code cannot represent games employment. CNAE3.0 timetable and new CBO occupation leads do not establish historical counts. See [classification review](classification-followup/README.md).


## Fortaleza regional census

Reviewed the2024 community survey published2026:101 voluntary responses, not employed-worker stock. Income chart denominators checked;61 zero-income overall versus56 in a five-role subset. Earlier2023 statewide mapping has different scope. See [review](fortaleza-followup/README.md).
