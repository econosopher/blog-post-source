# Japan employment-source extension — 2026-09-19

## Finding

**A dated, public and partially longitudinal Japanese employment series exists, but it is the government `3914 Game software industry` establishment classification, not the CESA Game Industry Report.** The recovered national observations are 246 establishments and 9,452 persons engaged in 2012, and 256 establishments and 18,216 persons engaged in 2016. Both are detailed-class industry tabulations with the required-item and management/support exclusions documented below. The 2021 Economic Census public tables inspected do not supply a `3914` worker row; do not turn the available broader classifications into a substitute value.

The classification is narrower than Japanese game employment in the ordinary sense: it covers establishments creating game software and related research, analysis and advice for console, portable and PC systems. It excludes game-disc/cartridge manufacturing, and it does not promise that a diversified publisher's or hardware maker's whole workforce is allocated to 3914. Treat it as a domestic establishment-class workforce, not total Japanese game-company headcount and not a developer-only occupation series.

## CESA release trace — five most recent annual editions available by 2026-09-19

| Edition | Publication date | What public release establishes | Workforce result |
|---|---:|---|---|
| Game Industry Report 2025 | 2025-12-15 | Full report is paid (462 pp); public release gives a newly articulated ecosystem estimate. | Core companies: ~58,000–83,000; broad ecosystem: ~200,000. Undated estimate; core expressly includes home-console hardware makers. Context only. |
| Game Industry Report 2024 | 2024-12-20 | First renamed/rebuilt report after the White Paper. Public release says the total domestic game-related industrial population is around 200,000. | No dated observation, detailed denominator, or public reproducible method. Do not treat as a 2024 stock. |
| CESA Games White Paper 2023 | 2023-07-31 | Paid annual report (233 pp) described as a report on the **home-console game industry**; public release foregrounds shipment and market data. | No public workforce total recovered. Exclude. |
| CESA Games White Paper 2022 | 2022-08-29 | Paid annual report (249 pp), likewise explicitly a **home-console game-industry** annual report. | No public workforce total recovered. Exclude. |
| CESA Games White Paper 2021 | issued 2022-04-25 after delay | Paid annual white paper; CESA's 2022 business report confirms the delayed issue. | No public workforce total recovered. Exclude. |

CESA itself says the report series was comprehensively renewed in 2024 after the White Paper was published through 2023. That supports a method break at the 2024 report, even if a paid older edition happens to contain employment material. It does **not** support backdating the 2024/25 ~200,000 estimate.

## Government routes

### Economic Census — recommended next route

- **2012 (survey date: 2012):** 9,452 workers, 246 establishments, national `3914` class. Exact value and locator recovered from the official national result PDF.
- **2016 (reference date: 2016-06-01):** **18,216 persons engaged**, 256 establishments, national `3914`. Official e-Stat Table 1, Service Industries B by detailed class, row `3914 ゲームソフトウェア業`. The accompanying official results note says industry tabulations exclude management/support-activity establishments and establishments without values for industry-specific items. This is the same restriction wording used in the accepted 2012 establishment tabulation.
- **2021 (reference date: 2021-06-01):** no exact `3914` worker value recovered from the public final-result tables. The 2021 results catalogue identifies the cross-industry worker table as industry **middle-classification** (not four-digit detailed class). The 2021 service-industry table explicitly excludes `G Information and Communications`, so its worker counts cannot stand in for game software. Treat 2021 as unresolved rather than substituting `391 Software industry`, a game-centre series, enterprise sales data, or a total-information-industry worker count.

This creates a two-point 2012/2016 census series. Do not interpolate annual values or claim a 2021 continuation without a published detailed-class worker row.

### METI Information and Communications Industry Basic Survey — promising but a different series

METI's annual survey did publish a `ゲームソフトウェア企業` / game-software-enterprise category and worker-status tables, including regular workers, regular employees, part-time workers, contracted workers (including freelancers), temporary daily workers and received dispatch workers. For example, its FY2010 results say game-software companies increased 20.8% in average regular workforce; the FY2020-results release says game-software-industry sales rose 16.7% year-on-year. These are enterprise survey estimates classified by each firm's largest-sales activity (main-industry basis), **not** census establishment totals. The source documents demonstrate annual longitudinal potential but this pass did not extract, reconcile and verify a common annual total across the releases, so no METI values are proposed for integration.

## Exclusions

- **CESA 2024/2025 ~200,000 and 58,000–83,000:** source-native contextual estimates only. Publication year is not the employment reference year; 2025's core range includes console hardware.
- **CEDEC developer surveys:** voluntary internet respondent demographics, not workforce stocks. 2024 has n=512 (fielded 2024-07-01 to 2024-09-02) and 2025 has n=339 (2025-06-02 to 2025-08-04); both primarily target commercial developers but admit educators and students. Their full-time/freelance shares must not be multiplied by national counts.
- **CESA 2008 economic-impact report's 63,863:** modelled *employment inducement* from roughly JPY1tn of direct and indirect production effects, not people employed by the game-software industry. The report itself warns production can be met through overtime, equipment or productivity without extra headcount.

## Next concrete work

1. Locate an official 2021 detailed-class `3914` worker tabulation, if one exists outside the inspected final-result catalogue; require the same exclusions and a row-level source locator before adding it.
2. Pull the annual METI Information and Communications Industry Basic Survey releases (2009–2021 releases / FY2008–2020 results), extracting the same `常時従業者数` field and annual survey universe before deciding whether their category is stable enough for a separate series.
3. Keep Census and METI series separate. Neither should be joined to CESA's ecosystem estimate or CEDEC respondent shares.


## Follow-up source checks

Kinki regional plan data appendix, October 2024, printed p12: Reject as2021 game-employment evidence. Downloaded full PDF and checked chart-specific footnotes. Confirms continued official use of older2016 games detail, not a new year.


## METI enterprise follow-up

Four division-workforce totals extracted with response counts, retained as proposed: FY2010 6,675 (46 firms), FY2017 14,094 (73), FY2018 13,343 (74), FY2020 candidate 14,519 (70). These cover development/production divisions of capital-threshold survey respondents, not whole companies or a national workforce. Fiscal dates, changing response composition and preliminary2010 vintage require further reconciliation. Main independently read the2021 workbook Table7 total/denominator and preserved its component-total mismatch. See [full evidence packet](meti-enterprise-followup/findings.md).


## Explicit within-series review

Approve a separate two-point, source-native Economic Census series for JSIC 3914 Game software industry establishment persons engaged. Do not join to JP_core_range or JP_broad. CESA's public estimates are undated, include console hardware in the core range, and broaden further to peripherals, distribution, retail, amusement, toys and media. Do not join to JP_status_2024, JP_status_2025, JP_freelancers_2024, or JP_freelancers_2025. CEDEC values are voluntary respondent shares, not national workforce stocks. Do not fill unobserved annual or post-2016 gaps, substitute a 2021 middle-classification/software/service table, or portray the 2016 value as a 2021 count. Do not join to METI Information and Communications Industry Basic Survey values. That enterprise survey is main-industry based and has a distinct enterprise universe and worker-status treatment; it is outside this review.
See trend-review.json for exact source locators and permitted joins.

## Later workforce-field review

FY2020 end is now established by the official questionnaire field5301. Accepted14,519 people at70 responding enterprises as a development/production survey snapshot only. Earlier pending-date statements are superseded. [Field review](meti-enterprise-followup/field-date-review.md) records the access limits and employee-category revision warning. No national trend approved.

## METI historical definition follow-up

[Definition review](meti-enterprise-followup/definition-review.json) finds matching FY2017/FY2018/FY2020 table totals, with changing respondents. The2018 published warning concerns a worker subcategory; it does not by itself demonstrate a break in the total. Common post2018 total definition remains a supported inference pending direct earlier industry-form5 readback. FY2010 uses a different employment-duration eligibility rule. No national trend or additional numeric acceptance approved in this pass.


## Direct 2018 questionnaire5 recovery

Accepted FY2017 total14,094 at73 responding game-software enterprises after directly checking table cells and form5 field5301. This is a respondent snapshot, withheld from the national chart. See meti-enterprise-followup/form5-followup.md. FY2018 field-page review remains incomplete.


## Adjacent 2020 survey table recovery

Verified12050 regular development/production workers at68 responding game-software enterprises from official2020-5 workbook. CandidateFY2019 stock, exact field-date pending. Preserve15-person difference between total and displayed categories without attribution. No acceptance or national trend. See [evidence](meti-2020-followup/README.md).2019 form5/guide403 persists;2020 instrument also not recovered.

## 20 September archive recheck

[Detailed recheck findings](recheck-2026-09-20/notes/findings.md). CESA and CEDEC have recurring publications, but the checked career-survey samples are not national employment totals. No additional comparable dated workforce count was recovered in this pass.

## Gemini Deep Research experiment

One run of current standard deep-research-preview-04-2026 completed. No new dated employment observations recovered or accepted. Explicit source-register prompting returned additional metadata, but verification caught incorrect document and table mappings. See [verification and sourcing audit](gemini-deep-research-2026-09-20/verification.md), prompt (excluded model research artifact), and unverified Gemini report (excluded model research artifact).
