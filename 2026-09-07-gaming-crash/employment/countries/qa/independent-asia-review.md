# Independent review: Asian country packets

Reviewed 2026-09-09 by the US/Canada/UK/Brazil researcher. Scope: KR, JP, CN, VN, IN, ID and SG JSON packets against saved primary PDFs, source extracts and table renders. Bounded review of consequential units, geography, dates, bounds and index eligibility. No other owner's country files were edited.

No consequential numeric transcription error was found in the reviewed observations. No reviewed series should be added to the 2019 index without further evidence; all seven packets currently withhold index eligibility appropriately.

## Output requirements to preserve

1. **China: keep the half-year period visible.** `CN_listed_panel` includes annual 2021-2024 values and a preferred H1 2025 estimate (202,800). The primary chart on PDF p8 labels the last point `2025年上半年E`, and H1 2024 is separately 203,400. The JSON retains these periods correctly. A renderer that uses only `year` would make H1 2025 look like a full-year observation. Label it “H1 2025 estimate” wherever shown and retain the company-panel / geography-unknown label. Do not infer domestic job losses from this panel.
2. **Singapore: preserve “nearly” in visible values.** The parliamentary answer says nearly 2,000 people were employed in the games sector in 2021. `value=2000` is an approximate display anchor, not exact stock. Show “nearly 2,000,” not an unqualified “2,000.” The same rule applies to the nearly-10,000 mobile-games interview claim in Vietnam and approximate 200,000 broader Japanese estimate.
3. **Korea: keep the population exclusion beside a national employment chart.** The 2025 White Paper p9 excludes operating companies with no revenue in the specified year from its sample population. This is already in the country limitation and index is withheld. The population rule should be visible in a chart note; do not describe the series as an exhaustive census of every person making games. The reviewed evidence does not establish when this rule began, so a new dated method break should not be invented.

These are presentation requirements, not requests to change the verified source values.

## Verified cases

| Country | Verification | Result |
|---|---|---|
| South Korea | Visually checked `raw/KR/kocca2025-en-p9.png` against JSON. Core subtotal is 45,262 / 48,514 / 51,783 / 54,285 for 2021-2024, separate from cafes/arcade venues. Checked vintage flags across older observations. | Correct people units and development/publishing scope; exactly one preferred core observation per year 2015-2024. Original 34,665 for 2017 remains nonpreferred, revised 34,666 preferred. |
| Japan | CESA release p3 explicitly describes domestic employment, includes console hardware in its core-company definition, and gives 5.8万-8.3万人 plus about 20万人 broadly. CEDEC 2025 p22 visual confirms n=339 and freelancer/independent developer 2.4%. | Range 58,000-83,000 correctly converted with no midpoint or invented observation year. Wider 200,000 kept separate; demographic sample is not national workforce. |
| China | CNG primary PDF p8 uses 万人 and names major listed game companies; figures 22.30/21.53/20.65/20.58/20.34/20.28. | ×10,000 conversions correct; first-half and estimated status retained; company panel correctly excluded from national core/index. |
| Vietnam | Saved source extracts preserve the same Vietnam Briefing page's contradictory 4,100 and 35,000 claims. Government article repeats the 4,100 figure via that source. | Correctly not treated as independent corroboration or annual trend. The nearly 10,000 interview estimate is mobile-specific and undated. |
| India | Primus source text says 1.3 lakh professionals across over 1,900 gaming companies. EY and Primus observation timing/scope remain unclear. | 130,000 conversion correct; no synthetic jump from 66,000 to 130,000 or forecast-to-observation conversion. |
| Indonesia | Public author-posted official-book abstract saved with access limitation; direct figure “at least 2,112”; separate affected-workforce range 513,000-1,030,000. | Lower bound and broad range retained; neither mapped automatically to 2024 nor reverse-calculated to 2015. Source-access limitation is explicit. |
| Singapore | Read full saved government parliamentary answer dated 2 August 2022. It explicitly states 2021 employment and explains 2,700 is expected manpower demand, not a target or achieved count. | Correct 2021 snapshot and rejection of 2,700. Wider games/esports boundary and absent contractor/nationality detail remain explicit. |

## Remaining uncertainty

This review did not re-audit every historical survey frame, obtain commercial reports, or convert public summaries into stronger evidence. It also did not independently re-download the blocked Indonesian book. Existing evidence restrictions should remain in the final source ledger and chart notes.
