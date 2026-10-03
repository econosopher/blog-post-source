# Americas and Asia chart review

Reviewed 32 of 32 assigned PNGs from `chart_manifest.csv`: US, CA, GB, BR, CN, JP, KR, SG, ID, TW and PH. Every image was opened at a legible size through the image viewer (2160 × 1800 source pixels; 1727 × 1440 displayed). This review covers the pre-fix render present on 9 September 2026. Root owns renderer changes and the final rebuild; items below need readback after that rebuild.

The plotted numerical values agree with selected country observations. Missing years remain gaps, unlike scopes remain separate, and revised Canadian 2021 FTEs and Korean 2017 counts follow the preferred observation selection. No title or footer overlaps the plot. Specific label, clipping and qualification issues are listed below.

## Requested fixes

| ID | Affected chart(s) | Finding and exact correction |
|---|---|---|
| Q1 | `gb-composition-02`, `gb-composition-03` | Footer promises orange method-break points, but every point is blue. Color the 2025 observations orange, matching `method_break=true`. |
| Q2 | `gb-composition-01`, `ph-employment-01` | Strict lower bounds are shown as inclusive: ≥4,245 and ≥2,000. Both observations say **more than**, so show **>4,245** and **>2,000**. Indonesia's **at least** ≥2,112 is already correct. |
| Q3 | `gb-employment-03` | CI whiskers cross the labels for 45,000 and 58,000. Offset these labels horizontally from the whiskers. State explicitly that the intervals are **95% confidence intervals**. Ranges themselves are correct: 2024 33,000–57,000 and 2025 46,000–71,000. |
| Q4 | `cn-employment-01` | 2025 is a first-half estimate, currently qualified only in footer. Show **H1 2025 estimate** directly on the x-axis or adjacent to the point. Retain the 2024 annual point separately from the excluded duplicate H1 2024 observation. |
| Q5 | Most composition charts; employment footers before Sources | Add punctuation between metadata clauses. Examples: CA immigration footer has `employer respondentssurvey sample`; JP has `respondents survey sample` without a separator; GB nationality has `denominator Orange point`; BR foreign sample has `Table12 survey sample`; many footers have `...2024 Sources:`. |
| Q6 | All US charts; `kr-employment-01` especially | Source labels repeat identical organization names several times and show `(date not resolved)` even when source editions/years are known in packet metadata. Use source publication year where verified and avoid repeated identical labels. Do not invent a precise date. |
| Q7 | `jp-composition-03`, `jp-composition-04` | Long titles extend to roughly 12 px from the PNG's right edge. Wrap to two lines or shorten within the content margin. Text is presently visible but leaves insufficient margin. |
| Q8 | `jp-composition-02`, `kr-composition-01` | Small-value markers (0.3% agency employees; 86 agency/dispatched workers) are partly clipped at the left plot boundary. Keep the full circle visible, for example with `clip_on=False` on the relevant scatter artist. |
| Q9 | `jp-employment-01` | The range is correctly rendered without a midpoint, but its `approximately` qualifier is lost. Label **approximately 58,000 to 83,000**. |
| Q10 | `tw-employment-01` | Survey-sample scope and unresolved year are visible, but the sample denominator is absent. Add **133 responding companies; some missing employee answers imputed from 104 Job Bank** to the footer. |

## Per-chart record

“Pass” below means values, selection, readable placement and scope qualifiers passed this visual review, subject to the explicitly referenced global footer fixes. No renderer or country JSON files were edited in this review.

| Chart ID | Review result |
|---|---|
| `br-employment-01` | Pass. 2018 4,234; 2022 12,441; 2023 13,225. The 2022 method break is orange. Missing years are gaps. |
| `br-composition-01` | Pass; Q5. Outsourced sample counts 999 and 1,307 with changing company/work-regime denominators visible. |
| `br-composition-02` | Pass; Q5. Sample shares 35%, 48.4%, 47%; not presented as national counts. |
| `br-composition-03` | Pass; Q5. Foreign people 7 and 27; employer-response denominators are stated. |
| `ca-employment-01` | Pass; Q5. Eight 2015–2022 administrative ILU points; ILU unit separated from FTEs/headcount. Final 59,689. |
| `ca-employment-02` | Pass; Q5. Legacy 21,700 and 27,700; superseded original 2021 estimate excluded. |
| `ca-employment-03` | Pass; Q5. Revised 2021 35,250 and 2024 34,010 displayed together; revision note visible. |
| `ca-composition-01` | Pass; Q5. 2017/2021/2024 contract shares 16%/18%/13%, sample interpretation retained. |
| `ca-composition-02` | Pass; Q5. 2015 temporary foreign workers 13%, permanent residents 12%, naturalized citizens 15%; employer sample of 46 visible; distinct statuses retained. |
| `cn-employment-01` | Q4, Q5. 2021–2024 annual listed-company panel plus 2025 H1 estimate; endpoint 202,800; panel scope is visible. |
| `gb-employment-01` | Pass; Q5. Six TIGA FTE observations from 2016 to 2024, final 25,419; omitted years remain gaps. |
| `gb-employment-02` | Pass; Q5. 2018/2020/2023/2024/2025 workforce counts, final 27,347; contractor-inclusive scope visible. |
| `gb-employment-03` | Q3, Q5. All eleven annual 2015–2025 DCMS estimates and two latest confidence intervals display. Point values and interval endpoints match packet. |
| `gb-composition-01` | Q2, Q5. 1,102; 3,625; more than 4,245. Bound marker visible but strictness needs correction. |
| `gb-composition-02` | Q1, Q5. 3% and 6%; recruitment/sample change note visible; 2025 point must be orange. |
| `gb-composition-03` | Q1, Q5. 2021 nationality shares 71%/20%/9%; 2025 76%/16%/8%; nationality question denominator visible. |
| `id-employment-01` | Pass; Q5. Correct ≥2,112 lower bound, unknown observation year, minimum mapping scope; no national extrapolation. |
| `jp-employment-01` | Q9, Q5. Range endpoints 58,000 and 83,000 correctly shown with no midpoint and no invented observation year. Hardware-included scope visible. |
| `jp-composition-01` | Pass; Q5. Seven 2024 categories match packet; denominator 512; no annual trend claim. |
| `jp-composition-02` | Q8, Q5. Seven 2025 categories match packet; denominator 339; 0.3% agency marker clipped at plot boundary. |
| `jp-composition-03` | Q7, Q5. 2024 freelancer/independent share 4.5%, denominator 512; title needs margin. |
| `jp-composition-04` | Q7, Q5. 2025 freelancer/independent share 2.4%, denominator 339; title needs margin. |
| `kr-employment-01` | Pass; Q5, Q6. Ten annual core observations 2015–2024 with final 54,285; revised 2017 34,666 used; no interpolation. |
| `kr-composition-01` | Q8, Q5, Q6. Original 2017 denominator 34,665 stated; 34,020 regular, 559 nonregular, 86 dispatched; smallest marker clipped. |
| `ph-employment-01` | Q2, Q5. Undated lower-bound snapshot 2,000 with related-services scope; change ≥ to >. |
| `sg-employment-01` | Pass; Q5. 2021 **nearly 2,000**, qualifier retained directly on point; esports context disclosed. |
| `tw-employment-01` | Q10, Q5. 9,929 survey aggregate and unresolved workforce date shown; add 133-company/imputation denominator. |
| `us-employment-01` | Pass; Q5, Q6. Core 61,230/71,991/60,276; discrete points with orange 2023 and 2025 methodology breaks. |
| `us-employment-02` | Pass; Q5, Q6. Broad 143,045/104,080/82,930; orange methodology breaks and explicit warning against like-for-like job-loss inference. |
| `us-employment-03` | Pass; Q5, Q6. Software 75,353/63,248; 2025 method-break point orange and scope caveat visible. |
| `us-employment-04` | Pass; Q5, Q6. Specialist services 3,431/2,409; method-break point orange; no addition to separate contractor counts. |
| `us-employment-05` | Pass; Q5, Q6. Earlier mapping 60,031/65,678; 2016 broader universe is orange; separation from newer series explicit. |

## Review completeness

All 32 assigned chart IDs appear exactly once in the per-chart table. Selection checked against the saved manifest and the current country observation packets. Source re-research was outside this bounded visual review. After fixes, root should re-open the affected PNGs and update final QA status; this document does not certify the future rebuild sight unseen.

## Final rebuild readback

After root's 79-chart / 329-observation rebuild on 9 September 2026, opened these **20 final PNGs** again at legible size: `gb-composition-01`, `gb-composition-02`, `gb-composition-03`, `gb-employment-03`, `cn-employment-01`, `jp-employment-01`, `jp-composition-02`, `jp-composition-03`, `jp-composition-04`, `kr-employment-01`, `kr-composition-01`, `tw-employment-01`, `ph-employment-01`, `us-employment-01`, `us-employment-02`, `us-employment-03`, `us-employment-04`, `us-employment-05`, `ca-composition-02`, `br-composition-03`.

| Issue | Final visually verified result |
|---|---|
| Q1 | Resolved. GB 2025-26 survey observations are orange; labels explicitly identify the cross-year survey. |
| Q2 | Resolved. GB displays >4,245 and PH displays >2,000. |
| Q3 | Resolved. 45,000 and 58,000 labels sit beside their whiskers without intersection; footer identifies 95% confidence intervals. |
| Q4 | Resolved. China's last tick reads H1 2025 estimate on two lines without axis-label collision. |
| Q5 | Resolved in re-opened representative footers across all affected render types. Denominator, population, observation and source clauses have punctuation; no overlap or clipping. |
| Q6 | Resolved. All five US charts and both KR charts show source organizations once with relevant editions/dates. |
| Q7 | Resolved. Both Japanese freelancer-share titles wrap to two lines within the content margin. |
| Q8 | Resolved. Full small-value markers are visible on JP composition 02 and KR composition 01. |
| Q9 | Resolved. Japan's range is labeled ~58,000 to 83,000 with no midpoint. |
| Q10 | Resolved. Taiwan's 133-company sample and imputation from 104 Job Bank are visible, alongside the explicit non-national-total qualification. |

The final Korean core chart also visibly states that the latest survey frame excludes operating firms with no revenue and that historical population-frame continuity is unresolved. The final plotted numerical values remained unchanged in the re-opened charts. No material presentation issues remain from this review. All 32 assigned charts were reviewed initially; the 20 listed above received targeted visual readback after fixes.
