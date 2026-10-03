# Gemini Deep Research experiment: verification

## Result

Completed with deep-research-preview-04-2026. Google's current documentation checked at https://ai.google.dev/gemini-api/docs/deep-research lists this standard agent and deep-research-max-preview-04-2026. Max is a separate comprehensiveness tier, not a newer release. No new employment observation accepted; chart unchanged.

Gemini's main numerical finding, CESA's 58,000-83,000 core-enterprise range, was already recorded in the country findings. Its observation table leaves every government employment count missing. It did not extract the annual METI Information and Communications Industry Basic Survey development/production workforce series already partially recovered by this project. It supplies leads, not an additional time series.

## Source metadata experiment

- API usage reports 42 Google Search queries.
- Final response has 101 URL citation annotations, representing 33 distinct Google grounding redirect URLs.
- Annotation keys are type, url, start_index, end_index. No title, page, table, canonical original URL, or access-status fields were present in those annotations.
- The explicitly requested source register supplies Japanese titles, publisher names, reference locators, some excerpts and 15 distinct non-redirect URLs in the report body. These are model-authored leads, not verified metadata.
- The JSON block supplies six source records but omits URLs and actual numerical observation rows; the prompt's requested complete machine-readable evidence contract was not met.
- No unprompted control run was performed. This demonstrates extra information in the prompted report body relative to API annotations, not a causal measurement of prompt improvement.
- sources/japan-employment.json retains annotation URLs. sources/explicit-report-urls.json retains model-written direct URLs. sources/resolved-citations.json records attempts to resolve annotation redirects. Resolved 29 of 33 redirect URLs; four returned HTTP errors. Their numbering is independent of the model's inline citation numbers; join by URL, never index.

## Decisive source checks

1. Gemini labels its CESA-25 direct URL as a CESA2025 press summary, but https://prtimes.jp/main/html/rd/p/000000097.000037875.html is the Digital Content Association of Japan's release for Digital Content White Paper 2026, published 1 September 2026. Incorrect source mapping; reject this locator for the CESA range.
2. Gemini proposes 2024 Economic Structure Survey table02003 under tstat000001231885 as a JSIC3914 employment lead. The official catalog labels this the MANUFACTURING establishment survey and table02003 specifically covers manufacturing establishments with 1-29 workers. It is not the claimed game-software workforce table. Verified at https://www.e-stat.go.jp/stat-search/database?collect_area=000&layout=dataset&page=1&result_page=1&toukei=00200555&tstat=000001231885 .
3. The report prose reverses dispatch-worker treatment in its cited JILPT definition. Section701 at https://www.jil.go.jp/kokunai/statistics/yougo/d05.html includes employees dispatched outward, and excludes inward-dispatched workers not paid by the establishment. Gemini's own source-register translation partially contradicts its prose. Do not transfer this definition claim into the dataset.
4. Claims that a significant part of the 2012-2016 increase is a reference-month artifact are not supported by a quantified decomposition. The different dates are known; the claimed impact is not established.
5. Calling the CESA range a confidence interval, or the most accurate development/publishing count, is unsupported here. Its hardware-inclusive scope and unresolved reference period remain material. Revenue scale does not independently validate workforce estimates.
6. Gemini identifies no actual 2021 JSIC3914 worker row. A catalog landing page and a service table title do not establish that row exists. The prior coverage gap remains unresolved.

## Assessment and next useful prompt

This broad run did not improve numerical coverage and made source-mapping errors despite explicit provenance instructions. Retain it as an audited experiment, not evidence for chart changes. A future bounded follow-up should request only METI Information and Communications Industry Basic Survey game-software form5/table7 originals, with exact workbook download URLs, response counts, field5301 definitions and source-native year values. It should return an access failure rather than substitute Census or CESA context. No second paid run was launched in this experiment.
