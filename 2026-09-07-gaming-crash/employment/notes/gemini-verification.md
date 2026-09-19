# Gemini Deep Research verification

Three runs used `deep-research-preview-04-2026`, checked against Google's supported versions on 20 September 2026. Prompts required primary sources, full URLs, report/table locators, reference dates, definitions and access limitations. Outputs were treated as discovery leads.

- **Turkey:** Gemini led to EGDF's newer report but inferred approximately 15000 from a Poland comparison. Original-table inspection recovered 14918 for 2024. Its 9300 Istanbul figure was incorrectly labelled exact; the report gives 9370. Speculative origins for the 25000 claim were rejected.
- **United States:** No additional employment years recovered. Verification corrected Gemini's 2012 label for the 2011 count 42527. The report's 2.02 employment multiplier means additional jobs per direct job, not total/direct jobs. Unsupported explanations of compensation changes were excluded.
- **Japan:** No new dated counts. A supposed CESA source link led to another organization's report, and a proposed game-employment table covered manufacturing establishments. Both source mappings were rejected.

The Japan API response supplied 101 annotations covering 33 distinct redirect URLs, with text positions but no titles or page fields. Prompting for a source register added 15 direct URLs and descriptive locators in the report body, but these model-written details required verification. No control run measured the causal effect of the prompt.

Raw API payloads, session metadata and unreviewed generated reports are not published here. Full prompts, outputs and detailed local audits are retained in the working archive; importing their safe portions is pending drive access.
