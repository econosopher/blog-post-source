# United States research extension, 19 September 2026

This is an additions-only research packet. It does not alter `countries/country/US.json`, charts, or the existing raw-source directory.

## Concrete additions

The strongest longer historical evidence is not a continuous annual government series. It is three discrete ESA/Siwek estimation families:

| Series | 2009 | 2011 | 2012 | 2013 | 2014 | 2015 | 2016 | Interpretation |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| Legacy direct employment, all establishments | 33,140 |  | 42,975 |  |  |  |  | ESA/Siwek estimate; the source explicitly includes workers at establishments with fewer than five employees. |
| Legacy direct employment, establishments with 5+ workers | 31,598 | 42,527 |  |  |  |  |  | Same legacy method before the small-establishment adjustment. |
| ESA Mapping Project reported employment |  |  |  | 56,712 | 58,963 | 60,031 | 65,678 | Reported ESA mapping data. The existing packet already contains 2015 and 2016; this extension recovers 2013 and 2014. |

The 2014 report gives exact values in two tables. Its all-worker Table D-2 labels 33,140 as 2009 and 42,975 as 2012. Its thresholded Table D-1 labels 31,598 as 2009 and 42,527 as 2011. These tables must remain separate because the latter excludes establishments with fewer than five employees. The report's displayed 2012 components, 42,527 plus a 449 small-establishment adjustment, sum to 42,976; retain the source-reported 42,975 and flag this one-person inconsistency.

The existing 2017 ESA report has a previously unused Table E-2: 56,712 (2013), 58,963 (2014), and 60,031 (2015). The rendered original confirms both values and the footnote that 1,176 locations did not report employment and 151 did not report founding year. The text says the 2013 to 2015 observations are ESA Mapping Project data and reports 2.88% CAGR. This supports adding the two missing rows to the existing `US_mapping_2017` series. Treat 2016 as a reporting-universe break: the existing 65,678 C-2 total uses a wider reporting universe than E-1/E-2.

## Why these are not one continuity line

The legacy 2014 report says U.S. government sources do not separately report the U.S. entertainment-software publishing industry. It estimates games employment by applying broader software-publishing employment ratios to locations taken from gamedevmap.com, with a small-establishment adjustment. The 2017 report uses ESA Mapping Project data. Neither is a fixed company panel. They should be retained as source-native snapshots, with `trend_eligible: false` and no 2019 index.

## Government series check

BLS CES publishes a long-run national **Software publishers** employment series (currently CES industry 50-513200 / NAICS 5132, beginning in 1990). It is not a game-only code. BLS itself notes CES industry codes are NAICS-based and may aggregate more than one NAICS industry. The CES series therefore cannot be represented as U.S. game employment.

The 2014 ESA report documents the same boundary: it says government statistics generally do not separately report software game publishing and uses broader software-publishing data only as an input to its estimate. Census product-line data can identify video-game software-publishing receipts in certain Economic Census vintages, but it does not yield a corresponding game-only annual employment stock. No government game-only employment time series was found.

## Older-report boundary

The 2010 ESA report is a primary report whose executive summary says the industry directly employed **more than 32,000** people in 2009. The later 2014 original provides the exact revised/paired 2009 figure of 33,140 used here. The 2007 ESA study is contemporaneously described by its author's firm as directly employing **over 24,000** people in 2006. Those lower-bound statements are useful history but are not added as numeric observations because they are not exact source-native totals and the complete 2007 report could not be retrieved from a current public host.

## Verification

- Downloaded and text-extracted the complete 2014 ESA/Siwek PDF.
- Visually checked Table D-2 at `raw/rendered/esa2014-16.png`: 33,140 (2009), 42,975 (2012).
- Read the authoritative project's 2017 ESA PDF and visually checked Table E-1/E-2 at `raw/rendered/esa2017-19.png`: 56,712 (2013), 58,963 (2014), 60,031 (2015).
- Read BLS CES published-series documentation directly; rejected the broad software-publisher code for game-only employment.


## Bounded within-source comparison review

The main reviewer re-read ESA2014 TableD2 and ESA2017 TablesE1/E2. These tables explicitly present historical comparisons, so the earlier blanket trend prohibition is refined: permit source-native modeled2009/2012 comparison and the source-reported2013/2014/2015 mapping segment, with coverage/method caveats. This does not validate a continuous national series. The2016 mapping observation has a wider reporting universe and must remain disconnected. Machine-readable allowed years and prohibited joins are in trend-review.json.

## Gemini Deep Research experiment

Completed current standard Deep Research run. No new employment reference years recovered. Original-source check corrected Gemini’s2012label forTableD1’s2011count42,527. [Verification notes](gemini-deep-research-2026-09-20/verification.md) and unverified report (excluded model research artifact).
