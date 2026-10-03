# Ukraine: KVED 58.21 statistical frame

## Decision

**Keep the six 2013–2018 observations as source-native, labelled annual published counts of persons employed in business entities classified to KVED 58.21, “Publishing of computer games”.** They can appear as six contextual markers, but must carry a **territorial break between 2013 and 2014**. The source is not an FTE series, a whole-games-industry measure, or a measure with an established within-year reference date. Do not label it an annual average or an end-of-year stock.

The four 2021–2024 values in the later Ministry/UCCR report should remain a separate proposed series. They must not be joined to the 2013–2018 line: the report calls them “workers” but gives no definition, reporting reference date, de-duplication rule, or source/method note for the table, and it explicitly says wartime non-reporting prevents complete/objective statistics for 2022–2023 and H1 2024.

## Recovered underlying State Statistics workbook

The original Ministry compilation identifies its source for this indicator in its methodology page (printed page i, footnote 6):

`http://www.ukrstat.gov.ua/operativ/operativ2018/fin/pssg/pssg_u/kzpsg_ek_2010_2018_u.xlsx`

The same workbook was recovered from the official URL on 2026-09-19 using HTTPS with certificate validation disabled, because the server certificate is expired. It is a 381 KiB Excel workbook, worksheet `Лист5`, headed **“Кількість зайнятих працівників у суб’єктів господарювання за видами економічної діяльності у 2010-2018 роках”** (“Number of persons employed in business entities by type of economic activity in 2010–2018”). Its columns are: KVED-2010, year, total persons employed including banks, of whom individual entrepreneurs (FOPs), then the corresponding columns excluding banks.

Workbook locators:

| Locator | Evidence |
|---|---|
| `Лист5!B6325:G6333` | KVED 58.21 and the nine annual rows 2010–2018. The recovered source gives total/FOP values for the six accepted years: 2013 887/502; 2014 841/622; 2015 708/481; 2016 897/558; 2017 953/646; 2018 1,072/769. The “including banks” and “excluding banks” totals are identical for this class. |
| `Лист5!A8729` | Footnote: data exclude budget institutions and, **for 2014–2018**, temporarily occupied Crimea, Sevastopol, and parts of Donetsk and Luhansk oblasts. |
| `Лист5!A1:G6` | Title and table structure, including the two bank-treatment columns. |

The Ministry’s original PDF independently gives the same source URL, defines the output as “number of persons employed in business entities”, names legal entities and FOPs as the entities in scope, and defines persons employed as regular, non-staff, and unpaid workers (owners/founders and family members). Locators: printed page i / PDF page 1, sections “1. Економічні показники”, “Пояснення”, and footnote 6; KVED 58.21 is listed as “Видання комп’ютерних ігор” in the IT sector list on printed page viii / PDF page 8.

## What the recovered evidence establishes

### Measure and classification

- This is a **business-entity headcount** in persons, classified by the entity’s KVED 2010 principal activity. It includes legal entities and FOPs; the field itself includes regular, non-staff, and unpaid owners/founders/family workers. It is therefore not an employee-only or FTE count.
- The 2020 State Statistics publication’s methodological explanations confirm the institutional approach: all activity of an entity is assigned to the activity defined as its principal activity; the principal activity is the one generating the largest share of value added. The publication also explicitly identifies legal entities and FOPs as business entities and repeats the employed-person definition. Locators: pp. 145–146, especially pp. 145–146 definitions and p. 146 “Кількість зайнятих працівників”.
- The State Statistics methodological provisions classify this output as **annual** and publish it nationally and by region at KVED-class level. They say business-entity values are formed by arithmetic summation of enterprise and FOP values. Locators: pp. 16 and 20–22 of the 2021 provisions.

### Time convention

The official evidence establishes an **annual reporting product**, not the observation rule for the headcount itself. No recovered source says that `кількість зайнятих працівників` is an annual mean, a year-end stock, a year-start stock, or a cumulative persons-ever-employed count. The separate “average number of workers” wording in size classifications and in the methodology’s quality checks cannot be substituted for the definition of this indicator.

Use: “annual published count of persons employed” / “annual business-entity headcount (timing not specified)”. Do not use: “average annual employment”, “employment at year-end”, or “FTE employment”.

### Geography and continuity

- **2013:** the recovered workbook footnote does not exclude territories for this year. It should be treated as pre-occupation territorial coverage.
- **2014–2018:** the same explicit footnote excludes occupied Crimea, Sevastopol, and parts of Donetsk and Luhansk. These five years share the stated exclusion and may be displayed consecutively as an annual contextual segment, subject to the unresolved time convention.
- This is a material territorial comparability break at 2013/2014. It is not defensible to present 2013→2014 as an unqualified national employment change.

## Later Ministry/UCCR table: exclusion rationale

The later report is [`Creative industries: key indicators 2023–2024`](https://mincult.gov.ua/wp-content/uploads/2025/11/kreatyvni2025-1.pdf). Its section “Працівники у креативних індустріях” begins at printed page 35. The KVED 58.21 row on printed page 39 reports 1,015 (2021), 1,601 (2022), 1,236 (2023), and 1,025 (2024).

The report does not specify whether those workers are employees, employed persons, tax-record workers, a point-in-time count, an average, or a sum across reporting records. It gives no table-specific source or matching definition tying it to the historical State Statistics workbook. Its printed pp. 6–7 state that wartime law allowed entities to omit statistical/financial reports and that this prevents complete and objective data for 2022–2023 and H1 2024; observed movement may reflect reporting delay rather than real activity.

Thus retain it only as a labelled, wartime-affected proposed table. Do not interpolate 2019–2020, calculate a 2018→2021 growth rate, or join/smooth it with the recovered series.

## Exact primary-source URLs

1. Historical underlying workbook (recovered; server TLS certificate expired):
   `https://www.ukrstat.gov.ua/operativ/operativ2018/fin/pssg/pssg_u/kzpsg_ek_2010_2018_u.xlsx`
2. Historical Ministry compilation, provenance and definitions:
   `https://drive.google.com/file/d/1XVeC3ZrkRMCHzzHzOZMtGWdNWHfMONjp/view`
3. State Statistics methodological provisions for the annual business-entity observation (Order No. 250, 2021):
   `https://www.ukrstat.gov.ua/norm_doc/2021/250/250.pdf`
4. State Statistics activity-of-business-entities publication, definitions and territorial note for 2016–2020:
   `https://www.ukrstat.gov.ua/druk/publicat/kat_u/2021/zb/11/zb_DSG_20.pdf`
5. Later Ministry/UCCR report, proposed 2021–2024 workers table:
   `https://mincult.gov.ua/wp-content/uploads/2025/11/kreatyvni2025-1.pdf`
