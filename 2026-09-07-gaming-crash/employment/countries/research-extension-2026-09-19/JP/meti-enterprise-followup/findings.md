# Japan game-software enterprise employment: bounded source findings

## Proposed result for review

This is an enterprise-survey series, not the Economic Census establishment series.
The appropriate table is **Information Services Industry, Table 7: Employment by
industry** (`業種別、従業者の状況`), under `〔開発・制作部門の状況〕`.
For the `ゲームソフトウェア企業` column it reports:

| Fiscal year | Respondent companies (`回答企業数`) | Regular employees (`常時従業者数`) |
| --- | ---: | ---: |
| FY2010 | 46 | 6,675 |
| FY2017 | 73 | 14,094 |
| FY2018 | 74 | 13,343 |
| FY2020 (proposed pending direct field-date text) | 70 | 14,519 |

`常時従業者数` is the direct published table total, in people. Do not reconstruct
it from components: in FY2020 the displayed component rows do **not** sum to the
published total (14,511 versus 14,519). Do not derive it from sales, or multiply a
company count by an average.

## Meaning and comparability

This is **not all employees of all Japanese game companies**, and it is not an
establishment count. It is the headcount in the **development/production division**
of responding enterprises classified as `ゲームソフトウェア企業` in the Information and
Communications Industry Basic Survey. The denominator is the number of respondents
to the employment item, which differs from the general company count in the survey.

The FY2010 rapid release explicitly says all results are sums of valid responses by
item; response-company counts can differ across chapters/items. It also says the
FY2010 release was preliminary (returns received through end-October 2011) and a
final release would follow. Therefore FY2010 is proposed only with `preliminary`
status. Later e-Stat workbooks are the published tables, but respondent counts still
change across years (46, 73, 74, 70), so this is a survey-result trend, not a census
series.

Definition (FY2010 source): paid directors and regular employees, irrespective of
job title, with employment exceeding one month or employed 18+ days in each of the
two months before fiscal year end/nearest accounting period. Temporary/daily workers
are excluded. Later questionnaires describe paid directors and employees without a
fixed term or employed for at least one month; minor wording changes should be
retained as a comparability note.

## Reference dates and editions

* FY2010: FY2010 fiscal-year-end employee figure. Retain the fiscal-year-end label;
  no calendar date is assigned in the observation. The source calls the release
  `速報` (rapid/preliminary).
* FY2017: H30 (2018) questionnaire explicitly asks for development/production
  division employees at **FY2017 end**.
* FY2018: 2019 questionnaire asks for employees at **FY2018 end**.
* FY2020: the 2021 workbook's surrounding tables expressly refer to `2020年度`, and
  the workbook is the FY2020 edition. The direct employee-field reference-period
  wording has not yet been retrieved, so preserve **FY2020 end (proposed)** rather
  than assigning a calendar date or substituting the survey/publication year.

The attempted direct FY2020 questionnaire URL returned a 5,045-byte HTML error page,
not a PDF; it is retained as `2021-questionnaire-5.failed-response.html` and not used
as source evidence.

## Source locators

* **FY2020:** e-Stat `2021-5 第5章 情報サービス業`, `7表`, column
  `ゲームソフトウェア企業`, rows `回答企業数` and `常時従業者数 / 計`.
  Downloaded raw workbook: `2021-5.xls`; converted review copy:
  `converted/2021-5.xlsx`, sheet `7表`.
* **FY2018:** e-Stat `2019-5 第5章 情報サービス業`, `第7表 業種別、従業者の状況`,
  column M (`ゲームソフトウェア企業`), rows 222-223 in the converted workbook.
* **FY2017:** e-Stat `H30-5 第5章 情報サービス業`, same Table 7, column M,
  rows 222-223 in the converted workbook.
* **FY2010:** METI/Soumu `平成23年情報通信業基本調査速報`, Chapter 5,
  `開発・制作部門に係る従業者数`; the game-software rows list FY21 and FY22;
  use FY22 (= FY2010) values 46 and 6,675. The extracted PDF text is `2011.txt`,
  lines around 3214-3238.

## Guardrails for parent review

* Do not mix these values with the supplied Census establishment values 9,452
  (2012) and 18,216 (2016).
* If a single "Japan game industry employment" series requires universe coverage,
  label this one `survey responding game-software enterprises, development/production
  division` and retain the respondent-company series alongside it.
* The information-services survey scope is enterprises with an information-services
  establishment under JSIC division 39 and **capital or investment of JPY30 million
  or more**. This excludes smaller-capital firms.
* No estimate is proposed for missing fiscal years.

## Later review

The FY2020 field-date question is resolved and that observation accepted as a respondent snapshot. See [field-date-review.md](field-date-review.md). Other years remain proposed.
