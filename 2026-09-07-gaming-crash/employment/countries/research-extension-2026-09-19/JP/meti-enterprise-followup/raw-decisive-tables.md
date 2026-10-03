# Decisive table readback (do not reconstruct totals)

| fiscal year | edition/table | exact table heading | exact column | exact row | direct total | respondent companies |
| --- | --- | --- | --- | --- | ---: | ---: |
| FY2010 | H23 rapid release, Chapter 5 | `開発・制作部門に係る従業者数` | `ゲームソフトウェア企業` | `22年度` | `常時従業者数 6,675` | `回答企業数 46` |
| FY2017 | H30-5, Table 7 | `〔開発・制作部門の状況〕 第７表 業種別、従業者の状況` | `ゲームソフトウェア企業` | `常時従業者数（臨時雇用者を除く）` | `14,094` | `回答企業数 73` |
| FY2018 | 2019-5, Table 7 | `〔開発・制作部門の状況〕 第７表 業種別、従業者の状況` | `ゲームソフトウェア企業` | `常時従業者数` | `13,343` | `回答企業数 74` |
| FY2020 | 2021-5, 7表 | `〔開発・制作部門の状況〕 第７表 業種別、従業者の状況` | `ゲームソフトウェア企業` | `常時従業者数 / 計` | `14,519` | `回答企業数 70` |

FY2020 component rows must not be summed: the direct total is 14,519, whereas the
displayed components (`正社員・正職員`, `パート・アルバイトなど`, `他企業等への出向者`,
`契約社員`) add to 14,511.

Raw source files remain alongside this readback: `h23sokugaikyo.pdf`, `H30-5.xls`,
`2019-5.xls`, and `2021-5.xls`.
