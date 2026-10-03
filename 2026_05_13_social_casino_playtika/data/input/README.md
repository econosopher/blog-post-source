# Subgenre Market CSV Drop-In

Drop the Sensor Tower market-level subgenre revenue CSV here when exported from
the website.

Preferred columns:

- `date`: first day of the month, e.g. `2026-04-01`
- `total_subgenre_revenue_usd`: aggregate social casino subgenre revenue in USD

The prepared MONOPOLY GO! exclusion file is:

- `../monopoly_go_monthly_revenue_for_subgenre_merge.csv`

Join by `date` and compute:

```text
revenue_without_monopoly_go_usd =
  total_subgenre_revenue_usd - monopoly_go_revenue_usd
```
