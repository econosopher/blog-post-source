# 2025_08_05_top_rpgs Folder Rules

This folder contains analysis of top RPG games using the sensortowerR package.

## Strict Rules for This Folder

### 1. Single Script Rule
- **ONLY ONE R SCRIPT** is allowed in this folder
- The script must be named `top_rpgs_analysis.R`
- No helper scripts, no test scripts, no alternate versions

### 2. Single Output Rule
- **ONLY ONE IMAGE OUTPUT** is allowed in the output folder
- The image must be named `top_rpgs_analysis_api.png`
- GT tables must be saved as PNG format (not JPEG)

### 3. Required Metrics
When creating RPG analysis tables, include these comprehensive metrics:
- Revenue metrics (30d, 180d, trend %)
- User metrics (Downloads, DAU, MAU)
- Monetization metrics (ARPDAU, Revenue per Download)
- Retention metrics (D1, D7, D30)
- Performance metrics (Stickiness/DAU:MAU ratio, Age, Rating)

### 4. GEC Theme Requirements
- Must use the comprehensive GEC theme with:
  - Black borders (top and bottom, 3px)
  - Gray header backgrounds (#f0f0f0)
  - Arial font family
  - Proper column grouping with spanners
  - Row striping for readability
  - Heat mapping for key metrics

### 5. Data Source
- Always use sensortowerR package
- Focus on US market data
- Sort by revenue (not DAU)
- Use custom filter ID when provided

### 6. Caching
- Implement smart caching to minimize API calls
- Cache files go in `.cache/` directory
- Clean up old cache files after successful fetch

## File Structure
```
2025_08_05_top_rpgs/
├── top_rpgs_analysis.R      # The ONLY R script allowed
├── output/
│   └── top_rpgs_analysis_api.png  # The ONLY output image allowed
├── .cache/                  # Cache directory (gitignored)
├── gec_theme.R             # Local GEC theme definition
└── CLAUDE.md               # This file
```

## Cleanup Protocol
If multiple scripts or outputs exist:
1. Keep only the comprehensive analysis script
2. Delete all other R files
3. Keep only the final comprehensive table image
4. Delete all other visualizations