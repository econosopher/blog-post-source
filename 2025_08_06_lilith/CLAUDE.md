# Lilith Games Analysis Project Settings

This project analyzes Lilith Games portfolio data using CSV exports from Sensor Tower.

## CSV File Management

### Required CSV Files
The following CSV files in the `validation/` directory are REQUIRED for the analysis:
- `Unified Revenue Jan 2023 - Jul 2025.csv` - Revenue data for all games
- `Unified Downloads Jan 2023 - Jul 2025.csv` - Download metrics  
- `Active Users MAU Jan 2023 - Jul 2025.csv` - MAU data

### Output CSV Files
The following output files are generated and should be kept:
- `output/gt_table_data.csv` - Data used for GT table generation

### Temporary CSV Cleanup Policy
- **ALWAYS clean up temporary CSV files** after their purpose is served
- **DO NOT create intermediate CSV files** unless absolutely necessary
- **Use data frames in memory** instead of writing intermediate results to disk
- **Never commit temporary CSV files** to version control

## Visualization Policy
- **USE GT TABLES EXCLUSIVELY** for all data visualizations in this project
- **DO NOT create ggplot2 charts** or other visualization types
- **GT tables provide**:
  - Professional, publication-ready formatting
  - Consistent styling across all outputs
  - Better data density and readability
  - Integrated titles, subtitles, and footnotes
- **Export GT tables as PNG** using `gtsave()` for sharing

## Lilith Games Portfolio
Key games to track:
- **AFK Arena** - Original flagship RPG
- **AFK Journey** - Sequel launched March 2024
- **Rise of Kingdoms** - Original strategy game
- **Call of Dragons** - Strategy sequel launched September 2023
- **Dislyte** - Urban mythology RPG
- **Warpath** - WWII strategy
- **BLOODLINE: HEROES OF LITHAS** - RPG
- **Soul Hunters** - Legacy RPG
- **Art of Conquest: Dark Horizon** - Strategy

## Project Structure
- `lilith_gt_table_ytd.R` - GT table generation for Lilith Games portfolio
- `validation/` - Source CSV data exports from Sensor Tower
- `output/` - Generated visualizations and final outputs

## Data Validation
- Always validate that data is correctly filtered for Lilith Games titles
- Handle platform-specific differences (iOS vs Android)
- Ensure proper deduplication using unified_app_id when applicable