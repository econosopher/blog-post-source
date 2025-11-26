# Project-Specific Claude Instructions for SUPERVIVE Analysis

## CSV File Policy

**IMPORTANT**: Do not generate CSV files as part of test scripts or intermediate steps. CSV files should only be created as final outputs when explicitly needed for the analysis.

### Guidelines:
1. **Use in-memory data frames** for all testing and intermediate calculations
2. **Only export CSV files** when they are the final deliverable
3. **Never use temporary CSV files** for data passing between functions
4. **Avoid write.csv() or write_csv()** in test scripts or debugging code

### Rationale:
Creating unnecessary CSV files during testing:
- Clutters the project directory
- Makes version control messy
- Can accidentally overwrite important data
- Slows down the development process

### Acceptable CSV exports:
- Final analysis results (e.g., `supervive_player_overlap_analysis.csv`)
- Data explicitly requested by the user for blog posts
- Cleaned datasets that serve as project deliverables

### Instead of CSV files for testing:
```r
# Bad - creates unnecessary file
test_data <- data.frame(x = 1:10, y = 11:20)
write.csv(test_data, "test.csv")
test_loaded <- read.csv("test.csv")

# Good - keep data in memory
test_data <- data.frame(x = 1:10, y = 11:20)
# Work directly with test_data
```

## Project Structure

This project analyzes player overlap and geographic distribution for SUPERVIVE using the Video Game Insights API. All scripts should:
- Use the VideoGameInsights R package from the parent directory
- Save visualizations as PNG files (GT tables, charts)
- Only export CSV files as final deliverables
- Keep test data in memory