# Data Files

Data files for test cases requiring external data.

## Files

| File | Format | Description |
|------|--------|-------------|
| `sample_scores.csv` | CSV | Student scores |
| `measurements.rds` | RDS | Environmental data |
| `growth_data.RData` | RData | Population data |
| `inventory.xlsx` | Excel | Product inventory |
| `sales_data.txt` | TXT | Sales info |

## Usage

Reference in test cases:

```r
test_cases <- list(
  inputs = list(list(data = "sample_scores.csv")),
  data_files = c("sample_scores.csv")
)
```
