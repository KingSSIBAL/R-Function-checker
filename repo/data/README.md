# Data Files

This folder contains data files used by test cases that require external data.

## 📁 Files

| File | Format | Description |
|------|--------|-------------|
| `sample_scores.csv` | CSV | Student scores (10 students, 3 subjects) |
| `measurements.rds` | RDS | Environmental measurements (temperature, humidity, pressure) |
| `growth_data.RData` | RData | Population growth data (2015-2024) |
| `inventory.xlsx` | Excel | Product inventory with categories and prices |
| `sales_data.txt` | TXT | Sales information (product, quantity, price) |

## 📊 File Details

### sample_scores.csv
```csv
student_id,name,math,science,english
1,Alice,85,90,88
2,Bob,78,82,85
...
```
- **Rows:** 10 students
- **Columns:** student_id, name, math, science, english
- **Use case:** Testing data.frame operations, mean/sum calculations

### measurements.rds
```r
data.frame(
  id = 1:8,
  temperature = c(23.5, 25.1, ...),
  humidity = c(45, 52, ...),
  pressure = c(1013.2, 1012.8, ...)
)
```
- **Rows:** 8 observations
- **Columns:** id, temperature, humidity, pressure
- **Use case:** Testing RDS loading, numeric analysis

### growth_data.RData
```r
# Contains three objects:
population   # numeric vector (10 values)
years        # 2015:2024
growth_rate  # 0.35
```
- **Objects:** 3 (population, years, growth_rate)
- **Use case:** Testing RData loading with multiple objects

### inventory.xlsx
```
item_id | category    | quantity | unit_price
A001    | Electronics | 50       | 299.99
A002    | Electronics | 30       | 149.99
...
```
- **Rows:** 5 items
- **Columns:** item_id, category, quantity, unit_price
- **Use case:** Testing Excel loading, inventory calculations

### sales_data.txt
```
Product A,150,12.99
Product B,200,24.99
...
```
- **Rows:** 5 products
- **Format:** Comma-separated (product, quantity, price)
- **Use case:** Testing text file parsing

## 🔧 Creating New Data Files

### CSV
```r
df <- data.frame(a = 1:10, b = letters[1:10])
write.csv(df, "my_data.csv", row.names = FALSE)
```

### RDS
```r
my_object <- list(x = 1:100, y = rnorm(100))
saveRDS(my_object, "my_data.rds")
```

### RData
```r
obj1 <- 1:10
obj2 <- data.frame(a = 1:5)
save(obj1, obj2, file = "my_data.RData")
```

### Excel
```r
library(openxlsx)
df <- data.frame(col1 = 1:10, col2 = LETTERS[1:10])
write.xlsx(df, "my_data.xlsx")
```

## 📝 Using Data in Test Cases

Reference data files by filename in your test case inputs:

```r
test_cases <- list(
  data_files = c("my_data.csv", "my_data.rds"),
  inputs = list(
    list(data = "my_data.csv"),
    list(data = "my_data.rds", option = TRUE)
  )
)
```

The autograder will:
1. Prefetch all files listed in `data_files`
2. Replace filename strings with loaded data at test runtime
3. Pass the actual data object to your function

## ⚠️ Notes

- Keep data files small (<1MB) for fast downloads
- Use RDS for complex R objects (lists, models, etc.)
- Use CSV/Excel for tabular data that students might inspect
- Data files are downloaded once per autograder session
