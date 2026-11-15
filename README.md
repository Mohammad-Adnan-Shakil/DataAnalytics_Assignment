# Exploratory Analysis of Bengaluru Residential Property Prices using R

**Data Analytics Lab — CA3 Mini Project**
Mohammad Adnan Shakil · 20241CSE0801 · Presidency University, Bengaluru

## Objective
Clean, transform and analyse 13,320 Bengaluru property listings to identify the drivers of housing prices: size, location, configuration, area type and construction status.

## Dataset
[Bengaluru House Price Data — Kaggle](https://www.kaggle.com/datasets/amitabhajoy/bengaluru-house-price-data) · 13,320 rows × 9 columns.
Download `Bengaluru_House_Data.csv` into `data/` (not committed).

## Lab Sheet Mapping
| Lab | Concept | Applied In |
|---|---|---|
| 1 | R basics, operators | Setup, arithmetic on price/sqft |
| 2 | Vectors, `seq`/`rep`, matrices | Sqft bins via `seq()`, BHK × availability price matrix, correlation matrix |
| 3 | `if-else` ladder, `switch`, loops | Unit conversion parser, price segmentation, per-location IQR loop |
| — | Data frames, lists, factors, I/O | CSV import, factor conversion, summary list, CSV/TXT/XLSX export |
| 4 | Missing values, imputation, scaling, log | Median imputation, z-score, min-max, log transform |
| 5 | EDA: descriptive stats, hist, boxplot, scatter | Stats table, base R plots 01–03 |
| 6 | ggplot2 | Plots 04–11 (scatter + lm, stacked/grouped bars, line, boxplot, heatmap) |

## Pipeline
`Load → Clean (blank→NA, unit parsing, imputation) → Outlier removal (domain rules + per-location IQR) → Transform → EDA → Visualise → Export`

Raw 13,320 rows → **11,634 clean rows** (12.7% removed).

## Key Findings
- **Size is the strongest price driver**: correlation between total sqft and price = **0.79**.
- **Location creates a 5.3× gap**: Rajaji Nagar (₹13,607/sqft) vs Chandapura (₹2,557/sqft).
- **Log transform reduced price skewness from 6.89 to 0.69.**
- **Large under-construction units cost more**: 5 BHK averages ₹377.6L vs ₹241.5L ready-to-move.
- **Plot Area has the highest median ₹/sqft (6,728)**, since land value is included.
- BHK and bathrooms are highly collinear (r = 0.86).

## Run
```bash
Rscript -e "install.packages(c('ggplot2','writexl','dplyr'))"
Rscript project.R
```

## Structure
```
├── project.R          # full pipeline
├── data/              # raw dataset (gitignored)
├── plots/             # 11 generated figures
└── output/            # clean CSV, stats TXT, multi-sheet XLSX
```