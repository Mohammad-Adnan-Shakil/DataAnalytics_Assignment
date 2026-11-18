# ============================================================
# Exploratory Analysis of Bengaluru Residential Property Prices
# Data Analytics Lab - CA3 Mini Project
# Author: Mohammad Adnan Shakil (20241CSE0801)
# ============================================================

# ---- Section 1: Setup & Load (Lab 1, Data Structures & I/O) ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(writexl)
  library(dplyr)
})

house <- read.csv("data/Bengaluru_House_Data.csv", stringsAsFactors = FALSE)

cat("Class:", class(house), "\n")
cat("Dimensions:", dim(house), "\n\n")
cat("Structure:\n"); str(house)
cat("\nFirst rows:\n"); print(head(house))
cat("\nNA count per column:\n"); print(colSums(is.na(house)))
cat("\nBlank-string count per column:\n"); print(colSums(house == "", na.rm = TRUE))
cat("\nUnique values - area_type:\n"); print(table(house$area_type))
cat("\nSample size values:\n"); print(head(unique(house$size), 15))
cat("\nNon-numeric total_sqft samples:\n")
print(head(house$total_sqft[is.na(suppressWarnings(as.numeric(house$total_sqft)))], 15))

# ---- Section 2: Data Cleaning (Lab 3 control flow + Lab 4 missing values) ----
cat("\n================ SECTION 2: CLEANING ================\n")
raw_rows <- nrow(house)

# 2.1 Blank strings -> NA, trim whitespace
house[] <- lapply(house, function(col) {
  if (is.character(col)) {
    col <- trimws(gsub("\\s+", " ", col))
    col[col == ""] <- NA
  }
  col
})
cat("Missing values after blank->NA conversion:\n"); print(colSums(is.na(house)))

# 2.2 Drop 'society' (41% missing - not imputable meaningfully)
pct_missing_society <- round(mean(is.na(house$society)) * 100, 2)
cat("\nsociety missing %:", pct_missing_society, "-> column dropped\n")
house$society <- NULL

# 2.3 Drop rows missing location/size (very few, cannot impute categorical id)
house <- house[!is.na(house$location) & !is.na(house$size), ]

# 2.4 Extract BHK number from size ("2 BHK", "4 Bedroom", "1 RK")
house$bhk <- as.integer(sub(" .*", "", house$size))

convert_sqft <- function(x) {
  if (grepl(" - ", x)) {                        # range -> average
    parts <- as.numeric(strsplit(x, " - ")[[1]])
    return(mean(parts))
  }
  num <- suppressWarnings(as.numeric(x))
  if (!is.na(num)) return(num)                  # already numeric

  value <- as.numeric(regmatches(x, regexpr("^[0-9.]+", x)))  # leading number
  unit  <- gsub(" ", "", sub("^[0-9.]+", "", x))              # rest = unit
  factor <- switch(unit,
                   "Sq.Meter" = 10.7639,
                   "Sq.Yards" = 9,
                   "Acres"    = 43560,
                   "Perch"    = 272.25,
                   "Cents"    = 435.6,
                   "Guntha"   = 1089,
                   "Grounds"  = 2400,
                   NA)
  if (is.na(factor)) cat("Unknown unit:", unit, "\n")
  return(value * factor)
}

sqft_clean <- numeric(nrow(house))
for (i in seq_len(nrow(house))) {
  sqft_clean[i] <- convert_sqft(house$total_sqft[i])
}
cat("\nUnparseable total_sqft values:", sum(is.na(sqft_clean)), "\n")
house$total_sqft <- sqft_clean
house <- house[!is.na(house$total_sqft), ]

# 2.6 Impute bath & balcony with median (Lab 4)
bath_median    <- median(house$bath, na.rm = TRUE)
balcony_median <- median(house$balcony, na.rm = TRUE)
house$bath[is.na(house$bath)]       <- bath_median
house$balcony[is.na(house$balcony)] <- balcony_median
cat("Imputed bath with median =", bath_median,
    "| balcony with median =", balcony_median, "\n")

# 2.7 Availability -> binary factor using ifelse
house$availability <- factor(ifelse(house$availability == "Ready To Move",
                                    "Ready To Move", "Under Construction"))

# 2.8 Factor conversion
house$area_type <- factor(house$area_type)

# 2.9 Domain rule: minimum 300 sqft per bedroom (removes data-entry errors)
before_rule <- nrow(house)
house <- house[house$total_sqft / house$bhk >= 300, ]
cat("Rows removed by 300 sqft/BHK rule:", before_rule - nrow(house), "\n")

# 2.10 Derived feature: price per sqft (price is in lakhs)
house$price_per_sqft <- round(house$price * 1e5 / house$total_sqft, 2)

# 2.11 Verify
cat("\nComplete cases:", sum(complete.cases(house)), "of", nrow(house), "\n")
cat("Rows: raw =", raw_rows, "| clean =", nrow(house),
    "| removed =", raw_rows - nrow(house), "\n\n")
str(house)
print(summary(house[, c("total_sqft", "bath", "balcony", "bhk", "price", "price_per_sqft")]))

# ---- Section 2B: Outlier Treatment ----
cat("\n================ SECTION 2B: OUTLIERS ================\n")

# Group rare locations (<= 10 listings) into "Other" (Lab: factors)
loc_counts <- table(house$location)
rare_locs  <- names(loc_counts[loc_counts <= 10])
house$location[house$location %in% rare_locs] <- "Other"
cat("Distinct locations after grouping:", length(unique(house$location)), "\n")

# Rule 1: bathrooms should not exceed bedrooms + 2
before <- nrow(house)
house <- house[house$bath <= house$bhk + 2, ]
cat("Removed by bath > bhk+2 rule:", before - nrow(house), "\n")

# Rule 2: IQR filter on price_per_sqft within each location (Lab 5 boxplot logic)
before <- nrow(house)
keep <- logical(nrow(house))
for (loc in unique(house$location)) {
  idx <- which(house$location == loc)
  q   <- quantile(house$price_per_sqft[idx], c(0.25, 0.75))
  iqr <- q[2] - q[1]
  keep[idx] <- house$price_per_sqft[idx] >= q[1] - 1.5 * iqr &
               house$price_per_sqft[idx] <= q[2] + 1.5 * iqr
}
house <- house[keep, ]
cat("Removed by per-location IQR filter:", before - nrow(house), "\n")
# Rule 3: residential sanity bounds
before <- nrow(house)
house <- house[house$total_sqft <= 30000 & house$price_per_sqft >= 1000, ]
cat("Removed by sanity bounds:", before - nrow(house), "\n")

house$location <- factor(house$location)
cat("Final clean rows:", nrow(house), "\n\n")
print(summary(house[, c("total_sqft", "bath", "bhk", "price", "price_per_sqft")]))

# ---- Section 3: Transformation & Feature Engineering (Lab 2, 3, 4) ----
cat("\n================ SECTION 3: TRANSFORMATION ================\n")

skewness <- function(x) mean((x - mean(x))^3) / sd(x)^3

# 3.1 Log transform (Lab 4) - price is right-skewed
house$log_price <- log(house$price)
cat("Skewness  price:", round(skewness(house$price), 3),
    "| log_price:", round(skewness(house$log_price), 3), "\n")

# 3.2 Z-score standardisation (Lab 4)
house$sqft_z <- as.numeric(scale(house$total_sqft))
cat("sqft_z  mean:", round(mean(house$sqft_z), 4), "| sd:", round(sd(house$sqft_z), 4), "\n")

# 3.3 Min-max normalisation (Lab 4)
minmax <- function(x) (x - min(x)) / (max(x) - min(x))
house$pps_norm <- minmax(house$price_per_sqft)
cat("pps_norm range:", range(house$pps_norm), "\n")

# 3.4 Price segment via if-else ladder (Lab 3)
classify_price <- function(p) {
  if (p < 50) {
    "Budget"
  } else if (p < 100) {
    "Mid-Range"
  } else if (p < 200) {
    "Premium"
  } else {
    "Luxury"
  }
}
house$segment <- factor(sapply(house$price, classify_price),
                        levels = c("Budget", "Mid-Range", "Premium", "Luxury"))
cat("\nSegment distribution:\n"); print(table(house$segment))

# 3.5 Area-type short label via switch (Lab 3)
house$area_short <- factor(sapply(as.character(house$area_type), function(a)
  switch(a,
         "Super built-up Area" = "SuperBuiltUp",
         "Built-up Area"       = "BuiltUp",
         "Plot Area"           = "Plot",
         "Carpet Area"         = "Carpet")))

# 3.6 Vectors with seq() / rep() for size bins (Lab 2)
sqft_breaks <- c(seq(0, 3000, by = 500), Inf)
sqft_labels <- paste0(head(sqft_breaks, -1), "-", tail(sqft_breaks, -1))
house$sqft_bin <- cut(house$total_sqft, breaks = sqft_breaks, labels = sqft_labels)
cat("\nSqft bins:\n"); print(table(house$sqft_bin))

# 3.7 Matrix: average price (lakhs) by BHK x availability (Lab 2)
sub_bhk <- house[house$bhk <= 5, ]
price_matrix <- round(tapply(sub_bhk$price, list(sub_bhk$bhk, sub_bhk$availability), mean), 2)
cat("\nAvg price matrix (BHK x Availability):\n"); print(price_matrix)
cat("Matrix dim:", dim(price_matrix), "\n")

# 3.8 List summarising the dataset (Data structures lab)
summary_list <- list(
  rows        = nrow(house),
  locations   = nlevels(house$location),
  median_pps  = median(house$price_per_sqft),
  top_segment = names(which.max(table(house$segment)))
)
cat("\nSummary list:\n"); str(summary_list)

# ---- Section 4: Exploratory Data Analysis (Lab 5) ----
cat("\n================ SECTION 4: EDA ================\n")

# 4.1 Descriptive statistics table for key numeric variables
num_cols <- c("price", "total_sqft", "price_per_sqft", "bhk", "bath")
desc_stats <- data.frame()
for (col in num_cols) {
  x <- house[[col]]
  desc_stats <- rbind(desc_stats, data.frame(
    variable = col,
    mean     = round(mean(x), 2),
    median   = round(median(x), 2),
    sd       = round(sd(x), 2),
    variance = round(var(x), 2),
    min      = min(x),
    q1       = quantile(x, 0.25, names = FALSE),
    q3       = quantile(x, 0.75, names = FALSE),
    max      = max(x),
    iqr      = IQR(x)
  ))
}
cat("Descriptive statistics:\n"); print(desc_stats, row.names = FALSE)

# 4.2 Correlation matrix (Lab 2 matrix + Lab 5)
cor_matrix <- round(cor(house[, c("price", "total_sqft", "bhk", "bath", "balcony", "price_per_sqft")]), 3)
cat("\nCorrelation matrix:\n"); print(cor_matrix)

# 4.3 Top 10 most expensive & cheapest locations by median price/sqft (min 30 listings)
loc_stats <- house %>%
  filter(location != "Other") %>%
  group_by(location) %>%
  summarise(listings = n(), median_pps = median(price_per_sqft)) %>%
  filter(listings >= 30) %>%
  arrange(desc(median_pps))
cat("\nTop 10 expensive locations:\n");  print(head(loc_stats, 10))
cat("\nTop 10 affordable locations:\n"); print(tail(loc_stats, 10))

# 4.4 Group means by area type and availability
cat("\nMedian price/sqft by area type:\n")
print(round(tapply(house$price_per_sqft, house$area_type, median), 0))
cat("\nMedian price/sqft by availability:\n")
print(round(tapply(house$price_per_sqft, house$availability, median), 0))

# 4.5 Base R plots (saved as PNG)
png("plots/01_hist_price.png", width = 900, height = 600)
par(mfrow = c(1, 2))
hist(house$price, breaks = 60, col = "#4C9F9F", border = "white",
     main = "Price Distribution (Raw)", xlab = "Price (Lakhs)")
hist(house$log_price, breaks = 40, col = "#E07A5F", border = "white",
     main = "Price Distribution (Log)", xlab = "log(Price)")
invisible(dev.off())

png("plots/02_boxplot_pps_bhk.png", width = 900, height = 600)
boxplot(price_per_sqft ~ bhk, data = house[house$bhk <= 6, ],
        col = "#81B29A", main = "Price per Sqft by BHK",
        xlab = "BHK", ylab = "Price per Sqft (INR)")
invisible(dev.off())

png("plots/03_scatter_sqft_price.png", width = 900, height = 600)
plot(house$total_sqft, house$price, pch = 20, col = rgb(0.3, 0.6, 0.6, 0.3),
     main = "Total Sqft vs Price", xlab = "Total Sqft", ylab = "Price (Lakhs)",
     xlim = c(0, 6000), ylim = c(0, 600))
abline(lm(price ~ total_sqft, data = house), col = "#E07A5F", lwd = 2)
invisible(dev.off())

cat("\nBase R plots saved to plots/ (01-03)\n")

# ---- Section 5: Visualisation with ggplot2 (Lab 6) ----
cat("\n================ SECTION 5: GGPLOT2 ================\n")

theme_set(theme_minimal(base_size = 13) +
          theme(plot.title = element_text(face = "bold"),
                plot.subtitle = element_text(colour = "grey40")))
pal <- c("#3D5A80", "#98C1D9", "#EE6C4D", "#81B29A")

# 5.1 Scatter + regression line, coloured by availability
g1 <- ggplot(house[house$total_sqft <= 5000 & house$price <= 500, ],
             aes(total_sqft, price, colour = availability)) +
  geom_point(alpha = 0.3, size = 1.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.1) +
  scale_colour_manual(values = pal[c(1, 3)]) +
  labs(title = "Total Sqft vs Price", subtitle = "Linear trend by availability",
       x = "Total Sqft", y = "Price (Lakhs)", colour = "Availability")
ggsave("plots/04_gg_scatter_sqft_price.png", g1, width = 9, height = 6, dpi = 150)

# 5.2 Bar chart: listings by BHK with count labels
g2 <- ggplot(house[house$bhk <= 6, ], aes(factor(bhk))) +
  geom_bar(fill = pal[1]) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4) +
  labs(title = "Listings by BHK", x = "BHK", y = "Count")
ggsave("plots/05_gg_bar_bhk.png", g2, width = 8, height = 5.5, dpi = 150)

# 5.3 Stacked bar: segment by area type (proportions)
g3 <- ggplot(house, aes(area_short, fill = segment)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  labs(title = "Price Segment Mix by Area Type", x = "Area Type", y = "Share", fill = "Segment")
ggsave("plots/06_gg_stacked_segment_area.png", g3, width = 8, height = 5.5, dpi = 150)

# 5.4 Grouped bar: avg price by BHK x availability (matches price_matrix)
avg_df <- house %>% filter(bhk <= 5) %>%
  group_by(bhk, availability) %>% summarise(avg_price = mean(price), .groups = "drop")
g4 <- ggplot(avg_df, aes(factor(bhk), avg_price, fill = availability)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = pal[c(1, 3)]) +
  labs(title = "Average Price by BHK and Availability",
       x = "BHK", y = "Avg Price (Lakhs)", fill = "Availability")
ggsave("plots/07_gg_grouped_bhk_avail.png", g4, width = 8, height = 5.5, dpi = 150)

# 5.5 Horizontal bar: top 10 vs bottom 10 locations by median price/sqft
top_bottom <- rbind(head(loc_stats, 10) %>% mutate(group = "Most Expensive"),
                    tail(loc_stats, 10) %>% mutate(group = "Most Affordable"))
g5 <- ggplot(top_bottom, aes(reorder(location, median_pps), median_pps, fill = group)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = pal[c(4, 3)]) +
  labs(title = "Locations by Median Price per Sqft", subtitle = "Locations with 30+ listings",
       x = NULL, y = "Median Price per Sqft (INR)", fill = NULL)
ggsave("plots/08_gg_locations.png", g5, width = 9, height = 7, dpi = 150)

# 5.6 Line chart: median price across sqft bins
line_df <- house %>% group_by(sqft_bin) %>% summarise(median_price = median(price))
g6 <- ggplot(line_df, aes(sqft_bin, median_price, group = 1)) +
  geom_line(colour = pal[1], linewidth = 1.2) +
  geom_point(colour = pal[3], size = 3) +
  labs(title = "Median Price Across Size Bands", x = "Sqft Band", y = "Median Price (Lakhs)")
ggsave("plots/09_gg_line_sqftbin.png", g6, width = 8, height = 5.5, dpi = 150)

# 5.7 Boxplot: log price by area type
g7 <- ggplot(house, aes(area_short, log_price, fill = area_short)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2) +
  scale_fill_manual(values = pal) +
  labs(title = "Log Price Distribution by Area Type", x = "Area Type", y = "log(Price)")
ggsave("plots/10_gg_box_area.png", g7, width = 8, height = 5.5, dpi = 150)

# 5.8 Correlation heatmap
cor_df <- as.data.frame(as.table(cor_matrix))
g8 <- ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = Freq), size = 3.5) +
  scale_fill_gradient2(low = pal[3], mid = "white", high = pal[1], midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap", x = NULL, y = NULL, fill = "r") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("plots/11_gg_corr_heatmap.png", g8, width = 8, height = 6.5, dpi = 150)

cat("ggplot2 visuals saved to plots/ (04-11)\n")

# ---- Section 6: Export Results (Data Structures & I/O Lab) ----
cat("\n================ SECTION 6: EXPORT ================\n")

export_cols <- c("area_type", "availability", "location", "bhk", "total_sqft", "bath",
                 "balcony", "price", "price_per_sqft", "log_price", "segment", "sqft_bin")
clean_out <- house[, export_cols]

# 6.1 CSV, TXT, XLSX
write.csv(clean_out, "output/bengaluru_clean.csv", row.names = FALSE)
write.table(desc_stats, "output/descriptive_stats.txt", sep = "\t", row.names = FALSE)

price_matrix_df <- data.frame(bhk = rownames(price_matrix), price_matrix, check.names = FALSE)
write_xlsx(list(
  clean_data      = clean_out,
  descriptive     = desc_stats,
  correlation     = data.frame(variable = rownames(cor_matrix), cor_matrix, check.names = FALSE),
  location_stats  = loc_stats,
  bhk_avail_price = price_matrix_df
), "output/analysis_results.xlsx")

# 6.2 Read back & verify round-trip integrity
check <- read.csv("output/bengaluru_clean.csv")
cat("Exported rows:", nrow(clean_out), "| Re-read rows:", nrow(check),
    "| Match:", identical(nrow(clean_out), nrow(check)), "\n")
cat("Files in output/:\n"); print(list.files("output"))
cat("Plots generated:", length(list.files("plots", pattern = "\\.png$")), "\n")

# ---- Section 7: Lab Coverage Extras (Labs 2, 3, 4, 5, 6, I/O) ----
cat("\n================ SECTION 7: LAB COVERAGE EXTRAS ================\n")
suppressPackageStartupMessages({
  library(readxl)
  library(visdat)
})

# 7.1 Visualise missingness in the RAW data (Lab 4 - vis_miss)
house_raw <- read.csv("data/Bengaluru_House_Data.csv", na.strings = c("", "NA"))
g_miss <- vis_miss(house_raw) +
  labs(title = "Missing Values in Raw Dataset")
ggsave("plots/12_vis_miss_raw.png", g_miss, width = 9, height = 6, dpi = 150)
cat("Raw missing cells:", sum(is.na(house_raw)),
    "| % of all cells:", round(mean(is.na(house_raw)) * 100, 2), "\n")

# 7.2 rep(): reshape the BHK x availability matrix into long format (Lab 2)
price_long <- data.frame(
  bhk          = rep(rownames(price_matrix), times = ncol(price_matrix)),
  availability = rep(colnames(price_matrix), each = nrow(price_matrix)),
  avg_price    = as.vector(price_matrix)
)
cat("\nMatrix reshaped with rep():\n"); print(price_long)

# 7.3 while loop: smallest area that covers 90% of listings (Lab 3)
sqft_limit <- 500
while (mean(house$total_sqft <= sqft_limit) < 0.90) {
  sqft_limit <- sqft_limit + 100
}
cat("\n90% of listings are at or below", sqft_limit, "sqft\n")

# 7.4 repeat loop: budget needed to afford 75% of listings (Lab 3)
budget <- 10
repeat {
  share <- mean(house$price <= budget)
  if (share >= 0.75) break
  budget <- budget + 10
}
cat("A budget of Rs", budget, "lakh covers", round(share * 100, 1), "% of listings\n")

# 7.5 aggregate() grouped statistics (Lab 5)
cat("\nMedian price/sqft by BHK (aggregate):\n")
print(aggregate(price_per_sqft ~ bhk, data = house[house$bhk <= 5, ], FUN = median))
cat("\nMean price (lakhs) by area type x availability (aggregate):\n")
agg2 <- aggregate(price ~ area_short + availability, data = house, FUN = mean)
agg2$price <- round(agg2$price, 2)
print(agg2)

# 7.6 pairs() scatterplot matrix (Lab 5)
set.seed(42)
pair_sample <- house[sample(nrow(house), 2000), c("price", "total_sqft", "bhk", "bath", "price_per_sqft")]
png("plots/13_pairs_matrix.png", width = 1000, height = 1000)
pairs(pair_sample, pch = 20, col = rgb(0.24, 0.35, 0.5, 0.3),
      main = "Scatterplot Matrix (random sample of 2,000 listings)")
invisible(dev.off())

# 7.7 Histogram + density curve (Lab 6)
g9 <- ggplot(house, aes(log_price)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = pal[2], colour = "white") +
  geom_density(colour = pal[3], linewidth = 1.1) +
  labs(title = "Log Price: Histogram with Density Curve", x = "log(Price)", y = "Density")
ggsave("plots/14_gg_hist_density.png", g9, width = 8, height = 5.5, dpi = 150)

# 7.8 Violin plot (Lab 6)
g10 <- ggplot(house, aes(availability, price_per_sqft, fill = availability)) +
  geom_violin(alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA) +
  scale_fill_manual(values = pal[c(1, 3)]) +
  labs(title = "Price per Sqft Distribution by Availability",
       x = NULL, y = "Price per Sqft (INR)")
ggsave("plots/15_gg_violin_avail.png", g10, width = 8, height = 5.5, dpi = 150)

# 7.9 facet_wrap (Lab 6)
g11 <- ggplot(house[house$total_sqft <= 5000 & house$price <= 500, ],
              aes(total_sqft, price)) +
  geom_point(alpha = 0.25, size = 0.9, colour = pal[1]) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = pal[3]) +
  facet_wrap(~ area_short) +
  labs(title = "Total Sqft vs Price, Faceted by Area Type",
       x = "Total Sqft", y = "Price (Lakhs)")
ggsave("plots/16_gg_facet_area.png", g11, width = 10, height = 7, dpi = 150)

# 7.10 read_excel(): read back the exported workbook (I/O lab)
xlsx_path <- "output/analysis_results.xlsx"
cat("\nSheets in exported workbook:", paste(excel_sheets(xlsx_path), collapse = ", "), "\n")
xl_clean <- read_excel(xlsx_path, sheet = "clean_data")
xl_desc  <- read_excel(xlsx_path, sheet = "descriptive")
cat("clean_data sheet rows:", nrow(xl_clean), "| Match with data frame:",
    nrow(xl_clean) == nrow(clean_out), "\n")
cat("descriptive sheet:\n"); print(xl_desc)

cat("\nTotal plots generated:", length(list.files("plots", pattern = "\\.png$")), "\n")
cat("\n=========== PIPELINE COMPLETE ===========\n")