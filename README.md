# The Wealth Migration: Predictive Modeling of "Art-Ready" Demographics in NY and FL

---

## Overview

As high-net-worth (HNW) capital migrates from New York to Florida, does Florida possess the "Human Capital" — measured by elite education density — required to sustain a high-end art ecosystem comparable to New York?

This project integrates three datasets (US Census ACS, Zillow, Masterworks) and applies seven statistical methods to test whether New York's "Collector Class" demographic has migrated alongside its wealth.

**Key finding:** Being in New York vs. Florida multiplies the odds of an art-ready neighborhood by **3.61×** — after controlling for income and home value. Florida has the money. It does not yet have the people.

---

## Repository Structure

```
wealth-migration-art-market/
│
├── README.md                  ← You are here
├── LICENSE                    ← MIT License
│
├── analysis/
│   └── WealthMigration_Final_Analysis.R   ← Full annotated R script (all 18 plots + 7 methods)
│
├── data/
│   ├── README_data.md         ← Data source descriptions and download instructions
│   └── zillow_listings.csv    ← Zillow data (download from Kaggle — see below)
│
├── plots/
│   ├── Rplot01_correlation_heatmap.png
│   ├── Rplot02_art_roi_trajectories.png
│   ├── Rplot03_market_bridge.png
│   ├── Rplot04_hnw_tract_volume.png
│   ├── Rplot05_education_density.png
│   ├── Rplot06_dataset_composition.png
│   ├── Rplot07_real_estate_hubs.png
│   ├── Rplot08_roi_variance.png
│   ├── Rplot09_income_vs_education.png
│   ├── Rplot10_value_vs_education.png
│   ├── Rplot11_pca_scree.png
│   ├── Rplot12_pca_biplot.png
│   ├── Rplot13_pca_loadings.png
│   ├── Rplot14_anova_clusters.png
│   ├── Rplot15_logit_probability.png
│   ├── Rplot16_odds_ratios.png
│   ├── Rplot17_choropleth.png
│   └── Rplot18_choropleth_hnw.png
│
├── reports/
│   ├── EDA_Report.pdf                  ← Phase 1: Exploratory Data Analysis
│   ├── Model_Results.pdf               ← Phase 2: Statistical Modeling Results
│   └── portfolio_essay.md              ← Final portfolio essay (1,553 words)
│
└── session_info.txt           ← R session info for reproducibility
```

---

## Data Sources

### 1. US Census Bureau — American Community Survey (ACS)
- **Access:** Via the `tidycensus` R package (API call — no download needed)
- **Year:** 2022 ACS 5-Year Estimates
- **Geography:** Census tract level, New York and Florida
- **API Key:** Register free at https://api.census.gov/data/key_signup.html
- **Variables used:** Household income (B19025), home value (B25082), educational attainment (B15003)

### 2. Zillow Real Estate Listings
- **Source:** Kaggle
- **Dataset ID:** `hmashiqurrahman/zillow-real-estate-listings-top-20-us-states`
- **Download:** https://www.kaggle.com/datasets/hmashiqurrahman/zillow-real-estate-listings-top-20-us-states
- **Instructions:** Download `zillow_listings.csv` and place in the `data/` folder

### 3. Masterworks Artist Sales
- **Source:** Kaggle
- **Dataset ID:** `amaboh/masterworks-top-10-1m-artists20182022`
- **Download:** https://www.kaggle.com/datasets/amaboh/masterworks-top-10-1m-artists20182022
- **Instructions:** Download all CSVs and place in a folder called `data/artworks/`

---

## Setup & Reproducibility

### Requirements
- R version 4.3 or higher (tested on R 4.5.2)
- Internet connection (for Census API and tigris shapefile downloads)

### Install Required Packages

Run this once in R before executing the main script:

```r
install.packages(c(
  "tidyverse",
  "tidycensus",
  "scales",
  "corrplot",
  "factoextra",
  "tigris",
  "sf",
  "ggrepel"
))
```

### Set Your Census API Key

```r
tidycensus::census_api_key("YOUR_API_KEY_HERE", install = TRUE)
```

### Update File Paths

Open `analysis/WealthMigration_Final_Analysis.R` and update the two path variables at the top of Section 1 to match your local directory:

```r
art_dir    <- "data/artworks"     # folder containing Masterworks CSVs
zillow_dir <- "data"              # folder containing zillow_listings.csv
```

### Run the Analysis

Source the full script in R:

```r
source("analysis/WealthMigration_Final_Analysis.R")
```

All 18 plots will render in your R graphics window. Uncomment the `ggsave()` block at the bottom of the script to export them to the `plots/` folder.

---

## Statistical Methods

| Lab | Method | Purpose |
|-----|--------|---------|
| Lab 1 | Welch Two-Sample T-Test | Compare elite education density: NY vs FL |
| Lab 2 | Principal Component Analysis (PCA) | Dimensionality reduction of 3 Collector Class indicators |
| Lab 3 | K-Means Clustering (k=3) | Segment HNW tracts into market tiers |
| Lab 4 | One-Way ANOVA + Tukey HSD | Validate cluster statistical distinctiveness |
| Lab 5 | Multiple Linear Regression + Interaction | Model education density; test HouseValue × State |
| Lab 6 | Logistic Regression | Classify "Art-Ready" tracts; compute odds ratios |
| Lab 8 | Spatial Choropleth Mapping | Geographic distribution of Collector Class |

---

## Key Results

- **T-Test:** NY mean elite education = 36.8% vs FL = 30.7% (t = −7.62, p = 1.34×10⁻¹³)
- **Interaction Model:** Home value predicts elite education in NY (p = 0.01) but NOT in FL (p = 0.81)
- **Logistic Regression:** NY tracts are 3.61× more likely to be Art-Ready than FL (OR = 3.61, CI: 2.45–5.39)
- **PCA:** PC1 explains 63.1% of variance — income, home value, and education share one signal
- **ANOVA:** F = 509.1, p < 2×10⁻¹⁶ — three market tiers are statistically distinct

---

## Visualizations

All 18 plots are in the `plots/` folder. They follow a consistent corporate design theme (navy `#1A365D` / gold `#D6AD60`) applied via a custom `theme_corporate()` function defined in the main R script.

---

## Session Info

```
R version 4.5.2 (2025-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Tahoe 26.5

Key packages:
tidyverse    2.0.0
tidycensus   1.7.3
ggplot2      4.0.0
factoextra   1.0.7
tigris       2.2.1
sf           1.0-21
corrplot     0.95
scales       1.4.0
```

Full session info available in `session_info.txt`.

---

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

---

## Citation

If you use this analysis or code, please cite:

> Kim, M. (2026). *The Wealth Migration: Analyzing Art Market Potential in NY vs. FL*. Pratt Institute, MS Data Analytics & Visualization. https://github.com/[your-username]/wealth-migration-art-market

---

## Contact

**Megan (Sohee) Kim**  
soheemegankim@gmail.com  
https://www.linkedin.com/in/soheemegankim/
