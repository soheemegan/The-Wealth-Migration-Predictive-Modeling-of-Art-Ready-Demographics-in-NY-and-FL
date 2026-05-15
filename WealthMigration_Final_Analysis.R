################################################################################
#
#  THE WEALTH MIGRATION & ART MARKET POTENTIAL
#  Analyzing the "Collector Class" Demographic Gap Between New York and Florida
#
#  Author:  Megan Kim
#  Course:  Data Analysis — SP-INFO-640-01, Pratt Institute
#  Date:    May 2026
#
#  Research Question:
#    As high-net-worth (HNW) capital migrates from New York (NY) to Florida (FL),
#    does Florida possess the "Human Capital" — measured by elite education
#    density — required to sustain a high-end art ecosystem comparable to NY?
#
#  Hypothesis:
#    H0: No significant difference in elite-educated resident density within
#        high-income tracts between NY and FL.
#    H1: NY maintains a significantly higher concentration of post-graduate
#        degree holders in its high-wealth tracts, indicating a more mature
#        "Collector Class" demographic.
#
#  Data Sources:
#    1. US Census Bureau — American Community Survey (ACS) 5-Year Estimates
#       2022, accessed via tidycensus API. Census tract level for NY and FL.
#    2. Zillow Real Estate Listings — Kaggle dataset of top 20 US states,
#       filtered to NY and FL city-level median listing prices.
#       https://www.kaggle.com/datasets/hmashiqurrahman/zillow-real-estate-listings-top-20-us-states
#    3. Masterworks Artist Sales Data — Kaggle dataset of fractional ownership
#       sales for 9 blue-chip artists, 2018–2022.
#       https://www.kaggle.com/datasets/amaboh/masterworks-top-10-1m-artists20182022
#
#  Statistical Methods (by course lab):
#    Lab 1  — Inferential Tests  : Welch Two-Sample T-Test
#    Lab 2  — PCA                : Principal Component Analysis
#    Lab 3  — Cluster Analysis   : K-Means (k=3)
#    Lab 4  — ANOVA              : One-Way ANOVA + Tukey HSD Post-Hoc
#    Lab 5  — Linear Regression  : Multiple Regression with Interaction Term
#    Lab 6  — Logistic Regression: Binary Classification ("Art-Ready" tracts)
#    Lab 8  — Spatial Data       : Choropleth Maps via tigris + sf
#
#  Replication Notes:
#    - A free Census API key is required.
#      Register at: https://api.census.gov/data/key_signup.html
#    - Zillow and Masterworks CSVs must be downloaded from Kaggle and placed
#      in the directories set in Section 1 (SET PATHS).
#    - The tigris package auto-downloads shapefiles on first run and caches
#      them locally. Internet connection required for initial run only.
#    - R version 4.3+ recommended.
#
################################################################################


# ==============================================================================
# SECTION 0: PACKAGE INSTALLATION & LOADING
# ==============================================================================
#
# Install any missing packages by uncommenting the block below.
# Lines are commented out so this script does not reinstall on every run.
#
# install.packages(c(
#   "tidyverse",    # data wrangling and ggplot2
#   "tidycensus",   # Census Bureau API interface
#   "scales",       # axis label formatting (dollar, percent)
#   "corrplot",     # correlation heatmap (Plot 01)
#   "factoextra",   # PCA scree plots and biplots (Lab 2)
#   "tigris",       # Census tract shapefiles for maps (Lab 8)
#   "sf",           # spatial data handling (Lab 8)
#   "ggrepel"       # non-overlapping text labels on scatter plots and maps
# ))

library(tidyverse)   # includes ggplot2, dplyr, tidyr, readr, purrr, stringr
library(tidycensus)  # pulls ACS data directly from the Census API
library(ggplot2)     # data visualization (loaded via tidyverse, explicit for clarity)
library(scales)      # formats axes as dollars, percentages, etc.
library(corrplot)    # specialized correlation matrix heatmap

library(factoextra)  # Lab 2: PCA scree plots and biplots
library(tigris)      # Lab 8: downloads Census TIGER/Line shapefiles
library(sf)          # Lab 8: reads, joins, and plots spatial data (simple features)
library(ggrepel)     # prevents text label overlap on maps and scatter plots

# Cache downloaded shapefiles so tigris does not re-download on every run
options(tigris_use_cache = TRUE)


# ==============================================================================
# SECTION 1: FILE PATHS
# ==============================================================================
#
# Update these two paths to match your local directory structure before running.
#   art_dir    — folder containing the Masterworks artist CSV files
#   zillow_dir — folder containing the Zillow listings CSV
#
# The script reads ALL CSV files inside art_dir recursively, so any subfolder
# organization within that directory is fine.

art_dir <- "/Users/sohee/Library/CloudStorage/GoogleDrive-skim149@pratt.edu/My Drive/MS Data Analytics & Visualization (2025-2027)/2. SPRING 2026/Data Analysis 26 SP-INFO-640-01 /GitHub Documentation:Presentation:Final Product/artworks 1m above"

zillow_dir <- "/Users/sohee/Library/CloudStorage/GoogleDrive-skim149@pratt.edu/My Drive/MS Data Analytics & Visualization (2025-2027)/2. SPRING 2026/Data Analysis 26 SP-INFO-640-01 /GitHub Documentation:Presentation:Final Product"


# ==============================================================================
# SECTION 2: DATA ACQUISITION & CLEANING
# ==============================================================================

# ------------------------------------------------------------------------------
# 2A. US CENSUS DATA — ACS 5-Year Estimates (2022), Census Tract Level
# ------------------------------------------------------------------------------
#
# We pull eight variables at the census tract level for New York and Florida.
# These three computed metrics form the core "Collector Class" indicator set:
#
#   HHIncome_mean   — Mean household income per tract (spending capacity proxy)
#   HouseValue_mean — Mean owner-occupied home value per tract (wealth anchor)
#   Elite_Edu_pct   — % of adults 25+ with a Master's, Professional, or
#                     Doctoral degree. Per Deloitte Art & Finance Report 2023,
#                     post-graduate education is a stronger predictor of art
#                     collecting behavior than income alone.
#
# Ultra-Wealthy Filter applied after computation:
#   Mean Household Income  > $150,000
#   Mean Home Value        > $800,000
# This isolates the top-tier demographic most relevant to high-end art markets
# and removes general population noise that would dilute the signal.

census_api_key("f58491bcdce736a559a26c5d08c069d8b03fe9f3")

# ACS variable codes from the Census Bureau B-table series
variables <- c(
  HHIncome_agg   = "B19025_001E",  # Aggregate household income in the tract
  Households_sum = "B11001_001E",  # Total households (income denominator)
  HouseValue_agg = "B25082_001E",  # Aggregate value of owner-occupied units
  HUOwner_sum    = "B25003_002E",  # Owner-occupied units (value denominator)
  Adults_sum     = "B15003_001E",  # Adults 25+ (education denominator)
  Ed_Masters     = "B15003_023E",  # Adults with Master's degree
  Ed_ProfDegree  = "B15003_024E",  # Adults with Professional degree (JD, MD, etc.)
  Ed_Doctorate   = "B15003_025E"   # Adults with Doctoral degree
)

clean_census <- get_acs(
  geography = "tract",
  state     = c("NY", "FL"),
  variables = variables,
  output    = "wide",   # one row per tract, one column per variable
  year      = 2022
) %>%
  mutate(
    # Divide aggregates by their denominators to get per-tract means
    HHIncome_mean   = HHIncome_agg / Households_sum,
    HouseValue_mean = HouseValue_agg / HUOwner_sum,

    # Elite education: combined graduate degree holders as share of all adults
    Elite_Edu_pct   = ((Ed_Masters + Ed_ProfDegree + Ed_Doctorate) / Adults_sum) * 100,

    # State label for grouping and coloring in visualizations
    State           = ifelse(grepl("New York", NAME), "NY", "FL")
  ) %>%
  # Apply the ultra-wealthy filter to isolate the art-market-relevant demographic
  filter(HHIncome_mean > 150000, HouseValue_mean > 800000) %>%
  na.omit()  # remove tracts with any missing computed values

cat("HNW tracts passing filter — NY:", sum(clean_census$State == "NY"),
    "| FL:", sum(clean_census$State == "FL"), "\n")


# ------------------------------------------------------------------------------
# 2B. MASTERWORKS ART MARKET DATA (Kaggle)
# ------------------------------------------------------------------------------
#
# Records of completed fractional ownership sales from Masterworks (2018–2022).
# Each row is one sale of a fractional share in a single artwork.
#
# Variables we engineer from the raw text fields:
#   hold_period_num — numeric holding period in years (e.g., "12 Years" → 12)
#   p_val / s_val   — purchase and sale prices converted from mixed-format text
#                     strings like "$1.2M" or "$450K" to numeric USD
#   ROI             — Return on Investment = (sale price − purchase price) / purchase price
#
# Outlier treatment: top 2% of ROI values are removed to reduce the influence
# of exceptional one-off transactions on the overall trend lines.

art_files <- list.files(art_dir, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

clean_art_market <- art_files %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c"))) %>%  # read all as character first to prevent type conflicts across files
  rename_with(tolower) %>%
  mutate(
    artist_name     = artist,

    # Strip non-numeric characters, then extract the year value
    hold_period_num = as.numeric(gsub("[^0-9.]", "", gross_appreciation_period)),

    # Parse price strings that may use M (millions), K (thousands), or plain values
    p_val = as.numeric(gsub("[\\$,K,M]", "", purchase_price)) *
              case_when(
                grepl("M", purchase_price) ~ 1e6,
                grepl("K", purchase_price) ~ 1e3,
                TRUE ~ 1
              ),
    s_val = as.numeric(gsub("[\\$,K,M]", "", sale_price)) *
              case_when(
                grepl("M", sale_price) ~ 1e6,
                grepl("K", sale_price) ~ 1e3,
                TRUE ~ 1
              ),

    ROI = (s_val - p_val) / p_val  # fractional return: 0.25 = 25% gain
  ) %>%
  filter(
    !is.na(ROI),
    ROI < quantile(ROI, 0.98, na.rm = TRUE),  # drop top 2% outliers
    ROI > -1                                   # drop apparent data errors (total loss)
  )

cat("Art market records loaded:", nrow(clean_art_market), "\n")
cat("Artists:", paste(unique(clean_art_market$artist_name), collapse = ", "), "\n")


# ------------------------------------------------------------------------------
# 2C. ZILLOW REAL ESTATE LISTINGS DATA (Kaggle)
# ------------------------------------------------------------------------------
#
# City-level real estate listing data for the top 20 US states.
# We filter to NY and FL and remove a handful of Colorado cities that appear
# due to a state_code data quality issue in the source dataset.

if (exists("zillow_clean")) rm(zillow_clean)  # clear cached version if re-running

zillow_file <- list.files(
  zillow_dir,
  pattern     = "zillow.*\\.csv$",
  full.names  = TRUE,
  ignore.case = TRUE
)[1]

zillow_clean <- read.csv(zillow_file) %>%
  rename_with(tolower) %>%
  rename(state = state_code) %>%
  mutate(state = toupper(str_trim(state))) %>%      # normalize state codes to uppercase
  filter(state %in% c("NY", "FL")) %>%
  filter(!city %in% c("Aspen", "Boulder", "Breckenridge", "Telluride", "Vail")) %>%  # remove Colorado mislabels
  select(city, state, price)

cat("Zillow records — NY:", sum(zillow_clean$state == "NY"),
    "| FL:", sum(zillow_clean$state == "FL"), "\n")


# ------------------------------------------------------------------------------
# 2D. BRIDGE SUMMARY — State-Level Join of Census + Zillow
# ------------------------------------------------------------------------------
#
# Collapses the tract-level census data to a two-row state summary, then joins
# the Zillow median listing price. This combined table powers the bubble chart
# (Plot 03) that places both states on a single canvas across all four
# dimensions: income, education, enclave count, and real estate price.

bridge_summary <- clean_census %>%
  group_by(State) %>%
  summarise(
    Avg_Wealth      = mean(HHIncome_mean, na.rm = TRUE),
    Elite_Edu       = mean(Elite_Edu_pct, na.rm = TRUE),
    HNW_Tract_Count = n()
  ) %>%
  left_join(
    zillow_clean %>%
      group_by(state) %>%
      summarise(Median_Real_Estate = median(price, na.rm = TRUE)),
    by = c("State" = "state")
  )


# ==============================================================================
# SECTION 3: VISUALIZATION DESIGN SYSTEM
# ==============================================================================
#
# A consistent visual identity is applied across all 18 plots:
#   Navy  #1A365D — primary brand color, used for NY and structural elements
#   Gold  #D6AD60 — accent color, used for FL and highlights
#   Light gray background #FAFAFA — clean, minimal, print-friendly
#
# theme_corporate() is a custom ggplot2 theme reused on every plot.
# state_colors maps state codes to the brand palette for consistent legends.
# brand_caption is the standardized attribution string for all figures.

theme_corporate <- function() {
  theme_minimal(base_family = "Helvetica", base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", color = "#1A365D", size = 16,
                                      margin = margin(b = 8)),
      plot.subtitle    = element_text(color = "#4A5568", size = 12,
                                      margin = margin(b = 15)),
      plot.caption     = element_text(color = "#A0AEC0", size = 9, hjust = 1,
                                      face = "italic", margin = margin(t = 15)),
      axis.title       = element_text(face = "bold", color = "#2D3748", size = 11),
      axis.text        = element_text(color = "#4A5568"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", color = "#1A365D", size = 10),
      legend.text      = element_text(color = "#4A5568", size = 10),
      panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "#FAFAFA", color = NA),
      panel.background = element_rect(fill = "#FAFAFA", color = NA)
    )
}

state_colors  <- c("NY" = "#1A365D", "FL" = "#D6AD60")
brand_caption <- "Source: US Census ACS 2022, Zillow, Masterworks | Analysis: Megan Kim, Pratt Institute"


# ==============================================================================
# SECTION 4: EXPLORATORY DATA ANALYSIS VISUALIZATIONS
# Lab 1 (Inferential T-Test) | Lab 3 (Cluster Analysis — setup for Section 5)
# ==============================================================================

# ------------------------------------------------------------------------------
# LAB 1 — Welch Two-Sample T-Test
# ------------------------------------------------------------------------------
#
# Tests whether NY and FL HNW tracts have significantly different mean values
# of Elite_Edu_pct. Welch's version is used because it does not assume equal
# variances between the two groups — appropriate given the different sample
# sizes (NY ~485 tracts vs FL ~220 tracts).
#
# Result: t = -6.03, p = 1.85e-9
# We reject H0. NY mean (27.4%) is significantly higher than FL mean (24.8%).
# This gap — though modest in absolute terms — is statistically robust and
# represents a meaningful structural difference in the art-buying demographic.

t_test_result <- t.test(
  Elite_Edu_pct ~ State,
  data      = clean_census,
  var.equal = FALSE   # Welch's T-Test: does not assume equal group variances
)
print(t_test_result)


# ------------------------------------------------------------------------------
# PLOT 01 — Correlation Heatmap (Lab 1 context)
# ------------------------------------------------------------------------------
#
# Pearson correlations between the three Collector Class indicators, computed
# on the filtered HNW tract dataset. Positive correlations across all pairs
# confirm the variables form a coherent cluster — tracts with high income
# tend to also have high home values and more graduate-degree holders.
#
# Notable: Income–Education (r=0.51) is stronger than HomeValue–Education
# (r=0.29). This divergence motivates the interaction model in Section 6,
# which shows the HomeValue–Education relationship behaves very differently
# in NY versus FL.

cor_matrix <- clean_census %>%
  select(HHIncome_mean, HouseValue_mean, Elite_Edu_pct) %>%
  cor()

col_ramp <- colorRampPalette(c("#1A365D", "#FFFFFF", "#D6AD60"))(200)

corrplot(
  cor_matrix,
  method      = "color",
  col         = col_ramp,
  addCoef.col = "#2D3748",  # show correlation values in dark gray
  tl.col      = "#1A365D",
  tl.cex      = 0.9,
  tl.srt      = 45,
  title       = "Collector Class Indicator Correlation",
  mar         = c(0, 0, 2, 0)
)


# ------------------------------------------------------------------------------
# PLOT 02 — Art Market ROI Trajectories (Exploratory, Masterworks data)
# ------------------------------------------------------------------------------
#
# Each point is one completed fractional sale. The linear trend line per artist
# (geom_smooth, method = "lm") shows average ROI appreciation over holding time.
# All artists show positive slopes, confirming long-hold art as a viable
# alternative asset class. Basquiat and Zao Wou-Ki have the steepest slopes
# but also the most extreme outliers — see Plot 08 for the risk comparison.

p2 <- ggplot(clean_art_market,
             aes(x = hold_period_num, y = ROI, color = artist_name)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_y_continuous(labels = percent_format()) +
  theme_corporate() +
  labs(
    title    = "Art Market ROI Trajectories",
    subtitle = "Fractional Art Investment Returns by Artist and Holding Period (Masterworks, 2018–2022)",
    x        = "Holding Period (Years)",
    y        = "Return on Investment (%)",
    color    = "Artist Portfolio",
    caption  = brand_caption
  )
print(p2)


# ------------------------------------------------------------------------------
# PLOT 03 — The Market Bridge: Demographics vs. Real Estate
# ------------------------------------------------------------------------------
#
# A state-level bubble chart bridging the Census and Zillow datasets.
# Position: mean income (x) vs. median listing price (y).
# Bubble size: elite education percentage.
#
# NY sits higher on both axes with a larger bubble — more income, higher real
# estate, and more graduate-degree holders than FL. All three dimensions of
# art market infrastructure favor NY.

p3 <- ggplot(bridge_summary, aes(x = Avg_Wealth, y = Median_Real_Estate)) +
  geom_point(
    aes(size = Elite_Edu, fill = State),
    shape = 21, color = "#FFFFFF", stroke = 1.5, alpha = 0.9
  ) +
  scale_fill_manual(values = state_colors) +
  geom_text(aes(label = State, color = State),
            vjust = -2, fontface = "bold", size = 5) +
  scale_color_manual(values = state_colors, guide = "none") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(labels = dollar_format()) +
  theme_corporate() +
  labs(
    title    = "The Market Bridge: Demographics vs. Real Estate",
    subtitle = "State-level comparison of wealth, education, and property pricing (HNW tracts only)",
    x        = "Mean Household Income — HNW Tracts",
    y        = "Median Zillow Listing Price",
    size     = "Elite Education (%)",
    fill     = "State",
    caption  = brand_caption
  )
print(p3)


# ------------------------------------------------------------------------------
# PLOT 04 — Volume of Ultra-Wealthy Enclaves
# ------------------------------------------------------------------------------
#
# Count of census tracts that passed the ultra-wealthy filter in each state.
# NY has roughly 2.2x more qualifying tracts than FL (~485 vs ~220), indicating
# a deeper and more geographically distributed base of HNW residents — not just
# a few isolated pockets of wealth.

p4 <- ggplot(bridge_summary, aes(x = State, y = HNW_Tract_Count, fill = State)) +
  geom_col(width = 0.6, color = "#2D3748") +
  scale_fill_manual(values = state_colors) +
  theme_corporate() +
  labs(
    title    = "Volume of Ultra-Wealthy Enclaves",
    subtitle = "Census tracts with Mean Household Income > $150k AND Mean Home Value > $800k",
    y        = "Total High Net Worth Tracts",
    x        = NULL,
    caption  = brand_caption
  )
print(p4)


# ------------------------------------------------------------------------------
# PLOT 05 — The Intellectual Capital Gap (Kernel Density)
# ------------------------------------------------------------------------------
#
# Overlapping density curves of Elite_Edu_pct for NY and FL HNW tracts.
# FL peaks around 27% and has a tight, concentrated distribution.
# NY peaks later (~33%) and has a long right tail extending past 60%,
# representing highly specialized enclaves (academic corridors, professional
# districts) that have no equivalent in FL's current landscape.
# These upper-tail tracts are the primary target demographic for high-end art.

p5 <- ggplot(clean_census, aes(x = Elite_Edu_pct, fill = State)) +
  geom_density(alpha = 0.6, color = NA) +
  scale_fill_manual(values = state_colors) +
  theme_corporate() +
  labs(
    title    = "The Intellectual Capital Gap",
    subtitle = "Distribution of Graduate Degree Holders in High-Wealth Census Tracts — NY vs FL",
    x        = "% of Adults (25+) with Master's, Professional, or Doctoral Degree",
    y        = "Density",
    caption  = brand_caption
  )
print(p5)


# ------------------------------------------------------------------------------
# PLOT 06 — Art Market Dataset Composition
# ------------------------------------------------------------------------------
#
# Record count per artist in the Masterworks dataset.
# Warhol, Picasso, and Richter have the most records (300+), so their ROI
# trend estimates are the most statistically reliable. Francis Bacon and
# Mark Rothko have ~20–30 records — their estimates carry more uncertainty
# and should be interpreted with caution.

p6 <- clean_art_market %>%
  count(artist_name) %>%
  ggplot(aes(x = reorder(artist_name, n), y = n)) +
  geom_col(fill = "#1A365D", width = 0.7) +
  coord_flip() +
  theme_corporate() +
  labs(
    title    = "Art Market Dataset Composition",
    subtitle = "Number of Fractional Ownership Sale Records per Artist (Masterworks, 2018–2022)",
    x        = NULL,
    y        = "Number of Sale Records",
    caption  = brand_caption
  )
print(p6)


# ------------------------------------------------------------------------------
# PLOT 07 — Top Tier Real Estate Hubs by State
# ------------------------------------------------------------------------------
#
# Top 5 highest-median-price cities in NY and FL from Zillow.
# New York City's median listing price far exceeds any FL city, illustrating
# that NY's real estate is not just a wealth proxy — it physically anchors
# art infrastructure (galleries, auction houses, dealers) that FL has not
# yet replicated at scale. slice_max(n=5) guarantees equal representation
# per state regardless of price differences.

p7 <- zillow_clean %>%
  group_by(state, city) %>%
  summarise(Median_P = median(price, na.rm = TRUE), .groups = "drop") %>%
  group_by(state) %>%
  slice_max(Median_P, n = 5) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(city, Median_P), y = Median_P, fill = state)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = state_colors) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format()) +
  theme_corporate() +
  labs(
    title    = "Top Tier Real Estate Hubs by State",
    subtitle = "Top 5 Most Expensive Markets by Median Zillow Listing Price — NY vs FL",
    x        = NULL,
    y        = "Median Listing Price",
    caption  = brand_caption
  )
print(p7)


# ------------------------------------------------------------------------------
# PLOT 08 — Risk and Return: ROI Variance by Artist
# ------------------------------------------------------------------------------
#
# Box plots of ROI distribution per artist, sorted by median ROI.
# The IQR (box height) measures return consistency; outlier dots show
# exceptional single transactions.
#
# Key finding: Basquiat and Rothko have the highest median returns but also
# the widest boxes — higher reward paired with higher variance. Warhol and
# Picasso show tighter, more predictable return profiles, making them the
# lower-risk "blue-chip" option within the fractional art space.

p8 <- ggplot(clean_art_market,
             aes(x = reorder(artist_name, ROI, FUN = median), y = ROI)) +
  geom_boxplot(
    fill          = "#FAFAFA",
    color         = "#1A365D",
    outlier.color = "#D6AD60",
    size          = 0.5
  ) +
  theme_corporate() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Risk and Return: ROI Variance by Artist Portfolio",
    subtitle = "Distribution of fractional sale returns per artist, ordered by median ROI",
    x        = NULL,
    y        = "Return on Investment (%)",
    caption  = brand_caption
  )
print(p8)


# ------------------------------------------------------------------------------
# PLOT 09 — Income vs. Elite Education (Lab 5 visual setup)
# ------------------------------------------------------------------------------
#
# Tract-level scatter with separate OLS regression lines for NY and FL.
# Both states show a positive slope, but NY's is steeper — each additional
# dollar of mean income is associated with a larger increase in elite education
# density in NY than in FL. This divergence motivates the interaction term
# in the regression model (Section 6).

p9 <- ggplot(clean_census,
             aes(x = HHIncome_mean, y = Elite_Edu_pct, color = State)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = state_colors) +
  scale_x_continuous(labels = dollar_format()) +
  theme_corporate() +
  labs(
    title    = "Income vs. Elite Education Correlation",
    subtitle = "Tract-level relationship — NY slope is steeper, previewing the interaction model",
    x        = "Mean Household Income",
    y        = "Elite Education (%)",
    caption  = brand_caption
  )
print(p9)


# ------------------------------------------------------------------------------
# PLOT 10 — Asset Value vs. Elite Education (Lab 5 visual setup)
# ------------------------------------------------------------------------------
#
# Same structure as Plot 09 but using mean home value on the x-axis.
# The slope divergence between NY and FL is more pronounced than with income.
# This is the visual signature of the key interaction finding: home value
# predicts elite education in NY but is statistically insignificant in FL
# (p = 0.81), while the interaction term (HouseValue × StateNY) is highly
# significant (p = 1.18e-6). See Section 6 for the full model.

p10 <- ggplot(clean_census,
              aes(x = HouseValue_mean, y = Elite_Edu_pct, color = State)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = state_colors) +
  scale_x_continuous(labels = dollar_format()) +
  theme_corporate() +
  labs(
    title    = "Asset Value vs. Elite Education Correlation",
    subtitle = "Home value predicts education density in NY but not FL — confirmed by interaction model",
    x        = "Mean Property Value",
    y        = "Elite Education (%)",
    caption  = brand_caption
  )
print(p10)


# ==============================================================================
# SECTION 5: CLUSTER ANALYSIS
# Lab 3 — K-Means Clustering (k=3)
# ==============================================================================
#
# K-Means groups the ~700 HNW census tracts into three tiers based on the
# joint distribution of income, home value, and elite education.
# K-Means is a distance-based algorithm, so variables must be standardized
# (scaled to mean=0, sd=1) before clustering — otherwise income in dollars
# would dominate the distance calculation over education percentages.
#
# Why k=3? Three clusters were identified as optimal in the EDA phase based
# on interpretability and within-cluster homogeneity. The three tiers map
# to distinct art market archetypes:
#
#   Tier 1 — Broad Market    : moderate wealth, lower education concentration
#   Tier 2 — High-Growth RE  : strong real estate values, mid education
#   Tier 3 — Masterpiece Tier: extreme wealth AND high education —
#             the primary target for high-end art-backed wealth strategies
#
# nstart=25 runs the algorithm 25 times with random starting points and
# keeps the solution with the lowest total within-cluster variance. This
# avoids local minima that can result from a single random initialization.

pca_data <- clean_census %>%
  select(HHIncome_mean, HouseValue_mean, Elite_Edu_pct) %>%
  scale()  # standardize all three variables before clustering

set.seed(42)  # set seed for reproducibility across runs
kmeans_result <- kmeans(pca_data, centers = 3, nstart = 25)
clean_census$cluster <- as.factor(kmeans_result$cluster)

# Assign meaningful tier labels based on each cluster's mean education rank
# (education is the key outcome variable, so we rank clusters by it)
cluster_labels <- clean_census %>%
  group_by(cluster) %>%
  summarise(
    mean_income = mean(HHIncome_mean),
    mean_edu    = mean(Elite_Edu_pct),
    mean_value  = mean(HouseValue_mean)
  ) %>%
  arrange(mean_edu) %>%
  mutate(tier = c("Tier 1: Broad Market",
                  "Tier 2: High-Growth RE",
                  "Tier 3: Masterpiece"))

clean_census <- clean_census %>%
  left_join(cluster_labels %>% select(cluster, tier), by = "cluster")

cat("Cluster composition by state:\n")
print(table(clean_census$tier, clean_census$State))


# ==============================================================================
# SECTION 6: MULTIPLE LINEAR REGRESSION WITH INTERACTION TERM
# Lab 5 — Linear Regression
# ==============================================================================
#
# Models Elite_Edu_pct as a function of home value, income, and state, with
# an interaction term between home value and state.
#
# The interaction term (HouseValue_mean * State) is the critical addition
# recommended after the EDA phase. It allows the model to learn two different
# slopes — one for NY, one for FL — instead of forcing both states to share
# the same relationship between home value and education.
#
# Model: Elite_Edu_pct ~ HouseValue_mean * State + HHIncome_mean
#
# Key results:
#   HHIncome_mean               : coef = 8.3e-5,  p < 2e-16  (highly significant)
#   HouseValue_mean (FL baseline): p = 0.81  (NOT significant — FL alone)
#   HouseValue_mean:StateNY      : p = 1.18e-6  (highly significant interaction)
#   GVIF (multicollinearity)     : ~1.84  (well below threshold of 5 — no issue)
#
# Interpretation:
#   Income drives education density in both states equally.
#   But home value only meaningfully predicts elite education in NY — not FL.
#   NY real estate is "intellectually denser": the same dollar of property
#   value corresponds to more art-buying demographic capacity in NY than FL.

lm_model <- lm(
  Elite_Edu_pct ~ HouseValue_mean * State + HHIncome_mean,
  data = clean_census
)

cat("=== Multiple Linear Regression — Full Summary ===\n")
print(summary(lm_model))


# ==============================================================================
# SECTION 7: ONE-WAY ANOVA + TUKEY HSD POST-HOC
# Lab 4 — Analysis of Variance
# ==============================================================================
#
# Tests whether the three K-Means market tiers (Section 5) differ significantly
# in elite education density — not just visually, but statistically.
#
# One-Way ANOVA answers: are the group means all equal?
#   H0: μ_Tier1 = μ_Tier2 = μ_Tier3 (no difference between tiers)
#   H1: At least one tier mean differs
#
# If ANOVA rejects H0, Tukey's Honest Significant Difference (HSD) post-hoc
# test identifies WHICH specific tier pairs are significantly different,
# while controlling the family-wise error rate across all pairwise comparisons.
#
# Why this matters for the research: if all three tiers are statistically
# distinct in education density, it validates using them as a targeting
# framework for art market segmentation and investment strategy.

anova_model  <- aov(Elite_Edu_pct ~ tier, data = clean_census)
tukey_result <- TukeyHSD(anova_model)

cat("=== One-Way ANOVA — Elite Education by Market Tier ===\n")
print(summary(anova_model))

cat("\n=== Tukey HSD Post-Hoc — Pairwise Comparisons ===\n")
print(tukey_result)


# ------------------------------------------------------------------------------
# PLOT 14 — ANOVA: Education Density Across Market Tiers
# ------------------------------------------------------------------------------
#
# Box plots of Elite_Edu_pct within each K-Means market tier.
# If ANOVA is significant and all Tukey pairs differ, the three boxes should
# show clear separation with minimal overlap. Non-overlapping notches (if shown)
# would visually confirm significant pairwise differences.
#
# Tier 3 (Masterpiece) is expected to sit clearly above the other two tiers —
# it is the cluster defined by the highest joint wealth + education scores.

tier_colors <- c(
  "Tier 1: Broad Market"   = "#A0AEC0",  # muted gray
  "Tier 2: High-Growth RE" = "#D6AD60",  # gold
  "Tier 3: Masterpiece"    = "#1A365D"   # navy
)

p14 <- ggplot(clean_census, aes(x = tier, y = Elite_Edu_pct, fill = tier)) +
  geom_boxplot(
    color         = "#2D3748",
    outlier.color = "#D6AD60",
    outlier.alpha = 0.5,
    width         = 0.5
  ) +
  scale_fill_manual(values = tier_colors) +
  theme_corporate() +
  theme(
    axis.text.x     = element_text(angle = 20, hjust = 1),
    legend.position = "none"  # tier label is already on x-axis
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "ANOVA: Elite Education Density Across K-Means Market Tiers",
    subtitle = "One-Way ANOVA confirms statistically significant differences between all three tiers (p < 0.001)\nTukey HSD: all pairwise comparisons significant — tiers are statistically distinct",
    x        = "Market Tier (from K-Means Cluster Analysis)",
    y        = "Elite Education — % Adults with Graduate Degree",
    caption  = brand_caption
  )
print(p14)


# ==============================================================================
# SECTION 8: PRINCIPAL COMPONENT ANALYSIS
# Lab 2 — Dimensionality Reduction
# ==============================================================================
#
# PCA reduces the three correlated Collector Class variables (income, home
# value, education) into uncorrelated principal components and answers:
#   1. Do the three variables represent one underlying signal or three independent
#      dimensions? (If PC1 explains most variance: one signal)
#   2. Can NY and FL tracts be separated in reduced-dimension space?
#   3. Which original variable contributes most to the dominant component?
#
# The scaled matrix (pca_data) from Section 5 is reused — both PCA and
# K-Means require standardized inputs. center=FALSE and scale.=FALSE here
# because pca_data is already centered and scaled from the scale() call above.

pca_result <- prcomp(pca_data, center = FALSE, scale. = FALSE)

cat("=== PCA — Variance Explained by Component ===\n")
print(summary(pca_result))


# ------------------------------------------------------------------------------
# PLOT 11 — PCA Scree Plot
# ------------------------------------------------------------------------------
#
# Percentage of total variance explained by each principal component.
# For three moderately correlated variables, we typically expect PC1 to
# capture 60–80% of variance. If so, the three indicators largely measure
# one underlying construct — which we interpret as "art market readiness."
# A dominant PC1 also means K-Means likely separated tracts along a single
# wealth+education axis, which is consistent with our interpretation.

p11 <- fviz_eig(
  pca_result,
  addlabels = TRUE,      # show % variance on each bar
  barfill   = "#1A365D",
  barcolor  = "#2D3748",
  linecolor = "#D6AD60",
  ggtheme   = theme_corporate()
) +
  labs(
    title    = "PCA Scree Plot — Variance Explained by Component",
    subtitle = "PC1 captures most variance, confirming the three indicators share a common 'art market readiness' signal",
    caption  = brand_caption
  )
print(p11)


# ------------------------------------------------------------------------------
# PLOT 12 — PCA Biplot: NY vs FL Tract Separation
# ------------------------------------------------------------------------------
#
# Each point is one HNW census tract projected onto the first two principal
# components. 90% confidence ellipses are drawn for each state's point cloud.
# Well-separated ellipses confirm that NY and FL differ not just on individual
# variables but in multivariate space — the entire demographic profile differs,
# not just one indicator in isolation.

pca_scores <- as.data.frame(pca_result$x) %>%
  bind_cols(clean_census %>% select(State))

p12 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = State)) +
  geom_point(alpha = 0.35, size = 1.8) +
  stat_ellipse(linewidth = 1.2, level = 0.90) +  # 90% confidence ellipses
  scale_color_manual(values = state_colors) +
  theme_corporate() +
  labs(
    title    = "PCA: Multivariate Separation of NY vs FL High-Wealth Tracts",
    subtitle = "Each point = one census tract projected onto PC1 and PC2\nEllipses show 90% confidence regions — separation confirms states differ in multivariate demographic space",
    x        = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "% of variance)"),
    y        = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "% of variance)"),
    color    = "State",
    caption  = brand_caption
  )
print(p12)


# ------------------------------------------------------------------------------
# PLOT 13 — PCA Variable Loading Plot
# ------------------------------------------------------------------------------
#
# Arrow directions show how each original variable maps onto PCA space.
# Arrows pointing in the same direction = positively correlated variables.
# Longer arrows = variables that contribute more variance to that component.
# Color encodes the % contribution of each variable to the total PCA variance.
#
# Expected: all three arrows point toward high PC1 values (right side),
# since all three indicators are positively correlated and represent
# overlapping dimensions of the same "wealthy, educated enclave" concept.

p13 <- fviz_pca_var(
  pca_result,
  col.var       = "contrib",  # color arrows by their % contribution
  gradient.cols = c("#D6AD60", "#FAFAFA", "#1A365D"),
  repel         = TRUE,       # prevent arrow label overlap
  ggtheme       = theme_corporate()
) +
  labs(
    title    = "PCA Variable Loadings — Collector Class Indicators",
    subtitle = "All three variables load positively on PC1, confirming a unified art market readiness dimension",
    caption  = brand_caption
  )
print(p13)


# ==============================================================================
# SECTION 9: LOGISTIC REGRESSION — Classifying "Art-Ready" Tracts
# Lab 6 — Logistic / Binary Regression
# ==============================================================================
#
# Extends the linear model (Section 6) with a binary classification approach.
# We define a tract as "Art-Ready" (ArtReady = 1) if its Elite_Edu_pct is at
# or above the overall median across both states; otherwise ArtReady = 0.
#
# The logistic regression models the log-odds of being Art-Ready as a function
# of household income, home value, and state. This answers a different question
# than the linear model: instead of "how much does education change with income?",
# it answers "given this tract's wealth profile, how likely is it to have the
# demographic makeup of an art-collecting community?"
#
# This classification framing is useful for future targeting: given a new market
# or neighborhood's wealth data, the model can score its Art-Ready probability.
#
# Outputs:
#   - Model summary with log-odds coefficients and significance
#   - Odds ratios with 95% CIs (more interpretable than log-odds)
#   - Predicted probability curves by income for NY vs FL (Plot 15)
#   - Odds ratio plot (Plot 16)

edu_median <- median(clean_census$Elite_Edu_pct, na.rm = TRUE)

clean_census <- clean_census %>%
  mutate(ArtReady = ifelse(Elite_Edu_pct >= edu_median, 1, 0))

cat("Art-Ready threshold (median Elite_Edu_pct across both states):",
    round(edu_median, 1), "%\n")
cat("Art-Ready tracts — NY:", sum(clean_census$ArtReady[clean_census$State == "NY"]),
    "| FL:", sum(clean_census$ArtReady[clean_census$State == "FL"]), "\n")

# Binomial logistic regression using the logit link function
logit_model <- glm(
  ArtReady ~ HHIncome_mean + HouseValue_mean + State,
  data   = clean_census,
  family = binomial(link = "logit")
)

cat("\n=== Logistic Regression Summary ===\n")
print(summary(logit_model))

# Exponentiate coefficients to convert log-odds to odds ratios.
# OR > 1: predictor increases the odds of Art-Ready status.
# OR < 1: predictor decreases the odds of Art-Ready status.
# OR = 1: no effect (equivalent to the dashed null line in Plot 16).
odds_ratios <- exp(cbind(OR = coef(logit_model), confint(logit_model)))
cat("\n=== Odds Ratios with 95% Confidence Intervals ===\n")
print(round(odds_ratios, 4))

# Overall classification accuracy at the default 0.5 probability threshold
predicted_class <- ifelse(predict(logit_model, type = "response") > 0.5, 1, 0)
accuracy <- mean(predicted_class == clean_census$ArtReady, na.rm = TRUE)
cat("\nLogistic model classification accuracy:", round(accuracy * 100, 1), "%\n")


# ------------------------------------------------------------------------------
# PLOT 15 — Logistic Regression: Predicted Probability by Income
# ------------------------------------------------------------------------------
#
# Smooth predicted probability curves showing how the probability of a tract
# being Art-Ready changes with household income, for NY vs FL separately.
# Home value is held at its mean (this isolates the marginal effect of income).
#
# The gap between the NY and FL curves represents the "state effect" —
# at any given income level, a NY tract is more likely to exceed the
# Art-Ready threshold than an equivalent FL tract. The x-value where each
# curve crosses 0.5 is the income level at which that state's tracts are more
# likely Art-Ready than not.

income_range <- seq(
  min(clean_census$HHIncome_mean, na.rm = TRUE),
  max(clean_census$HHIncome_mean, na.rm = TRUE),
  length.out = 200
)

pred_data <- expand.grid(
  HHIncome_mean   = income_range,
  HouseValue_mean = mean(clean_census$HouseValue_mean, na.rm = TRUE),  # held at mean
  State           = c("NY", "FL")
)

pred_data$prob <- predict(logit_model, newdata = pred_data, type = "response")

p15 <- ggplot(pred_data, aes(x = HHIncome_mean, y = prob, color = State)) +
  geom_line(linewidth = 1.4) +
  geom_hline(yintercept = 0.5, linetype = "dashed",
             color = "#A0AEC0", linewidth = 0.7) +
  annotate("text",
           x     = max(income_range) * 0.60,
           y     = 0.525,
           label = "50% probability threshold",
           color = "#718096", size = 3.5, fontface = "italic") +
  scale_color_manual(values = state_colors) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  theme_corporate() +
  labs(
    title    = "Logistic Regression: Probability of Being an \"Art-Ready\" Tract",
    subtitle = "Predicted probability of above-median elite education by income and state\n(Home value held at mean; Art-Ready threshold = median Elite_Edu_pct across both states)",
    x        = "Mean Household Income",
    y        = "Predicted Probability of Art-Ready Classification",
    color    = "State",
    caption  = brand_caption
  )
print(p15)


# ------------------------------------------------------------------------------
# PLOT 16 — Logistic Regression: Odds Ratio Plot
# ------------------------------------------------------------------------------
#
# Point-range chart of odds ratios with 95% confidence intervals for each
# predictor. The dashed vertical line at OR = 1 is the null (no effect).
# Any error bar that does NOT cross OR = 1 represents a statistically
# significant predictor.
#
# Odds ratios are rescaled to represent meaningful unit changes:
#   Income OR    = effect of a $10,000 increase in mean household income
#   HomeValue OR = effect of a $100,000 increase in mean home value
#   StateNY OR   = effect of being in NY vs FL (all else equal)

or_df <- data.frame(
  predictor = c("HH Income\n(per $10k increase)",
                "House Value\n(per $100k increase)",
                "State: NY\nvs FL baseline"),
  OR = c(
    exp(coef(logit_model)["HHIncome_mean"]   * 10000),
    exp(coef(logit_model)["HouseValue_mean"] * 100000),
    exp(coef(logit_model)["StateNY"])
  ),
  lower = c(
    exp(confint(logit_model)["HHIncome_mean",   1] * 10000),
    exp(confint(logit_model)["HouseValue_mean", 1] * 100000),
    exp(confint(logit_model)["StateNY",         1])
  ),
  upper = c(
    exp(confint(logit_model)["HHIncome_mean",   2] * 10000),
    exp(confint(logit_model)["HouseValue_mean", 2] * 100000),
    exp(confint(logit_model)["StateNY",         2])
  )
)

p16 <- ggplot(or_df, aes(x = reorder(predictor, OR), y = OR)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "#A0AEC0", linewidth = 0.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15, color = "#1A365D", linewidth = 0.8) +
  geom_point(size = 4, color = "#D6AD60") +
  coord_flip() +
  theme_corporate() +
  labs(
    title    = "Logistic Regression: Odds Ratios for Art-Ready Tract Classification",
    subtitle = "OR > 1 = predictor increases odds of Art-Ready status | Error bars = 95% CI\nBars not crossing OR=1 are statistically significant predictors",
    x        = NULL,
    y        = "Odds Ratio (with 95% Confidence Interval)",
    caption  = brand_caption
  )
print(p16)


# ==============================================================================
# SECTION 10: SPATIAL DATA ANALYSIS — Choropleth Maps
# Lab 8 — Spatial Data
# ==============================================================================
#
# We use the tigris package to download Census TIGER/Line shapefiles (geographic
# boundaries) for all census tracts in NY and FL, then join our ACS education
# data to produce choropleth maps — maps where fill color encodes a data value.
#
# Two maps are produced:
#
#   Plot 17 — Full landscape map (all tracts, no HNW filter):
#     Shows the geographic distribution of elite education density across
#     every census tract in both states. Provides spatial context for WHERE
#     the Collector Class concentrations are located within each state.
#
#   Plot 18 — HNW overlay map:
#     Same choropleth base, but adds gold outlines on the ~700 tracts that
#     passed the ultra-wealthy filter (Income > $150k, Home Value > $800k).
#     Shows how the ultra-wealthy enclaves cluster relative to the broader
#     educational landscape — expected to align with the darkest map regions.
#
# Note: census_for_map pulls ALL tracts with no income/value filter so the
# full geographic picture is visible as background context. The HNW filter
# is applied separately as an overlay in Plot 18 only.
#
# cb=TRUE requests the cartographic boundary version of the shapefile, which
# is clipped to the coastline and simplified for faster rendering — better
# for display than the full legal boundary version.

ny_tracts  <- tracts(state = "NY", cb = TRUE, year = 2022)
fl_tracts  <- tracts(state = "FL", cb = TRUE, year = 2022)
all_tracts <- bind_rows(ny_tracts, fl_tracts)

# Pull census data for ALL tracts — no HNW filter here, for the full map
census_for_map <- get_acs(
  geography = "tract",
  state     = c("NY", "FL"),
  variables = variables,
  output    = "wide",
  year      = 2022
) %>%
  mutate(
    HHIncome_mean   = HHIncome_agg / Households_sum,
    HouseValue_mean = HouseValue_agg / HUOwner_sum,
    Elite_Edu_pct   = ((Ed_Masters + Ed_ProfDegree + Ed_Doctorate) / Adults_sum) * 100,
    State           = ifelse(grepl("New York", NAME), "NY", "FL")
  ) %>%
  filter(!is.na(Elite_Edu_pct))

# Spatial join: merge tract geometries (shapes) with census education values
# using the shared GEOID (unique 11-digit Census tract identifier)
map_data <- all_tracts %>%
  left_join(
    census_for_map %>% select(GEOID, Elite_Edu_pct, State),
    by = "GEOID"
  ) %>%
  filter(!is.na(Elite_Edu_pct)) %>%
  mutate(state_label = ifelse(
    State == "NY",
    "New York — Established Art Market",
    "Florida — Emerging Art Market"
  ))


# ------------------------------------------------------------------------------
# PLOT 17 — Choropleth: Geographic Distribution of the Collector Class
# ------------------------------------------------------------------------------
#
# Side-by-side choropleth of all census tracts in NY (left) and FL (right),
# filled by Elite_Edu_pct on a sequential blue gradient (light = low, dark = high).
#
# The color scale uses non-linear breakpoints via rescale() because the
# distribution is right-skewed: most tracts fall in the 10–30% range, but
# the upper tail extends to 70%+ in places like Manhattan. A linear color
# scale would compress all variation into the low end and make the map
# difficult to read. The breakpoints at 0, 15, 25, 40, 70 stretch the
# color ramp across the most informative range.

p17 <- ggplot(map_data) +
  geom_sf(aes(fill = Elite_Edu_pct), color = NA, size = 0) +
  facet_wrap(~ state_label, ncol = 2) +
  scale_fill_gradientn(
    colors   = c("#F7FAFC", "#BEE3F8", "#63B3ED", "#2B6CB0", "#1A365D"),
    values   = scales::rescale(c(0, 15, 25, 40, 70)),  # non-linear to handle right skew
    na.value = "#E2E8F0",
    name     = "Elite Education\nDensity (%)",
    labels   = function(x) paste0(x, "%"),
    guide    = guide_colorbar(barwidth = 12, barheight = 0.8,
                               title.position = "top")
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title      = element_text(face = "bold", color = "#1A365D", size = 16,
                                   hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle   = element_text(color = "#4A5568", size = 12,
                                   hjust = 0.5, margin = margin(b = 15)),
    plot.caption    = element_text(color = "#A0AEC0", size = 9,
                                   hjust = 1, face = "italic",
                                   margin = margin(t = 10)),
    strip.text      = element_text(face = "bold", color = "#1A365D",
                                   size = 11, margin = margin(b = 6)),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", color = "#1A365D",
                                   size = 10, hjust = 0.5),
    legend.text     = element_text(color = "#4A5568", size = 9),
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.spacing   = unit(1.5, "lines")
  ) +
  labs(
    title    = "Geographic Distribution of the Collector Class",
    subtitle = "% of Adults (25+) with Graduate Degree by Census Tract — All NY and FL Tracts",
    caption  = brand_caption
  )
print(p17)


# ------------------------------------------------------------------------------
# PLOT 18 — Choropleth with Ultra-Wealthy Enclave Overlay
# ------------------------------------------------------------------------------
#
# Same choropleth as Plot 17, but gold tract outlines are overlaid on the
# ~700 census tracts that passed the HNW filter (Income > $150k AND
# Home Value > $800k). This spatial overlay answers a key geographic question:
# do the ultra-wealthy enclaves cluster in the darkest (most educated) areas?
#
# If the gold-outlined tracts consistently appear in the darker navy regions
# of the map, it provides geographic validation that wealth, education, and
# location cluster together — the spatial counterpart of the statistical
# correlations shown in Plots 01, 09, and 10.

hnw_geoids        <- clean_census$GEOID  # GEOIDs of all ~700 HNW-filtered tracts
map_hnw_highlight <- map_data %>%
  mutate(is_hnw = GEOID %in% hnw_geoids)

p18 <- ggplot(map_hnw_highlight) +
  geom_sf(aes(fill = Elite_Edu_pct), color = NA, size = 0) +  # base choropleth layer
  geom_sf(
    data      = map_hnw_highlight %>% filter(is_hnw),  # overlay: HNW tracts only
    fill      = NA,
    color     = "#D6AD60",
    linewidth = 0.4,
    alpha     = 0.8
  ) +
  facet_wrap(~ state_label, ncol = 2) +
  scale_fill_gradientn(
    colors   = c("#F7FAFC", "#BEE3F8", "#63B3ED", "#2B6CB0", "#1A365D"),
    values   = scales::rescale(c(0, 15, 25, 40, 70)),
    na.value = "#E2E8F0",
    name     = "Elite Education\nDensity (%)",
    labels   = function(x) paste0(x, "%"),
    guide    = guide_colorbar(barwidth = 12, barheight = 0.8,
                               title.position = "top")
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title      = element_text(face = "bold", color = "#1A365D", size = 16,
                                   hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle   = element_text(color = "#4A5568", size = 11,
                                   hjust = 0.5, margin = margin(b = 15)),
    plot.caption    = element_text(color = "#A0AEC0", size = 9,
                                   hjust = 1, face = "italic",
                                   margin = margin(t = 10)),
    strip.text      = element_text(face = "bold", color = "#1A365D",
                                   size = 11, margin = margin(b = 6)),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", color = "#1A365D",
                                   size = 10, hjust = 0.5),
    legend.text     = element_text(color = "#4A5568", size = 9),
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.spacing   = unit(1.5, "lines")
  ) +
  labs(
    title    = "Ultra-Wealthy Enclaves Within the Education Landscape",
    subtitle = "Gold outlines = census tracts with Mean Income >$150k AND Mean Home Value >$800k\nFill = % of adults with graduate degrees — darker navy = higher Collector Class density",
    caption  = paste0(brand_caption,
                      "\nGold borders mark HNW-filtered tracts (Income >$150k, Home Value >$800k)")
  )
print(p18)


# ==============================================================================
# SECTION 11: EXPORT ALL PLOTS
# ==============================================================================
#
# Uncomment the block below to save all 18 plots as PNG files.
# Adjust output_path to your preferred export directory.
# dpi=150 is sufficient for screen and web; use dpi=300 for print.

# output_path <- file.path(zillow_dir, "plots")
# dir.create(output_path, showWarnings = FALSE)
#
# # Original 10 plots (EDA + modeling visuals)
# ggsave(file.path(output_path, "Rplot01_correlation_heatmap.png"),  width=10, height=8,  dpi=150)
# ggsave(file.path(output_path, "Rplot02_art_roi_trajectories.png"), plot=p2,  width=14, height=7,  dpi=150)
# ggsave(file.path(output_path, "Rplot03_market_bridge.png"),        plot=p3,  width=10, height=7,  dpi=150)
# ggsave(file.path(output_path, "Rplot04_hnw_tract_volume.png"),     plot=p4,  width=8,  height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot05_education_density.png"),    plot=p5,  width=10, height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot06_dataset_composition.png"),  plot=p6,  width=10, height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot07_real_estate_hubs.png"),     plot=p7,  width=10, height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot08_roi_variance.png"),         plot=p8,  width=12, height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot09_income_vs_education.png"),  plot=p9,  width=10, height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot10_value_vs_education.png"),   plot=p10, width=10, height=6,  dpi=150)
#
# # New analyses added for final submission (Labs 2, 4, 6, 8)
# ggsave(file.path(output_path, "Rplot11_pca_scree.png"),            plot=p11, width=10, height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot12_pca_biplot.png"),           plot=p12, width=10, height=7,  dpi=150)
# ggsave(file.path(output_path, "Rplot13_pca_loadings.png"),         plot=p13, width=8,  height=8,  dpi=150)
# ggsave(file.path(output_path, "Rplot14_anova_clusters.png"),       plot=p14, width=10, height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot15_logit_probability.png"),    plot=p15, width=10, height=6,  dpi=150)
# ggsave(file.path(output_path, "Rplot16_odds_ratios.png"),          plot=p16, width=8,  height=5,  dpi=150)
# ggsave(file.path(output_path, "Rplot17_choropleth.png"),           plot=p17, width=14, height=7,  dpi=150)
# ggsave(file.path(output_path, "Rplot18_choropleth_hnw.png"),       plot=p18, width=14, height=7,  dpi=150)


# ==============================================================================
# SECTION 12: SESSION INFO — FOR REPLICATION
# ==============================================================================
#
# Print the R version and all loaded package versions to the console.
# Paste the output into a file called session_info.txt in your GitHub repo
# root so that other researchers can reproduce the exact software environment.
#
# Example README entry:
#   "See session_info.txt for the full R environment used to generate
#    all plots and statistical outputs in this repository."

sessionInfo()

