
# insurePriceR

A package for processing insurance data and calculating risk-adjusted
premiums with a Shiny dashboard to visualise different pricing
scenarios.

## Installation

``` r
devtools::install_github("joshyjoshj/insurePriceR")
```

Sample data is included in the `data-raw/` folder to be downloaded.

## What does it do?

Takes messy insurance data, cleans it up, and calculates premiums based
on different risk factors.

### 1. Data Import & Cleaning

``` r
# Load up the data from Excel files
data <- import_data("/customers.xlsx", 
                   "/policies.xlsx", 
                   "/claims.xlsx")

# Clean everything in one go
clean_data <- preprocess_insurance_data(data)
# OR clean individual dataframes
customers_clean <- pre_process_customers(data$customers)
```

The cleaning functions handle:

- Missing values
- Standardizing text (lowercase, “sub-stringing”)
- Converting dates
- Basic validation
- Removing duplicates

### 2. Data Preparation

``` r
# Combines the cleaned data into analysis-ready formats
analysis_data <- prepare_analysis_data(
  customers_clean, 
  policies_clean, 
  claims_clean
)
```

Creates two dataframes:

- `expanded_data`: All claims with customer/policy info
- `aggregated_data`: Summary per customer with total claims

### 3. Premium Calculations

``` r
# Quick premium calc with C++ (fast!)
prem_data <- calcBasePremiumCpp(
  analysis_data$aggregated_data,
  age_base = 50,
  age_factor = 0.01,
  bmi_base = 25,
  bmi_factor = 0.02,
  smoker_penalty = 0.5
)

# Or run multiple scenarios at the same time
prem_scenarios <- calculate_premium_scenarios(analysis_data$aggregated_data)
```

Calculates premiums based on:

- Age
- BMI - Smoking status
- Pre-existing conditions
- Different risk scenarios (conservative/moderate/aggressive)

### 4. Visualization

``` r
# Launch interactive dashboard
run_dashboard(prem_scenarios)
```

Shiny dashboard shows:

- Premium distributions
- Risk score analysis
- Claims analysis
- Interactive data table

Messy insurance data → clean data → risk calculations → visualisation.
