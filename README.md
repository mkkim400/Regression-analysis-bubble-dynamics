# Regression analysis for bubble dynamics

This project performs regression analysis, comparing different equations of state (EOS) of gas: the ideal gas, the full van der Waals, and the volume-limited van der Waals). It includes data preprocessing, derivative-based cutoff detection, model fitting, plotting, and exporting of results.

---

## Objective

To analyze multiple physical variables across different EOS models and identify behavioral trends using segmented linear regression on log-transformed data.

---

## Required R Packages

Make sure the following packages are installed:

ggplot2
data.table
dplyr
reshape2
scales
readxl
gridExtra
xlsx

---
## Functionality Overview

### 1. Data Preparation
- Loads Excel file: `DATA_CMES_vander.xlsx`
- Converts data from **wide to long format**
- Applies `log10` transformation to key variables (`dp_non` and `value`)

---

### 2. Cutoff Detection
- Computes the **second derivative** of `max_Rdot_c_l`  
- Identifies a **transition point (`x_cutoff`)** in data behavior  
- Used to split regression into two segments: **below** and **above cutoff**

---

### 3. Regression Modeling
- Performs **linear regressions** separately for:
  - Below the cutoff: `log(dp_non) < x_cutoff`
  - Above the cutoff: `log(dp_non) > x_cutoff`
- Models include **interaction terms** between `method` and `log(dp_non)`
- Regression results include coefficients and p-values

---

### 4. Visualization
- Generates **ggplot2** plots for each physical variable
- Includes:
  - Raw data points
  - Predicted regression lines (below and above cutoff)
  - Annotated **p-values** at representative points
- Combines all variable plots into one output figure:
  - `1_Plot_Regression.png`

---

### 5. Output Export
- Writes output to Excel:
  - **Coefficients and p-values** → `Coef_Pvalue` sheet
  - **Predicted values** → `Predicted_Y` sheet  
- File: `dat_regression_output.xlsx`

---

## Output Files

| File Name                 | Description                                  |
|--------------------------|----------------------------------------------|
| `1_Plot_Regression.png`  | Combined regression plots                    |
| `dat_regression_output.xlsx` | Regression coefficients and model predictions |

---
