# Pemodelan Geographically Weighted Ridge Regression pada Kasus Tuberkulosis di Indonesia
# Overview

This project applies the Geographically Weighted Ridge Regression (GWRR) model to analyze Tuberculosis (TB) case notification rates in Indonesia. The study aims to address multicollinearity and spatial heterogeneity in regression modeling by combining geographically weighted regression and ridge regression approaches.

# Objectives
Analyze factors influencing TB case notification rates in Indonesia
Handle multicollinearity and spatial heterogeneity issues
Evaluate the effectiveness of the GWRR model
Identify spatial variations in parameter estimates across provinces

# Dataset
The dataset consists of provincial-level data in Indonesia for 2024. Variables used in the analysis include:
Y : Tuberculosis Case Notification Rate (CNR)
X1 : Population percentage
X2 : Districts/Cities implementing GERMAS
X3 : Smoking population percentage
X4 : Number of HIV cases
X5 : Poor population percentage
X6 : Number of hospitals
X7 : Infants with malnutrition

# Data Sources
Badan Pusat Statistik
Indonesia Health Profile 2024

# Methodology
The analysis includes:
Descriptive statistical analysis
Multiple linear regression
Multicollinearity diagnostics
Spatial heterogeneity testing
Geographically Weighted Ridge Regression (GWRR)
Model evaluation using R² and RMSE

# Tools and Software
R Programming Language
Microsoft Excel
ArcGIS/QGIS
Results

The GWRR model showed improved performance in handling multicollinearity and spatial heterogeneity, providing better local parameter estimates for TB case modeling across Indonesian provinces.

# Repository Structure
data/          -> Dataset used in the analysis
scripts/       -> Data processing and GWRR analysis

# Author

Ni Putu Eka Martini
Mathematics Graduate (Statistics Concentration)
Universitas Udayana
