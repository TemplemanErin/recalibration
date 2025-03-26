# A recalibrated combined genetic risk score, derived in a genetically selected birth-cohort, offers accurate T1D prediction in an islet autoantibody-positive cohort

**Authors:**  Erin L. Templeman, Lauric A. Ferrat, Hemang M. Parikh, Lu You, Taylor Triolo, Andrea K. Steck, William A. Hagopian, Kendra Vehik, Suna Onengut-Gumuscu, Peter A. Gottlieb, Stephen S. Rich, Jeffery P. Krischer, Maria J. Redondo *, Richard A. Oram * 

**Journal:** BMC Medicine

For any questions about code on this page please contact Erin Templeman by email (et522@exeter.ac.uk).

---
## Table of Contents
1. [Project Overview](#project-overview)
2. [Installation](#installation)
3. [Usage](#usage)
4. [Recalibration Methodology](#recalibration-method)
---
### Project Overview
The project aims to assess the generalisability of a type 1 diabetes (T1D) prediction model across two cohorts: one where genetically-high risk children are followed prospectively from birth, and the validation cohort includes relatives of individuals living with T1D who were cross-sectionally screened for autoantibodies. 

The code is written in R. 

### Installation
- R Project: It is assumed that the .Rproj will be created in the same file location as "README.md" and "main.R"

Required files:
- "main.R" loads all libraries required, as well as iniating file paths to data, figures, and models.
- "load/load_data.R" reads the data files and load other local functions within this respository.

### Usage
- "main.R" and "load_data.R" will be required to run any script
- "recalibration.R" performs the logisitc recalibration which adjusts for the baseline risk and adjusting the coefficients by one factor

### Recalibration Methodology
1. Predict Linear Predictors from initial model in both train and testing data

- trainData_lp <- predict(original_model, type = "lp", newdata=trainData)
- testData_lp <- predict(original_model, type = "lp", newdata=testData)

2. Train a survival cox model using linear predictors, stratified by number of autoantibodies
- new_model <- coxph(Surv(time,status) ~ lp + strata(number_autoantibody), data = trainData, x = TRUE)

3. Test the new_model performance using discrimination (metric: ROC AUC) and calibration (metrics: Brier score and smoothed calibration curves)

---
