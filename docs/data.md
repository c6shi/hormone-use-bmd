---
title: "Data: SWAN"
nav_order: 3
---

# Data: SWAN
{: .no_toc }

We use data from the Study of Women's Health Across the Nation (SWAN) which 
is a longitudinal study that began in 1997 to understand women's health during 
perimenopause particularly in ethnic minority groups in the U.S. due to the 
lack of data in these populations. 

They chose seven cities in the U.S. and for each city, an ethnic minority group 
to compare against white women:
- Non-Hispanic Caucasian women in all cities
- African American women in Detroit, MI, Chicago, IL, Boston, MA, and Pittsburgh, PA
- Chinese women in Oakland, CA
- Hispanic women in Newark, NJ
- Japanese women in Los Angeles, CA

After a screening process from a cross-sectional study, xxx women were identified 
to be eligible for the longitudinal study which involved annual examinations of 
questionnaires, blood and urine specimen collection, and physical measures. Furthermore,
annual examinations were scheduled for days 2-5 after bleeding started to standardize 
serum hormone measure to the early phase of the menstrual cycle. It should also 
be noted that due to the irregularity in hormone levels during perimenopause, the 
measurement of hormone levels can often be quite different from day-to-day (check on this
for the early phase).

Data from 1997-2008 is publicly available on ICPSR (no site variable or specimen 
data), though the study is still ongoing today. 

# Our Setup
There were many variables collected in the SWAN dataset. Since we are interested
in HRTs and BMD, we handpicked a few variables to be included in our analysis, as 
including too many covariates in L-TMLE will be too computationally intensive 
given our available resources. 

## Variables
Certain baseline and time-varying covariates were chosen based on what is known 
to be predictive of BMD.
- **Baseline covariates**: age, race/ethnicity, and education.
- **Time-varying covariates**: menopausal status, anxiety, depression, height, weight, 
alcohol consumption, smoking, physical activity, diabetes status, insulin usage, 
and missingness at the visit.
- **Exposure**: use of HRTs since last visit, indicator variable denoted $$A(t)$$.
  - Combination estrogen/progestin, e.g. Premphase or Prempro, OR
  - Estrogen pills, e.g. Premarin, Estrace, or Ogen, OR
  - Estrogen by injection or path, e.g. Estraderm, OR
  - Progestin pills, e.g. Provera
- **Outcome**: BMD at the lumbar spine (LS) and total hip (TH), denoted $$Y(t)$$.
  - For these two outcomes, we run the analysis separately for both, and include 
  the other outcome as a time-varying covariate.
- **Censoring**: true right-censoring for the outcome, i.e. loss to follow up, denoted
$$C(t)$$.

## Data Structure
Following our typical $$O = (W, A, Y)$$ data structure, we have this longitudinal 
data structure: 

$$
O = (W, A(1), L(1), C(1), Y(1), \dots, A(10), L(10), C(10), Y(10))
$$

Note that we switch the order of the $$A$$'s and the $$L$$'s because all of the 
treatment, covariate, and outcome data are collected at the "same time" at each visit.
Additionally, some variables are questions about xyz since the last visit or 
within the past few weeks, e.g. anxiety, depression, insulin, smoking, etc., 
whereas some other variables are values collected exactly at the visit, 
e.g. height, weight, BMD. So, we have some ambiguity in the time-ordering. 

## Data Cleaning
Recall that SWAN collected data from seven cities. However, two of the seven 
cities did not measure BMD: Chicago and Newark. Since site information is not publicly
available, we clean based on missingness and end up with a dataset of 1,804 women. 
All data cleaning steps can be found in `data-cleaning.R` in the git repo.

### Missingness
We distinguish between missing data and right-censoring. 

For the missingness variable, should we distinguish between missing anything vs 
missing exposure? Because missing exposure seems more important than missing a covariate.