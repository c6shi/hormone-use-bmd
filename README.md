# Finding the Effect of MHTs on BMD in Menopausal Women Using L-TMLE
PBHLTH 243A Fall 2025 Research Project (Ongoing)

We further explore different types of interventions which results in different statistical questions.

Also check out https://c6shi.github.io/hormone-use-bmd/ for more information.

# Data:
We use the SWAN data publicly available on ICPSR: https://www.icpsr.umich.edu/web/ICPSR/series/253/studies. 
- Screener: ICPSR 04368
- Baseline: ICPSR 28762
- Visit 01: ICPSR 29221
- Visit 02: ICPSR 29401
- Visit 03: ICPSR 29701
- Visit 04: ICPSR 30142
- Visit 05: ICPSR 30501
- Visit 06: ICPSR 31181
- Visit 07: ICPSR 31901
- Visit 08: ICPSR 32122
- Visit 09: ICPSR 32721
- Visit 10: ICPSR 32961

Data cleaning is performed in `data-cleaning.R`. The final clean dataset can be found as two csv files corresponding 
to the two outcomes of interest: `data/hip_final.csv` and `data/spine_final.csv`. 

# Roadmap:
## Step 1. Question of Interest
Original question: What is the mean difference in bone mineral density (BMD) at the spine and hip at Visit 10 
for women in the SWAN study if all participants reported having menopausal hormone therapy (MHT) or using hormonal 
contraceptives at all visits vs if no participants reported having MHT or using hormonal contraceptives at any visits? 

This might not be a realistic intervention/have serious positivity violations, and we will need to change the question.

## Step 2. Observed Data & Statistical Model
Our observed data has the following structure: $$O = (W, A_1, L_1, C_1, Y_1, \dots, A_{10}, L_{10}, C_{10}, Y_{10})$$
where 
- $W$ are baseline covariates
- $A_t$ indicates MHT user vs. non-user at each visit $t$
- $L_t$ are time-varying covariates measured at each visit $t$
- $C_t$ denotes right-censoring at visit $t$
- $Y_t$ is observed BMD at visit $t$
  - When running analysis on spine BMD, $Y_t$ is observed spine BMD and 
  hip BMD is included in $L_t$, and vice versa for analysis on hip BMD. 

## Step 3. DAG & SEM

## Step 4. Causal Target Parameter

## Step 5. Identification & Statistical Estimand

## Step 6. Estimation

# References:
Alebna, P. L., Armendano, J. I., and Maleki, N. (2025). 
A longitudinal analysis on the effect of hormone use on allostatic load in perimenopausal women. 
Aging and Health Research, 5(1):100213. https://doi.org/10.1016/j.ahr.2024.100213. 

Gruber, S., Philips, R. V., Lee, H., and van der Laan, M. J. (2022).
Data-adaptive selection of the propensity score truncation level for inverse-probability-weighted and targeted maximum likelihood estimators of marginal point treatment effects.
American Journal of Epidemiology, 191(9):1640-1651. https://doi.org/10.1093/aje/kwac087. 

Philips, R. V., van der Laan, M. J., Lee, H., and Gruber, S. (2023).
Practical considerations for specifying a super learner. 
International Journal of Epidemiology, 52(4):1276-1285. https://doi.org/10.1093/ije/dyad023. 

Solomon, D. H., Ruppert, K., Cauley, J. A., Lian, Y., Altwies, H., Shieh, A., and Burnett-Bowie, S.-A. M. (2024). 
The effect of starting metformin on bone mineral density among women with type 2 diabetes in the Study of Women’s Health Across the Nation (SWAN). 
Osteoporosis International, 35(1):189–194. https://doi.org/10.1007/s00198-023-06915-3. 

Sowers, M., Crawford, S., Sternfeld, B., Morganstein, D., Gold, E., Greendale, G., Evans, D., Neer, R., Matthews, K., Sherman, S., Lo, A., Weiss, G., and Kelsey, J. (2000). 
SWAN: A Multicenter, Multiethnic, Community-Based Cohort Study of Women and the Menopausal Transition. 
In Lobo, R., Kelsey, J., and Marcus, R., editors, Menopause: Biology and Pathobiology, pages 175–188. 
San Diego: Academic Press. https://www.researchgate.net/publication/43196481_SWAN_A_Multicenter_Multiethnic_Community-Based_Cohort_Study_of_Women_and_the_Menopausal_Transition. 
