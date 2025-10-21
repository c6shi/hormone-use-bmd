# L-TMLE on Alebna et al. 2025
PBHLTH 243A Fall 2025 Research Project

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

Data cleaning is performed in `data-cleaning.R`. The final clean dataset can be found as a csv file at `data/clean_data.csv`. 

# Roadmap:
## Step 1. Question of Interest
What is the mean difference in bone mineral density (BMD) at the spine and hip at Visit 10 for women in the SWAN study if all participants reported having hormonal replacement therapy (HRT) or using hormonal contraceptives at all visits vs if no participants reported having HRT or using hormonal contraceptives at any visits. 

## Step 2. Observed Data & Statistical Model
Our observed data has the following structure: $$O = (B_0, A_0, C_0, C_0L_0, TV_1, A_1, C_1, C_1L_1 \dots, TV_{10}, A_{10}, C_{10}, C_{10}L_{10} = Y)$$
where 
- $B_0$ are baseline covariates
- $TV_t$ are time-varying covariates at time $t$
- $A_t$ is exposure at time $t$
- $C_t$ is censoring at time $t$
- $C_tL_t$ is observed BMD at time $t$

## Step 3. DAG & SEM

## Step 4. Causal Target Parameter

## Step 5. Identification & Statistical Estimand

## Step 6. Estimation

# References:
Alebna, P. L., Armendano, J. I., and Maleki, N. (2025). A longitudinal analysis on the effect of hormone use on allostatic load in perimenopausal women. Aging and Health Research, 5(1):100213.

Solomon, D. H., Ruppert, K., Cauley, J. A., Lian, Y., Altwies, H., Shieh, A., and Burnett-Bowie, S.-A. M. (2024). The effect of starting metformin on bone mineral density among women with type 2 diabetes in the Study of Women’s Health Across the Nation (SWAN). Osteoporosis International, 35(1):189–194.

Sowers, M., Crawford, S., Sternfeld, B., Morganstein, D., Gold, E., Greendale, G., Evans, D., Neer, R., Matthews, K., Sherman, S., Lo, A., Weiss, G., and Kelsey, J. (2000). SWAN: A Multicenter, Multiethnic, Community-Based Cohort Study of Women and the Menopausal Transition. In Lobo, R., Kelsey, J., and Marcus, R., editors, Menopause: Biology and Pathobiology, pages 175–188. San Diego: Academic Press.