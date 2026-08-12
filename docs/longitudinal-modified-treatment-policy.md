---
title: "Attempt 3: Longitudinal Modified Treatment Policy"
nav_order: 7
---

# Attempt 3: Longitudinal Modified Treatment Policy (LMTP)

Although dynamic interventions are an improvement to the positivity violations and impracticality of static interventions, dynamic interventions can still face positivity violations. We can instead consider an intervention that is based on the natural value of treatment (the treatment value that the individual actually had). This is called a modified treatment policy (MTP). An extension of MTPs to longitudinal data is called a longitudinal modified treatment policy (LMTP) which is what we will be using here. 

It is common to estimate some sort of risk difference where the baseline is no intervention. Thus, we must choose an intervention of interest to compare it to. A few come to mind:

- Among all women who were taking MHTs, what if they stayed on MHTs one visit longer after the first use of MHT? e.g. for a woman with natural exposure $$(0, 0, 1, 0, 0, 1, 0)$$, her LMTP would be $$(0, 0, 1, 1, 0, 1, 0)$$. 
- What if all women (with some randomness) who had stopped taking MHTs did not? e.g. for a woman with natural exposure $$(0, 0, 1, 0, 0, 1, 0)$$, her LMTP would be $$(0, 0, 1, 1, 1, 1, 1)$$.
- Taking inspiration from the guideline that women should start MHTs under age 60 or within 10 years, what if all women (with some randomness) started taking MHTs as soon as they were in late perimenopause or reach age 55, whichever comes first? 

> [!warning] TODO
> I don't know how to represent these in the $$d(a_t, h_t)$$ notation! 

LMTPs can only depend on an individual's history! So, even though an intervention of interest could be, "what if we started women on MHTs earlier?", that is not plausible to estimate under the LMTP framework. 

## Data Structure
We revisit the data structure from the previous attempts (as specified in [Our Setup](https://c6shi.github.io/hormone-use-bmd/data/#our-setup)) to stay consistent with the current literature. 

First, we recategorize age from a baseline covariate to a time-varying covariate. This will be important when we define LMTPs that take age into account. 

Then, let $$B$$ be baseline covariates, $$A_t = (A_{1,t}, A_{2,t})$$ be exposure at time $$t$$ where $$A_{1,t}$$ is an indicator of MHT use, and $$A_{2,t}$$ is an indicator of censoring (0 = censored, 1 = uncensored), and $$L_t = (L_{1,t}, L_{2,t}, \dots, L_{12, t}, Y_t)$$ be time-varying covariates & outcome where $$L_{1,t}$$ is age at time $$t$$, $$L_{2,t}$$ is height at time $$t$$, ..., $$L_{12, t}$$ is insulin usage at time $$t$$, and $$Y_{t}$$ is spine or hip BMD at time $$t$$. 

Furthermore, since each $$A_{1,t}$$ is an indicator of MHT use since visit $$t-1$$, to ensure the correct time-ordering, say for $$t=3$$, $$A_{1,3}$$ will correspond to indicator of MHT use since visit 3, **measured at visit 4**, whereas $$L_{\cdot, 3}$$ will correspond to covariates & outcome **measured at visit 3**. This ensures that $$L_t$$ does not depend on $$A_t$$. 

To reiterate, for the following data structure 

$$
O = (B, A_0, L_1, A_1, L_2, A_2, \dots, L_9, A_9, Y = Y_{10})
$$

- $$B = (B_1, B_2, L_0)$$ is (race/ethnicity at baseline, education at baseline, age at baseline, ..., spine/hip BMD at baseline)
- $$A_0 = (A_{1, 0}, A_{2, 0} = 1)$$ is (MHT use since baseline measured at visit 1, censoring since baseline measured at visit 1)
- $$L_1 = (L_{1, 1}, \dots, L_{12, 1}, Y_1)$$ is (age at visit 1, ..., insulin usage since baseline measured at visit 1, spine/hip BMD at visit 1)
- $$A_1 = (A_{1, 1}, A_{2, 1})$$ is (MHT use since visit 1 measured at visit 2, censoring since visit 1 measured at visit 2)
- $$L_2 = (L_{1, 2}, \dots, L_{12, 2}, Y_2)$$ is (age at visit 2, ..., insulin usage since visit 1 measured at visit 2, spine/hip BMD at visit 2)
- $$A_2 = (A_{1, 2}, A_{2, 2})$$ is (MHT use since visit 2 measured at visit 3, censoring since visit 2 measured at visit 3)
- $$L_9 = (L_{1, 9}, \dots, L_{12, 9}, Y_9)$$ is (age at visit 9, ..., insulin usage at visit 9, spine/hip BMD at visit 9)
- $$A_9 = (A_{1, 9}, A_{2, 9})$$ is (MHT use since visit 9 measured at visit 10, censoring since visit 9 measured at visit 10)
- $$Y = Y_{10}$$ is the spine/hip BMD at visit 10

Thus, we have $$A(t)$$ from $$t = 0, \dots, K$$ and $$L(t)$$ from $$t = 0, \dots, K+1$$. 

## LMTP 1: Among women taking MHTs, stay on MHTs one visit longer after first use of MHT

We revisit the necessary items of the causal roadmap:
- **Statistical model, $$\mathcal M$$**: Remains the nonparametric model, $$\mathcal M_{np}$$. 
- **Target estimand**: What would be the mean difference in spine/hip BMD if all women who were on MHTs remained on MHTs for one visit longer after their first use compared to no intervention?
- **Target Parameter, $$\psi$$**: 
$$
\Psi(P_0) = E_0 [Y^{\mathbb d, \bar c = 1} - Y^{\bar c = 1}]
$$
where 
  - $$Y^{\mathbb d}$$ is the counterfactual spine/hip BMD of women at visit 10 under the LMTP where women who took MHTs stayed on MHTs one year (visit) longer after their first use of MHT and were uncensored at all time points
  - $$Y$$ is the counterfactual spine/hip BMD of women at visit 10 under their observed treatment and were uncensored at all time points

This target parameter can be estimated via the [`lmtp`](https://github.com/nt-williams/lmtp) package. R code can be found in [`estimation.R`](https://github.com/c6shi/hormone-use-bmd/blob/v2/estimation.R). 

Alternatively, instead of spine/hip BMD at visit 10, we could also be interested in spine/hip BMD difference from visit 1 to visit 10, as baseline spine/hip BMD could be different for certain subgroups. Additionally, the rate at which BMD declines might be faster or slower depending on things like age, menopausal status, diet, etc. 

### Identification
In order to identify the above target estimand with the observed data, we must satisfy:
1. Positivity: if $$(a_t, h_t) \in \text{supp}\{A_t, H_t\}$$ then $$(\mathbb d(a_t, h_t), h_t) \in \text{supp}\{A_t, H_t\}$$ for all $$t \in \{0, \dots, K\}$$
2. Strong sequential randomization: $$U_{A, t} \perp (\underline U_{L, t+1}, \underline U_{A, t+1}) \mid H_t$$ for all $$t \in \{0, \dots, K\}$$

### Estimation
<hr>
References:

Díaz I., Williams N., Hoffman K.L., et al. (2023). Nonparametric causal effects based on longitudinal modified treatment policies. 

Hoffman, K.L., Salazar-Barreto, D., Williams, N.T., et al. (2024). Studying continuous, time-varying and-or complex exposures using longitudinal modified treatment policies.