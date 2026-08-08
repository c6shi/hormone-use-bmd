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

## LMTP 1: Among women taking MHTs, stay on MHTs one visit longer after first use of MHT

We revisit the necessary items of the causal roadmap:
- **Statistical model, $$\mathcal M$$**: Remains the nonparametric model, $$\mathcal M_{np}$$. 
- **Target estimand**: What would be the mean difference in spine/hip BMD if all 
women who were on MHTs remained on MHTs for one visit longer after their first use compared to no intervention?
- **Target Parameter, $$\psi$$**: 
$$
\Psi(P_0) = E_0 [Y^{\bar a = d(\bar a), \bar c= 0} - Y]
$$
where 
  - $$Y^{\bar a=1, \bar c=0}$$ is the counterfactual spine/hip BMD of women if 
  they had taken HRT and were uncensored at all time points
  - $$Y$$ is the observed outcome under the observed treatment

This target parameter can be estimated via the [`lmtp`](https://github.com/nt-williams/lmtp) package. R code can be found in [`estimation.R`](https://github.com/c6shi/hormone-use-bmd/blob/v2/estimation.R). 

<hr>
References:

Díaz I., Williams N., Hoffman K.L., et al. (2023). Nonparametric causal effects based on longitudinal modified treatment policies. 

Hoffman, K.L., Salazar-Barreto, D., Williams, N.T., et al. (2024). Studying continuous, time-varying and-or complex exposures using longitudinal modified treatment policies.