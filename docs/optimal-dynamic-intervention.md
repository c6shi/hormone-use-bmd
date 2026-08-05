---
title: "Attempt 2: Optimal Dynamic Intervention"
nav_order: 6
---

# Attempt 2: Optimal Dynamic Intervention

Static interventions in longitudinal settings might face a lot of positivity violations
(as we saw previously) and may also be simply unethical/not possible. Instead, we can 
consider dynamic interventions which are treatment decision rules that 
depend on covariates of each individual, denoted $$d: W \to d(W)$$. These are more flexible than 
static interventions and can also account for effect heterogeneity, which is often present. 
Moreover, we can find the optimal dynamic treatment rule (ODTR), denoted $$d_0: W \to d_0(W)$$ 
where $$d_0 = \arg\min_d E_0 Y_d$$ if $$Y$$ is some sort of negative outcome. 

In our problem, we might assume that BMD tends to decrease as women age (along with the drop in hormones). 
So, we want to administer HRTs to women with menopausal symptoms and low BMD, assuming there is a positive 
causal effect of HRTs on BMD. Also, each woman in the SWAN study may be working with their 
doctor to also establish their optimal HRT usage given their history. 

After thinking about this for a while, this doesn't really make sense (to me) to do. There are a multitude of things that can impact BMD, and BMD itself can be a marker for other diseases, so finding an ODTR that maximizes BMD doesn't really make sense, if the main purpose of an HRT is to reduce menopausal symptoms (e.g. I don't think a doctor would prescribe HRTs to women purely for the sake of increasing BMD). However, we still want to address the positivity issues of a static intervention; we next consider longitudinal modified treatment policies (LMTPs). 

References:

Williams, N. T., Hoffman, K. L., Díaz I., and Rudolph K. E. (2024). 
Learning optimal dyamic treatment regimes from longitudinal data.
American Journal of Epidemiology, 193(12):1768-1775. https://doi.org/10.1093/aje/kwae122. 
