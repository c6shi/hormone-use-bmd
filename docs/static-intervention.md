---
title: "Attempt 1: Static Intervention"
nav_order: 5
---

# Attempt 1: Static Intervention

The first thing we tried was investigating the causal effect under a static intervention
for all time points. 

We define the following items for the causal roadmap:
- **Statistical model, $$\mathcal M$$**: Since this is observational data, we have 
no real knowledge on the exposure mechanism and its conditional probabilities. 
So, we specify the statistical model to be the nonparametric statistical model, 
$$\mathcal M_{np}$$. 
- **Target estimand**: What would be the mean difference in spine/hip BMD if all 
perimenopausal women were on HRTs at all time points versus if no perimenopausal 
women were on HRTs at any of the time points? 
- **Target Parameter, $$\psi$$**: 
$$
\Psi(P_0) = E_0 [Y^{\bar a = 1, \bar c= 0} - Y^{\bar a = 0, \bar c = 0}]
$$
where 
  - $$Y^{\bar a=1, \bar c=0}$$ is the counterfactual spine/hip BMD of women if 
  they had taken HRT and were uncensored at all time points
  - $$Y^{\bar a=0, \bar c=0}$$ is the counterfactual spine/hip BMD of women if 
  they never took HRT and were uncensored at all time points

This is not really a realistic intervention and faces a lot of positivity issues. 
In particular, 797 women were never on HRTs for 10 years (this corresponds to the 
counterfactual outcome $$Y^{\bar a=0, \bar c=0}$$), and only 12 women were continuously 
on HRTs for 10 years (this corresponds to the counterfactual outcome $$Y^{\bar a=1, \bar c=0}$$).
Furthermore, with the number of covariates we have, the latter group will surely 
run into positivity violations. 

Let's take a look at the distribution of exposure and see how many individuals 
we have at each visit and for how long they are on the treatment. 

##### Figure 1: Number of women in each treatment group at each visit
![Figure 1](https://github.com/c6shi/hormone-use-bmd/blob/v2/plots/no_of_women_per_group_per_visit.png?raw=true)

##### Table 1: Number of women in each treatment group at each visit

| On HRTs Since Last Visit |  0 (Baseline) | Visit 1 | Visit 2 | Visit 3 | Visit 4 | Visit 5 | Visit 6 | Visit 7 | Visit 8 | Visit 9 | Visit 10 |
|:------------------------:|--------------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|---------:|
|          No (0)          |          1804 |    1682 |    1495 |    1389 |    1314 |    1258 |    1276 |    1366 |    1374 |    1397 |     1399 |
|         Yes (1)          |             0 |     122 |     204 |     260 |     306 |     338 |     340 |     216 |     160 |     133 |      125 |
|          Total           |          1804 |    1804 |    1699 |    1649 |    1620 |    1596 |    1616 |    1582 |    1534 |    1530 |     1524 |

There is a pretty noticeable imbalance between the two treatment groups at each visit.
We expect to see no women in the HRT user group at baseline because one of our criteria
for filtering was all women did not have any HRT usage prior to starting the longitudinal 
study. 
The least imbalance excluding baseline was at visit 5 with 78.8% in the no HRT user group
and 21.2% in the HRT user group. The most imbalance excluding baseline was at visit 
1 with 93.2% in the no HRT user group and 6.8% in the HRT user group.
We also had the least nulls in visit 1 with 0 nulls and the most nulls in visit 10 
with 280 nulls (15.5% of starting sample).
<br><br>

##### Figure 2: Number of women that started MHTs at each visit
![Figure 2](https://github.com/c6shi/hormone-use-bmd/blob/v2/plots/no_of_women_start_MHT_per_visit.png?raw=true)

<!--
##### Table 2: Number of women that started HRTs at each visit

| 0 (Baseline) | Visit 1 | Visit 2 | Visit 3 | Visit 4 | Visit 5 | Visit 6 | Visit 7 | Visit 8 | Visit 9 | Visit 10 |
|-------------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|---------:|
|            0 |     122 |     116 |     108 |      86 |      74 |      51 |      22 |      26 |      11 |       17 |
-->
Interpretation: 122 women started HRTs at visit 1, 116 women started HRTs at visit 2, etc.
Note that this does not say anything about the duration, e.g. women may have started HRTs
at visit 1, then never used HRTs ever again. Also, 17 women began using HRTs at visit 10. 
We might want to know why those 17 women started HRTs late, 
e.g. what is their age, are they still in perimenopause, etc. These include women that
may have nulls in some visits. So, there may be more than 17 women (if there were women
who truly started HRTs at visit 10 but their visit 10 value was 
null) or less than 17 women (if there were women who truly started HRTs before visit 10, but their visits 
before visit 10 were null).
<br><br>

##### Figure 3: Number of women on MHTs for each visit length
![Figure 3](https://github.com/c6shi/hormone-use-bmd/blob/v2/plots/no_of_women_stay_on_MHT_per_visit_length.png?raw=true)

<!--
##### Table 3: Number of women on HRTs for each visit length

| 0 (No HRTs) | 1 Visit | 2 Visits | 3 Visits | 4 Visits | 5 Visits | 6 Visits | 7 Visits | 8 Visits | 9 Visits | 10 Visits |
|------------:|--------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|----------:|
|        1167 |     174 |      112 |       85 |       69 |       58 |       55 |       24 |       26 |       17 |        12 |
-->
Interpretation: 1,167 women were never on HRTs (including visits with nulls; note
the difference with the 797 number from above is after filtering all women with any nulls). 
174 women were on HRTs for exactly one visit, 112 women were on HRTs for exactly two visits, etc. 
Again, these might not be accurate since there were women with null visits. 

## Results

We used L-TMLE to estimate the target parameter. For our super learner library,
we specified a small library to reduce computation time: 
`SL.library = list("SL.glm", "SL.earth", c("SL.glm", "screen.corP"), c("SL.earth", "screen.corP"))`. 
For the spine BMD, this gave us a parameter estimate of 0.065513 with SE 0.010989, yielding a 95% CI (0.043975, 0.087051) and p-value 2.4962e-09.
For the hip BMD, this gave us a parameter estimate of 0.025075 with SE 0.0088218, yielding a 95% CI (0.0077841, 0.042365) and p-value 0.0044784. 
Unfortunately, the units for BMD were not specified, but we can interpret the spine BMD results as 
"the spine BMD of women who were on HRTs for 10 years was 0.065513 units greater than
the spine BMD of women who were never on HRTs for 10 years", and similarly for the hip BMD outcome. 
We see that HRTs have a positive effect on BMD in the spine and hip, as established in 
literature (again, see [Background]({{ site.baseurl }}{% link background.md %})). 

### Sensitivity Analysis
causal gap