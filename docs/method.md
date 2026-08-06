---
title: "Method: L-TMLE"
nav_order: 4
---

# Method: L-TMLE

To understand longitudinal targeted maximum likelihood estimation (L-TMLE), we first need to understand targeted maximum likelihood estimation (TMLE).

## TMLE
For simplicity, suppose you have the following data structure: random variable $$O$$ which is observed $$n$$ times (iid, for simplicity). Let $$O = (W, A, Y) \sim P_0$$ without missingness or censoring:
- $$W$$: vector of covariates
- $$A$$: exposure or treatment
- $$Y$$: outcome

Suppose the probability distribution $$P_0 \in \mathcal M$$ where $$\mathcal M$$ is the set of possible probability distributions for $$P_0$$, otherwise known as the statistical model. 

Say you are interested in the target parameter of interest, $$\Psi(P_0)$$, which is some function of the true probability distribution. A common choice in causal inference is the ATE: $$\Psi(P_0) = E_0[Y^{a=1} - Y^{a=0}]$$ where $$Y^{a=1}, Y^{a=0}$$ are counterfactual outcomes. 

We know that under some assumptions, namely 1) randomization, 2) positivity, and 3) consistency, the ATE is identifiable by the observed data, so (suppose all variables are discrete):

$$
\begin{align*}
\Psi(P_0) &= E_0[E_0[Y\mid A=1, W] - E_0[Y\mid A=0,W]] \\
&= \sum_{w}\bigg(\sum_{y}yP(Y = y \mid A = 1, W = w)\\
&\qquad\qquad - yP(Y = y \mid A = 0, W = w)\bigg)P(W = w) \\
&= E_0[\bar Q_0(1, W) - \bar Q_0(0, W)]
\end{align*}
$$

where we now use the notation, $$\bar Q_0(a, w) = E_0[Y \mid A = a, W = w]$$ and $$g_0(a \mid w) = P_0(A = a \mid W = w)$$.

TMLE is a two-stage procedure that:

1. Constructs an initial estimate of the $$\bar Q_0$$'s using SuperLearner.
2. Updates the initial estimate by estimating the $$g_0$$'s (also using SuperLearner; or if $$g_0$$ is known e.g. in RCTs, use the true value), constructing a clever covariate with the estimated $$g_0$$'s and corrects the bias in the initial estimate by solving the score equation. These $$g_0$$'s are also necessary for the influence curves. 

This process can be done for both counterfactual outcomes, and the standard error can be calculated from the influence curves. TMLE is a doubly robust semiparametric method: it estimates both outcome regression and propensity scores using ML whilst providing inference. 

## L-TMLE
L-TMLE is the extension of TMLE to the longitudinal setting. Now, suppose the data structure is $$O = (L_1, A_1, L_2, A_2, Y)$$ with two timepoints and no censoring. $$L_t$$ are time-varying covariates and $$A_t$$ are time-varying exposures/treatments.

Iterated conditional expectation (ICE) 

# Causal Assumptions in Longitudinal Causal Inference

# Comparison with Other Methods
Why L-TMLE is better compared to other (causal) longitudinal analysis methods