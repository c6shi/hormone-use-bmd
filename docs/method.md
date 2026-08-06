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
&= \sum_{w}\bigg(\sum_{y}P(Y = y \mid A = 1, W = w)P(A = 1 \mid W = w) \\
&\qquad\qquad - P(Y = y \mid A = 0, W = w)P(A = 0 \mid W = w)\bigg)P(W = w) \\
&= \sum_w \bigg(Q(1, W=w) g(1\mid W = w) - Q(0, W=w) g(0 \mid W=w)\bigg)P(W=w)
\end{align*}
$$

where we now use the notation, $$Q(a, w) = E[Y \mid A = a, W = w]$$ and $$g(a \mid w) = P(A = a \mid W = w)$$.

TMLE is a two-stage procedure that:

1. Constructs an initial estimate of the 

## L-TMLE

# Causal Assumptions in Longitudinal Causal Inference

# Comparison with Other Methods
Why L-TMLE is better compared to other (causal) longitudinal analysis methods