---
title: "Survival analysis"
author: "Daniel Falster"
output: pdf_document
bibliography: data/refs.bib
csl: downloads/style.csl
---

Modelling survival and mortality within a population is of fundamental importance in ecology, epidemiology, demography, engineering and economics. While often applied to be the survival of individuals in population, survival analysis applies to any system where one is interested in mapping a series of events onto time.

## Background theory

Given a system with entities and a measurable event, a **suvivorship** or **survival function** gives the probability $S$ that an event does not occur before a given time $t$, i.e.
$$ S(t) = \Pr(T > t)$$
where "Pr" stands for probability and $T$ is a random variable denoting the time of event. For a population of individuals, $S(t)$ gives the fraction of individuals expected to survive from $0 \rightarrow t$.

The survival function is intimately related to the **hazard** or **instantaneous mortality function**,  denoted $\lambda(t)$, which gives the event rate at time $t$ conditional on survival until time $t$:

\begin{equation} \label{eq:lambda} \lambda(t) = \lim_{dt \rightarrow 0} \frac{\Pr(t \leq T < t+dt)}{dt\cdot S(t)} = -\frac{S'(t)}{S(t)}.\end{equation}

Quoting from @kleinbaum_survival_2005,

 	The hazard function h(t) gives the instantaneous potential per unit time for the event to occur, given that the individual has survived up to time t.

The term

\begin{equation} \label{eq:cumHaz} \Lambda(t)  = \int_{0}^{t}\lambda (t^\prime) \textrm{d} t^\prime \end{equation}

is known as the **cumulative hazard** because it represents the  is the "accumulation" of the hazard over time. From eq. \ref{eq:lambda} we can  derive a relationship between $S$ and the cumulative hazard:

\begin{equation} \label{eq:HazS2} S(t) = \exp \left(- \int_{0}^{t}\lambda (t^\prime) \textrm{d} t^\prime \right)=\exp(-\Lambda(t)). \end{equation}

By definition, the following hold:

* $0 \leq \lambda(t) < \infty$
* $0 \leq \Lambda(t) < \infty$
* $0 < \S(t) \leq \infty$.

Another quantity of interest is **lifetime distribution function** $F$:

\begin{equation} \label{eq:F} F(t) = \Pr(T \le t) = 1 - S(t).  \end{equation}

The derivative of $F$ gives the **event density** per unit time:

\begin{equation} \label{eq:F}  f(t) = \frac{\textrm{d}}{\textrm{d}t} F(t).\end{equation}

Also, while we most data come in form of survival, we are mostly interested making inferences about the underling hazard function, because "it is the vehicle by which mathematical modelling of survival data is carried out; that is, the survival model is usually written in terms of the hazard function." [@kleinbaum_survival_2005]

## Parametric survival model

Parametric survival models assume the hazard function has a particular form, and as a result survival time (the outcome) follows a known distribution.For example, assuming a constant hazard rate  $\lambda(t) = \lambda_0$ leads to the well-known **exponential survival function**: $S(t) = e^{- \lambda_0 t}$; with lifetime distribution $F(t) = 1 - e^{- \lambda_0t}$, event density $f(t) = \lambda_0 e^{- \lambda_0t}$, and cumulative hazard $\Lambda = e^{- \lambda_0t}$.

Another commonly used model is based on **Weibull hazard function**, which can either accelerating or decelerating forms. (Note also, the exponential distribution can be seen as a special case of the
Weibull distribution where hazard is constant over time).

Parametric models can be described in terms of hazard function or resulting survival distribution, because "specifying any one of the probability density function, survival function, or hazard function allows the other two functions to be ascertained" using equations \ref{eq:lambda}-\ref{eq:eq:F} [@kleinbaum_survival_2005].


## Non-parametric survival models

In reality, survival process rarely follow the assumptions of parametric survival models, hence most analyses assume a **non-** or **semi-parametric form**. For example, the Cox proportional hazards model (below), is a semi-parametric model because even if the regression parameters are known, the distribution of the outcomes and the baseline hazard remains unknown.

### The (Cox) proportional hazards model

The proportional hazards model assumes a hazard function with form:

\begin{equation} \label{eq:prop_hazards} \lambda(t) =  \lambda_0(t) \exp\left(\sum_{i=1}^n \beta_i x_i\right) =   \lambda_0(t)\prod_{i=1}^n \exp\left( \beta_i x_i\right),\end{equation}

where $x_1, \ldots, x_n$ are various risk factors and $\beta_1, \ldots, \beta_n$ are coefficients. This leads (via eq. \ref{eq:HazS2}) to the following model for survival:

\begin{equation} \label{eq:prop_hazards_S}
S(t) =  \exp\left(\exp\left(-\sum_{i=1}^n \beta_i x_i\right)\right) S_0(t), \end{equation}

where $S_0(t) = \exp\left(-\int_0^t \lambda_0(t)\right)$ is the baseline survival function.

A key assumption of the PH model is that the relative risk from different predictors remains constant over time. For example, if taking drug X halves your hazard at time 0, it also halves your hazard at time 1, or time 0.5, or time t for any value of t. Sir David Cox observed that if the proportional hazards assumption holds then it is possible to estimate the effect parameter(s) without knowing the baseline hazard function $\lambda_0(t)$ [@cox_regression_1972].

In the special case where the baseline hazard function $\lambda_0(t)$ is constant, the proportional hazards model gives a constant hazard rate, and thus becomes a variant of the exponential model.

The effect parameter(s) estimated by any proportional hazards model can be reported as hazard ratios, giving the relative hazard for one one individual relative to another. A useful comparison is against the hypothetical individual for which all covariates =0:
\begin{equation} H_i = \exp\left(\beta_i x_i\right).   \end{equation}

There are various methods for fitting a proportional hazards model, depending on the type of data that one has. For datasets where time of event $T$ is known, you can use Cox regression.

For censused datasets where we only know the state at specific time points $t_1, t_2$ we can use the complimentary-log-log (cloglog) model to estimate the number of individuals surviving from $t_1 \rightarrow t_2$. From with eq. \ref{eq:prop_hazards_S} and assuming $\lambda_0(t)$ is constant, we have

\begin{equation} \label{eq:cloglog}
\log\left(\log\left(-S(t)\right)\right) =  \sum_{i=1}^n \beta_i x_i\ + \lambda_0 t. \end{equation}

### Time-dependent covariates (Extended cox model)

The extended Cox model is similar to the co model, but allows for effects of different covariates to vary over time.

### Competing risks model

In general, survival analysis is applied to data where a single event is being considered. However, when there are at least two possible pathways leading to any given outcome (e.g., death from any of several causes), the problem is characterized as a competing risk problem.

Let's assume we have two alternative and independent mechanisms leading to death. Our hazard function then has form:

\begin{equation} \label{eq:additive_hazards} \lambda(t) =  \lambda_1(t) + \lambda_2(t) ,\end{equation}

leading to the survival function

\begin{equation} \label{eq:additive_hazards_S} S(t) =  \exp\left( - \int_0^t \lambda_1(t) + \lambda_2(t) \, \textrm{d} t\right) =  \exp\left( - \int_0^t \lambda_1(t) \textrm{d} t\right)\exp\left( - \int_0^t \lambda_2(t) \textrm{d} t\right).\end{equation}

In the same way as we did for single event, each event may  be related to a number of covariates through a proportional hazards model, such that

\begin{equation} \lambda_1(t) =  \lambda_{1,0}(t) \exp\left(\sum_{i=1}^n \beta_{i,1} x_i\right),\end{equation}
and
\begin{equation} \lambda_2(t) =  \lambda_{2,0}(t) \exp\left(\sum_{i=1}^n \beta_{i,2} x_i\right).\end{equation}

We then have a model of the form

\begin{equation} \label{eq:additive_hazards_S2} S(t) =  S_{0}(t) \times \exp\left(\exp\left(-\sum_{i=1}^n \beta_{i,2} x_i\right)\right) \times \exp\left(\exp\left(-\sum_{i=1}^n \beta_{i,1} x_i\right)\right),\end{equation}

where $S_{0}(t)=\exp\left(-\int_0^t \lambda_{1,0}(t)\right) \exp\left(-\int_0^t \lambda_{2,0}(t)\right)$ is combined baseline hazard function.

If the different time-of-event is known for each event type, a competing risk analysis can be used to estimate the underlying covariates (essentially this involves partitioning the dataset into two, one for each type of event, and adjusting the population sizes to account for deaths due to the other event)(see ch 9 of @kleinbaum_survival_2005).

Our challenge is to see if we can estimate the parameters of eq.  \ref{eq:additive_hazards_S2} using census data where we do not know what tree died of.

# References
