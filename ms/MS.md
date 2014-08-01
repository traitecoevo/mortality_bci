% Survival analysis
% Daniel Falster

Modelling survival and mortality within a population is of fundamental importance in ecology, epidemiology, demography, engineering and economics. While often applied to be the survival of individuals in population, survival analysis applies to any system where one is interested in mapping a series of events onto time.

## Background theory

Given a system with entities and a measurable event, a **suvivorship** or **survival function** gives the probability $S$ that an event does not occur before a given time $t$, i.e.
$$ S(t) = \Pr(T > t)$$
where $T$ is a random variable denoting the time of death, and "Pr" stands for probability. Given a population of individuals, $S$ gives the fraction of individuals expected to survive from $0 \rightarrow t$.

The survival function is intimately related to the **hazard** or **instantaneous mortality function**,  denoted $\lambda(t)$, which gives the event rate at time $t$ conditional on survival until time $t$:

\begin{equation} \label{eq:lambda} \lambda(t) = \lim_{dt \rightarrow 0} \frac{\Pr(t \leq T < t+dt)}{dt\cdot S(t)} = \frac{f(t)}{S(t)} = -\frac{S'(t)}{S(t)}.\end{equation}

The survival function is related to the hazard function as follows:
\begin{equation} \label{eq:HazS} \frac{\textrm{d}}{\textrm{d}t}S(t) = - \lambda (t) \, S(t). \end{equation}

The term

\begin{equation} \label{eq:cumHaz} \Lambda(t)  = \int_{0}^{t}\lambda (t^\prime) \textrm{d} t^\prime \end{equation}

is known as the **cumulative hazard function** because it represents the  is the "accumulation" of the hazard over time. From eq. \ref{eq:HazS} we can  derive a relationship between $S$ and the cumulative hazard:

\begin{equation} \label{eq:HazS2} S(t) = \exp \left(- \int_{0}^{t}\lambda (t^\prime) \textrm{d} t^\prime \right)=\exp(-\Lambda(t)). \end{equation}

Another quantity of interest is **lifetime distribution function** $F$:

\begin{equation} \label{eq:F} F(t) = \Pr(T \le t) = 1 - S(t).  \end{equation}

The derivative of $F$ gives the **event density** per unit time:

\begin{equation} \label{eq:F}  f(t) = \frac{\textrm{d}}{\textrm{d}t} F(t).\end{equation}

## Alternative formulations of the hazard function

**Parametric survival models** assume the hazard function has a particular form, and as a result, that event density follow a certain probability distribution. For example, assuming a constant hazard rate  $\lambda(t) = \lambda_0$ leads to the well-known **exponential survival function**: $S(t) = e^{- \lambda_0 t}$; with lifetime distribution $F(t) = 1 - e^{- \lambda_0t}$, event density $f(t) = \lambda_0 e^{- \lambda_0t}$, and cumulative hazard $\Lambda = e^{- \lambda_0t}$.

Another commonly used model is based on Weibull distribution, which can lead to either accelerating or decelerating hazard over time. Note also, the exponential distribution can be seen as a special case of the
Weibull distribution.

In reality, survival process rarely follow the assumptions of parametric survival models, hence most analyses assume a **non-parametric model** (??true??).
