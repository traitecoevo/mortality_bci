# How does species growth strategy influence growth dependent and growth independent sources of tree mortality?

## Background

Understanding who lives or dies, and why, is critical to accurately forecasting population, community and ecosystem dynamics (Hawkes 2000, McDowell 2011). However, the mechanisms affecting mortality rates are poorly understood, especially for plants (McDowell 2011; Hawkes 2000). In plants, the carbon budget is considered a fundamental determinant of mortality, such that if carbon assimilation is low or becomes insufficient for growth, an individual is more likely to die (Hawkes 2000, McDowell 2011, Keene et al 2001). Numerous empirical studies have supported this assumption by correlating past growth rate (a proxy for plant vigor and past available carbon; Givnish 1988) to whether a plant lives or dies (Buchman et al. 1983; Kobe et al. 1995; Kobe and Coates 1997; Wyckoff and Clark 2000,Montegam et al 2003). Consequently, in most vegetation dynamic models, including tree gap models (Botkin et al 1972, Keane et al 2001) and Dynamic Global Vegetation Models (DGVM's; Woodward & Lomas et al 2004), mortality is modelled as a function of carbon supply or growth rate (Hawkes 2000, McDowell 2011). However, an assumption of these models is that the growth-mortality relationship does not vary across an within or between species. Furthermore, many models assume that the growth related mortality is the only source of mortality (McDowell 2011). In this study we attempt to remedy this by building a model that partitions tree mortality into growth dependent and growth independent processes (i.e. all other causes). We then use this model to examine how species growth strategy influence both growth-dependent and independent causes of tree mortality.


## Dataset
This project data from BCI

## Modelling approach
Bayesian; using rstan


## Computational set-up

Analysis divided into multiple stages

### 1. Building of dataset controlled via remake.

Run

```
remake export
```

### 2. Comparison of growth models

Analysis set to run in docker containers in Amazon cloud.

See `docker/README.md`

