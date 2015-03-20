library(rstan)
library(parallel)
source('R/make.bci.mainstem.R')
filename <- 'data/BCI_50haplot.rda'
if(!file.exists(filename))
  BCI_download_50ha_plot_full(filename)
filename <- 'data/BCI_nomenclature.rda'
if(!file.exists(filename))
  BCI_download_species_table('data/BCI_nomenclature.rda')

load('data/BCI_50haplot.rda')
load('data/BCI_nomenclature.rda')
bci.traits <- read.csv('data/BCI_traits_20101220.csv')
bci.mainstem <- BCI_calculate_individual_growth()
bci.mainstem <- subset(bci.mainstem, censusid==4) #1995
names(bci.traits) <- tolower(names(bci.traits)) # lowers trait column names for merging
bci.traits$sp <- tolower(bci.traits$sp) # lowers species code names for merging
bci.mainstem <- merge(bci.mainstem,bci.traits[,c('sp','sg100c_avg')],by = 'sp') #only uses species trait data exists for.
bci.mainstem <- subset(bci.mainstem, !is.na(sg100c_avg) & !is.na(dbh_dt) & !is.na(dead_next_census))

stan_data <- list(
  n_obs = nrow(bci.mainstem),
  n_ind = length(unique(bci.mainstem$treeid)),
  n_spp = length(unique(bci.mainstem$sp)),
  spp = as.numeric(factor(bci.mainstem$sp), as.character(unique(bci.mainstem$sp))),
  rho =  unique(bci.mainstem$sg100c_avg)*1000, # converts wood density to kg/m2
  dbh_dt = bci.mainstem$dbh_dt + abs(min(bci.mainstem$dbh_dt)), # makes most negative growth rate zero.
  census_length = bci.mainstem$census_interval,
  y = as.integer(bci.mainstem$dead_next_census))

StanModel <- '
  data {
    int<lower=0> n_obs;
    int<lower=0, upper=1> y[n_obs];
    int<lower=0> n_spp;
    int<lower=1> spp[n_obs];
    vector[n_obs] census_length;
    vector[n_obs] dbh_dt;
    vector[n_spp] rho;
  }
transformed data { // centers and standardizes predictors
  vector[n_spp] cs_lnrho;
  vector[n_obs] s_dbh_dt;
  cs_lnrho <- (log(rho) - mean(log(rho)))/ (2*sd(log(rho)));
  s_dbh_dt <- dbh_dt/ (2*sd(dbh_dt)); 
}
parameters { // assumes uniform priors on all parameters
  real a0_raw[n_spp];
  real b0_raw[n_spp];
  real c0_raw[n_spp];
  real a1; // effect of rho on a_log
  real b1; // effect of rho on b_log
  real c1; // effect of rho on ho_log
  real a0_mu; // a_log effect for average species
  real<lower=0> a0_sigma; // a_log effect species variation
  real b0_mu;  // b_log effect for average species
  real<lower=0> b0_sigma; // b_log effect species variation
  real c0_mu;  // ho_log effect for average species
  real<lower=0> c0_sigma; // ho_log effect species variation
}  

transformed parameters {
  real<lower=0, upper=1> p[n_obs];
  real a_log[n_spp];
  real b_log[n_spp];
  real ho_log[n_spp];
  real a0[n_spp];
  real b0[n_spp];
  real c0[n_spp];
  
  for (s in 1:n_spp){ # Add species random effect & effects of rho
    a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
    b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
    c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
    a_log[s] <- a0[s] + a1 * cs_lnrho[s];
    b_log[s] <- b0[s] + b1 * cs_lnrho[s];
    ho_log[s] <- c0[s] + c1 * cs_lnrho[s];
  }
  
  for (i in 1:n_obs) { // Estimate p
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[spp[i]] - exp(b_log[spp[i]]) * s_dbh_dt[i]) + exp(ho_log[spp[i]]))));
  }
}

model {
  for (s in 1:n_spp) { // non-centered parameterization Papaspiliopoulos et al. (2007).
    a0_raw[s] ~ normal(0,1); // implies normal(a0_mu, a0_sigma);
    b0_raw[s] ~ normal(0,1); // implies normal(b0_mu, b0_sigma); 
    c0_raw[s] ~ normal(0,1); // implies normal(c0_mu, c0_sigma); 
  }
  // Sample Pr(Dying) from bernoulli
  y ~ bernoulli(p);
}
'

stan_model <- sflist2stanfit(mclapply(1:3,mc.cores=3,
                                      function(i) stan(model_code=StanModel, data=stan_data, 
                                                       pars = c('a0_mu','b0_mu','c0_mu','a0_sigma','b0_sigma','c0_sigma','a1','b1','c1'),
                                                       iter = 1000, seed=123, chains=1, chain_id=i, refresh=-1)))
