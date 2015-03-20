library(rstan)
library(parallel)
source('R/make.bci.mainstem.R')
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
  dbh = bci.mainstem$dbh,
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
    vector[n_obs] dbh;
    vector[n_obs] dbh_dt;
    vector[n_spp] rho;
  }
transformed data { // centers and standardizes predictors
  vector[n_spp] cs_lnrho;
  vector[n_obs] cs_lndbh;
  vector[n_obs] s_dbh_dt;
  cs_lnrho <- (log(rho) - mean(log(rho)))/ (2*sd(log(rho)));
  cs_lndbh <- (log(dbh) - mean(log(dbh)))/ (2*sd(log(dbh)));
  s_dbh_dt <- dbh_dt/ (2*sd(dbh_dt)); 
}
parameters { // assumes uniform priors on all parameters
  real<lower=0> ho; // growth independent hazard
  real a0; // a_log intercept
  real b0; // b_log intercept
  real a1; // effect of rho on a_log
  real a2; // effect of dbh on a_log
  real b1; // effect of rho on b_log
  real b2; // effect of dbh on b_log
}  

transformed parameters {
  real<lower=0, upper=1> p[n_obs];
  real a_log[n_obs];
  real b_log[n_obs];
  
  for (i in 1:n_obs) { // Calculate a_log & b_log for each observation
    a_log[i] <- a0 + a1 * cs_lnrho[spp[i]] + a2 * cs_lndbh[i];
    b_log[i] <- b0 + b1 * cs_lnrho[spp[i]] + b2 * cs_lndbh[i];
    
    // Estimate Pr(Dying)
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * s_dbh_dt[i]) + ho)));
  }
}

model {
  // Sample Pr(Dying) from bernoulli
  y ~ bernoulli(p);
}
'
stan_model <- sflist2stanfit(mclapply(1:3,mc.cores=3,
                                      function(i) stan(model_code=StanModel, data=stan_data, 
                                                       pars = c('a0','b0','a1','a2','b1','b2','ho'),
                                                       iter = 2000, seed=123, chains=1, chain_id=i, refresh=-1)))
