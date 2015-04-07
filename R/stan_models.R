
combine_stan_chains <- function(..., d=list(...)) {
	sflist2stanfit(d)
 }

run_single_stan_chain <- function(model, data, chain_id, iter=1000, seed=123){
	stan(model_code=model$model_code, data=data,
       pars = model$pars,
       iter = iter, seed=seed, chains=1, chain_id=chain_id, refresh=-1)
}

stan_data_BCI <- function(data) {
  list(
    n_obs = nrow(data),
    n_spp = length(unique(data$sp)),
    spp = as.numeric(factor(data$sp), as.character(unique(data$sp))),
    rho =  unique(data$sg100c_avg)*1000, # converts wood density to kg/m2
    dbh_dt = data$dbh_dt,
    census_length = data$census_interval,
    y = as.integer(data$dead_next_census)
    )
}

make_stan_model <- function() {
	list(
		pars = c('a0_mu','b0_mu','c0_mu','a0_sigma','b0_sigma','c0_sigma','a1','b1','c1'),
		model_code = '
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
')
}