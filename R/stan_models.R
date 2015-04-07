
combine_stan_chains <- function(..., d=list(...)) {
	sflist2stanfit(d)
 }

run_single_stan_chain <- function(model, data, chain_id, iter=1000, seed=123){
	stan(model_code=model$model_code, data=data,
       pars = model$pars,
       iter = iter, seed=seed, chains=1, chain_id=chain_id, refresh=-1)
}

stan_data_BCI <- function(data, growth_measure = "dbh_dt") {
  list(
    n_obs = nrow(data),
    n_spp = length(unique(data$sp)),
    spp = as.numeric(factor(data$sp), as.character(unique(data$sp))),
    rho =  unique(data$rho),
    growth_dt = data[[growth_measure]],
    census_length = data$census_interval,
    y = as.integer(data$dead_next_census)
    )
}


make_stan_model <- function() {
	list(
		pars = c('a0_mu','a0_sigma','a1','b0_mu','b0_sigma','b1','c0_mu','c0_sigma','c1'),
		model_code = '
data {
  int<lower=0> n_obs;
  int<lower=0, upper=1> y[n_obs];
  int<lower=0> n_spp;
  int<lower=1> spp[n_obs];
  vector[n_obs] census_length;
  vector[n_obs] growth_dt;
  vector[n_spp] rho;
}
transformed data { // centers and standardizes predictors
  vector[n_obs] growth_dt_s;
  vector[n_spp] log_rho_cs;

  growth_dt_s <- growth_dt / (2*sd(growth_dt));
  log_rho_cs  <- (log(rho) - mean(log(rho)))/ (2*sd(log(rho)));
}
parameters { // assumes uniform priors on all parameters
  real a0_raw[n_spp];
  real a1; // effect of rho on a_log
  real a0_mu; // a_log effect for average species
  real<lower=0> a0_sigma; // a_log effect species variation

  real b0_raw[n_spp];
  real b1; // effect of rho on b_log
  real b0_mu;  // b_log effect for average species
  real<lower=0> b0_sigma; // b_log effect species variation

  real c0_raw[n_spp];
  real c1; // effect of rho on c_log
  real c0_mu;  // c_log effect for average species
  real<lower=0> c0_sigma; // c_log effect species variation
}

transformed parameters {
  real<lower=0, upper=1> p[n_obs];

  real a_log[n_spp];
  real a0[n_spp];

  real b_log[n_spp];
  real b0[n_spp];

  real c_log[n_spp];
  real c0[n_spp];

  for (s in 1:n_spp){ # Add species random effect & effects of rho
    a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
    a_log[s] <- a0[s];

    b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
    b_log[s] <- b0[s];

    c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
    c_log[s] <- c0[s];
  }

  for (i in 1:n_obs) { // Estimate p
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[spp[i]] - exp(b_log[spp[i]]) * growth_dt_s[i]) + exp(c_log[spp[i]]))));
  }
}

model {
  // non-centered parameterization Papaspiliopoulos et al. (2007).
  // x_raw[s]implies normal(x_mu, x_sigma)

  for (s in 1:n_spp) {
    a0_raw[s] ~ normal(0,1);
    b0_raw[s] ~ normal(0,1);
    c0_raw[s] ~ normal(0,1);
  }
  // Sample Pr(Dying) from bernoulli
  y ~ bernoulli(p);
}
')
}