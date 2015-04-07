
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


make_stan_model <- function(chunks) {
	list(
		pars = chunks$pars,
		model_code = sprintf('
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
  %s
}

transformed parameters {
  real<lower=0, upper=1> p[n_obs];

 %s

  for (s in 1:n_spp){ # Add species random effect & effects of rho
    %s
  }

  for (i in 1:n_obs) { // Estimate p
    %s
  }
}

model {
  // non-centered parameterization Papaspiliopoulos et al. (2007).
  // x_raw[s]implies normal(x_mu, x_sigma)

  for (s in 1:n_spp) {
    %s
  }
  // Sample Pr(Dying) from bernoulli
  y ~ bernoulli(p);
}
', chunks$parameters, chunks$transformed_parameters_declare, chunks$transformed_parameters_assign, chunks$transformed_parameters_p, chunks$model)
  )
}