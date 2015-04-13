
combine_stan_chains <- function(..., d=list(...), tmp=NULL) {
	sflist2stanfit(d)
 }

run_single_stan_chain <- function(model, data, chain_id, iter=1000,
  sample_file=NA, diagnostic_file=NA) {

  data_for_stan <- prep_data_for_stan(data, model$growth_measure)
	stan(model_code = model$model_code,
       data = data_for_stan,
       pars = model$pars,
       iter = iter,
       chains=1, chain_id=chain_id,
       refresh=-1, # What is this?
       sample_file=sample_file,
       diagnostic_file=diagnostic_file)
}

prep_data_for_stan <- function(data, growth_measure = "dbh_dt") {

  growth_data <- data[[growth_measure]]
  rho = unique(data$rho)

  list(
    n_obs = nrow(data),
    n_spp = length(unique(data$sp)),
    spp = as.numeric(factor(data$sp)),
    y = as.integer(data$dead_next_census),
    census_length = data$census_interval,
    growth_dt = growth_data,
    growth_dt_s = growth_data / (2*sd(growth_data)),
    rho =  rho,
    log_rho_cs  = (log(rho) - mean(log(rho)))/ (2*sd(log(rho)))
    )
}


make_stan_model <- function(chunks, growth_measure= "dbh_dt") {
	list(
		pars = chunks$pars,
    growth_measure = growth_measure,
		model_code = sprintf('
data {
  int<lower=0> n_obs;
  int<lower=0, upper=1> y[n_obs];
  int<lower=0> n_spp;
  int<lower=1> spp[n_obs];
  vector[n_obs] census_length;
  vector[n_obs] growth_dt_s;
  vector[n_spp] log_rho_cs;
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

# define accessory functions - equivalenets to those used in stan
inv_cloglog <- function(x) {1 - exp(-exp(x))}

bernoulli_log <- function(y, p) {
  dbinom(y, size=1, p, log = TRUE)
}
