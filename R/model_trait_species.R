get_chunks_model1_trait_species <- function() {
  list(
  pars = c("c0_mu","c0_sigma","c0","c2"),
  parameters = "
  real c0_raw[n_spp];
  real c0_mu;
  real<lower=0> c0_sigma;
  real c2;",
  transformed_parameters = "
  real c0[n_spp];
  real c_log[n_spp];
  
  for (s in 1:n_spp) {
  c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
  c_log[s] <- c0[s] + c2 * log_rho_cs[s];
  }
  
  for (i in 1:n_obs) {
  p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log[spp[i]]))));
  }",
  model = "
  for (s in 1:n_spp) {
  c0_raw[s] ~ normal(0,1);
  }",
  r_model = function(stan_data, pars) {
    for (i in 1:stan_data$n_obs) {
    c_log[i] <- pars$c0[stan_data$spp[i]] + pars$c2 * stan_data$log_rho_cs[stan_data$spp[i]]
    }
    inv_cloglog(log(stan_data$census_length * (exp(c_log))))
  }
  )
}

get_chunks_model2_trait_species <- function() {
  list(
  pars = c("a0_mu","a0_sigma","a0","a2",
           "b0_mu","b0_sigma","b0","b2"),
  parameters = "
  real a0_raw[n_spp];
  real a0_mu; 
  real<lower=0> a0_sigma;
  real a2;

  real b0_raw[n_spp];
  real b0_mu; 
  real<lower=0> b0_sigma;
  real b2;",
  transformed_parameters= "

  real a0[n_spp];
  real a_log[n_spp];

  real b0[n_spp];
  real b_log[n_spp];

  for (s in 1:n_spp) {
  a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
  a_log[s] <- a0[s] + a2 * log_rho_cs[s];

  b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
  b_log[s] <- b0[s] + b2 * log_rho_cs[s];
  }
  
  for (i in 1:n_obs) {
  p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[spp[i]] - exp(b_log[spp[i]]) * growth_dt_s[i]))));
  }",
  model = "
  for (s in 1:n_spp) {
  a0_raw[s] ~ normal(0,1);
  b0_raw[s] ~ normal(0,1);
  }",
  r_model = function(stan_data, pars) {
    for (i in 1:stan_data$n_obs) {
    a_log[i] <- pars$a0 + pars$a2[stan_data$spp[i]] * stan_data$log_rho_cs[stan_data$spp[i]]
    b_log[i] <- pars$b0 + pars$b2[stan_data$spp[i]] * stan_data$log_rho_cs[stan_data$spp[i]]
    }
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s))))
  }
  )
}

get_chunks_model3_trait_species <- function() {
  list(
  pars = c("a0_mu","a0_sigma","a0","a2",
           "b0_mu","b0_sigma","b0","b2",
           "c0_mu","c0_sigma","c0","c2"),
  parameters = "
  real a0_raw[n_spp];
  real a0_mu;
  real<lower=0> a0_sigma;
  real a2; 

  real b0_raw[n_spp];
  real b0_mu;  
  real<lower=0> b0_sigma;
  real b2;

  real c0_raw[n_spp];
  real c0_mu;
  real<lower=0> c0_sigma;
  real c2;",
  transformed_parameters = "
  real a0[n_spp];
  real a_log[n_spp];

  real b0[n_spp];
  real b_log[n_spp];
  
  real c0[n_spp];
  real c_log[n_spp];
  
  for (s in 1:n_spp) {
  a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
  a_log[s] <- a0[s] + a2 * log_rho_cs[s];

  b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
  b_log[s] <- b0[s] + b2 * log_rho_cs[s];

  c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
  c_log[s] <- c0[s] + c2 * log_rho_cs[s];
  }
  
  for (i in 1:n_obs) {
  p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[spp[i]] - exp(b_log[spp[i]]) * growth_dt_s[i]) + exp(c_log[spp[i]]))));
  }",
  model = "
  for (s in 1:n_spp) {
  a0_raw[s] ~ normal(0,1);
  b0_raw[s] ~ normal(0,1);
  c0_raw[s] ~ normal(0,1);
  }",
  r_model = function(stan_data, pars) {
    for (i in 1:stan_data$n_obs) {
    a_log[i] <- pars$a0[stan_data$spp[i]] + pars$a2 * stan_data$log_rho_cs[stan_data$spp[i]]
    b_log[i] <- pars$b0[stan_data$spp[i]] + pars$b2 * stan_data$log_rho_cs[stan_data$spp[i]]
    c_log[i] <- pars$c0[stan_data$spp[i]] + pars$c2 * stan_data$log_rho_cs[stan_data$spp[i]]
    }
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s) + exp(c_log))))
  }
  )
}
