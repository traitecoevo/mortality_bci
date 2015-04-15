get_chunks_model1_trait <- function() {
  list(
  pars = c("c0","c2"),
  parameters = "
  real c0;
  real c2;",
  transformed_parameters = "
  real c_log[n_spp];

  for (s in 1:n_spp) {
  c_log[s] <- c0 + c2 * log_rho_cs[s];
  }
  
  for (i in 1:n_obs) {
  p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log[spp[i]]))));
  }",
  model = "",
  r_model = function(stan_data, pars) {
    for (i in 1:stan_data$n_obs) {
    c_log[i] <- pars$c0 + pars$c2 * stan_data$log_rho_cs[stan_data$spp[i]]
    }
    inv_cloglog(log(stan_data$census_length * (exp(c_log))))
  }
  )
}

get_chunks_model2_trait <- function() {
  list(
  pars = c("a0","a2",
           "b0","b2"),
  parameters = "
  real a0;
  real a2;
  real b0;
  real b2;",
  transformed_parameters = "
  real a_log[n_spp];
  real b_log[n_spp];

  for (s in 1:n_spp) {
  a_log[s] <- a0 + a2 * log_rho_cs[s];
  b_log[s] <- b0 + b2 * log_rho_cs[s];
  }

  for (i in 1:n_obs) {
  p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[spp[i]] - exp(b_log[spp[i]]) * growth_dt_s[i]))));
  }",
  model = "",
  r_model = function(stan_data, pars) {
    for (i in 1:stan_data$n_obs) {
    a_log[i] <- pars$a0 + pars$a2 * stan_data$log_rho_cs[stan_data$spp[i]]
    b_log[i] <- pars$b0 + pars$b2 * stan_data$log_rho_cs[stan_data$spp[i]]
    }
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s))))
  }
  )
}

get_chunks_model3_trait <- function() {
  list(
  pars = c("a0","a2",
           "b0","b2",
           "c0","c2"),
  parameters = "
  real a0;
  real a2; // effect of rho on a_log

  real b0;
  real b2; // effect of rho on b_log

  real c0;
  real c2; // effect of rho on c_log",
  transformed_parameters= "
  real a_log[n_spp];
  real b_log[n_spp];
  real c_log[n_spp];
  
  for (s in 1:n_spp) {
    a_log[s] <- a0 + a2 * log_rho_cs[s];
    b_log[s] <- b0 + b2 * log_rho_cs[s];
    c_log[s] <- c0 + c2 * log_rho_cs[s];
  }

  for (i in 1:n_obs) {
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[spp[i]] - exp(b_log[spp[i]]) * growth_dt_s[i]) + exp(c_log[spp[i]]))));
  }",
  model = "",
  r_model = function(stan_data, pars) {
    for (i in 1:stan_data$n_obs) {
    a_log[i] <- pars$a0 + pars$a2 * stan_data$log_rho_cs[stan_data$spp[i]]
    b_log[i] <- pars$b0 + pars$b2 * stan_data$log_rho_cs[stan_data$spp[i]]
    c_log[i] <- pars$c0 + pars$c2 * stan_data$log_rho_cs[stan_data$spp[i]]
    }
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s) + exp(c_log))))
  }
  )
}