get_chunks_model1_trait <- function() {
  list(
  pars = c("c0","c1"),
  parameters = "
  real c0;
  real c1; // effect of rho on c_log",
  transformed_parameters_declare = "
  real c_log;",
  transformed_parameters_assign = "
  c_log <- c0 + c1 * log_rho_cs[s];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log))));",
  model = ""
  r_model = function(stan_data, pars) {
    c_log <- pars$c0 + pars$c1 * stan_data$log_rho_cs
    inv_cloglog(log(stan_data$census_length * (exp(c_log))))
  }
  )
}

get_chunks_model2_trait <- function() {
  list(
  pars = c("a0","a1","b0","b1"),
  parameters = "
  real a0;
  real a1; // effect of rho on a_log

  real b0;
  real b1; // effect of rho on b_log",
  transformed_parameters_declare = "
  real a_log;
  real b_log;",
  transformed_parameters_assign = "
  a_log <- a0 + a1 * log_rho_cs[s];
  b_log <- b0 + b1 * log_rho_cs[s];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log - exp(b_log) * growth_dt_s[i]))));",
  model = ""
  r_model = function(stan_data, pars) {
    a_log <- pars$a0 + pars$a1 * stan_data$log_rho_cs
    b_log <- pars$b0 + pars$b1 * stan_data$log_rho_cs
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s))))
  }
  )
}

get_chunks_model3_trait <- function() {
  list(
  pars = c("a0","a1","b0","b1","c0","c1"),
  parameters = "
  real a0;
  real a1; // effect of rho on a_log

  real b0;
  real b1; // effect of rho on b_log

  real c0;
  real c1; // effect of rho on c_log",
  transformed_parameters_declare = "
  real a_log;
  real b_log;
  real c_log;",
  transformed_parameters_assign = "
  a_log <- a0 + a1 * log_rho_cs[s];
  b_log <- b0 + b1 * log_rho_cs[s];
  c_log <- c0 + c1 * log_rho_cs[s];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log - exp(b_log) * growth_dt_s[i]) + exp(c_log))));",
  model = ""
  r_model = function(stan_data, pars) {
    a_log <- pars$a0 + pars$a1 * stan_data$log_rho_cs
    b_log <- pars$b0 + pars$b1 * stan_data$log_rho_cs
    c_log <- pars$c0 + pars$c1 * stan_data$log_rho_cs
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s) + exp(c_log))))
  }
  )
}