get_chunks_model1_constant <- function() {
  list(
  pars = c("c0"),
  parameters = "
  real c0;",
  transformed_parameters_declare = "
  real c_log;",
  transformed_parameters_assign = "
  c_log <- c0;",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log))));",
  model = "",
  r_model = function(stan_data, pars) {
    c_log <- pars$c0
    inv_cloglog(log(stan_data$census_length * (exp(c_log))))
  }
  )
}

get_chunks_model2_constant <- function() {
  list(
  pars = c("a0","b0"),
  parameters = "
  real a0;
  real b0;",
  transformed_parameters_declare = "
  real a_log;
  real b_log;",
  transformed_parameters_assign = "
  a_log <- a0;
  b_log <- b0;",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log - exp(b_log) * growth_dt_s[i]))));",
  model = "",
  r_model = function(stan_data, pars) {
    a_log <- pars$a0
    b_log <- pars$b0
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s))))
  }
  )
}

get_chunks_model3_constant <- function() {
  list(
  pars = c("a0","b0","c0"),
  parameters = "
  real a0;
  real b0;
  real c0;",
  transformed_parameters_declare = "
  real a_log;
  real b_log;
  real c_log;",
  transformed_parameters_assign = "
  a_log <- a0;
  b_log <- b0;
  c_log <- c0;",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log - exp(b_log) * growth_dt_s[i]) + exp(c_log))));",
  model = "",
  r_model = function(stan_data, pars) {
    a_log <- pars$a0
    b_log <- pars$b0
    c_log <- pars$c0
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s) + exp(c_log))))
  }
  )
}