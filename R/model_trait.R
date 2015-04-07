get_chunks_model1_trait <- function() {
  list(
  pars = c("c0","c1"),
  parameters = "
  real c0;
  real c1; // effect of rho on c_log",
  transformed_parameters_declare = "
  real c_log;",
  transformed_parameters_assign = "
  c_log <- c0 + c1 * log_rho_cs[n_spp];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log))));",
  model = ""
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
  a_log <- a0 + a1 * log_rho_cs[n_spp];
  b_log <- b0 + b1 * log_rho_cs[n_spp];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log - exp(b_log) * growth_dt_s[i]))));",
  model = ""
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
  a_log <- a0 + a1 * log_rho_cs[n_spp];
  b_log <- b0 + b1 * log_rho_cs[n_spp];
  c_log <- c0 + c1 * log_rho_cs[n_spp];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log - exp(b_log) * growth_dt_s[i]) + exp(c_log))));",
  model = ""
  )
}