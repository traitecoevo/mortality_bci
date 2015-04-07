get_chunks_model1_trait_species <- function() {
  list(
  pars = c("c0_mu","c0_sigma","c1"),
  parameters = "
  real c0_raw[n_spp];
  real c1; // effect of rho on c_log
  real c0_mu;  // c_log effect for average species
  real<lower=0> c0_sigma; // c_log effect species variation",
  transformed_parameters_declare = "
  real c_log[n_spp];
  real c0[n_spp];",
  transformed_parameters_assign = "
  c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
  c_log[s] <- c0[s]+ c1 * log_rho_cs[s];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log[spp[i]]))));",
  model = "
  c0_raw[s] ~ normal(0,1);"
  )
}

get_chunks_model2_trait_species <- function() {
  list(
  pars = c("a0_mu","a0_sigma","a1","b0_mu","b0_sigma","b1"),
  parameters = "
  real a0_raw[n_spp];
  real a1; // effect of rho on a_log
  real a0_mu; // a_log effect for average species
  real<lower=0> a0_sigma; // a_log effect species variation

  real b0_raw[n_spp];
  real b1; // effect of rho on b_log
  real b0_mu;  // b_log effect for average species
  real<lower=0> b0_sigma; // b_log effect species variation",
  transformed_parameters_declare = "
  real a_log[n_spp];
  real a0[n_spp];

  real b_log[n_spp];
  real b0[n_spp];",
  transformed_parameters_assign = "
  a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
  a_log[s] <- a0[s] + a1 * log_rho_cs[s];

  b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
  b_log[s] <- b0[s]+ b1 * log_rho_cs[s];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[spp[i]] - exp(b_log[spp[i]]) * growth_dt_s[i]))));",
  model = "
  a0_raw[s] ~ normal(0,1);
  b0_raw[s] ~ normal(0,1);"
  )
}

get_chunks_model3_trait_species <- function() {
  list(
  pars = c("a0_mu","a0_sigma","a1","b0_mu","b0_sigma","b1","c0_mu","c0_sigma","c1"),
  parameters = "
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
  real<lower=0> c0_sigma; // c_log effect species variation",
  transformed_parameters_declare = "
  real a_log[n_spp];
  real a0[n_spp];

  real b_log[n_spp];
  real b0[n_spp];

  real c_log[n_spp];
  real c0[n_spp];",
  transformed_parameters_assign = "
  a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
  a_log[s] <- a0[s] + a1 * log_rho_cs[s];

  b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
  b_log[s] <- b0[s]+ b1 * log_rho_cs[s];

  c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
  c_log[s] <- c0[s]+ c1 * log_rho_cs[s];",
  transformed_parameters_p = "p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[spp[i]] - exp(b_log[spp[i]]) * growth_dt_s[i]) + exp(c_log[spp[i]]))));",
  model = "
  a0_raw[s] ~ normal(0,1);
  b0_raw[s] ~ normal(0,1);
  c0_raw[s] ~ normal(0,1);"
  )
}

