get_chunks_model1_full <- function() {
  list(
    pars = c("c0_raw","c0_mu","c0_sigma",
             "c1_raw","c1_mu","c1_sigma",
             "c2"),
    parameters = "
  real c0_raw[n_spp];
  real c0_mu;
  real<lower=0> c0_sigma;

  real c1_raw[n_census];
  real c1_mu;  
  real<lower=0> c1_sigma;

  real c2; // effect of rho on c_log",
    
    transformed_parameters = "
  real c0[n_spp];
  real c1[n_census];
  real c_log[n_obs];

  for (s in 1:n_spp) {
  c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
  }

  for (t in 1:n_census) {
  c1[t] <- c1_raw[t] * c1_sigma + c1_mu;
  }

  for (i in 1:n_obs) {
  c_log[i] <- c0[spp[i]] + c1[census[i]] + c2 * log_rho_cs[spp[i]];
  p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log[i]))));
  }",
    model = "
  c0_mu ~ cauchy(0, 10);
  c0_sigma ~ cauchy(0, 25);

  c1_mu ~ cauchy(0, 10);
  c1_sigma ~ cauchy(0, 25);

  c2 ~ cauchy(0, 10);

  for (s in 1:n_spp) {
  c0_raw[s] ~ normal(0,1);
  }
  
  for (t in 1:n_census) {
  c1_raw[t] ~ normal(0,1);
  }
  ",
    r_model = function(stan_data, pars) {
      for (i in 1:stan_data$n_obs) {
        c_log[i] <- pars$c0_mu + pars$c1_mu + pars$c2 * stan_data$log_rho_cs[stan_data$spp[i]]
      }
      inv_cloglog(log(stan_data$census_length * (exp(c_log))))
    }
  )
}

get_chunks_model2_full <- function() {
  list(
    pars = c("a0_raw","a0_mu","a0_sigma","a1_mu","a1_sigma","a2",
             "b0_raw","b0_mu","b0_sigma","b1_mu","b1_sigma","b2"),
    parameters = "
  real a0_raw[n_spp];
  real a0_mu;
  real<lower=0> a0_sigma;

  real a1_raw[n_census];
  real a1_mu;
  real<lower=0> a1_sigma;

  real b0_raw[n_spp];
  real b0_mu;  
  real<lower=0> b0_sigma;

  real b1_raw[n_census];
  real b1_mu;  
  real<lower=0> b1_sigma;

  real a2;
  real b2;",
    transformed_parameters = "
  real a0[n_spp];
  real a1[n_census];
  real a_log[n_obs];

  real b0[n_spp];
  real b1[n_census];
  real b_log[n_obs];

  for (s in 1:n_spp) {
  a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
  b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
  }
  
  for (t in 1:n_census) {
  a1[t] <- a1_raw[t] * a1_sigma + a1_mu;
  b1[t] <- b1_raw[t] * b1_sigma + b1_mu;
  }

  for (i in 1:n_obs) {
  a_log[i] <- a0[spp[i]] + a1[census[i]] + a2 * log_rho_cs[spp[i]];
  b_log[i] <- b0[spp[i]] + b1[census[i]] + b2 * log_rho_cs[spp[i]];
  p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * growth_dt_s[i]))));
  }",
    model = "
  a0_mu ~ cauchy(0, 10);
  a0_sigma ~ cauchy(0, 25);
  b0_mu ~ cauchy(0, 10);
  b0_sigma ~ cauchy(0, 25);

  a1_mu ~ cauchy(0, 10);
  a1_sigma ~ cauchy(0, 25);
  b1_mu ~ cauchy(0, 10);
  b1_sigma ~ cauchy(0, 25);

  a2 ~ cauchy(0, 10);
  b2 ~ cauchy(0, 10);

  for (s in 1:n_spp) {
  a0_raw[s] ~ normal(0,1);
  b0_raw[s] ~ normal(0,1);
  }
  
  for (t in 1:n_census) {
  a1_raw[t] ~ normal(0,1);
  b1_raw[t] ~ normal(0,1);
  }
  ",
    r_model = function(stan_data, pars) {
      for (i in 1:stan_data$n_obs) {
        a_log[i] <- pars$a0_mu + pars$a1_mu + pars$a2 * stan_data$log_rho_cs[stan_data$spp[i]]
        b_log[i] <- pars$b0_mu + pars$b1_mu + pars$b2 * stan_data$log_rho_cs[stan_data$spp[i]]
      }
      inv_cloglog(log(stan_data$census_length * (exp(pars$a_log - exp(pars$b_log) * stan_data$growth_dt_s))))
    }
  )
}

get_chunks_model3_full <- function() {
  list(
    pars = c("a0_raw","a0_mu","a0_sigma","a1_mu","a1_sigma","a2",
             "b0_raw","b0_mu","b0_sigma","b1_mu","b1_sigma","b2",
             "c0_raw","c0_mu","c0_sigma","c1_mu","c1_sigma","c2"),
    parameters = "
  real a0_raw[n_spp];
  real a0_mu;
  real<lower=0> a0_sigma;
  
  real a1_raw[n_census];
  real a1_mu;
  real<lower=0> a1_sigma;
  
  real b0_raw[n_spp];
  real b0_mu;  
  real<lower=0> b0_sigma;
  
  real b1_raw[n_census];
  real b1_mu;  
  real<lower=0> b1_sigma;

  real c0_raw[n_spp];
  real c0_mu;  
  real<lower=0> c0_sigma;
  
  real c1_raw[n_census];
  real c1_mu;  
  real<lower=0> c1_sigma;
  
  real a2;
  real b2;
  real c2;",
    transformed_parameters = "
  real a0[n_spp];
  real a1[n_census];
  real a_log[n_obs];
  
  real b0[n_spp];
  real b1[n_census];
  real b_log[n_obs];
  
  real c0[n_spp];
  real c1[n_census];
  real c_log[n_obs];

  for (s in 1:n_spp) {
  a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
  b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
  c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
  }

  for (t in 1:n_census) {
  a1[t] <- a1_raw[t] * a1_sigma + a1_mu;
  b1[t] <- b1_raw[t] * b1_sigma + b1_mu;
  c1[t] <- c1_raw[t] * c1_sigma + c1_mu;
  }

  for (i in 1:n_obs) {
  a_log[i] <- a0[spp[i]] + a1[census[i]] + a2 * log_rho_cs[spp[i]];
  b_log[i] <- b0[spp[i]] + b1[census[i]] + b2 * log_rho_cs[spp[i]];
  c_log[i] <- c0[spp[i]] + c1[census[i]] + c2 * log_rho_cs[spp[i]];
  p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * growth_dt_s[i]) + exp(c_log[i]))));
  }",
    model = "
  a0_mu ~ cauchy(0, 10);
  a0_sigma  ~ cauchy(0, 10);
  b0_mu ~ cauchy(0, 10);
  b0_sigma  ~ cauchy(0, 10);
  c0_mu  ~ cauchy(0, 10);
  c0_sigma  ~ cauchy(0, 10);

  a1_mu  ~ cauchy(0, 10);
  a1_sigma  ~ cauchy(0, 10);
  b1_mu  ~ cauchy(0, 10);
  b1_sigma  ~ cauchy(0, 10);
  c1_mu  ~ cauchy(0, 10);
  c1_sigma ~ cauchy(0, 10);
  
  a2 ~ cauchy(0, 2.5);
  b2 ~ cauchy(0, 2.5);
  c2 ~ cauchy(0, 2.5);

  for (s in 1:n_spp) {
  a0_raw[s] ~ normal(0,1);
  b0_raw[s] ~ normal(0,1);
  c0_raw[s] ~ normal(0,1);
  }

  for (t in 1:n_census) {
  a1_raw[t] ~ normal(0,1);
  b1_raw[t] ~ normal(0,1);
  c1_raw[t] ~ normal(0,1);
  }",
    r_model = function(stan_data, pars) { # Still need to tweak this
      for (i in 1:stan_data$n_obs) {
        a_log[i] <- pars$a0_mu + pars$a1_mu + pars$a2 * stan_data$log_rho_cs[stan_data$spp[i]]
        b_log[i] <- pars$b0_mu + pars$b1_mu + pars$b2 * stan_data$log_rho_cs[stan_data$spp[i]]
        c_log[i] <- pars$c0_mu + pars$c1_mu + pars$c2 * stan_data$log_rho_cs[stan_data$spp[i]]
      }
      inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s) + exp(c_log))))
    }
  )
}

