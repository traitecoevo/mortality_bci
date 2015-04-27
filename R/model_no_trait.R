get_chunks_model1_no_trait <- function() {
  list(
    pars = c("c0_raw","c0_mu","c0_sigma",
             "c1_raw","c1_mu","c1_sigma"),
    parameters = "
    real c0_raw[n_spp];
    real c0_mu;
    real<lower=0> c0_sigma;
    
    real c1_raw[n_census];
    real c1_mu;  
    real<lower=0> c1_sigma;",
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
    c_log[i] <- c0[spp[i]] + c1[census[i]];
    p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log[i]))));
    }",
  model = "
  for (s in 1:n_spp) {
  c0_raw[s] ~ normal(0,1);
  }
  
  for (t in 1:n_census) {
  c1_raw[t] ~ normal(0,1);
  }
  ",
  r_model = function(stan_data, pars) {
    for (i in 1:stan_data$n_obs) {
      c_log[i] <- pars$c0_mu + pars$c1_mu
    }
    inv_cloglog(log(stan_data$census_length * (exp(c_log))))
  }
  )
  }

get_chunks_model2_no_trait <- function() {
  list(
    pars = c("a0_raw","a0_mu","a0_sigma",
             "a1_raw","a1_mu","a1_sigma",
             "b0_raw","b0_mu","b0_sigma",
             "b1_raw","b1_mu","b1_sigma"),
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
    real<lower=0> b1_sigma;",
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
    a_log[i] <- a0[spp[i]] + a1[census[i]];
    b_log[i] <- b0[spp[i]] + b1[census[i]];
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * growth_dt_s[i]))));
    }",
  model = "
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
      a_log[i] <- pars$a0_mu + pars$a1_mu
      b_log[i] <- pars$b0_mu + pars$b1_mu
    }
    inv_cloglog(log(stan_data$census_length * (exp(pars$a_log - exp(pars$b_log) * stan_data$growth_dt_s))))
  }
  )
  }

get_chunks_model3_no_trait <- function() {
  list(
    pars = c("a0_raw","a0_mu","a0_sigma",
             "a1_raw","a1_mu","a1_sigma",
             "b0_raw","b0_mu","b0_sigma",
             "b1_raw","b1_mu","b1_sigma",
             "c0_raw","c0_mu","c0_sigma",
             "c1_raw","c1_mu","c1_sigma"),
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
    real<lower=0> c1_sigma;",
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
    a_log[i] <- a0[spp[i]] + a1[census[i]];
    b_log[i] <- b0[spp[i]] + b1[census[i]];
    c_log[i] <- c0[spp[i]] + c1[census[i]];
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * growth_dt_s[i]) + exp(c_log[i]))));
    }",
  model = "
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
      a_log[i] <- pars$a0_mu + pars$a1_mu
      b_log[i] <- pars$b0_mu + pars$b1_mu
      c_log[i] <- pars$c0_mu + pars$c1_mu
    }
    inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s) + exp(c_log))))
}
  )
  }