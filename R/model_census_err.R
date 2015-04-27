get_chunks_model1_census_err <- function() {
  list(
    pars = c("c1_raw","c1_mu","c1_sigma","c1"),
    parameters = "
    real c1_raw[n_census];
    real c1_mu;  
    real<lower=0> c1_sigma;",
    transformed_parameters = "
    real c1[n_census];
    real c_log[n_obs];
    
    for (t in 1:n_census) {
    c1[t] <- c1_raw[t] * c1_sigma + c1_mu;
    }
    
    for (i in 1:n_obs) {
    c_log[i] <- c1[census[i]];
    p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log[i]))));
    }",
    model = "
  for (t in 1:n_census) {
  c1_raw[t] ~ normal(0,1);
  }
  ",
    r_model = function(stan_data, pars) {
      for (i in 1:stan_data$n_obs) {
        c_log[i] <-pars$c1_mu
      }
      inv_cloglog(log(stan_data$census_length * (exp(c_log))))
    }
  )
}

get_chunks_model2_census_err <- function() {
  list(
    pars = c("a1_raw","a1_mu","a1_sigma","a1",
             "b1_raw","b1_mu","b1_sigma","b1"),
    parameters = "
    real a1_raw[n_census];
    real a1_mu;
    real<lower=0> a1_sigma;
    real b1_raw[n_census];
    real b1_mu;  
    real<lower=0> b1_sigma;",
    transformed_parameters = "
    real a1[n_census];
    real a_log[n_obs];
    
    real b1[n_census];
    real b_log[n_obs];
    
    for (t in 1:n_census) {
    a1[t] <- a1_raw[t] * a1_sigma + a1_mu;
    b1[t] <- b1_raw[t] * b1_sigma + b1_mu;
    }
    
    for (i in 1:n_obs) {
    a_log[i] <- a1[census[i]];
    b_log[i] <- b1[census[i]];
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * growth_dt_s[i]))));
    }",
    model = "
  
  for (t in 1:n_census) {
  a1_raw[t] ~ normal(0,1);
  b1_raw[t] ~ normal(0,1);
  }
  ",
    r_model = function(stan_data, pars) {
      for (i in 1:stan_data$n_obs) {
        a_log[i] <- pars$a1_mu
        b_log[i] <- pars$b1_mu
      }
      inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s))))
    }
  )
}

get_chunks_model3_census_err <- function() {
  list(
    pars = c("a1_raw","a1_mu","a1_sigma",
             "b1_raw","b1_mu","b1_sigma",
             "c1_raw","c1_mu","c1_sigma"),
    parameters = "
    real a1_raw[n_census];
    real a1_mu;
    real<lower=0> a1_sigma;
    real b1_raw[n_census];
    real b1_mu;  
    real<lower=0> b1_sigma;
    real c1_raw[n_census];
    real c1_mu;  
    real<lower=0> c1_sigma;",
    transformed_parameters = "
    real a1[n_census];
    real a_log[n_obs];
    
    real b1[n_census];
    real b_log[n_obs];
    
    real c1[n_census];
    real c_log[n_obs];
    
    for (t in 1:n_census) {
    a1[t] <- a1_raw[t] * a1_sigma + a1_mu;
    b1[t] <- b1_raw[t] * b1_sigma + b1_mu;
    c1[t] <- c1_raw[t] * c1_sigma + c1_mu;
    }
    
    for (i in 1:n_obs) {
    a_log[i] <- a1[census[i]];
    b_log[i] <- b1[census[i]];
    c_log[i] <- c1[census[i]];
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * growth_dt_s[i]) + exp(c_log[i]))));
    }",
    model = "
  for (t in 1:n_census) {
  a1_raw[t] ~ normal(0,1);
  b1_raw[t] ~ normal(0,1);
  c1_raw[t] ~ normal(0,1);
  }",
    r_model = function(stan_data, pars) {
      for (i in 1:stan_data$n_obs) {
        a_log[i] <- pars$a1_mu
        b_log[i] <- pars$b1_mu
        c_log[i] <- pars$c1_mu
      }
      inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s) + exp(c_log))))
    }
  )
}