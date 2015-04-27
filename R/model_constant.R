get_chunks_model1_constant <- function() {
  list(
    pars = c("c0"),
    parameters = "
    real c0;",
    transformed_parameters = "
    real c_log[n_obs];
    
    for (i in 1:n_obs) {
    c_log[i] <- c0;
    p[i] <- inv_cloglog(log(census_length[i] * (exp(c_log[i]))));
    }",
    model = "",
    r_model = function(stan_data, pars) {
      for (i in 1:stan_data$n_obs) {
        c_log[i] <-pars$c0
      }
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
    transformed_parameters = "
    real a_log[n_obs];
    real b_log[n_obs];
    
    for (i in 1:n_obs) {
    a_log[i] <- a0;
    b_log[i] <- b0;
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * growth_dt_s[i]))));
    }",
    model = "",
    r_model = function(stan_data, pars) {
      for (i in 1:stan_data$n_obs) {
        a_log[i] <- pars$a0
        b_log[i] <- pars$b0
      }
      inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s))))
    }
  )
}

get_chunks_model3_constant <- function() {
  list(
    pars = c("a0", "b0","c0"),
    parameters = "
    real a0;
    real b0;
    real c0;",
    transformed_parameters = "
    real a_log[n_obs];
    real b_log[n_obs];
    real c_log[n_obs];
    
    for (i in 1:n_obs) {
    a_log[i] <- a0;
    b_log[i] <- b0;
    c_log[i] <- c0;
    p[i] <- inv_cloglog(log(census_length[i] * (exp(a_log[i] - exp(b_log[i]) * growth_dt_s[i]) + exp(c_log[i]))));
    }",
    model = "",
    r_model = function(stan_data, pars) {
      for (i in 1:stan_data$n_obs) {
        a_log[i] <- pars$a0
        b_log[i] <- pars$b0
        c_log[i] <- pars$c0
      }
      inv_cloglog(log(stan_data$census_length * (exp(a_log - exp(b_log) * stan_data$growth_dt_s) + exp(c_log))))
    }
  )
}