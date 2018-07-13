#' Run optimisation procedure to estimate true growth for each individual per census
#' 
#' Run optimisation procedure to estimate true growth for each individual per census
#' @param data Dataframe.
#' @return Stan fit object
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

run_true_dbh_model <- function(data) {
  
  stan_data <- list(
    n_obs = nrow(data),
    n_spp = length(unique(data$sp_id)),
    spp = data$sp_id,
    census = data$censusid,
    census_length = data$census_interval,
    obs_dbh1 = data$dbh_prev,
    obs_dbh2 = data$dbh)
  
  true_dbh_model <-'
  data {
  int<lower=1> n_obs;
  int<lower=1> n_spp;
  int<lower=1> spp[n_obs];
  int<lower=1> census[n_obs];
  vector[n_obs] census_length;
  vector[n_obs] obs_dbh1;
  vector[n_obs] obs_dbh2;
  }
  
  parameters {
  real<lower=0> true_dbh1[n_obs];
  real log_mu_true_dbh1;
  real<lower=0> log_sigma_true_dbh1;
  real<lower=0> true_growth_rate[n_obs];
  }
  
  model {
  real true_dbh2[n_obs];
  
  for (i in 1:n_obs) {
  // Calculating true growth for fitted data
  true_dbh2[i] = true_dbh1[i] + (true_growth_rate[i] * census_length[i]);
  }
  
  // Observation error
  obs_dbh1 ~ normal(true_dbh1, 0.75);
  obs_dbh2 ~ normal(true_dbh2, 0.75);
  
  // Priors
  true_dbh1 ~ lognormal(log_mu_true_dbh1, log_sigma_true_dbh1);
  true_growth_rate ~ lognormal(0,1); 
  }
  
  generated quantities {
  real true_dbh2[n_obs];
  
  // Recalculate true_dbh2
  for (i in 1:n_obs) {
  true_dbh2[i] = true_dbh1[i] + (true_growth_rate[i] * census_length[i]);
  }
  }'
  
  
  model <- stan_model(model_code = true_dbh_model)
  
  fit <- optimizing(model,
                    data = stan_data, iter=50000, refresh=1000)
}