run_true_dbh_model <- function() {
  data <-make('BCI_model_dataset_unique')
  
  # Extract Sigma from dbh error model
  if(file.exists('results/true_dbh/true_dbh_model.rds')) {
    stop("true_dbh_model has already been run")
  }
  if (!file.exists('results/dbh_error/dbh_error_model.rds')) {
    warning('dbh error model must be run first')
    }
  else {
    error_mod <- readRDS('results/dbh_error/dbh_error_model.rds')
    error_mod@.MISC <- new.env(parent = globalenv())
  }
  sigma <- summary(error_mod, 'sigma')$summary
  sigma <- round(sigma[,'mean'],3)
  
  
  stan_data <- list(
    n_obs = nrow(data),
    census_length = data$census_interval,
    obs_dbh1 = data$dbh_prev,
    obs_dbh2 = data$dbh)
  
  true_dbh_model <-sprintf('
    data {
      int<lower=1> n_obs;
      vector[n_obs] census_length;
      vector[n_obs] obs_dbh1;
      vector[n_obs] obs_dbh2;
    }
    
    parameters {
      real<lower=0> true_dbh1[n_obs];
      real log_mu_true_dbh1;
      real<lower=0> log_sigma_true_dbh1;
      real<lower=0> dbh_increment[n_obs];
    }
    
    model {
      real true_dbh2[n_obs];
      
      for (i in 1:n_obs) {
        // Calculating true growth for fitted data
        true_dbh2[i] <- true_dbh1[i] + (dbh_increment[i] * census_length[i]);
        
        
      }
      
      // Observation error
      obs_dbh1 ~ normal(true_dbh1, %1$s);
      obs_dbh2 ~ normal(true_dbh2, %1$s);
      
      // Priors
      log_mu_true_dbh1 ~ normal(0,2.5);
      log_sigma_true_dbh1 ~ cauchy(0,2.5);
      true_dbh1 ~ lognormal(log_mu_true_dbh1, log_sigma_true_dbh1);
      dbh_increment ~ lognormal(0,1);
      
    }
    
    generated quantities {
      real true_dbh2[n_obs];
      
      for (i in 1:n_obs) {
        // recalculating true growth for fitted data
        true_dbh2[i] <- true_dbh1[i] + (dbh_increment[i] * census_length[i]);
      }
    }', sigma)
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  fit <- stan(model_code = true_dbh_model, 
              data= stan_data,
              pars = c("log_mu_true_dbh1","log_sigma_true_dbh1"), 
              chains = 3,
              iter = 1000, 
              control=list(adapt_delta=0.9,stepsize=0.05),
              refresh=1,
              thin=5)

  create_dirs('results/true_dbh')
  #fit@.MISC <- emptyenv()
  saveRDS(fit, 'results/true_dbh/true_dbh_model.rds')
}