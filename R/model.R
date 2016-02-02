## true growth model

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
        true_dbh2[i] <- true_dbh1[i] + (true_growth_rate[i] * census_length[i]);
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
      
      for (i in 1:n_obs) {
        // Calculating true growth for fitted data
        true_dbh2[i] <- true_dbh1[i] + (true_growth_rate[i] * census_length[i]);
      }
    }'

  
  model <- stan_model(model_code = true_dbh_model)
  
  fit <- optimizing(model,
              data = stan_data, iter=50000, refresh=1000)
  
  fit
}
## baseline hazard model

get_model_chunks_base_haz <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("gamma","log_lik_heldout",
             "sum_log_lik_fit","sum_log_lik_heldout"),
    data = "
        int<lower=1> n_obs;
        int<lower=0, upper=1> y[n_obs];
        vector[n_obs] census_length;
        
        // Held out data
        int<lower=1> n_obs_heldout;
        int<lower=0, upper=1> y_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;",
    parameters ="
      real<lower=0> gamma;",
    model ="
      real cumulative_hazard;
      
      for (i in 1:n_obs) {
        cumulative_hazard <- -census_length[i] * gamma;
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      // Priors
      gamma ~ lognormal(0, 1);",
    generated_quantities ="
      real cumulative_hazard_fit;
      real log_lik_fit;
      real sum_log_lik_fit;
      
      // Declaring heldout parameters
      real cumulative_hazard_heldout;
      real log_lik_heldout[n_obs_heldout];
      real sum_log_lik_heldout;
      
      // Initialization of summed log likelihoods
      sum_log_lik_fit <- 0;
      sum_log_lik_heldout <- 0;
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        
        cumulative_hazard_fit <- -census_length[i] * gamma;
        
        if (y[i] == 0) {
          log_lik_fit <- cumulative_hazard_fit;
        }
        else {
          log_lik_fit <- log1m_exp(cumulative_hazard_fit);
        }
        sum_log_lik_fit <- sum_log_lik_fit + log_lik_fit;
      }
      
      // log likelihood for held out data
      for (j in 1:n_obs_heldout) {
        
        cumulative_hazard_heldout <- -census_length_heldout[j] * gamma;
        
        if (y_heldout[j] == 0) {
          log_lik_heldout[j] <- cumulative_hazard_heldout;
        }
        else {
          log_lik_heldout[j] <- log1m_exp(cumulative_hazard_heldout);
        }
        sum_log_lik_heldout <- sum_log_lik_heldout + log_lik_heldout[j];
      }"
  )
}

## growth hazard model
get_model_chunks_growth_haz <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("alpha","beta",
             "log_lik_heldout","sum_log_lik_fit","sum_log_lik_heldout"),
    data = "
        int<lower=1> n_obs;
        int<lower=0, upper=1> y[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] growth_dt;
        
        // Held out data
        int<lower=1> n_obs_heldout;
        int<lower=0, upper=1> y_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;
        vector[n_obs_heldout] growth_dt_heldout;",
    parameters ="
      // Mortality model parameters
      real<lower=0> alpha;
      real<lower=0> beta;",
    model ="
      real cumulative_hazard;

      for (i in 1:n_obs) {
        // Likelihood for hazard model
        cumulative_hazard <- -census_length[i] * (alpha * exp(-beta * growth_dt[i]));
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      alpha ~ lognormal(0, 1);
      beta ~ lognormal(0, 2);",
    generated_quantities ="
      // Declaring fitted parameters
      real cumulative_hazard_fit;
      real log_lik_fit;
      real sum_log_lik_fit;
      
      real cumulative_hazard_heldout;
      real log_lik_heldout[n_obs_heldout];
      real sum_log_lik_heldout;
      
      // Initialization of summed log likelihoods
      sum_log_lik_fit <- 0;
      sum_log_lik_heldout <- 0;
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        
        cumulative_hazard_fit <- -census_length[i] * (alpha * exp(-beta * growth_dt[i]));
        
        if (y[i] == 0) {
          log_lik_fit <- cumulative_hazard_fit;
        }
        else {
          log_lik_fit <- log1m_exp(cumulative_hazard_fit);
        }
        sum_log_lik_fit <- sum_log_lik_fit + log_lik_fit;
      }
      
      // log likelihood for held out data
      for (j in 1:n_obs_heldout) {
        
        cumulative_hazard_heldout <- -census_length_heldout[j] * (alpha * exp(-beta * growth_dt_heldout[j]));
        
        if (y_heldout[j] == 0) {
          log_lik_heldout[j] <- cumulative_hazard_heldout;
        }
        else {
          log_lik_heldout[j] <- log1m_exp(cumulative_hazard_heldout);
        }
        sum_log_lik_heldout <- sum_log_lik_heldout + log_lik_heldout[j];
    }"
  )
}

## base_growth hazard model
get_model_chunks_base_growth_haz <- function(tasks) {
  rho_combo <- tasks$rho_combo
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("a0",if("a" %in% rho_combo) "a1",
             "b0",if("b" %in% rho_combo) "b1",
             "c0",if("c" %in% rho_combo) "c1",
             "log_lik_heldout","sum_log_lik_fit","sum_log_lik_heldout"),
    data ="
      int<lower=1> n_obs;
      int<lower=1> n_spp;
      int<lower=1> spp[n_obs];
      int<lower=0, upper=1> y[n_obs];
      vector[n_obs] census_length;
      vector[n_obs] growth_dt;
      vector[n_spp] rho_c;
    
      // Held out data
      int<lower=1> n_obs_heldout;
      int<lower=1> n_spp_heldout;
      int<lower=1> spp_heldout[n_obs_heldout];
      int<lower=0, upper=1> y_heldout[n_obs_heldout];
      vector[n_obs_heldout] census_length_heldout;
      vector[n_obs_heldout] growth_dt_heldout;
      vector[n_spp_heldout] rho_c_heldout;",
    parameters = sprintf("
      // Mortality model parameters
      real<lower=0> a0;
      real<lower=0> b0;
      real<lower=0> c0;
      
      %s
      %s
      %s",
      ifelse("a" %in% rho_combo, "real a1;", ""),
      ifelse("b" %in% rho_combo, "real b1;", ""),
      ifelse("c" %in% rho_combo, "real c1;", "")),
    model = sprintf("
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      real cumulative_hazard;

      for (s in 1:n_spp) {
        alpha[s] <- a0%s; 
        beta[s] <- b0%s; 
        gamma[s] <- c0%s;
      }
      for (i in 1:n_obs) {
        
        // Likelihood for hazard model
        cumulative_hazard <- -census_length[i] * (alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]);
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      a0 ~ lognormal(0, 1);
      b0 ~ lognormal(0, 2);
      c0 ~ lognormal(0, 1);
      
      %s
      %s
      %s",
      ifelse("a" %in% rho_combo, " * pow(rho_c[s], a1)", ""),
      ifelse("b" %in% rho_combo, " * pow(rho_c[s], b1)", ""),
      ifelse("c" %in% rho_combo, " * pow(rho_c[s], c1)", ""),
      ifelse("a" %in% rho_combo, "a1 ~ normal(0,5);", ""),
      ifelse("b" %in% rho_combo, "b1 ~ normal(0,5);", ""),
      ifelse("c" %in% rho_combo, "c1 ~ normal(0,5);", "")),
    generated_quantities = sprintf("
      // Declaring fitted parameters
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      
      real cumulative_hazard_fit;
      real log_lik_fit;
      real sum_log_lik_fit;
      
      // Declaring heldout parameters
      
      real cumulative_hazard_heldout;
      real log_lik_heldout[n_obs_heldout];
      real sum_log_lik_heldout;
      
      // Initialization of summed log likelihoods
      sum_log_lik_fit <- 0;
      sum_log_lik_heldout <- 0;
      
      // log likelihood for fitted model
      for (s in 1:n_spp) {
        alpha[s] <- a0%s; 
        beta[s] <- b0%s; 
        gamma[s] <- c0%s;
      }
      for (i in 1:n_obs) {
        
        cumulative_hazard_fit <- -census_length[i] * (alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]);
        
        if (y[i] == 0) {
          log_lik_fit <- cumulative_hazard_fit;
        }
        else {
          log_lik_fit <- log1m_exp(cumulative_hazard_fit);
        }
        sum_log_lik_fit <- sum_log_lik_fit + log_lik_fit;
      }
      
      // log likelihood for heldout data
      for (j in 1:n_obs_heldout) {
        
        cumulative_hazard_heldout <- -census_length_heldout[j] * (alpha[spp_heldout[j]] * exp(-beta[spp_heldout[j]] * growth_dt_heldout[j]) + gamma[spp_heldout[j]]);
        
        if (y_heldout[j] == 0) {
          log_lik_heldout[j] <- cumulative_hazard_heldout;
        }
        else {
          log_lik_heldout[j] <- log1m_exp(cumulative_hazard_heldout);
        }
        sum_log_lik_heldout <- sum_log_lik_heldout + log_lik_heldout[j];
      }",
      ifelse("a" %in% rho_combo, " * pow(rho_c[s], a1)", ""),
      ifelse("b" %in% rho_combo, " * pow(rho_c[s], b1)", ""),
      ifelse("c" %in% rho_combo, " * pow(rho_c[s], c1)", ""))
  )
}

## base_growth hazard model with species random effect

get_model_chunks_base_growth_haz_re <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("mu_log_alpha","sigma_log_alpha",
             "mu_log_beta","sigma_log_beta",
             "mu_log_gamma","sigma_log_gamma",
             "log_lik_heldout","sum_log_lik_fit","sum_log_lik_heldout"),
    data = "
        int<lower=1> n_obs;
        int<lower=1> n_spp;
        int<lower=1> spp[n_obs];
        int<lower=0, upper=1> y[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] growth_dt;
        
        // Held out data
        int<lower=1> n_obs_heldout;
        int<lower=1> spp_heldout[n_obs_heldout];
        int<lower=0, upper=1> y_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;
        vector[n_obs_heldout] growth_dt_heldout;",
    parameters ="
      // Mortality model parameters
      real raw_log_alpha[n_spp];
      real mu_log_alpha;
      real<lower=0> sigma_log_alpha;
      
      real raw_log_beta[n_spp];
      real mu_log_beta;
      real<lower=0> sigma_log_beta;
      
      real raw_log_gamma[n_spp];
      real mu_log_gamma;
      real<lower=0> sigma_log_gamma;",
    model ="
      // Declaring mortality parameters
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      real cumulative_hazard;
      
      // Calculating species random effects
      for (s in 1:n_spp) {
        alpha[s] <- exp(raw_log_alpha[s] * sigma_log_alpha + mu_log_alpha); // e.g. implies lognormal(mu_log_alpha, sigma_log_alpha)
        beta[s] <- exp(raw_log_beta[s] * sigma_log_beta + mu_log_beta);
        gamma[s] <- exp(raw_log_gamma[s] * sigma_log_gamma + mu_log_gamma);
      }
      
      for (i in 1:n_obs) {
        // Likelihood for hazard model
        cumulative_hazard <- -census_length[i] * (alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]);
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      raw_log_alpha ~ normal(0,1);
      mu_log_alpha ~ normal(0, 1);
      sigma_log_alpha ~ cauchy(0, 2.5);
      
      raw_log_beta ~ normal(0, 1);
      mu_log_beta ~ normal(0, 2);
      sigma_log_beta ~ cauchy(0, 2.5);
      
      raw_log_gamma ~ normal(0, 1);
      mu_log_gamma ~ normal(0, 1);
      sigma_log_gamma ~ cauchy(0, 2.5);",
    generated_quantities ="
      // Declaring fitted parameters
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      
      real cumulative_hazard_fit;
      real log_lik_fit;
      real sum_log_lik_fit;
      
      real cumulative_hazard_heldout;
      real log_lik_heldout[n_obs_heldout];
      real sum_log_lik_heldout;
      
      // Initialization of summed log likelihoods
      sum_log_lik_fit <- 0;
      sum_log_lik_heldout <- 0;
      
      // recalulate species random effects
      for (s in 1:n_spp) {
        alpha[s] <- exp(raw_log_alpha[s] * sigma_log_alpha + mu_log_alpha);
        beta[s] <- exp(raw_log_beta[s] * sigma_log_beta + mu_log_beta);
        gamma[s] <- exp(raw_log_gamma[s] * sigma_log_gamma + mu_log_gamma);
      }
      
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        cumulative_hazard_fit <- -census_length[i] * (alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]);
        
        if (y[i] == 0) {
          log_lik_fit <- cumulative_hazard_fit;
        }
        else {
          log_lik_fit <- log1m_exp(cumulative_hazard_fit);
        }
        sum_log_lik_fit <- sum_log_lik_fit + log_lik_fit;
      }
      
      // log likelihood for held out data
      for (j in 1:n_obs_heldout) {
        cumulative_hazard_heldout <- -census_length_heldout[j] * (alpha[spp_heldout[j]] * exp(-beta[spp_heldout[j]] * growth_dt_heldout[j]) + gamma[spp_heldout[j]]);
        
        if (y_heldout[j] == 0) {
          log_lik_heldout[j] <- cumulative_hazard_heldout;
        }
        else {
          log_lik_heldout[j] <- log1m_exp(cumulative_hazard_heldout);
        }
        sum_log_lik_heldout <- sum_log_lik_heldout + log_lik_heldout[j];
    }"
  )
}
  

## FINAL FULL FIT MODEL WITH RANDOM EFFECTS   (NOT HOOKED UP TO REST OF CODE YET)   
get_model_chunks_full_fit <- function(rho_combo) {
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }
  
  list(
    pars = c("mu_log_a0","sigma_log_a0","a0",if("a" %in% rho_combo) "a1",
             "mu_log_b0","sigma_log_b0","b0",if("b" %in% rho_combo) "b1",
             "mu_log_c0","sigma_log_c0","c0",if("c" %in% rho_combo) "c1",
             "sum_log_lik_fit"),
    data =" 
      int<lower=1> n_obs;
      int<lower=1> n_spp;
      int<lower=1> spp[n_obs];
      int<lower=0, upper=1> y[n_obs];
      vector[n_obs] census_length;
      vector[n_obs] growth_dt;
      vector[n_spp] rho_c;",
    parameters = sprintf("
      // Mortality model parameters
      real raw_log_a0[n_spp];
      real mu_log_a0;
      real<lower=0> sigma_log_a0;
      
      real raw_log_b0[n_spp];
      real mu_log_b0;
      real<lower=0> sigma_log_b0;
      
      real raw_log_c0[n_spp];
      real mu_log_c0;
      real<lower=0> sigma_log_c0;
      
      %s
      %s
      %s",
      ifelse("a" %in% rho_combo, "real a1;", ""),
      ifelse("b" %in% rho_combo, "real b1;", ""),
      ifelse("c" %in% rho_combo, "real c1;", "")),
    model = sprintf("
      // Declaring mortality parameters
      real alpha;
      real a0[n_spp];
      
      real beta;
      real b0[n_spp];
      
      real gamma;
      real c0[n_spp];
      
      real cumulative_hazard;
      
      // Calculating species random effects
      for (s in 1:n_spp) {
        a0[s] <- exp(raw_log_a0[s] * sigma_log_a0 + mu_log_a0); // e.g. implies lognormal(mu_log_a0, sigma_log_a0)
        b0[s] <- exp(raw_log_b0[s] * sigma_log_b0 + mu_log_b0);
        c0[s] <- exp(raw_log_c0[s] * sigma_log_c0 + mu_log_c0);
      }
      
      for (i in 1:n_obs) {
        // Calculating mortality parameters
        alpha <- a0[spp[i]]%s; 
        beta <- b0[spp[i]]%s; 
        gamma <- c0[spp[i]]%s;
        
        // Likelihood for hazard model
        cumulative_hazard <- -census_length[i] * (alpha * exp(-beta * growth_dt[i]) + gamma);
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      
      // Mortality model priors
      raw_log_a0 ~ normal(0,1);
      mu_log_a0 ~ normal(-0.61, 0.57);
      sigma_log_a0 ~ cauchy(0, 2.5);
      
      raw_log_b0 ~ normal(0, 1);
      mu_log_b0 ~ normal(0, 5);
      sigma_log_b0 ~ cauchy(0, 2.5);
      
      raw_log_c0 ~ normal(0, 1);
      mu_log_c0 ~ normal(-3.86, 0.2);
      sigma_log_c0 ~ cauchy(0, 2.5);
      
      %s
      %s
      %s",
      ifelse("a" %in% rho_combo, " * pow(rho_c[spp[i]], a1)", ""),
      ifelse("b" %in% rho_combo, " * pow(rho_c[spp[i]], b1)", ""),
      ifelse("c" %in% rho_combo, " * pow(rho_c[spp[i]], c1)", ""),
      ifelse("a" %in% rho_combo, "a1 ~ normal(0,5);", ""),
      ifelse("b" %in% rho_combo, "b1 ~ normal(0,5);", ""),
      ifelse("c" %in% rho_combo, "c1 ~ normal(0,5);", "")),
    generated_quantities = sprintf("
      // Declaring fitted parameters
      real a0[n_spp];
      real b0[n_spp];
      real c0[n_spp];
      
      real alpha;
      real beta;
      real gamma;
      
      real cumulative_hazard_fit;
      real log_lik_fit;
      real sum_log_lik_fit;
      
      // Initialization of summed log likelihoods
      sum_log_lik_fit <- 0;
      
      // recalulate species random effects
      for (s in 1:n_spp) {
        a0[s] <- exp(raw_log_a0[s] * sigma_log_a0 + mu_log_a0);
        b0[s] <- exp(raw_log_b0[s] * sigma_log_b0 + mu_log_b0);
        c0[s] <- exp(raw_log_c0[s] * sigma_log_c0 + mu_log_c0);
      }
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        alpha <- a0[spp[i]]%s; 
        beta <- b0[spp[i]]%s; 
        gamma <- c0[spp[i]]%s;
        
        cumulative_hazard_fit <- -census_length[i] * (alpha * exp(-beta * growth_dt[i]) + gamma);
        
        if (y[i] == 0) {
          log_lik_fit <- cumulative_hazard_fit;
        }
        else {
          log_lik_fit <- log1m_exp(cumulative_hazard_fit);
        }
        sum_log_lik_fit <- sum_log_lik_fit + log_lik_fit;
      }",
      ifelse("a" %in% rho_combo, " * pow(rho_c[spp[i]], a1)", ""),
      ifelse("b" %in% rho_combo, " * pow(rho_c[spp[i]], b1)", ""),
      ifelse("c" %in% rho_combo, " * pow(rho_c[spp[i]], c1)", ""))
  )
}

