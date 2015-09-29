
## NULL MODEL

get_model_chunks_null <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("log_hazard",
             "sum_log_lik_fit","sum_log_lik_heldout"),
    data = "
        int<lower=1> n_obs;
        int<lower=0, upper=1> y[n_obs];
        vector[n_obs] census_length;
        
        // Held out data
        int<lower=1> n_obs_heldout;
        int<lower=0, upper=1> y_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;
      ",
    parameters ="
      // Mortality model parameters
      real log_hazard;",
    model ="
      // Declaring mortality parameters
      real hazard;
      real cumulative_hazard;
      
      // Calculate fixed effects on normal scale
      hazard <- exp(log_hazard);
      
      for (i in 1:n_obs) {
        // Likelihood for hazard model
        cumulative_hazard <- -census_length[i] * hazard;
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      log_hazard ~ normal(-3.86, 0.2);",
    generated_quantities ="
      // Declaring fitted parameters
      real hazard;
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
      
      // recalulate fixed effects
      hazard <- exp(log_hazard);
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        
        cumulative_hazard_fit <- -census_length[i] * hazard;
        
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
        
        cumulative_hazard_heldout <- -census_length_heldout[j] * hazard;
        
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


## NULL MODEL WITH RANDOM EFFECTS

get_model_chunks_null_re <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("mu_log_hazard","sigma_log_hazard",
             "sum_log_lik_fit","sum_log_lik_heldout"),
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
      real raw_log_hazard[n_spp];
      real mu_log_hazard;
      real<lower=0> sigma_log_hazard;",
    model ="
      // Declaring mortality parameters
      real hazard[n_spp];
      real cumulative_hazard;
      
      // Calculating species random effects
      for (s in 1:n_spp) {
        hazard[s] <- exp(raw_log_hazard[s] * sigma_log_hazard + mu_log_hazard); // e.g. implies lognormal(mu_log_hazard, sigma_log_hazard)
      }
      
      for (i in 1:n_obs) {
        // Likelihood for hazard model
        cumulative_hazard <- -census_length[i] * hazard[spp[i]];
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      raw_log_hazard ~ normal(0, 1);
      mu_log_hazard ~ normal(-3.86, 0.2);
      sigma_log_hazard ~ cauchy(0, 2.5);",
    generated_quantities ="
      // Declaring fitted parameters
      real hazard[n_spp];
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
        hazard[s] <- exp(raw_log_hazard[s] * sigma_log_hazard + mu_log_hazard);
      }
      
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        
        cumulative_hazard_fit <- -census_length[i] * hazard[spp[i]];
        
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
        
        cumulative_hazard_heldout <- -census_length_heldout[j] * hazard[spp_heldout[j]];
        
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


## NO GAMMA MODEL
get_model_chunks_no_gamma <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("log_alpha",
             "log_beta",
             "sum_log_lik_fit","sum_log_lik_heldout"),
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
      real log_alpha;
      real log_beta;",
    model ="
      // Declaring mortality parameters
      real alpha;
      real beta;
      real cumulative_hazard;
      
      alpha <- exp(log_alpha);
      beta <- exp(log_beta);
      
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
      log_alpha ~ normal(-0.61, 0.57);
      log_beta ~ normal(0, 5);",
    generated_quantities ="
      // Declaring fitted parameters
      real alpha;
      real beta;
      
      real cumulative_hazard_fit;
      real log_lik_fit;
      real sum_log_lik_fit;
      
      real cumulative_hazard_heldout;
      real log_lik_heldout[n_obs_heldout];
      real sum_log_lik_heldout;
      
      // Initialization of summed log likelihoods
      sum_log_lik_fit <- 0;
      sum_log_lik_heldout <- 0;
      
      alpha <- exp(log_alpha);
      beta <- exp(log_beta);
      
      
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

## NO GAMMA MODEL WITH RANDOM EFFECTS
get_model_chunks_no_gamma_re <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("mu_log_alpha","sigma_log_alpha",
             "mu_log_beta","sigma_log_beta",
             "sum_log_lik_fit","sum_log_lik_heldout"),
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
      real<lower=0> sigma_log_beta;",
    model ="
      // Declaring mortality parameters
      real alpha[n_spp];
      real beta[n_spp];
      real cumulative_hazard;
      
      // Calculating species random effects
      for (s in 1:n_spp) {
        alpha[s] <- exp(raw_log_alpha[s] * sigma_log_alpha + mu_log_alpha); // e.g. implies lognormal(mu_log_alpha, sigma_log_alpha)
        beta[s] <- exp(raw_log_beta[s] * sigma_log_beta + mu_log_beta);
      }
      
      for (i in 1:n_obs) {
        // Likelihood for hazard model
        cumulative_hazard <- -census_length[i] * (alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]));
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      raw_log_alpha ~ normal(0,1);
      mu_log_alpha ~ normal(-0.61, 0.57);
      sigma_log_alpha ~ cauchy(0, 2.5);
      
      raw_log_beta ~ normal(0, 1);
      mu_log_beta ~ normal(0, 5);",
    generated_quantities ="
      // Declaring fitted parameters
      real alpha[n_spp];
      real beta[n_spp];
      
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
      }
      
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        
        cumulative_hazard_fit <- -census_length[i] * (alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]));
        
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
        
        cumulative_hazard_heldout <- -census_length_heldout[j] * (alpha[spp_heldout[j]] * exp(-beta[spp_heldout[j]] * growth_dt_heldout[j]));
        
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
  

## GROWTH COMPARISON MODEL

get_model_chunks_growth_comparison <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("mu_log_alpha","sigma_log_alpha",
             "mu_log_beta","sigma_log_beta",
             "mu_log_gamma","sigma_log_gamma",
             "sum_log_lik_fit","sum_log_lik_heldout"),
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
      mu_log_alpha ~ normal(-0.61, 0.57);
      sigma_log_alpha ~ cauchy(0, 2.5);
      
      raw_log_beta ~ normal(0, 1);
      mu_log_beta ~ normal(0, 5);
      sigma_log_beta ~ cauchy(0, 2.5);
      
      raw_log_gamma ~ normal(0, 1);
      mu_log_gamma ~ normal(-3.86, 0.2);
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
  

## RHO COMBINATION MODELS
get_model_chunks_rho_combinations <- function(tasks) {
  rho_combo <- tasks$rho_combo
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("mu_log_a0",if("a" %in% rho_combo) "a1",
             "mu_log_b0",if("b" %in% rho_combo) "b1",
             "mu_log_c0",if("c" %in% rho_combo) "c1",
             "sum_log_lik_fit","sum_log_lik_heldout"),
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
      real mu_log_a0;
      real mu_log_b0;
      real mu_log_c0;
      
      %s
      %s
      %s",
      ifelse("a" %in% rho_combo, "real a1;", ""),
      ifelse("b" %in% rho_combo, "real b1;", ""),
      ifelse("c" %in% rho_combo, "real c1;", "")),
    model = sprintf("
      // Declaring mortality parameters
      real alpha;
      real beta;
      real gamma;
      real cumulative_hazard;
      
      // Calculating species random effects
      
      for (i in 1:n_obs) {
        // Calculating mortality parameters
        alpha <- exp(mu_log_a0)%s; 
        beta <- exp(mu_log_b0)%s; 
        gamma <- exp(mu_log_c0)%s;
        
        // Likelihood for hazard model
        cumulative_hazard <- -census_length[i] * (alpha * exp(-beta * growth_dt[i]) + gamma);
        
        if (y[i] == 0) {
          increment_log_prob(cumulative_hazard);
        } else {
          increment_log_prob(log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      mu_log_a0 ~ normal(-0.61, 0.57);
      mu_log_b0 ~ normal(0, 5);
      mu_log_c0 ~ normal(-3.86, 0.2);
      
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
        alpha[s] <- exp(mu_log_a0)%s; 
        beta[s] <- exp(mu_log_b0)%s; 
        gamma[s] <- exp(mu_log_c0)%s;
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

