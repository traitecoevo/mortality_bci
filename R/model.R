## true growth model

allowed_tasks <- function() {
  c("null_model", "function_growth_comparison","rho_combinations", "gap_combinations", "size_combinations","species_random_effects",
    "multi_trait_all","multi_trait_parsimony","rho_gap_all", "rho_size_all", "gap_size_all", "final_model","final_base_growth_hazard_re")
}

check_task_is_allowed <- function(task_name) {
  if(! task_name %in% allowed_tasks())
  stop(sprintf("invalid task name: %s, must be one of %s", task_name, paste(allowed_tasks(), collapse = ", ")))
}

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

# Null Model
get_model_chunks_null <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("log_gamma",
             "logloss_heldout"),
    data = "
        // Fitted data
        int<lower=1> n_obs;
        vector[n_obs] census_length;
        int<lower=0, upper=1> y[n_obs];
        
        // Heldout data
        int<lower=1> n_obs_heldout;
        vector[n_obs_heldout] census_length_heldout;
        int<lower=0, upper=1> y_heldout[n_obs_heldout];",
    parameters ="
      real log_gamma;",
    model ="
      real cumulative_hazard;
      real gamma;
      
      // Put gamma on normal scale
      gamma = exp(log_gamma);

      for (i in 1:n_obs) {
        cumulative_hazard = -census_length[i] * gamma;
        
        if (y[i] == 0) {
          target += (cumulative_hazard);
        } else {
          target += (log1m_exp(cumulative_hazard));
        }
      }
      // Priors
      log_gamma ~ normal(0, 2.5);",
    generated_quantities ="
      real gamma;
      
      // Declaring heldout
      real cumulative_hazard_heldout;
      real loglik_heldout;
      real sum_loglik_heldout;
      real logloss_heldout;

      // Initialization of summed parameter
      sum_loglik_heldout = 0;

      // Put normal on normal scale;
      gamma = exp(log_gamma);
      
      // Calculate log likelihood for heldout data
      for (j in 1:n_obs_heldout) {
        cumulative_hazard_heldout = -census_length_heldout[j] * gamma;
        
        if (y_heldout[j] == 0) {
          loglik_heldout = cumulative_hazard_heldout;
        }
        else {
          loglik_heldout = log1m_exp(cumulative_hazard_heldout);
        }
        sum_loglik_heldout = sum_loglik_heldout + loglik_heldout;
      }
        // Calculation of average negative log likelihood
        logloss_heldout = -sum_loglik_heldout/n_obs_heldout;"
  )
}


## baseline hazard model

get_model_chunks_base_haz <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("log_gamma","sigma_log_census_err","census_err",
             "logloss_heldout"),
    data = "
        // Fitted data
        int<lower=1> n_obs;
        int<lower=1> n_census;
        int<lower=0> census[n_obs];
        vector[n_obs] census_length;
        int<lower=0, upper=1> y[n_obs];
        
        // Heldout data
        int<lower=1> n_obs_heldout;
        int<lower=1> n_census_heldout;
        int<lower=1> census_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;
        int<lower=0, upper=1> y_heldout[n_obs_heldout];",
    parameters ="
      real log_gamma;
      real raw_log_census_err[n_census];
      real<lower=0> sigma_log_census_err;",
    model ="
      real cumulative_hazard;
      real census_err[n_census];
      real gamma;
      
      // Put gamma on normal scale
      gamma = exp(log_gamma);

      // Calculate census random effects
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err); 
      // non-centered parameterisation implies lognormal(0, sigma_log_alpha);
      }

      for (i in 1:n_obs) {
        cumulative_hazard = -census_length[i] * (gamma * census_err[census[i]]);
        
        if (y[i] == 0) {
          target += (cumulative_hazard);
        } else {
          target += (log1m_exp(cumulative_hazard));
        }
      }
      // Priors
      log_gamma ~ normal(0, 2.5);
      raw_log_census_err ~ normal(0,1);
      sigma_log_census_err ~ cauchy(0,2.5);",
    generated_quantities ="
      real gamma;
      real census_err[n_census];
      
      // Declaring heldout
      real cumulative_hazard_heldout;
      real loglik_heldout;
      real sum_loglik_heldout;
      real logloss_heldout;

      // Initialization of summed parameter
      sum_loglik_heldout = 0;

      // Put normal on normal scale;
      gamma = exp(log_gamma);

      // Recalculate census random effects      
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }
      
      // Calculate log likelihood for heldout data
      for (j in 1:n_obs_heldout) {
        cumulative_hazard_heldout = -census_length_heldout[j] * (gamma * census_err[census_heldout[j]]);
        
        if (y_heldout[j] == 0) {
          loglik_heldout = cumulative_hazard_heldout;
        }
        else {
          loglik_heldout = log1m_exp(cumulative_hazard_heldout);
        }
        sum_loglik_heldout = sum_loglik_heldout + loglik_heldout;
      }
        // Calculation of average negative log likelihood
        logloss_heldout = -sum_loglik_heldout/n_obs_heldout;"
  )
}

## growth hazard model
get_model_chunks_growth_haz <- function(tasks) {
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("log_alpha","log_beta",
             "census_err", "sigma_log_census_err",
             "logloss_heldout"),
    data = "
        // Fitted data
        int<lower=1> n_obs;
        int<lower=1> n_census;
        int<lower=1> census[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] growth_dt;
        int<lower=0, upper=1> y[n_obs];
        
        // Heldout data
        int<lower=1> n_obs_heldout;
        int<lower=1> n_census_heldout;
        int<lower=1> census_heldout[n_obs_heldout];
        int<lower=0, upper=1> y_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;
        vector[n_obs_heldout] growth_dt_heldout;",
    parameters ="
      // Parameters
      real log_alpha;
      real log_beta;
      real raw_log_census_err[n_census];
      real<lower=0> sigma_log_census_err;",
    model ="
      real alpha;
      real beta;
      real census_err[n_census];
      real cumulative_hazard;
      
      // Put alpha and beta on normal scale
      alpha = exp(log_alpha);
      beta = exp(log_beta);

      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }

      for (i in 1:n_obs) {
        // Likelihood for hazard model
        cumulative_hazard = -census_length[i] * ((alpha * exp(-beta * growth_dt[i])) * census_err[census[i]]);
        
        if (y[i] == 0) {
          target += (cumulative_hazard);
        } else {
          target += (log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      log_alpha ~ normal(0, 2.5);
      log_beta ~ normal(0, 2.5);
      raw_log_census_err ~ normal(0,1);
      sigma_log_census_err ~ cauchy(0,2.5);",
    generated_quantities ="
      real alpha;
      real beta;
      real census_err[n_census];
      
      // Declaring heldout
      real cumulative_hazard_heldout;
      real loglik_heldout;
      real sum_loglik_heldout;
      real logloss_heldout;

      // Initialization of summed parameter
      sum_loglik_heldout = 0;
      
      // Put alpha and beta on normal scale
      alpha = exp(log_alpha);
      beta = exp(log_beta);

      // Recalculating census random effects
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }
      
      // Calculating log likelihood for heldout data
      for (j in 1:n_obs_heldout) {
        
        cumulative_hazard_heldout = -census_length_heldout[j] * ((alpha * exp(-beta * growth_dt_heldout[j])) * census_err[census_heldout[j]]);
        
        if (y_heldout[j] == 0) {
          loglik_heldout = cumulative_hazard_heldout;
        }
        else {
          loglik_heldout = log1m_exp(cumulative_hazard_heldout);
        }
        sum_loglik_heldout = sum_loglik_heldout + loglik_heldout;
    }
        // Calculation of average negative log likelihood
        logloss_heldout = -sum_loglik_heldout/n_obs_heldout;"
    )
}

## base_growth hazard model
get_model_chunks_base_growth_haz <- function(tasks) {
  
  f <- function(x) {
    x[x=="none"] = ""
    sapply(seq_len(nchar(x)), function(i) substr(x, i, i))
  }

  rho_combo <- f(tasks$rho_combo)
  gap_combo <- f(tasks$gap_combo)
  size_combo <- f(tasks$size_combo)
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("log_a0",
             if("a" %in% rho_combo)  "a1",
             if("a" %in% gap_combo)  "a2",
             if("a" %in% size_combo) "a3",
             "log_b0",
             if("b" %in% rho_combo)  "b1",
             if("b" %in% gap_combo)  "b2",
             if("b" %in% size_combo) "b3",
             "log_c0",
             if("c" %in% rho_combo)  "c1",
             if("c" %in% gap_combo)  "c2",
             if("c" %in% size_combo) "c3",
             "census_err", "sigma_log_census_err",
             "logloss_heldout"),
    data ="
      // Fitted data
      int<lower=1> n_obs;
      int<lower=1> n_census;
      int<lower=1> n_spp;
      int<lower=1> census[n_obs];
      int<lower=1> spp[n_obs];
      vector[n_obs] census_length;
      vector[n_obs] growth_dt;
      vector[n_spp] rho_c;
      vector[n_spp] gap_index_c;
      vector[n_spp] dbh_95_c;
      int<lower=0, upper=1> y[n_obs];
    
      // Heldout data
      int<lower=1> n_obs_heldout;
      int<lower=1> n_census_heldout;
      int<lower=1> n_spp_heldout;
      int<lower=1> census_heldout[n_obs_heldout];
      int<lower=1> spp_heldout[n_obs_heldout];
      vector[n_obs_heldout] census_length_heldout;
      vector[n_obs_heldout] growth_dt_heldout;
      vector[n_spp_heldout] rho_c_heldout;
      vector[n_spp_heldout] gap_index_c_heldout;
      vector[n_spp_heldout] dbh_95_c_heldout;
      int<lower=0, upper=1> y_heldout[n_obs_heldout];",
    parameters = sprintf("
      // Mortality model parameters
      real log_a0;
      real log_b0;
      real log_c0;
      real raw_log_census_err[n_census];
      real<lower=0> sigma_log_census_err;
      %s%s%s%s%s%s%s%s%s",
      ifelse("a" %in% rho_combo, "real a1;\n\t\t\t", ""),
      ifelse("a" %in% gap_combo, "real a2;\n\t\t\t", ""),
      ifelse("a" %in% size_combo, "real a3;\n\t\t\t", ""),
      ifelse("b" %in% rho_combo, "real b1;\n\t\t\t", ""),
      ifelse("b" %in% gap_combo, "real b2;\n\t\t\t", ""),
      ifelse("b" %in% size_combo, "real b3;\n\t\t\t", ""),
      ifelse("c" %in% rho_combo, "real c1;\n\t\t\t", ""),
      ifelse("c" %in% gap_combo, "real c2;\n\t\t\t", ""),
      ifelse("c" %in% size_combo, "real c3;\n", "")),
                model = sprintf("
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      real census_err[n_census];
      real cumulative_hazard;

      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }

      for (s in 1:n_spp) {
        alpha[s] = exp(log_a0)%s%s%s; 
        beta[s] = exp(log_b0)%s%s%s; 
        gamma[s] = exp(log_c0)%s%s%s;
      }
      for (i in 1:n_obs) {
        cumulative_hazard = -census_length[i] * ((alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]) * census_err[census[i]]);
        
        if (y[i] == 0) {
          target += (cumulative_hazard);
        } else {
          target += (log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      log_a0 ~ normal(0, 2.5);
      log_b0 ~ normal(0, 2.5);
      log_c0 ~ normal(0, 2.5);
      raw_log_census_err ~ normal(0, 1);
      sigma_log_census_err ~ cauchy(0, 2.5);
      %s%s%s%s%s%s%s%s%s",
      ifelse("a" %in% rho_combo,  " * pow(rho_c[s], a1)", ""),
      ifelse("a" %in% gap_combo,  " * pow(gap_index_c[s], a2)", ""),
      ifelse("a" %in% size_combo, " * pow(dbh_95_c[s], a3)", ""),
      ifelse("b" %in% rho_combo,  " * pow(rho_c[s], b1)", ""),
      ifelse("b" %in% gap_combo,  " * pow(gap_index_c[s], b2)", ""),
      ifelse("b" %in% size_combo, " * pow(dbh_95_c[s], b3)", ""),
      ifelse("c" %in% rho_combo,  " * pow(rho_c[s], c1)", ""),
      ifelse("c" %in% gap_combo,  " * pow(gap_index_c[s], c2)", ""),
      ifelse("c" %in% size_combo, " * pow(dbh_95_c[s], c3)", ""),

      ifelse("a" %in% rho_combo,  "a1 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("a" %in% gap_combo,  "a2 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("a" %in% size_combo, "a3 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("b" %in% rho_combo,  "b1 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("b" %in% gap_combo,  "b2 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("b" %in% size_combo, "b3 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("c" %in% rho_combo,  "c1 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("c" %in% gap_combo,  "c2 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("c" %in% size_combo, "c3 ~ normal(0,2.5);\n", "")),
    generated_quantities = sprintf("
      real census_err[n_census];
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      
      // Declaring heldout
      real cumulative_hazard_heldout;
      real loglik_heldout;
      real sum_loglik_heldout;
      real logloss_heldout;
      
      // Initialization of summed parameter
      sum_loglik_heldout = 0;


      // Recalculating census random effects
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }

      // Recalculating parameters
      for (s in 1:n_spp) {
        alpha[s] = exp(log_a0)%s%s%s; 
        beta[s] = exp(log_b0)%s%s%s; 
        gamma[s] = exp(log_c0)%s%s%s;
      }
      
      // Calculate log likelihood for heldout data
      for (j in 1:n_obs_heldout) {
        
        cumulative_hazard_heldout = -census_length_heldout[j] * ((alpha[spp_heldout[j]] * exp(-beta[spp_heldout[j]] * growth_dt_heldout[j]) + gamma[spp_heldout[j]]) * census_err[census_heldout[j]]);
        
        if (y_heldout[j] == 0) {
          loglik_heldout = cumulative_hazard_heldout;
        }
        else {
          loglik_heldout = log1m_exp(cumulative_hazard_heldout);
        }
        sum_loglik_heldout = sum_loglik_heldout + loglik_heldout;
      }
        // Calculation of average negative log likelihood
        logloss_heldout = -sum_loglik_heldout/n_obs_heldout;",
      ifelse("a" %in% rho_combo,  " * pow(rho_c[s], a1)", ""),
      ifelse("a" %in% gap_combo,  " * pow(gap_index_c[s], a2)", ""),
      ifelse("a" %in% size_combo, " * pow(dbh_95_c[s], a3)", ""),
      ifelse("b" %in% rho_combo,  " * pow(rho_c[s], b1)", ""),
      ifelse("b" %in% gap_combo,  " * pow(gap_index_c[s], b2)", ""),
      ifelse("b" %in% size_combo, " * pow(dbh_95_c[s], b3)", ""),
      ifelse("c" %in% rho_combo,  " * pow(rho_c[s], c1)", ""),
      ifelse("c" %in% gap_combo,  " * pow(gap_index_c[s], c2)", ""),
      ifelse("c" %in% size_combo, " * pow(dbh_95_c[s], c3)", "")
      )
  )
}

## base_growth hazard model with species random effect

get_model_chunks_base_growth_haz_re <- function(tasks, full_data = FALSE) {
  
  list(
    growth_measure = tasks$growth_measure,
    
    pars = if(full_data == FALSE) {
            c("mu_log_alpha","sigma_log_alpha",
             "mu_log_beta","sigma_log_beta",
             "mu_log_gamma","sigma_log_gamma",
             "census_err","sigma_log_census_err",
             "logloss_heldout") 
    }
    else {
    c("mu_log_alpha","sigma_log_alpha",
             "mu_log_beta","sigma_log_beta",
             "mu_log_gamma","sigma_log_gamma",
             "census_err","sigma_log_census_err",
             "logloss")
    },
    data = if(full_data == FALSE) {"
        // Fitted data
        int<lower=1> n_obs;
        int<lower=1> n_census;
        int<lower=1> n_spp;
        int<lower=1> census[n_obs];
        int<lower=1> spp[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] growth_dt;
        int<lower=0, upper=1> y[n_obs];
        
        // Heldout data
        int<lower=1> n_obs_heldout;
        int<lower=1> n_census_heldout;
        int<lower=1> n_spp_heldout;
        int<lower=1> census_heldout[n_obs_heldout];
        int<lower=1> spp_heldout[n_obs_heldout];
        vector[n_obs_heldout] census_length_heldout;
        vector[n_obs_heldout] growth_dt_heldout;
        int<lower=0, upper=1> y_heldout[n_obs_heldout];"
      }
    else {"
        // Fitted data
        int<lower=1> n_obs;
        int<lower=1> n_census;
        int<lower=1> n_spp;
        int<lower=1> census[n_obs];
        int<lower=1> spp[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] growth_dt;
        int<lower=0, upper=1> y[n_obs];"
      },
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
      real<lower=0> sigma_log_gamma;
    
      real raw_log_census_err[n_census];
      real<lower=0> sigma_log_census_err;",
    model ="
      // Declaring mortality parameters
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      real census_err[n_census];
      real cumulative_hazard;

      // Calculating census random effects      
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }
      // Calculating species random effects
      for (s in 1:n_spp) {
        alpha[s] = exp(raw_log_alpha[s] * sigma_log_alpha + mu_log_alpha); // e.g. implies lognormal(mu_log_alpha, sigma_log_alpha)
        beta[s] = exp(raw_log_beta[s] * sigma_log_beta + mu_log_beta);
        gamma[s] = exp(raw_log_gamma[s] * sigma_log_gamma + mu_log_gamma);
      }
      
      for (i in 1:n_obs) {
        // Likelihood for hazard model
        cumulative_hazard = -census_length[i] * ((alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]) * census_err[census[i]]);
        
        if (y[i] == 0) {
          target += (cumulative_hazard);
        } else {
          target += (log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      
      //Mortality model priors
      raw_log_alpha ~ normal(0,1);
      mu_log_alpha ~ normal(0, 2.5);
      sigma_log_alpha ~ cauchy(0, 2.5);
      
      raw_log_beta ~ normal(0, 1);
      mu_log_beta ~ normal(0, 2.5);
      sigma_log_beta ~ cauchy(0, 2.5);
      
      raw_log_gamma ~ normal(0, 1);
      mu_log_gamma ~ normal(0, 2.5);
      sigma_log_gamma ~ cauchy(0, 2.5);
    
      raw_log_census_err ~ normal(0, 1);
      sigma_log_census_err ~ cauchy(0, 2.5);",
    generated_quantities =
    if(full_data==FALSE) {"
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      real census_err[n_census];
      
      // Declaring heldout
      real cumulative_hazard_heldout;
      real loglik_heldout;
      real sum_loglik_heldout;
      real logloss_heldout;
      
      // Initialization of summed parameter
      sum_loglik_heldout = 0;

      // Recalulate species random effects
      for (s in 1:n_spp) {
        alpha[s] = exp(raw_log_alpha[s] * sigma_log_alpha + mu_log_alpha);
        beta[s] = exp(raw_log_beta[s] * sigma_log_beta + mu_log_beta);
        gamma[s] = exp(raw_log_gamma[s] * sigma_log_gamma + mu_log_gamma);
      }

      // Recalculate census random effects      
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }
      
      // Calculate log likelihood for heldout data
      for (j in 1:n_obs_heldout) {
        cumulative_hazard_heldout = -census_length_heldout[j] * ((alpha[spp_heldout[j]] * exp(-beta[spp_heldout[j]] * growth_dt_heldout[j]) + gamma[spp_heldout[j]]) * census_err[census_heldout[j]]);
        
        if (y_heldout[j] == 0) {
          loglik_heldout = cumulative_hazard_heldout;
        }
        else {
          loglik_heldout = log1m_exp(cumulative_hazard_heldout);
        }
        sum_loglik_heldout = sum_loglik_heldout + loglik_heldout;
    }
        // Calculation of average negative log likelihoods
        logloss_heldout = -sum_loglik_heldout/n_obs_heldout;"
    }
    else {"
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      real census_err[n_census];
      real cumulative_hazard;
      real loglik;
      real sum_loglik;
      real logloss;
      
      // Initialization of summed parameter
      sum_loglik = 0;

      // Recalulate species random effects
      for (s in 1:n_spp) {
        alpha[s] = exp(raw_log_alpha[s] * sigma_log_alpha + mu_log_alpha);
        beta[s] = exp(raw_log_beta[s] * sigma_log_beta + mu_log_beta);
        gamma[s] = exp(raw_log_gamma[s] * sigma_log_gamma + mu_log_gamma);
      }

      // Recalculate census random effects      
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }
      
      // Calculate log likelihood
      for (i in 1:n_obs) {
        cumulative_hazard = -census_length[i] * ((alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]) * census_err[census[i]]);
        
        if (y[i] == 0) {
          loglik = cumulative_hazard;
        }
        else {
          loglik = log1m_exp(cumulative_hazard);
        }
        sum_loglik = sum_loglik + loglik;
    }
        // Calculation of average negative log likelihoods
        logloss = -sum_loglik/n_obs;"
    }
  )
}
  

## FINAL FULL FIT MODEL WITH RANDOM EFFECTS

get_final_model_chunks <- function(tasks) {
  
  f <- function(x) {
    x[x=="none"] = ""
    sapply(seq_len(nchar(x)), function(i) substr(x, i, i))
  }

  rho_combo <- f(tasks$rho_combo)
  gap_combo <- f(tasks$gap_combo)
  size_combo <- f(tasks$size_combo)
  
  list(
    growth_measure = tasks$growth_measure,
    pars = c("mu_log_alpha","sigma_log_alpha",
             if("a" %in% rho_combo)  "a1",
             if("a" %in% gap_combo)  "a2",
             if("a" %in% size_combo) "a3",
             "mu_log_beta","sigma_log_beta",
             if("b" %in% rho_combo)  "b1",
             if("b" %in% gap_combo)  "b2",
             if("b" %in% size_combo) "b3",
             "mu_log_gamma","sigma_log_gamma",
             if("c" %in% rho_combo)  "c1",
             if("c" %in% gap_combo)  "c2",
             if("c" %in% size_combo) "c3",
             "census_err","sigma_log_census_err",
             "alpha","beta","gamma","logloss"),
    data = "
        // Fitted data
        int<lower=1> n_obs;
        int<lower=1> n_census;
        int<lower=1> n_spp;
        int<lower=1> census[n_obs];
        int<lower=1> spp[n_obs];
        vector[n_obs] census_length;
        vector[n_obs] growth_dt;
        vector[n_spp] rho_c;
        vector[n_spp] gap_index_c;
        vector[n_spp] dbh_95_c;
        int<lower=0, upper=1> y[n_obs];",
    parameters =sprintf("
      // Mortality model parameters
      real raw_log_alpha[n_spp];
      real mu_log_alpha;
      real<lower=0> sigma_log_alpha;
      
      real raw_log_beta[n_spp];
      real mu_log_beta;
      real<lower=0> sigma_log_beta;
      
      real raw_log_gamma[n_spp];
      real mu_log_gamma;
      real<lower=0> sigma_log_gamma;
    
      real raw_log_census_err[n_census];
      real<lower=0> sigma_log_census_err;
      %s%s%s%s%s%s%s%s%s",
      ifelse("a" %in% rho_combo, "real a1;\n\t\t\t", ""),
      ifelse("a" %in% gap_combo, "real a2;\n\t\t\t", ""),
      ifelse("a" %in% size_combo, "real a3;\n\t\t\t", ""),
      ifelse("b" %in% rho_combo, "real b1;\n\t\t\t", ""),
      ifelse("b" %in% gap_combo, "real b2;\n\t\t\t", ""),
      ifelse("b" %in% size_combo, "real b3;\n\t\t\t", ""),
      ifelse("c" %in% rho_combo, "real c1;\n\t\t\t", ""),
      ifelse("c" %in% gap_combo, "real c2;\n\t\t\t", ""),
      ifelse("c" %in% size_combo, "real c3;\n", "")),
    model =sprintf("
      // Declaring mortality parameters
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      real census_err[n_census];
      real cumulative_hazard;

      // Calculating census random effects      
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }
      // Calculating species random effects
      for (s in 1:n_spp) {
        alpha[s] = exp(raw_log_alpha[s] * sigma_log_alpha + mu_log_alpha)%s%s%s; // e.g. implies lognormal(mu_log_alpha, sigma_log_alpha)
        beta[s] = exp(raw_log_beta[s] * sigma_log_beta + mu_log_beta)%s%s%s;
        gamma[s] = exp(raw_log_gamma[s] * sigma_log_gamma + mu_log_gamma)%s%s%s;
      }
      
      for (i in 1:n_obs) {
        // Likelihood for hazard model
        cumulative_hazard = -census_length[i] * ((alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]) * census_err[census[i]]);
        
        if (y[i] == 0) {
          target += (cumulative_hazard);
        } else {
          target += (log1m_exp(cumulative_hazard));
        }
      }
      
      // Priors
      raw_log_alpha ~ normal(0,1);
      mu_log_alpha ~ normal(0, 2.5);
      sigma_log_alpha ~ cauchy(0, 2.5);
      
      raw_log_beta ~ normal(0, 1);
      mu_log_beta ~ normal(0, 2.5);
      sigma_log_beta ~ cauchy(0, 2.5);
      
      raw_log_gamma ~ normal(0, 1);
      mu_log_gamma ~ normal(0, 2.5);
      sigma_log_gamma ~ cauchy(0, 2.5);
    
      raw_log_census_err ~ normal(0, 1);
      sigma_log_census_err ~ cauchy(0, 2.5);
      %s%s%s%s%s%s%s%s%s",
      ifelse("a" %in% rho_combo,  " * pow(rho_c[s], a1)", ""),
      ifelse("a" %in% gap_combo,  " * pow(gap_index_c[s], a2)", ""),
      ifelse("a" %in% size_combo, " * pow(dbh_95_c[s], a3)", ""),
      ifelse("b" %in% rho_combo,  " * pow(rho_c[s], b1)", ""),
      ifelse("b" %in% gap_combo,  " * pow(gap_index_c[s], b2)", ""),
      ifelse("b" %in% size_combo, " * pow(dbh_95_c[s], b3)", ""),
      ifelse("c" %in% rho_combo,  " * pow(rho_c[s], c1)", ""),
      ifelse("c" %in% gap_combo,  " * pow(gap_index_c[s], c2)", ""),
      ifelse("c" %in% size_combo, " * pow(dbh_95_c[s], c3)", ""),

      ifelse("a" %in% rho_combo,  "a1 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("a" %in% gap_combo,  "a2 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("a" %in% size_combo, "a3 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("b" %in% rho_combo,  "b1 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("b" %in% gap_combo,  "b2 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("b" %in% size_combo, "b3 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("c" %in% rho_combo,  "c1 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("c" %in% gap_combo,  "c2 ~ normal(0,2.5);\n\t\t\t", ""),
      ifelse("c" %in% size_combo, "c3 ~ normal(0,2.5);\n", "")),
    generated_quantities = sprintf("
      real alpha[n_spp];
      real beta[n_spp];
      real gamma[n_spp];
      real census_err[n_census];
      
      // Declaring heldout
      real cumulative_hazard;
      real loglik;
      real sum_loglik;
      real logloss;
      
      // Initialization of summed parameter
      sum_loglik = 0;

      // Recalulate species random effects
      for (s in 1:n_spp) {
        alpha[s] = exp(raw_log_alpha[s] * sigma_log_alpha + mu_log_alpha)%s%s%s;
        beta[s] = exp(raw_log_beta[s] * sigma_log_beta + mu_log_beta)%s%s%s;
        gamma[s] = exp(raw_log_gamma[s] * sigma_log_gamma + mu_log_gamma)%s%s%s;
      }

      // Recalculate census random effects      
      for (t in 1:n_census) {
      census_err[t] = exp(raw_log_census_err[t] * sigma_log_census_err);
      }
      
      // Calculate log likelihood
      for (i in 1:n_obs) {
        cumulative_hazard = -census_length[i] * ((alpha[spp[i]] * exp(-beta[spp[i]] * growth_dt[i]) + gamma[spp[i]]) * census_err[census[i]]);
        
        if (y[i] == 0) {
          loglik = cumulative_hazard;
        }
        else {
          loglik = log1m_exp(cumulative_hazard);
        }
        sum_loglik = sum_loglik + loglik;
    }
        // Calculation of average negative log likelihoods
        logloss = -sum_loglik/n_obs;",
      ifelse("a" %in% rho_combo,  " * pow(rho_c[s], a1)", ""),
      ifelse("a" %in% gap_combo,  " * pow(gap_index_c[s], a2)", ""),
      ifelse("a" %in% size_combo, " * pow(dbh_95_c[s], a3)", ""),
      ifelse("b" %in% rho_combo,  " * pow(rho_c[s], b1)", ""),
      ifelse("b" %in% gap_combo,  " * pow(gap_index_c[s], b2)", ""),
      ifelse("b" %in% size_combo, " * pow(dbh_95_c[s], b3)", ""),
      ifelse("c" %in% rho_combo,  " * pow(rho_c[s], c1)", ""),
      ifelse("c" %in% gap_combo,  " * pow(gap_index_c[s], c2)", ""),
      ifelse("c" %in% size_combo, " * pow(dbh_95_c[s], c3)", "")
    )
  )
}
