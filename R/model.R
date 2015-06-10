get_model_chunks <- function(tasks) {
  rho_combo <- tasks$rho_combo
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }
  
  list(
    pars = c("a0_raw","a0_mu","a0_sigma","a0","a1_raw","a1_mu","a1_sigma","a1",if("a" %in% rho_combo) "a2",
             "b0_raw","b0_mu","b0_sigma","b0","b1_raw","b1_mu","b1_sigma","b1",if("b" %in% rho_combo) "b2",
             "c0_raw","c0_mu","c0_sigma","c0","c1_raw","c1_mu","c1_sigma","c1",if("c" %in% rho_combo) "c2",
             "log_lik_fit", "log_lik_tilde","log_lik_fit_total","log_lik_tilde_total"),
    parameters = sprintf("
    real a0_raw[n_spp];
    real a0_mu;
    real<lower=0> a0_sigma;
    
    real a1_raw[n_census];
    real a1_mu;
    real<lower=0> a1_sigma;
    
    real b0_raw[n_spp];
    real<lower= -20, upper=20> b0_mu;  
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
    
    %s
    %s
    %s",ifelse("a" %in% rho_combo, "real a2;", ""),
        ifelse("b" %in% rho_combo, "real b2;", ""),
        ifelse("c" %in% rho_combo, "real c2;", "")),
    transformed_parameters = "
    real a0[n_spp];
    real a1[n_census];
    
    real b0[n_spp];
    real b1[n_census];
    
    real c0[n_spp];
    real c1[n_census];
    
    for (s in 1:n_spp) {
    a0[s] <- a0_raw[s] * a0_sigma + a0_mu;
    b0[s] <- b0_raw[s] * b0_sigma + b0_mu;
    c0[s] <- c0_raw[s] * c0_sigma + c0_mu;
    }
    
    for (t in 1:n_census) {
    a1[t] <- a1_raw[t] * a1_sigma + a1_mu;
    b1[t] <- b1_raw[t] * b1_sigma + b1_mu;
    c1[t] <- c1_raw[t] * c1_sigma + c1_mu;
    }",
    model = sprintf("
    real a_log;
    real b_log;
    real c_log;
    real cumulative_hazard;
    
    for (s in 1:n_spp) {
    a0_raw[s] ~ normal(0,1);
    b0_raw[s] ~ normal(0,1);
    c0_raw[s] ~ normal(0,1);
    }
    
    for (t in 1:n_census) {
    a1_raw[t] ~ normal(0,1);
    b1_raw[t] ~ normal(0,1);
    c1_raw[t] ~ normal(0,1);
    }
    
    for (i in 1:n_obs) {
    a_log <- a0[spp[i]] + a1[census[i]]%s;
    b_log <- b0[spp[i]] + b1[census[i]]%s;
    c_log <- c0[spp[i]] + c1[census[i]]%s;
    
    cumulative_hazard <- -census_length[i] * (exp(a_log - exp(b_log) * growth_dt[i]) + exp(c_log));
    
    if (y[i] == 0) {
    increment_log_prob(cumulative_hazard);
    } else {
    increment_log_prob(log1m_exp(cumulative_hazard));
    }
    }
    
    //Priors
    a0_mu ~ normal(-0.87, 1);
    a0_sigma ~ cauchy(0, 2.5);
    b0_sigma ~ cauchy(0, 2.5);
    c0_mu ~ normal(-4.42, 0.17);
    c0_sigma ~ cauchy(0, 2.5);
    a1_mu  ~ normal(0, 0.1); 
    a1_sigma ~ cauchy(0, 2.5);
    b1_mu  ~ normal(0, 0.1);
    b1_sigma ~ cauchy(0, 2.5);
    c1_mu  ~ normal(0, 0.1);
    c1_sigma ~ cauchy(0, 2.5);
    
    %s 
    %s
    %s", 
    ifelse("a" %in% rho_combo, " + a2 * log_rho_c[spp[i]]", ""),
    ifelse("b" %in% rho_combo, " + b2 * log_rho_c[spp[i]]", ""),
    ifelse("c" %in% rho_combo, " + c2 * log_rho_c[spp[i]]", ""),
    ifelse("a" %in% rho_combo, "a2 ~ normal(0,5);", ""),
    ifelse("b" %in% rho_combo, "b2 ~ normal(0,5);", ""),
    ifelse("c" %in% rho_combo, "c2 ~ normal(0,5);", "")),  
    generated_quantities = sprintf("
      // Fitted objects
      real a_log_fit;
      real b_log_fit;
      real c_log_fit;
      real cumulative_hazard_fit;
      real log_lik_fit[n_obs];
      real log_lik_fit_total;
      
      // Heldout objects
      real a_log_tilde;
      real b_log_tilde;
      real c_log_tilde;
      real cumulative_hazard_tilde;
      real log_lik_tilde[n_obs_tilde];
      real log_lik_tilde_total;
      
      // Initialization of summed log likelihoods
      log_lik_fit_total <- 0;
      log_lik_tilde_total <- 0;
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        a_log_fit <- a0[spp[i]] + a1[census[i]]%s;
        b_log_fit <- b0[spp[i]] + b1[census[i]]%s;
        c_log_fit <- c0[spp[i]] + c1[census[i]]%s;
        cumulative_hazard_fit <- -census_length[i] * (exp(a_log_fit - exp(b_log_fit) * growth_dt[i]) + exp(c_log_fit));
        if (y[i] == 0) {
          log_lik_fit[i] <- cumulative_hazard_fit;
        }
        else {
          log_lik_fit[i] <- log1m_exp(cumulative_hazard_fit);
        }
        log_lik_fit_total <- log_lik_fit_total + log_lik_fit[i];
      }
      
      // log likelihood for held out data
      for (i in 1:n_obs_tilde) {
        a_log_tilde <- a0[spp_tilde[i]] + a1[census_tilde[i]]%s;
        b_log_tilde <- b0[spp_tilde[i]] + b1[census_tilde[i]]%s;
        c_log_tilde <- c0[spp_tilde[i]] + c1[census_tilde[i]]%s;
        
        cumulative_hazard_tilde <- -census_length_tilde[i] * (exp(a_log_tilde - exp(b_log_tilde) * growth_dt_tilde[i]) + exp(c_log_tilde));
        
        if (y_tilde[i] == 0) {
          log_lik_tilde[i] <- cumulative_hazard_tilde;
        } 
        else {
          log_lik_tilde[i] <- log1m_exp(cumulative_hazard_tilde);
        }
        log_lik_tilde_total <- log_lik_tilde_total + log_lik_tilde[i];
      }",ifelse("a" %in% rho_combo, " + a2 * log_rho_c[spp[i]]", ""),
         ifelse("b" %in% rho_combo, " + b2 * log_rho_c[spp[i]]", ""),
         ifelse("c" %in% rho_combo, " + c2 * log_rho_c[spp[i]]", ""),
         ifelse("a" %in% rho_combo, " + a2 * log_rho_c_tilde[spp_tilde[i]]", ""),
         ifelse("b" %in% rho_combo, " + b2 * log_rho_c_tilde[spp_tilde[i]]", ""),
         ifelse("c" %in% rho_combo, " + c2 * log_rho_c_tilde[spp_tilde[i]]", ""))
    )
}
