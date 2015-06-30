get_model_chunks <- function(tasks) {
  rho_combo <- tasks$rho_combo
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }
  
  list(
    pars = c("log_a0_raw","log_a0_mu","log_a0_sigma","log_a0","log_a1_raw","log_a1_mu","log_a1_sigma","log_a1",if("a" %in% rho_combo) "log_a2",
             "log_b0_raw","log_b0_mu","log_b0_sigma","log_b0","log_b1_raw","log_b1_mu","log_b1_sigma","log_b1",if("b" %in% rho_combo) "log_b2",
             "log_c0_raw","log_c0_mu","log_c0_sigma","log_c0","log_c1_raw","log_c1_mu","log_c1_sigma","log_c1",if("c" %in% rho_combo) "log_c2",
             "log_lik_fit", "log_lik_tilde","sum_log_lik_fit","sum_log_lik_tilde"),
    parameters = sprintf("
    real log_a0_raw[n_spp];
    real log_a0_mu;
    real<lower=0> log_a0_sigma;
    
    real log_a1_raw[n_census];
    real log_a1_mu;
    real<lower=0> log_a1_sigma;
    
    real log_b0_raw[n_spp];
    real<lower= -20, upper=20> log_b0_mu;  
    real<lower=0> log_b0_sigma;
    
    real log_b1_raw[n_census];
    real log_b1_mu;  
    real<lower=0> log_b1_sigma;
    
    real log_c0_raw[n_spp];
    real log_c0_mu;  
    real<lower=0> log_c0_sigma;
    
    real log_c1_raw[n_census];
    real log_c1_mu;  
    real<lower=0> log_c1_sigma;
    
    %s
    %s
    %s",ifelse("a" %in% rho_combo, "real log_a2;", ""),
        ifelse("b" %in% rho_combo, "real log_b2;", ""),
        ifelse("c" %in% rho_combo, "real log_c2;", "")),
    transformed_parameters = "
    real log_a0[n_spp];
    real log_a1[n_census];
    
    real log_b0[n_spp];
    real log_b1[n_census];
    
    real log_c0[n_spp];
    real log_c1[n_census];
    
    for (s in 1:n_spp) {
    log_a0[s] <- log_a0_raw[s] * log_a0_sigma + log_a0_mu;
    log_b0[s] <- log_b0_raw[s] * log_b0_sigma + log_b0_mu;
    log_c0[s] <- log_c0_raw[s] * log_c0_sigma + log_c0_mu;
    }
    
    for (t in 1:n_census) {
    log_a1[t] <- log_a1_raw[t] * log_a1_sigma + log_a1_mu;
    log_b1[t] <- log_b1_raw[t] * log_b1_sigma + log_b1_mu;
    log_c1[t] <- log_c1_raw[t] * log_c1_sigma + log_c1_mu;
    }",
    model = sprintf("
    real log_alpha;
    real log_beta;
    real log_gamma;
    real cumulative_hazard;
    
    #species random effects
    log_a0_raw ~ normal(0,1);
    log_b0_raw ~ normal(0,1);
    log_c0_raw ~ normal(0,1);
    
    #census random effects
    log_a1_raw ~ normal(0,1);
    log_b1_raw ~ normal(0,1);
    log_c1_raw ~ normal(0,1);
    
    for (i in 1:n_obs) {
    log_alpha <- log_a0[spp[i]] + log_a1[census[i]]%s;
    log_beta <- log_b0[spp[i]] + log_b1[census[i]]%s;
    log_gamma <- log_c0[spp[i]] + log_c1[census[i]]%s;
    
    cumulative_hazard <- -census_length[i] * (exp(log_alpha - exp(log_beta) * growth_dt[i]) + exp(log_gamma));
    
    if (y[i] == 0) {
    increment_log_prob(cumulative_hazard);
    } else {
    increment_log_prob(log1m_exp(cumulative_hazard));
    }
    }
    
    //Priors
    log_a0_mu ~ normal(-0.87, 1);
    log_a0_sigma ~ cauchy(0, 2.5);
    log_b0_sigma ~ cauchy(0, 2.5);
    log_c0_mu ~ normal(-4.42, 0.17);
    log_c0_sigma ~ cauchy(0, 2.5);
    log_a1_mu  ~ normal(0, 0.1); 
    log_a1_sigma ~ cauchy(0, 2.5);
    log_b1_mu  ~ normal(0, 0.1);
    log_b1_sigma ~ cauchy(0, 2.5);
    log_c1_mu  ~ normal(0, 0.1);
    log_c1_sigma ~ cauchy(0, 2.5);
    
    %s 
    %s
    %s", 
    ifelse("a" %in% rho_combo, " + log_a2 * log_rho_c[spp[i]]", ""),
    ifelse("b" %in% rho_combo, " + log_b2 * log_rho_c[spp[i]]", ""),
    ifelse("c" %in% rho_combo, " + log_c2 * log_rho_c[spp[i]]", ""),
    ifelse("a" %in% rho_combo, "log_a2 ~ normal(0,5);", ""),
    ifelse("b" %in% rho_combo, "log_b2 ~ normal(0,5);", ""),
    ifelse("c" %in% rho_combo, "log_c2 ~ normal(0,5);", "")),  
    generated_quantities = sprintf("
      // Fitted objects
      real log_alpha_fit;
      real log_beta_fit;
      real log_gamma_fit;
      real cumulative_hazard_fit;
      real log_lik_fit[n_obs];
      real sum_log_lik_fit;
      
      // Heldout objects
      real log_alpha_tilde;
      real log_beta_tilde;
      real log_gamma_tilde;
      real cumulative_hazard_tilde;
      real log_lik_tilde[n_obs_tilde];
      real sum_log_lik_tilde;
      
      // Initialization of summed log likelihoods
      sum_log_lik_fit <- 0;
      sum_log_lik_tilde <- 0;
      
      // log likelihood for fitted model
      for (i in 1:n_obs) {
        log_alpha_fit <- log_a0[spp[i]] + log_a1[census[i]]%s;
        log_beta_fit <- log_b0[spp[i]] + log_b1[census[i]]%s;
        log_gamma_fit <- log_c0[spp[i]] + log_c1[census[i]]%s;

        cumulative_hazard_fit <- -census_length[i] * (exp(log_alpha_fit - exp(log_beta_fit) * growth_dt[i]) + exp(log_gamma_fit));
        if (y[i] == 0) {
          log_lik_fit[i] <- cumulative_hazard_fit;
        }
        else {
          log_lik_fit[i] <- log1m_exp(cumulative_hazard_fit);
        }
        sum_log_lik_fit <- sum_log_lik_fit + log_lik_fit[i];
      }
      
      // log likelihood for held out data
      for (i in 1:n_obs_tilde) {
        log_alpha_tilde <- log_a0[spp_tilde[i]] + log_a1[census_tilde[i]]%s;
        log_beta_tilde <- log_b0[spp_tilde[i]] + log_b1[census_tilde[i]]%s;
        log_gamma_tilde <- log_c0[spp_tilde[i]] + log_c1[census_tilde[i]]%s;
        
        cumulative_hazard_tilde <- -census_length_tilde[i] * (exp(log_alpha_tilde - exp(log_beta_tilde) * growth_dt_tilde[i]) + exp(log_gamma_tilde));
        
        if (y_tilde[i] == 0) {
          log_lik_tilde[i] <- cumulative_hazard_tilde;
        } 
        else {
          log_lik_tilde[i] <- log1m_exp(cumulative_hazard_tilde);
        }
        sum_log_lik_tilde <- sum_log_lik_tilde + log_lik_tilde[i];
      }",ifelse("a" %in% rho_combo, " + log_a2 * log_rho_c[spp[i]]", ""),
         ifelse("b" %in% rho_combo, " + log_b2 * log_rho_c[spp[i]]", ""),
         ifelse("c" %in% rho_combo, " + log_c2 * log_rho_c[spp[i]]", ""),
         ifelse("a" %in% rho_combo, " + log_a2 * log_rho_c_tilde[spp_tilde[i]]", ""),
         ifelse("b" %in% rho_combo, " + log_b2 * log_rho_c_tilde[spp_tilde[i]]", ""),
         ifelse("c" %in% rho_combo, " + log_c2 * log_rho_c_tilde[spp_tilde[i]]", ""))
    )
}
