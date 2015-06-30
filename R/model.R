get_model_chunks <- function(tasks) {
  growth_measure <- tasks$growth_measure
  rho_combo <- tasks$rho_combo
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }
  
  list(
    pars = c('log_mu_true_dbh1','log_sigma_true_dbh1','log_mu_dbh_increment','log_sigma_dbh_increment',
             'log_mu_true_dbh1_heldout','log_sigma_true_dbh1_heldout','log_mu_dbh_increment_heldout','log_sigma_dbh_increment_heldout',
             "log_raw_a0","log_mu_a0","log_sigma_a0","log_a0","log_raw_a1","log_mu_a1","log_sigma_a1","log_a1",if("a" %in% rho_combo) "log_a2",
             "log_raw_b0","log_mu_b0","log_sigma_b0","log_b0","log_raw_b1","log_mu_b1","log_sigma_b1","log_b1",if("b" %in% rho_combo) "log_b2",
             "log_raw_c0","log_mu_c0","log_sigma_c0","log_c0","log_raw_c1","log_mu_c1","log_sigma_c1","log_c1",if("c" %in% rho_combo) "log_c2",
             "log_lik_fit", "log_lik_heldout","sum_log_lik_fit","sum_log_lik_heldout"),
    parameters = sprintf("
      // DBH measurement error parameters for fitted data
      real log_raw_true_dbh1[n_obs];
      real log_mu_true_dbh1;
      real log_sigma_true_dbh1;

      real log_raw_dbh_increment[n_obs];
      real log_mu_dbh_increment;
      real log_sigma_dbh_increment;

      // DBH measurement error parameters for heldout data
      real log_raw_true_dbh1_heldout[n_obs_heldout];
      real log_mu_true_dbh1_heldout;
      real log_sigma_true_dbh1_heldout;

      real log_raw_dbh_increment_heldout[n_obs_heldout];
      real log_mu_dbh_increment_heldout;
      real log_sigma_dbh_increment_heldout;


      // Mortality model parameters
      real log_raw_a0[n_spp];
      real log_mu_a0;
      real<lower=0> log_sigma_a0;
      
      real log_raw_a1[n_census];
      real log_mu_a1;
      real<lower=0> log_sigma_a1;
      
      real log_raw_b0[n_spp];
      real<lower= -20, upper=20> log_mu_b0;  
      real<lower=0> log_sigma_b0;
      
      real log_raw_b1[n_census];
      real log_mu_b1;  
      real<lower=0> log_sigma_b1;
      
      real log_raw_c0[n_spp];
      real log_mu_c0;  
      real<lower=0> log_sigma_c0;
      
      real log_raw_c1[n_census];
      real log_mu_c1;  
      real<lower=0> log_sigma_c1;
      
      %s
      %s
      %s",
    ifelse("a" %in% rho_combo, "real log_a2;", ""),
    ifelse("b" %in% rho_combo, "real log_b2;", ""),
    ifelse("c" %in% rho_combo, "real log_c2;", "")),
  model = sprintf("
    // Declaring DBH error parameters
    real true_dbh1[n_obs];
    real true_dbh2[n_obs];
    real dbh_increment[n_obs];
    real growth_dt[n_obs];

    real true_dbh1_heldout[n_obs_heldout];
    real true_dbh2_heldout[n_obs_heldout];
    real dbh_increment_heldout[n_obs_heldout];
    real growth_dt_heldout[n_obs_heldout];

    // Declaring mortality parameters
    real log_alpha;
    real log_a0[n_spp];
    real log_a1[n_census];
    
    real log_beta;
    real log_b0[n_spp];
    real log_b1[n_census];
    
    real log_gamma;
    real log_c0[n_spp];
    real log_c1[n_census];
    
    real cumulative_hazard;
    
    // Calculating species random effects
    for (s in 1:n_spp) {
      log_a0[s] <- log_raw_a0[s] * log_sigma_a0 + log_mu_a0;
      log_b0[s] <- log_raw_b0[s] * log_sigma_b0 + log_mu_b0;
      log_c0[s] <- log_raw_c0[s] * log_sigma_c0 + log_mu_c0;
    }
    
    // Calculating census period random effects
    for (t in 1:n_census) {
      log_a1[t] <- log_raw_a1[t] * log_sigma_a1 + log_mu_a1;
      log_b1[t] <- log_raw_b1[t] * log_sigma_b1 + log_mu_b1;
      log_c1[t] <- log_raw_c1[t] * log_sigma_c1 + log_mu_c1;
    }
    
    for (i in 1:n_obs) {
      // Calculating true growth for fitted data
      true_dbh1[i] <- exp(log_raw_true_dbh1[i] * log_sigma_true_dbh1 + log_mu_true_dbh1);
      dbh_increment[i] <- exp(log_raw_dbh_increment[i] * log_sigma_dbh_increment + log_mu_dbh_increment);
      true_dbh2[i] <- true_dbh1[i] + dbh_increment[i];
      growth_dt[i] <- %s;
      
      // Calculating mortality parameters
      log_alpha <- log_a0[spp[i]] + log_a1[census[i]]%s;
      log_beta <- log_b0[spp[i]] + log_b1[census[i]]%s;
      log_gamma <- log_c0[spp[i]] + log_c1[census[i]]%s;

    // Likelihood for both dbh measurement errors
    // SDs come from mixture model run on data explicitly collected to examine measurement error
    increment_log_prob(log_sum_exp(log1m(0.046908) + normal_log(obs_dbh1[i], true_dbh1[i], 0.085871 + 0.005716 * true_dbh1[i]),
                                   log(0.046908) + normal_log(obs_dbh1[i], true_dbh1[i], 3.496852))); 

    increment_log_prob(log_sum_exp(log1m(0.046908) + normal_log(obs_dbh2[i], true_dbh2[i], 0.085871 + 0.005716 * true_dbh2[i]),
                                   log(0.046908) + normal_log(obs_dbh2[i], true_dbh2[i], 3.496852))); 


    // Likelihood for hazard model
    cumulative_hazard <- -census_length[i] * (exp(log_alpha - exp(log_beta) * growth_dt[i]) + exp(log_gamma));
      
      if (y[i] == 0) {
        increment_log_prob(cumulative_hazard);
      } else {
        increment_log_prob(log1m_exp(cumulative_hazard));
      }
    }

      
    for (j in 1:n_obs_heldout) {
      // Calculating true growth for heldout data
      true_dbh1_heldout[j] <- exp(log_raw_true_dbh1_heldout[j] * log_sigma_true_dbh1_heldout + log_mu_true_dbh1_heldout);
      dbh_increment_heldout[j] <- exp(log_raw_dbh_increment_heldout[j] * log_sigma_dbh_increment_heldout + log_mu_dbh_increment_heldout);
      true_dbh2_heldout[j] <- true_dbh1_heldout[j] + dbh_increment_heldout[j];
      growth_dt_heldout[j] <- %s;

    increment_log_prob(log_sum_exp(log1m(0.046908) + normal_log(obs_dbh1_heldout[j], true_dbh1_heldout[j], 0.085871 + 0.005716 * true_dbh1_heldout[j]),
                                   log(0.046908) + normal_log(obs_dbh1_heldout[j], true_dbh1_heldout[j], 3.496852))); 

    increment_log_prob(log_sum_exp(log1m(0.046908) + normal_log(obs_dbh2_heldout[j], true_dbh2_heldout[j], 0.085871 + 0.005716 * true_dbh2_heldout[j]),
                                   log(0.046908) + normal_log(obs_dbh2_heldout[j], true_dbh2_heldout[j], 3.496852))); 
  }
    
    // Priors
    // Fitted data DBH measurement error priors
    log_raw_true_dbh1 ~ normal(0,1);
    log_mu_true_dbh1 ~ normal(0,5);
    log_sigma_true_dbh1 ~ cauchy(0,2.5);

    log_raw_dbh_increment ~ normal(0,1);
    log_mu_dbh_increment ~ normal(0,5);
    log_sigma_dbh_increment ~ cauchy(0,2.5);

    // Heldout data DBH measurement error priors
    log_raw_true_dbh1_heldout ~ normal(0,1);
    log_mu_true_dbh1_heldout ~ normal(0,5);
    log_sigma_true_dbh1_heldout ~ cauchy(0,2.5);

    log_raw_dbh_increment_heldout ~ normal(0,1);
    log_mu_dbh_increment_heldout ~ normal(0,5);
    log_sigma_dbh_increment_heldout ~ cauchy(0,2.5);


    //Mortality model priors
    log_raw_a0 ~ normal(0,1);
    log_mu_a0 ~ normal(-0.87, 1);
    log_sigma_a0 ~ cauchy(0, 2.5);
    
    log_raw_b0 ~ normal(0,1);
    log_sigma_b0 ~ cauchy(0, 2.5);
    
    log_raw_c0 ~ normal(0,1);
    log_mu_c0 ~ normal(-4.42, 0.17);
    log_sigma_c0 ~ cauchy(0, 2.5);  
    
    log_raw_a1 ~ normal(0,1);
    log_mu_a1 ~ normal(0, 0.1); 
    log_sigma_a1 ~ cauchy(0, 2.5);
    
    log_raw_b1 ~ normal(0,1);
    log_mu_b1  ~ normal(0, 0.1);
    log_sigma_b1 ~ cauchy(0, 2.5);
    
    log_raw_c1 ~ normal(0,1);
    log_mu_c1  ~ normal(0, 0.1);
    log_sigma_c1 ~ cauchy(0, 2.5);
    
    %s 
    %s
    %s", 
    switch(growth_measure,
           dbh_dt='(true_dbh2[i] - true_dbh1[i])/census_length[i]',
           dbh_dt_rel='(log(true_dbh2[i]) - log(true_dbh1[i]))/census_length[i]',
           basal_area_dt='(0.25*pi()*true_dbh2[i]^2 - 0.25*pi()*true_dbh1[i]^2)/census_length[i]',
           basal_area_dt_rel='(log(0.25*pi()*true_dbh2[i]^2) - log(0.25*pi()*true_dbh1[i]^2))/census_length[i]'),
    ifelse("a" %in% rho_combo, " + log_a2 * log_rho_c[spp[i]]", ""),
    ifelse("b" %in% rho_combo, " + log_b2 * log_rho_c[spp[i]]", ""),
    ifelse("c" %in% rho_combo, " + log_c2 * log_rho_c[spp[i]]", ""),
    switch(growth_measure,
           dbh_dt='(true_dbh2_heldout[j] - true_dbh1_heldout[j])/census_length_heldout[j]',
           dbh_dt_rel='(log(true_dbh2_heldout[j]) - log(true_dbh1_heldout[j]))/census_length_heldout[j]',
           basal_area_dt='(0.25*pi()*true_dbh2_heldout[j]^2 - 0.25*pi()*true_dbh1_heldout[j]^2)/census_length_heldout[j]',
           basal_area_dt_rel='(log(0.25*pi()*true_dbh2_heldout[i]^2) - log(0.25*pi()*true_dbh1_heldout[j]^2))/census_length_heldout[j]'),
    ifelse("a" %in% rho_combo, "log_a2 ~ normal(0,5);", ""),
    ifelse("b" %in% rho_combo, "log_b2 ~ normal(0,5);", ""),
    ifelse("c" %in% rho_combo, "log_c2 ~ normal(0,5);", "")),  
  generated_quantities = sprintf("
    // Declaring fitted parameters
    real true_dbh1[n_obs];
    real true_dbh2[n_obs];
    real dbh_increment[n_obs];
    real growth_dt[n_obs];

    real log_a0[n_spp];
    real log_a1[n_census];
    
    real log_b0[n_spp];
    real log_b1[n_census];
    
    real log_c0[n_spp];
    real log_c1[n_census];
    
    real log_alpha_fit;
    real log_beta_fit;
    real log_gamma_fit;
    real cumulative_hazard_fit;
    real log_lik_fit[n_obs];
    real sum_log_lik_fit;
    
    // Declaring heldout parameters
    real true_dbh1_heldout[n_obs_heldout];
    real true_dbh2_heldout[n_obs_heldout];
    real dbh_increment_heldout[n_obs_heldout];
    real growth_dt_heldout[n_obs_heldout];

    real log_alpha_heldout;
    real log_beta_heldout;
    real log_gamma_heldout;
    real cumulative_hazard_heldout;
    real log_lik_heldout[n_obs_heldout];
    real sum_log_lik_heldout;
    
    // Initialization of summed log likelihoods
    sum_log_lik_fit <- 0;
    sum_log_lik_heldout <- 0;
    
    // recalulate species random effects
    for (s in 1:n_spp) {
      log_a0[s] <- log_raw_a0[s] * log_sigma_a0 + log_mu_a0;
      log_b0[s] <- log_raw_b0[s] * log_sigma_b0 + log_mu_b0;
      log_c0[s] <- log_raw_c0[s] * log_sigma_c0 + log_mu_c0;
    }
    
    // recalulate species random effects
    for (t in 1:n_census) {
      log_a1[t] <- log_raw_a1[t] * log_sigma_a1 + log_mu_a1;
      log_b1[t] <- log_raw_b1[t] * log_sigma_b1 + log_mu_b1;
      log_c1[t] <- log_raw_c1[t] * log_sigma_c1 + log_mu_c1;
    }
    
    // log likelihood for fitted model
    for (i in 1:n_obs) {
      // recalculating true growth for fitted data
      true_dbh1[i] <- exp(log_raw_true_dbh1[i] * log_sigma_true_dbh1 + log_mu_true_dbh1);
      dbh_increment[i] <- exp(log_raw_dbh_increment[i] * log_sigma_dbh_increment + log_mu_dbh_increment);
      true_dbh2[i] <- true_dbh1[i] + dbh_increment[i];
      growth_dt[i] <- %s;

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
    for (j in 1:n_obs_heldout) {
      // recalculating true growth for heldout data
      true_dbh1_heldout[j] <- exp(log_raw_true_dbh1_heldout[j] * log_sigma_true_dbh1_heldout + log_mu_true_dbh1_heldout);
      dbh_increment_heldout[j] <- exp(log_raw_dbh_increment_heldout[j] * log_sigma_dbh_increment_heldout + log_mu_dbh_increment_heldout);
      true_dbh2_heldout[j] <- true_dbh1_heldout[j] + dbh_increment_heldout[j];
      growth_dt_heldout[j] <- %s;

      log_alpha_heldout <- log_a0[spp_heldout[j]] + log_a1[census_heldout[j]]%s;
      log_beta_heldout <- log_b0[spp_heldout[j]] + log_b1[census_heldout[j]]%s;
      log_gamma_heldout <- log_c0[spp_heldout[j]] + log_c1[census_heldout[j]]%s;
      
      cumulative_hazard_heldout <- -census_length_heldout[j] * (exp(log_alpha_heldout - exp(log_beta_heldout) * growth_dt_heldout[j]) + exp(log_gamma_heldout));
      
      if (y_heldout[j] == 0) {
        log_lik_heldout[j] <- cumulative_hazard_heldout;
      } 
      else {
        log_lik_heldout[j] <- log1m_exp(cumulative_hazard_heldout);
      }
      sum_log_lik_heldout <- sum_log_lik_heldout + log_lik_heldout[j];
    }",
 
         switch(growth_measure,
                dbh_dt='(true_dbh2[i] - true_dbh1[i])/census_length[i]',
                dbh_dt_rel='(log(true_dbh2[i]) - log(true_dbh1[i]))/census_length[i]',
                basal_area_dt='(0.25*pi()*true_dbh2[i]^2 - 0.25*pi()*true_dbh1[i]^2)/census_length[i]',
                basal_area_dt_rel='(log(0.25*pi()*true_dbh2[i]^2) - log(0.25*pi()*true_dbh1[i]^2))/census_length[i]'),
         ifelse("a" %in% rho_combo, " + log_a2 * log_rho_c[spp[i]]", ""),
         ifelse("b" %in% rho_combo, " + log_b2 * log_rho_c[spp[i]]", ""),
         ifelse("c" %in% rho_combo, " + log_c2 * log_rho_c[spp[i]]", ""),
         switch(growth_measure,
                dbh_dt='(true_dbh2_heldout[j] - true_dbh1_heldout[j])/census_length_heldout[j]',
                dbh_dt_rel='(log(true_dbh2_heldout[j]) - log(true_dbh1_heldout[j]))/census_length_heldout[j]',
                basal_area_dt='(0.25*pi()*true_dbh2_heldout[j]^2 - 0.25*pi()*true_dbh1_heldout[j]^2)/census_length_heldout[j]',
                basal_area_dt_rel='(log(0.25*pi()*true_dbh2_heldout[j]^2) - log(0.25*pi()*true_dbh1_heldout[j]^2))/census_length_heldout[j]'),
         ifelse("a" %in% rho_combo, " + log_a2 * log_rho_c_heldout[spp_heldout[j]]", ""),
         ifelse("b" %in% rho_combo, " + log_b2 * log_rho_c_heldout[spp_heldout[j]]", ""),
         ifelse("c" %in% rho_combo, " + log_c2 * log_rho_c_heldout[spp_heldout[j]]", ""))
    )
}
