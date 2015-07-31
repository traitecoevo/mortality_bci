get_model_chunks <- function(tasks) {
  growth_measure <- tasks$growth_measure
  rho_combo <- tasks$rho_combo
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }

  list(
    pars = c("mu_log_a0","sigma_log_a0","log_a0","sigma_log_a1","log_a1",if("a" %in% rho_combo) "a2",
             "mu_log_b0","sigma_log_b0","log_b0","sigma_log_b1","log_b1",if("b" %in% rho_combo) "b2",
             "mu_log_c0","sigma_log_c0","log_c0","sigma_log_c1","log_c1",if("c" %in% rho_combo) "c2",
             "sum_log_lik_fit","sum_log_lik_heldout"),
    parameters = sprintf("
      // Mortality model parameters
      real log_raw_a0[n_spp];
      real mu_log_a0;
      real<lower=0> sigma_log_a0;

      real log_raw_a1[n_census];
      real<lower=0> sigma_log_a1;

      real log_raw_b0[n_spp];
      real mu_log_b0;
      real<lower=0> sigma_log_b0;

      real log_raw_b1[n_census];
      real<lower=0> sigma_log_b1;

      real log_raw_c0[n_spp];
      real mu_log_c0;
      real<lower=0> sigma_log_c0;

      real log_raw_c1[n_census];
      real<lower=0> sigma_log_c1;

      %s
      %s
      %s",
    ifelse("a" %in% rho_combo, "real a2;", ""),
    ifelse("b" %in% rho_combo, "real b2;", ""),
    ifelse("c" %in% rho_combo, "real c2;", "")),
  model = sprintf("
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
      log_a0[s] <- log_raw_a0[s] * sigma_log_a0 + mu_log_a0; // e.g. implies normal(mu_log_a0, sigma_log_a0)
      log_b0[s] <- log_raw_b0[s] * sigma_log_b0 + mu_log_b0;
      log_c0[s] <- log_raw_c0[s] * sigma_log_c0 + mu_log_c0;
    }

    // Calculating census period random effects
    for (t in 1:n_census) {
      log_a1[t] <- log_raw_a1[t] * sigma_log_a1; // e.g. implies normal(0, sigma_log_a1)
      log_b1[t] <- log_raw_b1[t] * sigma_log_b1;
      log_c1[t] <- log_raw_c1[t] * sigma_log_c1;
    }

    for (i in 1:n_obs) {
      // Calculating mortality parameters
      log_alpha <- log_a0[spp[i]] + log_a1[census[i]]%s;
      log_beta <- log_b0[spp[i]] + log_b1[census[i]]%s;
      log_gamma <- log_c0[spp[i]] + log_c1[census[i]]%s;

    // Likelihood for hazard model
    cumulative_hazard <- -census_length[i] * (exp(log_alpha - exp(log_beta) * growth_dt[i]) + exp(log_gamma));

      if (y[i] == 0) {
        increment_log_prob(cumulative_hazard);
      } else {
        increment_log_prob(log1m_exp(cumulative_hazard));
      }
    }

    // Priors

    //Mortality model priors
    log_raw_a0 ~ normal(0,1);
    mu_log_a0 ~ normal(-0.87, 0.75);
    sigma_log_a0 ~ cauchy(0, 2.5);

    log_raw_b0 ~ normal(0, 1);
    mu_log_b0 ~ normal(0, 2.5);
    sigma_log_b0 ~ cauchy(0, 2.5);

    log_raw_c0 ~ normal(0, 1);
    mu_log_c0 ~ normal(-4.43, 0.23);
    sigma_log_c0 ~ cauchy(0, 2.5);

    log_raw_a1 ~ normal(0,1);
    sigma_log_a1 ~ cauchy(0, 2.5);

    log_raw_b1 ~ normal(0, 1);
    sigma_log_b1 ~ cauchy(0, 2.5);

    log_raw_c1 ~ normal(0, 1);
    sigma_log_c1 ~ cauchy(0, 2.5);

    %s
    %s
    %s",
    ifelse("a" %in% rho_combo, " + a2 * log_rho_c[spp[i]]", ""),
    ifelse("b" %in% rho_combo, " + b2 * log_rho_c[spp[i]]", ""),
    ifelse("c" %in% rho_combo, " + c2 * log_rho_c[spp[i]]", ""),
    ifelse("a" %in% rho_combo, "a2 ~ normal(0,2.5);", ""),
    ifelse("b" %in% rho_combo, "b2 ~ normal(0,2.5);", ""),
    ifelse("c" %in% rho_combo, "log_c2 ~ normal(0,2.5);", "")),
  generated_quantities = sprintf("
    // Declaring fitted parameters
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
    real log_lik_fit;
    real sum_log_lik_fit;

    // Declaring heldout parameters
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
      log_a0[s] <- log_raw_a0[s] * sigma_log_a0 + mu_log_a0;
      log_b0[s] <- log_raw_b0[s] * sigma_log_b0 + mu_log_b0;
      log_c0[s] <- log_raw_c0[s] * sigma_log_c0 + mu_log_c0;
    }

    // recalulate species random effects
    for (t in 1:n_census) {
      log_a1[t] <- log_raw_a1[t] * sigma_log_a1;
      log_b1[t] <- log_raw_b1[t] * sigma_log_b1;
      log_c1[t] <- log_raw_c1[t] * sigma_log_c1;
    }

    // log likelihood for fitted model
    for (i in 1:n_obs) {
      log_alpha_fit <- log_a0[spp[i]] + log_a1[census[i]]%s;
      log_beta_fit <- log_b0[spp[i]] + log_b1[census[i]]%s;
      log_gamma_fit <- log_c0[spp[i]] + log_c1[census[i]]%s;

      cumulative_hazard_fit <- -census_length[i] * (exp(log_alpha_fit - exp(log_beta_fit) * growth_dt[i]) + exp(log_gamma_fit));

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
         ifelse("a" %in% rho_combo, " + a2 * log_rho_c[spp[i]]", ""),
         ifelse("b" %in% rho_combo, " + b2 * log_rho_c[spp[i]]", ""),
         ifelse("c" %in% rho_combo, " + c2 * log_rho_c[spp[i]]", ""),
         ifelse("a" %in% rho_combo, " + a2 * log_rho_c_heldout[spp_heldout[j]]", ""),
         ifelse("b" %in% rho_combo, " + b2 * log_rho_c_heldout[spp_heldout[j]]", ""),
         ifelse("c" %in% rho_combo, " + c2 * log_rho_c_heldout[spp_heldout[j]]", ""))
    )
}
