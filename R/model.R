get_model_chunks <- function(tasks) {
  growth_measure <- tasks$growth_measure
  rho_combo <- tasks$rho_combo
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }

  list(
    pars = c("mu_log_a0","sigma_log_a0","a0","sigma_log_a1","a1",if("a" %in% rho_combo) "a2",
             "mu_log_b0","sigma_log_b0","b0","sigma_log_b1","b1",if("b" %in% rho_combo) "b2",
             "mu_log_c0","sigma_log_c0","c0","sigma_log_c1","c1",if("c" %in% rho_combo) "c2",
             "sum_log_lik_fit","sum_log_lik_heldout"),
    parameters = sprintf("
      // Mortality model parameters
      real raw_log_a0[n_spp];
      real mu_log_a0;
      real<lower=0> sigma_log_a0;

      real raw_log_a1[n_census];
      real<lower=0> sigma_log_a1;

      real raw_log_b0[n_spp];
      real mu_log_b0;
      real<lower=0> sigma_log_b0;

      real raw_log_b1[n_census];
      real<lower=0> sigma_log_b1;

      real raw_log_c0[n_spp];
      real mu_log_c0;
      real<lower=0> sigma_log_c0;

      real raw_log_c1[n_census];
      real<lower=0> sigma_log_c1;

      %s
      %s
      %s",
    ifelse("a" %in% rho_combo, "real<lower=0> a2;", ""),
    ifelse("b" %in% rho_combo, "real<lower=0> b2;", ""),
    ifelse("c" %in% rho_combo, "real<lower=0> c2;", "")),
  model = sprintf("
    // Declaring mortality parameters
    real alpha;
    real a0[n_spp];
    real a1[n_census];

    real beta;
    real b0[n_spp];
    real b1[n_census];

    real gamma;
    real c0[n_spp];
    real c1[n_census];

    real cumulative_hazard;

    // Calculating species random effects
    for (s in 1:n_spp) {
      a0[s] <- exp(raw_log_a0[s] * sigma_log_a0 + mu_log_a0); // e.g. implies lognormal(mu_log_a0, sigma_log_a0)
      b0[s] <- exp(raw_log_b0[s] * sigma_log_b0 + mu_log_b0);
      c0[s] <- exp(raw_log_c0[s] * sigma_log_c0 + mu_log_c0);
    }

    // Calculating census period random effects
    for (t in 1:n_census) {
      a1[t] <- exp(raw_log_a1[t] * sigma_log_a1); // e.g. implies lognormal(0, sigma_log_a1)
      b1[t] <- exp(raw_log_b1[t] * sigma_log_b1);
      c1[t] <- exp(raw_log_c1[t] * sigma_log_c1);
    }

    for (i in 1:n_obs) {
      // Calculating mortality parameters
      alpha <- a0[spp[i]] * a1[census[i]]%s;
      beta <- b0[spp[i]] * b1[census[i]]%s;
      gamma <- c0[spp[i]] * c1[census[i]]%s;

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
    raw_log_a0 ~ normal(0,1);
    mu_log_a0 ~ normal(-0.87, 0.75);
    sigma_log_a0 ~ cauchy(0, 2.5);

    raw_log_b0 ~ normal(0, 1);
    mu_log_b0 ~ normal(0, 5);
    sigma_log_b0 ~ cauchy(0, 2.5);

    raw_log_c0 ~ normal(0, 1);
    mu_log_c0 ~ normal(-4.43, 1);
    sigma_log_c0 ~ cauchy(0, 2.5);

    raw_log_a1 ~ normal(0,1);
    sigma_log_a1 ~ cauchy(0, 2.5);

    raw_log_b1 ~ normal(0, 1);
    sigma_log_b1 ~ cauchy(0, 2.5);

    raw_log_c1 ~ normal(0, 1);
    sigma_log_c1 ~ cauchy(0, 2.5);

    %s
    %s
    %s",
    ifelse("a" %in% rho_combo, " * pow(log_rho_c[spp[i]], a2)", ""),
    ifelse("b" %in% rho_combo, " * pow(log_rho_c[spp[i]], b2)", ""),
    ifelse("c" %in% rho_combo, " * pow(log_rho_c[spp[i]], c2)", ""),
    ifelse("a" %in% rho_combo, "a2 ~ lognormal(0,5);", ""),
    ifelse("b" %in% rho_combo, "b2 ~ lognormal(0,5);", ""),
    ifelse("c" %in% rho_combo, "c2 ~ lognormal(0,5);", "")),
  generated_quantities = sprintf("
    // Declaring fitted parameters
    real a0[n_spp];
    real a1[n_census];

    real b0[n_spp];
    real b1[n_census];

    real c0[n_spp];
    real c1[n_census];

    real alpha_fit;
    real beta_fit;
    real gamma_fit;
    real cumulative_hazard_fit;
    real log_lik_fit;
    real sum_log_lik_fit;

    // Declaring heldout parameters
    real alpha_heldout;
    real beta_heldout;
    real gamma_heldout;
    real cumulative_hazard_heldout;
    real log_lik_heldout[n_obs_heldout];
    real sum_log_lik_heldout;

    // Initialization of summed log likelihoods
    sum_log_lik_fit <- 0;
    sum_log_lik_heldout <- 0;

    // recalulate species random effects
    for (s in 1:n_spp) {
      a0[s] <- exp(raw_log_a0[s] * sigma_log_a0 + mu_log_a0);
      b0[s] <- exp(raw_log_b0[s] * sigma_log_b0 + mu_log_b0);
      c0[s] <- exp(raw_log_c0[s] * sigma_log_c0 + mu_log_c0);
    }

    // recalulate species random effects
    for (t in 1:n_census) {
      a1[t] <- exp(raw_log_a1[t] * sigma_log_a1);
      b1[t] <- exp(raw_log_b1[t] * sigma_log_b1);
      c1[t] <- exp(raw_log_c1[t] * sigma_log_c1);
    }

    // log likelihood for fitted model
    for (i in 1:n_obs) {
      alpha_fit <- a0[spp[i]] * a1[census[i]]%s;
      beta_fit <- b0[spp[i]] * b1[census[i]]%s;
      gamma_fit <- c0[spp[i]] * c1[census[i]]%s;

      cumulative_hazard_fit <- -census_length[i] * (alpha_fit * exp(-beta_fit * growth_dt[i]) + gamma_fit);

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
      alpha_heldout <- a0[spp_heldout[j]] * a1[census_heldout[j]]%s;
      beta_heldout <- b0[spp_heldout[j]] * b1[census_heldout[j]]%s;
      gamma_heldout <- c0[spp_heldout[j]] * c1[census_heldout[j]]%s;

      cumulative_hazard_heldout <- -census_length_heldout[j] * (alpha_heldout * exp(-beta_heldout * growth_dt_heldout[j]) + gamma_heldout);

      if (y_heldout[j] == 0) {
        log_lik_heldout[j] <- cumulative_hazard_heldout;
      }
      else {
        log_lik_heldout[j] <- log1m_exp(cumulative_hazard_heldout);
      }
      sum_log_lik_heldout <- sum_log_lik_heldout + log_lik_heldout[j];
    }",
         ifelse("a" %in% rho_combo, " * pow(log_rho_c[spp[i]], a2)", ""),
         ifelse("b" %in% rho_combo, " * pow(log_rho_c[spp[i]], b2)", ""),
         ifelse("c" %in% rho_combo, " * pow(log_rho_c[spp[i]], c2)", ""),
         ifelse("a" %in% rho_combo, " * pow(log_rho_c_heldout[spp_heldout[j]], a2)", ""),
         ifelse("b" %in% rho_combo, " * pow(log_rho_c_heldout[spp_heldout[j]], b2)", ""),
         ifelse("c" %in% rho_combo, " * pow(log_rho_c_heldout[spp_heldout[j]], c2)", ""))
    )
}
