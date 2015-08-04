get_model_chunks <- function(tasks) {
  growth_measure <- tasks$growth_measure
  rho_combo <- tasks$rho_combo
  if(nchar(rho_combo) > 0) {
    rho_combo <- sapply(seq_len(nchar(rho_combo)), function(i) substr(rho_combo, i, i))
  }
  
  list(
    pars = c("mu_log_a0","sigma_log_a0","log_a0","sigma_log_a1","log_a1",if("a" %in% rho_combo) "a2",
             "mu_log_b0","sigma_log_b0","log_b0","sigma_log_b1","log_b1",if("b" %in% rho_combo) "b2",
             "mu_log_c0","sigma_log_c0","log_c0","sigma_log_c1","log_c1",if("c" %in% rho_combo) "c2"),
    parameters = sprintf("
                         // Mortality model parameters
                         real log_a0[n_spp];
                         real mu_log_a0;
                         real<lower=0> sigma_log_a0;
                         
                         real log_a1[n_census];
                         real<lower=0> sigma_log_a1;
                         
                         real log_b0[n_spp];
                         real mu_log_b0;
                         real<lower=0> sigma_log_b0;
                         
                         real log_b1[n_census];
                         real<lower=0> sigma_log_b1;
                         
                         real log_c0[n_spp];
                         real mu_log_c0;
                         real<lower=0> sigma_log_c0;
                         
                         real log_c1[n_census];
                         real<lower=0> sigma_log_c1;
                         
                         %s
                         %s
                         %s",
                         ifelse("a" %in% rho_combo, "real a2;", ""),
                         ifelse("b" %in% rho_combo, "real b2;", ""),
                         ifelse("c" %in% rho_combo, "real c2;", "")),
    model = sprintf("
                    // Declaring mortality parameters
                    real alpha;
                    real beta;
                    real gamma;
                    real cumulative_hazard;
                    for (i in 1:n_obs) {
                    // Calculating mortality parameters
                    alpha <- exp(log_a0[spp[i]]) * exp(log_a1[census[i]])%s;
                    beta <- exp(log_b0[spp[i]]) * exp(log_b1[census[i]])%s;
                    gamma <- exp(log_c0[spp[i]]) * exp(log_c1[census[i]])%s;
                    // Likelihood for hazard model
                    cumulative_hazard <- census_length[i] * (alpha * exp(-beta * growth_dt[i]) + gamma);
                    if (y[i] == 0) {
                    increment_log_prob(-cumulative_hazard);
                    } else {
                    increment_log_prob(log1m_exp(-cumulative_hazard));
                    }
                    }
                    // Priors
                    //Mortality model priors
                    log_a0 ~ normal(mu_log_a0,sigma_log_a0);
                    mu_log_a0 ~ normal(-0.87, 2);
                    sigma_log_a0 ~ cauchy(0, 2.5);
                    
                    log_b0 ~ normal(mu_log_b0, sigma_log_b0);
                    mu_log_b0 ~ normal(0, 5);
                    sigma_log_b0 ~ cauchy(0, 2.5);

                    log_c0 ~ normal(mu_log_c0, sigma_log_c0);
                    mu_log_c0 ~ normal(-4.43, 1);
                    sigma_log_c0 ~ cauchy(0, 2.5);

                    log_a1 ~ normal(0,sigma_log_a1);
                    sigma_log_a1 ~ cauchy(0, 2.5);

                    log_b1 ~ normal(0, sigma_log_b1);
                    sigma_log_b1 ~ cauchy(0, 2.5);

                    log_c1 ~ normal(0, sigma_log_c1);
                    sigma_log_c1 ~ cauchy(0, 2.5);
                    %s
                    %s
                    %s",
                    ifelse("a" %in% rho_combo, " * pow(rho_c[spp[i]], a2)", ""),
                    ifelse("b" %in% rho_combo, " * pow(rho_c[spp[i]], b2)", ""),
                    ifelse("c" %in% rho_combo, " * pow(rho_c[spp[i]], c2)", ""),
                    ifelse("a" %in% rho_combo, "a2 ~ normal(0,5);", ""),
                    ifelse("b" %in% rho_combo, "b2 ~ normal(0,5);", ""),
                    ifelse("c" %in% rho_combo, "c2 ~ normal(0,5);", "")),
    generated_quantities = sprintf("
                    real alphaB;
                    alphaB <- a2;"
  )
  )
}