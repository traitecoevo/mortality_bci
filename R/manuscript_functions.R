
logloss_comparisons <- function(logloss_summary) {
  logloss_summary %>%
    filter(model_type %in% c("function_growth_comparison_base_growth_hazard_none", 
                             "rho_combinations_base_growth_hazard_c",
                             "species_random_effects_base_growth_hazard_none") & growth_measure=='true_dbh_dt') %>%
    select(model_type, mean) %>%
    mutate(model_type = factor(model_type, labels =c('base_growth','base_growth_rho','base_growth_re'))) %>%
    spread(model_type, mean) %>%
    mutate(rho_v_base = round((1-base_growth_rho/base_growth)*100,2),
           re_v_base = round((1-base_growth_re/base_growth)*100,2),
           rho_v_re = round((rho_v_base/re_v_base)*100,2))
}

census_effects <- function(model,census) {
  model <- model$fits[[1]]
  get_posterior_mean(model, 'census_err')[census,4]
}

# Predict median baseline hazard
predict_point_mu_baseline_hazard <- function(model,data, wood_density) {
  fit <- model$fits[[1]]
  samples <- rstan::extract(fit, pars=c("mu_log_gamma","c1"))
  samples <- as.data.frame(lapply(samples, as.vector)) 
  covariates <- data.frame(wood_density = wood_density,
                           wood_density_centered = wood_density/0.6)
  samples %>%
    merge(covariates) %>%
    mutate(inst_hazard = exp(mu_log_gamma) * wood_density_centered^c1,
           annual_mort = 1-exp(-exp(mu_log_gamma) * wood_density_centered^c1)) %>%
    gather('prediction','estimate', -mu_log_gamma,-c1,-wood_density,-wood_density_centered) %>%
    group_by(prediction) %>%
    summarise(mean = mean(estimate),
              `2.5%` = quantile(estimate,0.025),
              `97.5%` = quantile(estimate, 0.975)) %>%
    ungroup()
}