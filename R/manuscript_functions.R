# Manuscript functions

n_obs <- function(data) {
  prettyNum(nrow(data), big.mark = ",")
}
# Number of individuals used
n_inds <- function(data) {
  prettyNum(length(unique(data$treeid)), big.mark = ",")
}
prettyNum(sum(c(199630,211350,722040,15274700,13019600)),big.mark=",")
# Number of species used in study
n_spp <- function(data) {
  prettyNum(length(unique(data$sp)),big.mark = ",")
}

# Proportion of negative observations
prop_neg_growth <- function(data) {
  round(nrow(subset(data, obs_dbh_dt<0))/nrow(data)*100)
}

# Number of observations in remeasurement error data
nobs_meas_err_data <- function(data) {
  prettyNum(nrow(data),big.mark = ",")
}

# Observed standard deviation of measurement error
obs_error_sigma <- function(data) {
  round(sd(data$discrep),2)
}

# Number of heldout observations
n_heldout <- function(data) {
 prettyNum(nrow(data[[1]]$heldout), big.mark = ",")
}
  
# Proportion of deaths by census
prop_deaths <- function(data, census) {
res <-data %>%
  group_by(censusid) %>%
  summarise(deaths = sum(dead_next_census),
            survivors = sum(dead_next_census==0)) %>%
    ungroup() %>%
  mutate(
    total = survivors + deaths,
    prop_died = deaths/total,
    census = factor(censusid, labels=c('1995to2000','2000to2005','2005to2010'))) %>%
  select(census,censusid, total, deaths, survivors, prop_died)
signif(res[res$censusid==census,"prop_died"],2)*100
}

# Extract census scaling factor
census_effects <- function(model,census) {
  res <- model$fits[[1]]
  abs(1-signif(rstan::get_posterior_mean(res, 'census_err')[census,4],2))*100
}

# Extract proportion explained.
extract_prop_explained <- function(data, param) {
  signif(data[data$param==param,'proportion'],2)*100
}

# Prop change in logloss.
pred_gain_wd <- function(data) {
  
  df <- data %>%
    filter(growth_measure=='true_dbh_dt' &
             model_type %in% c('function_growth_comparison_base_growth_hazard_none',
                               'rho_combinations_base_growth_hazard_c',
                               'species_random_effects_base_growth_hazard_none')) %>%
    select(comparison, mean) %>%
    spread(comparison, mean) %>%
    mutate(rho_diff = function_growth_comparison - rho_combinations,
           spp_diff = function_growth_comparison - species_random_effects,
           prop_spp_gain = rho_diff/spp_diff)
  
  return(signif(df$prop_spp_gain,2)*100)
}

# Extract code for full model
fullmodelcode <- function() {
  task <- tasks_2_run("final_model")[1,]
  chunks <- get_final_model_chunks(task)
  model <- make_stan_model(chunks)
  writeLines(model$model_code)
}

# Extract baseline mortality estimate (prob death or hazard values) for different wood density
extract_baseline_estimate <- function(model, hazard=FALSE, wood_density) {
  fit <- model$fits[[1]]
  samples <- rstan::extract(fit, pars=c("mu_log_gamma","c1"))
  samples <- as.data.frame(lapply(samples, as.vector)) 
  
  
  output <-samples %>%
    mutate(wood_density_centered = wood_density/0.6,
           inst_hazard = exp(mu_log_gamma) * wood_density_centered^c1,
           prob_death = 1 - exp(-exp(mu_log_gamma) * wood_density_centered^c1))
  
  if(hazard==FALSE) {
    output %>%
      summarise(mean = mean(prob_death),
                `2.5%` = quantile(prob_death,0.025),
                `97.5%` = quantile(prob_death, 0.975))
  } else {
    output %>%
      summarise(mean = mean(inst_hazard),
                `2.5%` = quantile(inst_hazard,0.025),
                `97.5%` = quantile(inst_hazard, 0.975))
  }
}
