# Manuscript functions

# Number of individuals used
n_inds <- function(data) {
  as.character(round(nrow(data),-2))
}

# Number of species used in study
n_spp <- function(data) {
  length(unique(data$sp))
}

# Proportion of negative observations
prop_neg_growth <- function(data) {
  round(nrow(subset(data, obs_dbh_dt<0))/nrow(data)*100)
}

# Number of observations in remeasurement error data
nobs_meas_err_data <- function(data) {
  nrow(data$discrep)
}

# Observed standard deviation of measurement error
obs_error_sigma <- function(data) {
  round(sd(data$discrep),2)
}

# Number of heldout observations
n_heldout <- function(data) {
  as.character(nrow(data[[1]]$heldout))
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
return(signif(res[res$censusid==census,"prop_died"],2)*100)
}

# Extract census scaling factor
census_effects <- function(model,census) {
  res <- model$fits[[1]]
  abs(1-signif(rstan::get_posterior_mean(res, 'census_err')[census,4],2))*100
}

# Extract proportion explained.
extract_prop_explained <- function(data, param) {
  if(param == "species") {
    signif(data[data$param=='species','proportion'] + data[data$param=='wood_density','proportion'],2)*100
  }
  else {
  signif(data[data$param==param,'proportion'],2)*100
  }
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
