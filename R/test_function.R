# Source packages
packages <- c("rstan","dplyr","tidyr", "ggplot2","viridis","cowplot")
for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}

# Source code
sources <- list.files("R/", full.names = TRUE)
for (s in sources) {
  source(s)
}


model <- make('final_model')
data <- readRDS("data/bci_data_full.rds")
  
  # Extract mean parameter estimates
  mu_params <- summarise_hyper_params(model, data) %>%
    dplyr::select(param, mean) %>%
    filter(param %in% c("mu_log_alpha","mu_log_beta","mu_log_gamma")) %>%
    mutate_at(vars(mean),funs(exp)) %>%
    mutate(param = paste0("mu_",sub('.*\\_', '', param))) %>%
    tidyr::spread(param, mean)
  
  # Extract mean species parameter estimates
  spp_params <- summarise_spp_params(model, data) %>%
    dplyr::select(param,species,sp,mean) %>%
    dplyr::mutate(sp = as.character(sp),
                  species = as.character(species)) %>%
    dplyr::distinct() %>%
    tidyr::spread(param,mean)
  
  # Extract trait effect
  traits <- summarise_trait_params(model) %>%
    dplyr::select(param, mean) %>%
    tidyr::spread(param, mean)
  
  # Extract census effects
  census_error <- as.data.frame(rstan::extract(model$fits[[1]], pars=c('census_err'))$census_err) %>%
    dplyr::rename(`1` = V1, `2`= V2, `3` = V3) %>%
    tidyr::gather(censusid, estimate) %>%
    dplyr::group_by(censusid) %>%
    dplyr::mutate(census_error = mean(estimate)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-estimate) %>%
    dplyr::distinct() %>%
    dplyr::mutate(censusid = as.numeric(censusid))
  
  # Extract observed deaths
  observed_death <- death_proportion(data) %>%
    select(sp,n_deaths, prop_died)
  
  
  # function to predict probability of death for different levels of effects and time interval dt
  mort <- function(growth_indepenent, growth_depenent, census, dt = 1) {
    1-exp(-dt * ((growth_indepenent + growth_depenent)*census))
  }
  
  # Combine parameter estimates with demographic data
  df <- spp_params %>%
    dplyr::left_join(data, by=c("species","sp")) %>%
    dplyr::left_join(census_error, by="censusid") %>%
    dplyr::left_join(observed_death, by="sp") %>%
    dplyr::mutate(
      mu_alpha = mu_params$mu_alpha,
      mu_beta = mu_params$mu_beta,
      mu_gamma = mu_params$mu_gamma,
      a1 = traits$a1,
      a2 = traits$a2,
      a3 = traits$a3,
      b1 = traits$b1,
      b2 = traits$b2,
      b3 = traits$b3,
      c1 = traits$c1,
      c2 = traits$c2,
      c3 = traits$c3,
      true_dbh_dt_c = true_dbh_dt/0.172,
      rho_c = rho/0.6,
      gap_index_c = gap_index/0.7,
      dbh_95_c = dbh_95/15)
  
  
  # Now calculate predicted 1-yr mortality removing different effects included
  df %>% dplyr::mutate(
    full_model = mort(alpha * exp(-beta * true_dbh_dt_c), gamma, census_error, census_interval),
    full_minus_census = mort(alpha * exp(-beta * true_dbh_dt_c), gamma, mean(unique(census_error)),census_interval),
    full_minus_rho = mort((alpha/(rho_c^a1)) * exp(-(beta/(rho_c^b1)) * true_dbh_dt_c), gamma/(rho_c^c1), census_error, census_interval),
    full_minus_gap = mort((alpha/(gap_index_c^a2)) * exp(-(beta/(gap_index_c^b2)) * true_dbh_dt_c), gamma/(gap_index_c^c2), census_error, census_interval),
    full_minus_dbh = mort((alpha/(dbh_95_c^a2)) * exp(-(beta/(dbh_95_c^b2)) * true_dbh_dt_c), gamma/(dbh_95_c^c2), census_error, census_interval),
    full_minus_spp = mort(mu_alpha * exp(-mu_beta * true_dbh_dt_c), mu_gamma, census_error, census_interval), # Should this include trait effects?
    full_minus_growthdep = mort(median(alpha * exp(-beta * true_dbh_dt_c)), gamma, census_error, census_interval),
    full_minus_growthindep =  mort(alpha * exp(-beta * true_dbh_dt_c), median(gamma), census_error, census_interval)
  ) %>%
    dplyr::select(
      sp,
      prop_died,
      full_model, 
      census = full_minus_census, 
      wood_density = full_minus_rho, 
      gap_index = full_minus_gap,
      dbh_95 = full_minus_dbh,
      species = full_minus_spp, 
      growth_dependent = full_minus_growthdep, 
      growth_independent = full_minus_growthindep) %>%
    dplyr::group_by(sp, prop_died) %>%
    dplyr::summarise_all(dplyr::funs(mean)) 