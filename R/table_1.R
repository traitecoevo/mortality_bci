# Prepare table 1 for manuscript
#' 
# Prepare table 1 for manuscript
#' @param model List containing stan fit object
#' @param data Dataframe used to fit model
#' @return Dataframe
#' @details This function examines how both R2 and AUC change when certain parameters are removed from the model.
#' R2 is calculated as a function of a the modelled mean probability of death across individuals for a given species
#' against the observed proportion of individuals that died per species. AUC simply compares the raw observations against the model
#' predicted probabilities for each individual.
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
table_1 <- function(model, data) {
  
  `%>%` <- magrittr::`%>%`
  
  # Extract mean parameter estimates
  mu_params <- summarise_hyper_params(model, data) %>%
    dplyr::select(param, mean) %>%
    filter(param %in% c("mu_log_alpha","mu_log_beta","mu_log_gamma")) %>%
    mutate_at(vars(mean),funs(exp)) %>%
    mutate(param = paste0("mu_",sub('.*\\_', '', param))) %>%
    tidyr::spread(param, mean)
  
  # Extract mean species parameter estimates
  # These parameters willbe alpha beta and gamma. They encompass any trait effects
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
    dplyr::select(sp,n_deaths, prop_died)
  
  
  # Function to predict probability of death
  mort <- function(growth_depenent, growth_indepenent, census, dt = 1) {
    1-exp(-dt * ((growth_indepenent + growth_depenent)*census))
  }
  
  # Combine parameter estimates with demographic data
  full_dat <- spp_params %>%
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
      true_dbh_dt_c = true_dbh_dt - 0.172,
      rho_c = rho/0.6,
      gap_index_c = gap_index/0.7,
      dbh_95_c = dbh_95/15)
  
  
  # Now calculate predicted mortality under full and submodels
  predictions <- full_dat %>% dplyr::mutate(
    full_model = mort(alpha * exp(-beta * true_dbh_dt_c), gamma, census_error, census_interval),
    full_minus_census = mort(alpha * exp(-beta * true_dbh_dt_c), gamma, 1,census_interval), 
    full_minus_rho = mort((alpha/(rho_c^a1)) * exp(-(beta/(rho_c^b1)) * true_dbh_dt_c), gamma/(rho_c^c1), census_error, census_interval),
    full_minus_gap = mort((alpha/(gap_index_c^a2)) * exp(-(beta/(gap_index_c^b2)) * true_dbh_dt_c), gamma/(gap_index_c^c2), census_error, census_interval),
    full_minus_dbh = mort((alpha/(dbh_95_c^a3)) * exp(-(beta/(dbh_95_c^b3)) * true_dbh_dt_c), gamma/(dbh_95_c^c3), census_error, census_interval),
    full_minus_sppre = mort((mu_alpha*(rho_c^a1)*(gap_index_c^a2)*(dbh_95_c^a3)) * exp(-(mu_beta*(rho_c^b1)*(gap_index_c^b2)*(dbh_95_c^b3)) * true_dbh_dt_c),
                            (mu_gamma*(rho_c^c1)*(gap_index_c^c2)*(dbh_95_c^c3)), census_error, census_interval),
    full_minus_spp = mort(mu_alpha * exp(-mu_beta * true_dbh_dt_c), mu_gamma, census_error, census_interval), 
    full_minus_growthdep = mort(0, gamma, census_error, census_interval),
    full_minus_growthindep =  mort(alpha * exp(-beta * true_dbh_dt_c), 0, census_error, census_interval) 
  ) %>%
    dplyr::select(
      sp,
      prop_died,
      dead_next_census,
      `Full model` = full_model, 
      `Full - Census error` = full_minus_census, 
      `Full - Wood density` = full_minus_rho, 
      `Full - Gap index` = full_minus_gap,
      `Full - Max dbh` = full_minus_dbh,
      `Full - Species error` = full_minus_sppre,
      `Full - Species traits & species error` = full_minus_spp,
      `Full - Growth dependent hazard` = full_minus_growthdep, 
      `Full - Growth independent hazard` = full_minus_growthindep)
  
  
  # Calculate R2 relative to observed proportion of death per species
  R2 <- predictions %>%
    dplyr::select(-dead_next_census) %>%
    dplyr::group_by(sp, prop_died) %>%
    dplyr::summarise_all(dplyr::funs(mean)) %>%
    dplyr::ungroup() %>%
    tidyr::gather(Model, mean_prob_death, -c(sp,prop_died)) %>%
    dplyr::group_by(Model) %>%
    dplyr::mutate(r2 = cor(mean_prob_death,prop_died)^2) %>%
    dplyr::select(-c(sp, prop_died,mean_prob_death)) %>%
    dplyr::distinct()
  
  # Calculate AUC based on observed predictions against truth
  AUC <- function(response, predicted) {
    pROC::auc(response, predicted)[1]
  }
  
  auc <- predictions %>%
    dplyr::select(-c(dead_next_census,sp, prop_died)) %>%
    dplyr::summarise_all(.funs = AUC, response =predictions$dead_next_census) %>%
    tidyr::gather(Model, AUC)
  
  dplyr::left_join(R2, auc, by="Model")
}