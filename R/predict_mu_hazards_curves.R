# Predict average hazard rates/ annual mortality rates for each species along a range of growth rates for particular trait values.
#' 
# Predict average hazard rates/ annual mortality rates for each species along a range of growth rates for particular trait values.
#' @param model List. Model object obtained from compile_models()
#' @param growth_range Numeric vector. Vector defining the annual range of dhb growth in cm
#' @param trait Character. Name of trait. Can only be `wood density`, `dbh_95` or `gap_index`
#' @param trait_values Numeric vector. The trait values one wishes to examine.
#' @param hazard_curve Logical. Whether to output instantaneous hazard (TRUE) or annual probabilities (FALSE) 
#' @return Dataframe containing mean simulated annual mortality rates (or instantaneous hazard rates) for each species for a single trait. 
#' Other trait effects kept are kept at their mean values.
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
predict_mu_hazards_curves <- function(model,growth_range = c(0.03,0.5), trait = "wood_density", trait_values, hazard_curve = TRUE) {
  
  if(trait %in% c("wood_density", "gap_index", "dbh_95")) {
    stop("trait can only be one of `wood_density`, `gap_index` or `dbh_95`")
  }
  
  base::switch(trait,
               "wood_density" = {
                 params <- c("mu_log_alpha","mu_log_beta","mu_log_gamma","a1","b1","c1")
                 scale_weight = 0.6 
                 },
               "gap_index" = {
                 params <- c("mu_log_alpha","mu_log_beta","mu_log_gamma","a2","b2","c2")
                 scale_weight = 0.7 
               },
               "dbh_95" = {
                 params <- c("mu_log_alpha","mu_log_beta","mu_log_gamma","a3","b3","c3")
                 scale_weight = 15 
               })
  
  fit <- model$fits[[1]]
  samples <- rstan::extract(fit, pars=params)
  samples <- base::as.data.frame(lapply(samples, as.vector)) 
  covariates <- base::data.frame(type = as.factor(rep(c('low','high'), each = 100)),
                                 dbh_growth = rep(seq(min(growth_range),max(growth_range),length.out = 100),2), 
                                 dbh_growth_centered = rep(seq(min(growth_range),max(growth_range),length.out = 100) - 0.172,2),
                                 trait_value = rep(trait_range, each = 100),
                                 trait_centered = rep(trait_range/scale_weight, each = 100))
  
  output <-samples %>% # only propogate uncertainty in wood density effect
    base::merge(covariates) %>%
    dplyr::mutate(mu_alpha = mean(exp(mu_log_alpha)),
                  mu_beta = mean(exp(mu_log_beta)),
                  mu_gamma = mean(exp(mu_log_gamma))) %>%
    dplyr::rename(trait_alpha = starts_with("a"),
                  trait_beta = starts_with("b"),
                  trait_gamma = starts_with("c")) %>%
    dplyr::mutate(
      trait = trait,
      inst_hazard = (mu_alpha * trait_centered^trait_alpha) * exp(-(mu_beta * trait_centered^trait_beta) * dbh_growth_centered) + mu_gamma * trait_centered^trait_gamma,
      annual_prob_mort = 1-exp(-((mu_alpha * trait_centered^trait_alpha) * exp(-(mu_beta * trait_centered^trait_beta) * dbh_growth_centered) + mu_gamma * trait_centered^trait_gamma))) %>%
    dplyr::group_by(type, dbh_growth, trait, trait_value)
  
  if(hazard_curve == FALSE) { 
    output %>%
      dplyr::summarise(mean = mean(annual_prob_mort),
                       `2.5%` = quantile(annual_prob_mort,0.025),
                       `97.5%` = quantile(annual_prob_mort, 0.975)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(type, dbh_growth)
  }
  else {
    output %>%
      dplyr::summarise(mean = mean(inst_hazard),
                       `2.5%` = quantile(inst_hazard,0.025),
                       `97.5%` = quantile(inst_hazard, 0.975)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(type, dbh_growth)
  }
}
