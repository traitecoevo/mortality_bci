#' Calculate hazard along trait range for combination of model parameters
#' 
#' Calculate hazard along trait range for combination of model parameters
#' @param model List object obtained from compile_models()
#' @param data Dataframe. Data used to fit model
#' @param trait Character. Name of trait (can only be `wood density`, `gap_index` or `dbh_95`)
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
param_by_trait_mu_trends <- function(model,data,trait) {
  
  if(trait %in% c("wood_density", "gap_index", "dbh_95")) {
    stop("trait can only be one of `wood_density`, `gap_index` or `dbh_95`")
  }
lapply(trait, function(x) {
  
  base::switch(x,
               "wood_density" = {
                 params = c("mu_log_alpha","mu_log_beta","mu_log_gamma","a1","b1","c1")
                 scale_weight = 0.6
                 trait_range = range(data$rho)
               },
               "gap_index" = {
                 params = c("mu_log_alpha","mu_log_beta","mu_log_gamma","a2","b2","c2")
                 scale_weight = 0.7
                 trait_range = range(data$gap_index)
               },
               "dbh_95" = {
                 params = c("mu_log_alpha","mu_log_beta","mu_log_gamma","a3","b3","c3")
                 scale_weight = 15
                 trait_range = range(data$dbh_95)
               })
  
  fit <- model$fits[[1]]
  samples <- rstan::extract(fit, pars=params)
  samples <- base::as.data.frame(lapply(samples, as.vector), stringsAsFactors = FALSE) 
  covariates <- base::data.frame(trait = rep(x,100),
                                 trait_value = seq(min(trait_range),max(trait_range),length.out = 100),
                                 trait_centered = seq(min(trait_range),max(trait_range),length.out = 100)/scale_weight,
                                 stringsAsFactors = FALSE)
  
  samples %>%
    base::merge(covariates) %>%
    dplyr::rename(trait_alpha = starts_with("a"),
                  trait_beta = starts_with("b"),
                  trait_gamma = starts_with("c")) %>%
    dplyr::mutate(alpha_only = exp(mu_log_alpha) * trait_centered^trait_alpha,
                  beta_only = exp(mu_log_beta) * trait_centered^trait_beta,
                  gamma_only = exp(mu_log_gamma) * trait_centered^trait_gamma,
                  alpha_gamma = (exp(mu_log_alpha) * trait_centered^trait_alpha) + exp(mu_log_gamma) * trait_centered^trait_gamma) %>%
    dplyr::select(alpha_only, beta_only, gamma_only, alpha_gamma, trait, trait_value,trait_centered) %>%
    tidyr::gather(param_combo,estimate, -c(trait,trait_value,trait_centered)) %>%
    dplyr::group_by(trait,param_combo,trait_value, trait_centered) %>%
    dplyr::summarise(mean = mean(estimate),
                     `2.5%` = quantile(estimate,0.025),
                     `97.5%` = quantile(estimate, 0.975)) %>%
    dplyr::ungroup()
}) %>% bind_rows
}