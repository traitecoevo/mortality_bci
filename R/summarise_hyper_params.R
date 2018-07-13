#' Extract summary statistics for hyper parameters from a single model
#' 
#' Extract summary statistics for hyper parameters from a single model
#' @param model List. A list containing a stan model fit and meta data obtained from compile_models()
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
summarise_hyper_params <- function(model, data) {
  fit <- model$fits[[1]] # Use indexing to remove need to know list object name
  growth_measure <- model$model_info[[1]][["growth_measure"]]
  dat <- prep_data_for_stan(data,growth_measure, FALSE)
  samples <- rstan::extract(fit, pars=c("mu_log_alpha","mu_log_beta","mu_log_gamma",
                                        "sigma_log_alpha", "sigma_log_beta", "sigma_log_gamma"))
  
  lapply(samples, function(x) {
    cbind.data.frame(
      mean = mean(x),
      median = median(x),
      sd = sd(x),
      t(quantile(x, c(0.025,0.975))))
  }) %>% dplyr::bind_rows(.id = "param")
}