# Summarise posteriors of species level trait parameters
#' 
# Summarise posteriors of species level trait parameters
#' @param model List. Model object obtained from compile_models()
#' @param data Dataframe. Data used to fit model
#' @param logscale. Logical. To return posteriors on logscale or not.
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
summarise_spp_params <- function(model, data, logscale=FALSE) {
  fit <- model$fits[[1]] # Indexing to avoid list name
  dat <- prep_data_for_stan(data, growth_measure = "true_dbh_dt")
  samples <- rstan::extract(fit, pars=c("alpha","beta","gamma"))
  samples[["alpha_gamma"]] <- samples$alpha + samples$gamma
  
  if(logscale==TRUE) {
    res <-lapply(samples, function(x) {
      cbind.data.frame(
        species = dat$species,
        sp = dat$sp,
        wood_density = dat$raw_rho,
        gap_index = dat$raw_gap_index,
        raw_dbh_95 = dat$raw_dbh_95,
        mean = apply(log(x),2, mean),
        median = apply(log(x),2, median),
        sd = apply(log(x),2,sd),
        aperm(apply(log(x),2, quantile, c(0.025,0.975)), c(2,1)))
    })
    names(res) <- paste0('log_', names(res))
    return(res)
  }
  else {
    lapply(samples, function(x) {
      cbind.data.frame(
        species = dat$species,
        sp = dat$sp,
        wood_density = dat$raw_rho,
        gap_index = dat$raw_gap_index,
        raw_dbh_95 = dat$raw_dbh_95,
        mean = apply(x,2, mean),
        median = apply(x,2, median),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, c(0.025,0.975)), c(2,1)))
    })
  }
}
