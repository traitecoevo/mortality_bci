# Summarise parameter posteriors at species level
#' 
# Summarise parameter posteriors at species level
#' @param model List. Model object obtained from compile_models()
#' @param data Dataframe. Data used to fit model
#' @param logscale. Logical. To return posteriors on logscale or not.
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
summarise_spp_params <- function(model, data, logscale=FALSE) {
  
  `%>%` <- magrittr::`%>%`
  
  fit <- model$fits[[1]] # Indexing to avoid list name
  dat <- prep_data_for_stan(data, growth_measure = model$model_info[[1]][,'growth_measure'],crossval = FALSE)
  samples <- rstan::extract(fit, pars=c("alpha","beta","gamma"))
  
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
  }
  else {
    res <-lapply(samples, function(x) {
      cbind.data.frame(
        species = dat$species,
        sp = dat$sp,
        wood_density = dat$raw_rho,
        gap_index = dat$raw_gap_index,
        dbh_95 = dat$raw_dbh_95,
        mean = apply(x,2, mean),
        median = apply(x,2, median),
        sd = apply(x,2,sd),
        aperm(apply(x,2, quantile, c(0.025,0.975)), c(2,1)))
    })
  }
  dplyr::bind_rows(res,.id= "param") %>%
    tidyr::gather(trait, trait_value, -c(param,species,sp,mean,median,sd,`2.5%`,`97.5%`))
}
