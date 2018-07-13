# Summarise trait posteriors at species level
#' 
# Summarise trait posteriors at species level
#' @param model List. Model object obtained from compile_models()
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
summarise_trait_params <- function(model) {
  
  `%>%` <- magrittr::`%>%`
  
  fit <- model$fits[[1]] # Indexing to avoid list name
  samples <- rstan::extract(fit, pars=c("a1","a2","a3","b1","b2","b3","c1","c2","c3"))
  
  res <-lapply(samples, function(x) {
    cbind.data.frame(
      mean = mean(x),
      median = median(x),
      sd = sd(x),
      `2.5%` = quantile(x, c(0.025)),
      `97.5%` = quantile(x, c(0.975)))
  }) %>% dplyr::bind_rows(.id= "param") %>%
    dplyr::mutate(parameter = rep(c("alpha","beta", "gamma"), each =3),
                  trait = rep(c("wood density","gap index", "max dbh"), 3))
}