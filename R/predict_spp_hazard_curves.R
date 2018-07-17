# Predict hazard rates/ annual mortality rates for each species along a range of growth rates
#' 
# Predict hazard rates/ annual mortality rates for each species along a range of growth rates
#' @param model List. Model object obtained from compile_models()
#' @param data Dataframe. Data used to fit model
#' @param growth_range Numeric vector. Vector defining the annual range of dhb growth in cm
#' @return Dataframe containing simulated annual mortality rates (and instantaneous hazard rates) for each species
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
predict_spp_hazard_curves <- function(model, data, growth_range = c(0.03,0.5)) {
  
  `%>%` <- magrittr::`%>%`
  
  spp_parameters <- summarise_spp_params(model, data)
  growth_rates <- base::data.frame(dbh_growth = seq(min(growth_range),max(growth_range),length.out = 100), 
                                   dbh_growth_centered = seq(min(growth_range),max(growth_range),length.out = 100) - 0.172)
  
  res <- spp_parameters %>%
    dplyr::select(-c(median,sd,`2.5%`,`97.5%`)) %>%
    tidyr::spread(param, mean) %>%
    base::merge(growth_rates) %>%
    dplyr::mutate(
      inst_hazard = alpha * exp(-beta * dbh_growth_centered) + gamma,
      annual_prob_mort = 1- exp(-(alpha * exp(-beta * dbh_growth_centered) + gamma)))
}