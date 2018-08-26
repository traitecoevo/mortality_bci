#' Summarise times for each model comparison
#' 
#' Summarise times for each model comparison
#' @param times Dataframe obtained from get_chain_times().
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
summarise_model_times <- function(times) {
  `%>%` <- magrittr::`%>%`
  
  res <- times %>%
    dplyr::group_by(comparison, model, growth_measure, rho_combo) %>%
    dplyr::summarise(mn = median(total_hours))
  return(res)
}