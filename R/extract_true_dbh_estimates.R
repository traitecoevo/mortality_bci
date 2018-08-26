#' Extract dbh estimates from measurement error model
#' 
#' Extract estimated dbh estimates (without measurement error) for each individual at each census from optimisation output
#' @param optimization_results Model output from function run_true_dbh_model()
#' @return Dataframe containing estimated true growth rate and true dbh's
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
extract_true_dbh_estimates <- function(optimization_results) {
  `%>%` <- magrittr::`%>%`
  
  fit <- optimization_results$par
  base::data.frame(fit, nm=names(fit)) %>%
    tidyr::separate(nm, c('variable', 'ind'), sep='\\[', fill='right') %>%
    dplyr::mutate(ind=sub('\\]', '', ind)) %>%
    dplyr::filter(variable %in% c('true_dbh1','true_dbh2','true_growth_rate')) %>%
    tidyr::spread(variable, fit) %>%
    dplyr::mutate(ind = as.integer(ind)) %>%
    dplyr::arrange(ind)
}
