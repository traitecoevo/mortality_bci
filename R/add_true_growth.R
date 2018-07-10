#' Add estimated true growth data to compiled BCI data
#' 
#' Add estimated true growth data (accounting for measurement error) to compiled BCI data
#' @param data Dataframe.
#' @param true_dbh_mod Stan object. Obtained from run_true_dbh_model()
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
add_true_growth <- function(data, true_dbh_mod) {
  true <- extract_true_dbh_estimates(true_dbh_mod)
  data$true_dbh1 <- true$true_dbh1
  data$true_dbh2 <- true$true_dbh2
  data$obs_dbh_dt <- (data$dbh - data$dbh_prev)/data$census_interval
  data$true_dbh_dt <- (data$true_dbh2 - data$true_dbh1)/data$census_interval
  data$true_dbh_dt_rel <- (log(data$true_dbh2) - log(data$true_dbh1))/data$census_interval
  data$obs_basal_area_dt <- ((0.25 * pi * data$dbh^2) -  (0.25 * pi * data$dbh_prev^2))/data$census_interval
  data$true_basal_area_dt <- ((0.25 * pi * data$true_dbh2^2) -  (0.25 * pi * data$true_dbh1^2))/data$census_interval
  data$true_basal_area_dt_rel <- (log(0.25 * pi * data$true_dbh2^2) -  log(0.25 * pi * data$true_dbh1^2))/data$census_interval
  data
}