#' Loads BCI dbh measurement error data
#' 
#' Loads BCI dbh measurement error data
#' @param file Character. Path to file
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
BCI_load_dbh_error_data <- function(file) {
  data <-  load_rdata(file)
  data$dbh1 <- data$dbh1/10 # convert mm to cm
  data$dbh2 <- data$dbh2/10 # convert mm to cm 
  data$discrep <- data$dbh1 - data$dbh2
  data
}