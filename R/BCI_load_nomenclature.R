#' Loads species nomenclature table
#' 
#' Loads species nomenclature table
#' @param file Character. Path to file
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
BCI_load_nomenclature <- function(file){
  data <- load_rdata(file)
  names(data) <- tolower(names(data))
  data
}