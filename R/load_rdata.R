#' Generic loading RData loading function
#' 
#' Loads an RData file, and returns it
#' @param file Character. Path to file
#' @return Loads RData object into R
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
load_rdata <- function(file) {
  v <- load(file)
  get(v)
}