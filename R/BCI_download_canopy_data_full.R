#' Download BCI canopy strata data
#' 
#' Download BCI canopy strata data
#' @param dest Character. Path to destination
#' @return .zip file
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
BCI_download_canopy_data_full<- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/21929/Canopy_Ascii.zip"
  download(url, dest, mode="wb")
}
