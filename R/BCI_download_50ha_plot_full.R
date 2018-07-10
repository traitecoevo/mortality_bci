#' Download BCI plot data
#' 
#' Download BCI plot data
#' @param dest Character. Path to destination
#' @return .zip file
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
BCI_download_50ha_plot_full<- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.full.Rdata31Aug2012.zip"
  download(url, dest, mode="wb")
}