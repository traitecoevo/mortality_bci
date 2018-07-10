#' Extract and load BCI plot data
#' 
#' Extract and load BCI plot data
#' @param path_to_zip Character. Path to zip file
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
BCI_load_50ha_plot <- function(path_to_zip) {
  
  tmp <- tempfile()
  unzip(path_to_zip, exdir=tmp)
  on.exit(unlink(tmp, recursive=TRUE))
  
  files <- list.files(tmp, pattern=".rdata", full.names=TRUE)
  data <- tbl_df(lapply(list.files(tmp, pattern=".rdata", full.names=TRUE), function(x) load_rdata(x)) %>% dplyr::bind_rows(.))
  names(data) <- tolower(names(data)) # lower case for all column names
  data
}