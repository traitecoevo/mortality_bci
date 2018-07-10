#' Loads wood density data into R
#' 
#' Loads wood density data into R
#' @param file Character. Path to file
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
load_wood_density <- function(file) {
  data <- read.csv(file, stringsAsFactors=FALSE)
  names(data) <- tolower(names(data)) # lowers trait column names for merging
  data$sp <- tolower(data$sp) # lowers species code names for merging
  
  dplyr::mutate(data, rho = sg100c_avg) %>% # in g/cm3
    dplyr::select(-sg100c_avg)
}
