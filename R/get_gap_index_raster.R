#' Creates gap index raster for BCI plot for censuses 1985 to 1990 and 1990 to 1995
#' 
#' Creates gap index raster for BCI plot for censuses 1985 to 1990 and 1990 to 1995
#' @param canopy_data Dataframe containing canopy data.
#' @param weight_matrix Integer matrix. A weight to be applied to both focal cell and those surround it. 
#' Default: matrix(c(1, 1, 1, 1, 8, 1, 1, 1, 1), 3, 3) weights the focal cell as the sum of all immediate neighbouring cells
#' @return List of rasters
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
get_gap_index_raster <- function(canopy_data,weight_matrix = matrix(c(1, 1, 1, 1, 8, 1, 1, 1, 1), 3, 3)) {
  
  `%>%` <- magrittr::`%>%`
  
  gap_data(canopy_data) %>%
    raster::as.data.frame(.) %>% # converts to df for sp package
    {sp::coordinates(.) <- ~x+y; .} %>%
    raster::shift(x=2.5, y=2.5) %>% # centers coordinates on cell mid point
    sp::split(.$censusid) %>% # splits by census
    lapply(function(x) { # Converts to raster
      raster::rasterFromXYZ(raster::as.data.frame(x)[c('x','y','gap_index')], res = c(5,5)) 
    }) %>% # calculates mean gap index value using weights
    lapply(raster::focal, weight_matrix, mean) %>% # calculates mean gap index value using weights
    lapply(setNames, 'gap_index') %>% # Names the gap index column
    lapply(function(x) x/raster::maxValue(x)) %>% # Converts to binary scale 0 = no gap 1 = full gap.
    {names(.) <- c("1985 to 1990", "1990 to 1995"); .}
}