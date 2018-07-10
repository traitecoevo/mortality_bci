#' Extract weighted gap index for each recruit
#' 
#' Extract weighted gap index for each recruit
#' @param recruits Dataframe containing recruit data.
#' @param gap_index_raster List of rasters by census.
#' @return List of SpatialPointsDataDrames
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
get_recruit_gap_conditions <- function(recruits, gap_index_raster) {
  # get light environment for recruits
  recruits  %>% 
    base::as.data.frame(.) %>% # converts to df for sp package
    {sp::coordinates(.) <- ~x+y; .} %>% 
    raster::crop(raster::extent(.) - 20*2) %>%  # removes edge effects out to 20 m
    sp::split(.$censusid) %>% # splits by census
    mapply(function(raster, points) raster::extract(raster, points, sp=TRUE), gap_index_raster, .)
}