#' Calculates the mean recruit weighted gap index for each species
#' 
#' Calculates the mean recruit weighted gap index for each species
#' @param recruits Dataframe containing recruit data.
#' @param gap_index_raster List of rasters by census.
#' @return Dataframe
#' @details Species with low tolerance should have higher proportions of recruits in high gap_index conditions relative to shade tolerant species.
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
get_mean_spp_gap_index <- function(recruit_gap_conditions) {
  `%>%` <- magrittr::`%>%`
  
  base::as.data.frame(do.call(rbind, recruit_gap_conditions)) %>%
    dplyr::mutate(sp = as.character(sp)) %>% 
    dplyr::group_by(sp) %>%
    dplyr::summarise(gap_index = mean(gap_index))
}