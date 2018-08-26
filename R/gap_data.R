#' Calculate minimum gap index across BCI plot for each census
#' 
#' Calculate minimum gap index across BCI plot for each census
#' @param data Dataframe containing canopy data.
#' @param k Integer. Number of data folds required
#' @return Dataframe
#' @details The presence of stratum in the following height categories (2 to 5 m; 5 to 10 m; 10 to 20 m; 20 to 30 m; and > 30 m) is summed for each year between 1985 and 1995. 
#' The minimum for each grid cell is then calculated and rescaled as to be between 0 and 1. Here a 1 signifies a gap (i.e. no vegetation above 2 m) and a 0 signifies a area with vegetation present at
#' all strata levels above 2m (i.e. non-gap)
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
gap_data <- function(canopy_data) {
  `%>%` <- magrittr::`%>%`
  canopy_data %>%
    dplyr::select(-ht0_1) %>% # Not used in earlier years
    dplyr::filter(year >=1985 & year <= 1995) %>% # 2003 onwwards uses voxels which is not comparable to earlier years
    dplyr::group_by(x,y,year) %>%
    dplyr::mutate(sum_canopy_above2m = sum(ht2_5,ht5_10,ht10_20,ht20_30,ht30_, na.rm=TRUE)) %>% 
    # sums presences above 2m and converts it to a scale between 0 and 1.
    #where 1 = gap (no veg above 2 m) & 0 = non gap (veg in all strata above 2 m)
    dplyr::ungroup() %>%
    dplyr::mutate(censusid = base::findInterval(year,c(1990))+1) %>% # 1= past light environment for 1985 recruits etc..
    dplyr::group_by(x,y,censusid) %>%
    dplyr::summarise(gap_index = (5 - min(sum_canopy_above2m))/5) %>% # Find minumum canopy stratum above 2m per census.
    dplyr::ungroup() %>%
    dplyr::select(x,y,censusid,gap_index)
}