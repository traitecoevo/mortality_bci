# Calculate observed proportion of death per species
#' 
# Calculate observed proportion of death per species
#' @param data Dataframe.
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

death_proportion <- function(data) {
  data %>%
    dplyr::select(species, sp, n_ind, dead_next_census) %>%
    dplyr::group_by(species,sp, n_ind) %>%
    dplyr::summarise(n_deaths = sum(dead_next_census)) %>%
    dplyr::mutate(prop_died = n_deaths/n_ind) %>%
    dplyr::ungroup()
}




