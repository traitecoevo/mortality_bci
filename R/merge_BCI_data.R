#' Merge BCI individual data with trait data
#' 
#' Merge BCI individual data with trait data
#' @param BCI_demography Dataframe of individual demographic data (obtained from BCI_clean()).
#' @param traits_wood Dataframe of species wood density (obtained from load_wood_density()).
#' @param traits_dbh_95 Dataframe containing species max dbh's at 95th quantile (obtained from get_spp_dbh_95())
#' @param trait_gap_index Dataframe containing gap index data (from obtained from (get_mean_spp_gap_index()))  
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
merge_BCI_data <- function(BCI_demography, traits_wood, traits_dbh_95, traits_gap_index) {
  
  `%>%` <- magrittr::`%>%`
  
  data_1 <- base::merge(BCI_demography, traits_wood[,c('sp','rho')],by = 'sp') %>% #only uses species trait data exists for
    dplyr::filter(!is.na(rho)) %>%
    dplyr::mutate(sp_id = as.numeric(factor(sp)),
                  censusid = as.numeric(factor(census)))
  
  
  # Now merge with data on dbh_95 and gap_index
  
  # Note, in our initial submission to the journal, the traits gap_index
  # and dbh_95 were only considered in a post-hoc analyses. So in that 
  # case we used the dataset above (data_1) in the our analysis
  
  # In the revision we added dbh_95 and gap_index into the main analysis
  # The code below merges those variables in with the original variables
  # To preserve the integrity of our initial simulations, we'll require 
  # that the new dataset has the same number of rows as the original
  
  # First check have the same species
  sp1 <- unique(data_1$sp)
  sp2 <- unique(traits_dbh_95$sp)
  sp3 <- unique(traits_gap_index$sp)
  # all(sp1 %in% sp2)
  # all(sp1 %in% sp3)
  
  ## Note that traits_dbh_95 covers all species in data_1 but 
  ## traits_gap_index does not. There are 5 missing species 
  ## ( "amaico" "anacex" "caseco" "ficuto" "mar1la")
  missing_sp <- unique(sp1[!sp1 %in% sp3])
  
  ## For the 5 missing species we'll set their gap index to the  
  ## centred value (= 0.7), which is very close to the mean (=0.697)
  # mean(traits_gap_index$gap_index)
  
  gap_data2 <- rbind(traits_gap_index,
                     tibble(sp = missing_sp, gap_index = 0.7))
  
  # now combine with the original data
  data_2 <- data_1 %>% dplyr::left_join(traits_dbh_95) %>% dplyr::left_join(gap_data2) 
  
  ## Sanity check: 
  ## the new dataset should have the same number of rows as the original
  ## and have no nas
  # nrow(data_1) == nrow(data_2)
  # data_2 %>% select(rho, dbh_95, gap_index) %>% is.na() %>% sum()
  
  data_2
}