#' Find the 95th percentile dbh (cm) observed for each species.
#' 
#' Find the 95th percentile dbh (cm) observed for each species.
#' @param raw_BCI_data Dataframe containing raw/unprocessed BCI demographic data.
#' @return Dataframe
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export
get_spp_dbh_95 <- function(raw_BCI_data) {
  
  `%>%` <- magrittr::`%>%`
  
  raw_BCI_data %>%
    dplyr::arrange(sp, treeid, exactdate) %>%
    dplyr::select(sp, treeid, nostems, pom, dbh) %>%
    dplyr::filter(!is.na(sp)) %>%
    dplyr::mutate(dbh = dbh/10) %>%
    dplyr::group_by(treeid) %>%
    dplyr::filter(max(nostems, na.rm = TRUE)==1 & !is.na(dbh)) %>%
    dplyr::filter(pom == '1.3') %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sp) %>%
    dplyr::summarise(dbh_95 = quantile(dbh, 0.95, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(sp,dbh_95)
}