#' Create recruit dataframe between years 1985 to 1995 at BCI
#' 
#' Create recruit dataframe between years 1985 to 1995 at BCI
#' @param raw_BCI_data Dataframe containing raw/unprocessed BCI demographic data.
#' @return Dataframe
#' @details This function identifies individuals as recruits between census 2 (1985) and census 4 (1995).
#' Recruits are defined as those individuals that first appear in the dataset between 1985 and 1995 and have a dbh < 250 mm
#' @author James Camac (\email{james.camac@gmail.com})
#' @export
recruits_8595 <- function(raw_BCI_data) {
  
  raw_BCI_data %>%
    dplyr::filter(censusid >1 & censusid < 5) %>% # Only using recruit data from 1985 to 1995 as this uses only vertical line approach
    dplyr::arrange(sp, treeid, censusid) %>%
    dplyr::select(gx,gy,sp, treeid, censusid, dfstatus, dbh) %>%
    dplyr::filter(dfstatus=="alive" & !is.na(dbh) & !is.na(gx) & !is.na(gy)) %>% 
    # above removes dead observations & NA dbh or coordinates.
    dplyr::group_by(treeid) %>%
    dplyr::slice(1) %>% # Takes first observation per treeid
    ungroup %>%
    dplyr::filter(censusid!=2 & # removes first census as unsure whether these are recruits or not
                    dbh < 250) %>% # removes individuals with a dbh > 25 cm - unlikely to be recruits
    dplyr::mutate(censusid = as.numeric(factor(censusid))) %>% # rescales censusid such that 1 = 85. 
    dplyr::select(x=gx,y=gy,sp,treeid,censusid)
}