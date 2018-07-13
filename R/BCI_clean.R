#' Clean and processBCI plot data
#' 
#' Clean and prcoess BCI plot data
#' @param BCI_data Dataframe. Dataframe containing individual data
#' @param spp_table Dataframe. Dataframe containing species nomenclature data
#' @return Cleaned dataframe
#' @details This function runs a series of sub routines to clean and process the data. Specifically it:
#' 1) Removes families that don't exhibit dbh growth e.g. palms It removes zombie plants,
#' 2) Removes data to from census 1 and 2 (i.e. data is from 1990-02-06)
#' 3) Removes observations without a species code
#' 4) Removes individuals with multiple stems
#' 5) Removes zombies - individuals that are recorded as dead but reappear at later date
#' 6) Removes extreme growth rates (i.e. those greater than 5 cm/yr or those that shrink by more than 25% of the previous diameter)
#' 7) Removes any measurement that was not recorded at 1.3m
#' 8) Ensures at each species has a minimum of 10 individuals
#' 9) Fixes minor typos identified in the data
#' 
#' The function also:
#' 1) Estimates annual growth rates
#' 2) Whether the individual died in the subsequent census
#' 3) Links species codes with species nomenculture
#' 4) Creates a new ID for each individual
#' @author James Camac (\email{james.camac@gmail.com}) & Daniel Falster (\email{daniel.falster@unsw.edu.au})
#' @export

BCI_clean <- function(BCI_data, spp_table) {
  
  `%>%` <- magrittr::`%>%`
  # SUB FUNCTION: Drop last observation
  drop_last <- function(x) {
    if(length(x) > 0)
      x[seq_len(length(x)-1)]
    else
      NULL
  }
  
  # SUB FUNCTION: Look up family
  lookup_family <- function(tag, spp_table){
    i <- match(tag, tolower(spp_table[['sp']]))
    spp_table$family[i]
  }
  
  # SUB FUNCTION: Look up species code
  lookup_latin <- function(tag, spp_table){
    spp_table$latin <- paste(spp_table$genus, spp_table$species)
    i <- match(tag, tolower(spp_table[['sp']]))
    spp_table[['latin']][i]
  }
  
  # SUB FUNCTION: Identify zombie plants
  # Identifies individuals that return from the dead
  # i.e. Individuals given dbh=NA and then later given numeric value
  # Note this function must be used prior to subsetting only observations with pom=1.3
  is_zombie <- function(dbh) {
    any(diff(is.na(dbh)) == -1)
  }
  
  # SUB FUNCTION: Calculates growth rate as a function of past size
  calculate_growth_rate <- function(x, t, f=function(y) y){
    dt = diff(t)/365.25
    if(any(dt < 0, na.rm=TRUE)){
      stop("time must be sorted")
    }
    c(NA, diff(f(x))/dt)
  }
  
  # SUB FUNCTION: Check if individual died next census
  mortality_in_next_census <- function(status){
    if(length(status) > 1){
      i <- 1:(length(status)-1)} # if more than 1 obs
    else{
      i <- 0
    }
    as.numeric(c(status[i] == 'alive' & status[i+1] == 'dead', NA))
  }
  
  # Add taxonomic information
  BCI_data$species = lookup_latin(BCI_data$sp, spp_table)
  BCI_data$family = lookup_family(BCI_data$sp, spp_table)
  
  # Census id for period 7 was entered incorrectly
  BCI_data$census <- BCI_data$censusid
  BCI_data$census[ BCI_data$census==171] <- 7
  
  #Converts dbh from mm to cm
  BCI_data$dbh <- BCI_data$dbh/10
  
  data <- BCI_data %>%
    dplyr::arrange(sp, treeid, exactdate) %>%
    dplyr::select(gx,gy,sp, species, family, treeid, nostems, census, exactdate, dfstatus, pom, dbh) %>%
    dplyr::filter(
      # Remove stems from earlier census, measured with course resolution
      # First measurement in 1990 ='1990-02-06'
      census >= 3 &
        # Remove families that don't exhibit dbh growth e.g. palms
        !family %in% c('Arecaceae', 'Cyatheaceae', 'Dicksoniaceae', 'Metaxyaceae',
                       'Cibotiaceae', 'Loxomataceae', 'Culcitaceae', 'Plagiogyriaceae',
                       'Thyrsopteridaceae') &
        # Remove observations without a species code
        !is.na(sp)
    ) %>%
    # fix missing species name.
    dplyr::mutate(species = replace(species, sp == "swars2", "Swartzia simplex2")) %>%
    # For each individual..
    dplyr::group_by(treeid) %>%
    # Filter plants with multiple stems
    dplyr::filter(max(nostems)==1) %>%
    # Remove zombies - individuals that are recorded as dead but reappear at later date
    dplyr::filter(!is_zombie(dbh)) %>%
    # Remove any measurement that was not recorded at 1.3m or that was recorded as 'dead'.
    # Second argument is because dead individuals always have pom = NA. This needs
    # to occur after zombies function
    dplyr::filter(pom == '1.3' | dfstatus=='dead') %>%
    # Remove individuals that are not alive for at least 2 censuses
    dplyr::mutate(
      dead_next_census = mortality_in_next_census(dfstatus)) %>%
    dplyr::filter(
      # Only keep alive stems
      dfstatus=="alive" &
        #Removes data from most recent survey because survival unknown
        !is.na(dead_next_census)
    ) %>%
    dplyr::mutate(
      n_census = length(census),
      # First measurement in 1990 ='1990-02-06'
      julian = as.vector(julian(as.Date(exactdate,"%Y-%m-%d"), as.Date("1990-02-06", "%Y-%m-%d"))),
      census_interval = c(NA, diff(julian/365.25)),
      dbh_dt = calculate_growth_rate(dbh, julian),
      dbh_prev = c(NA, drop_last(dbh))
    ) %>%
    dplyr::filter(
      # Some individuals
      n_census > 1 &
        # Some individuals skipped a census and therefore have interval much more than 5 years
        census_interval < 8  &
        # Remove extreme growth rates
        dbh_dt < 5 &
        dbh_dt/(dbh) > -0.25 &
        # Remove anything that doesn't have adequate growth from previous period
        !is.na(census_interval*dbh_dt)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(sp) %>%
    dplyr::mutate(n_ind = length(unique(treeid))) %>%
    # ensures at least 1 individual is in the heldout dataset
    dplyr::filter(n_ind >=10) %>%
    dplyr::ungroup() %>%
    dplyr::select(gx,gy,species,sp,n_ind,treeid,census,exactdate,julian,census_interval,pom,nostems,
                  dbh_prev,dbh,dead_next_census)
}