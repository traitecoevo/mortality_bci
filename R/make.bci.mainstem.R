#List of functions used to manipulate and summarise data from BCI.

#Download BCI data
# download function from package downloader provides wrapper
# to download file so that works for https and across platforms
BCI_download_50ha_plot_full<- function(dest) {
  require(httr)
  if(!is.character(dest)) stop('dest must be character')
  if(file.exists(dest)) stop('dest already exists')
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.full.Rdata31Aug2012.zip"
  GET(url, write_disk(f <- tempfile(fileext='.zip')), progress())
  tryCatch({
    ff <- unzip(f, exdir=tempdir())
    e <- new.env()
    lapply(ff, load, e)
    BCI_50haplot <- do.call(rbind, mget(ls(e), e))
  }, error=function(e) 
    sprintf(':c something bad happened... but your file is here: %s', f))
  save(BCI_50haplot, file=dest)
}

BCI_download_species_table <- function(dest) {
  require(httr)
  if(!is.character(dest)) stop('dest must be character')
  if(file.exists(dest)) stop('dest already exists')
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.spptable.rdata"
  GET(url, write_disk(f <- tempfile()), progress())
  nm <- load(f)
  BCI_nomenclature <- get(nm)
  save(BCI_nomenclature, file=dest)
}

BCI_calculate_individual_growth <- function(BCI_data=BCI_50haplot, spp_table=BCI_nomenclature) {
  require(dplyr)
  #Look up family
  lookup_family <- function(tag, nomen){
    i <- match(tag, tolower(nomen[['sp']]))
    nomen$Family[i]
  }
  
  #Look up species code
  lookup_latin <- function(tag, nomen){
    nomen$latin <- paste(nomen$Genus, nomen$Species)
    i <- match(tag, tolower(nomen[['sp']]))
    nomen[['latin']][i]
  }
  
  # Identifies individuals that return from the dead or are supposably refound
  # i.e. Individuals given dbh=NA and then later given numeric value
  # Note this function must be used prior to subsetting only observations with pom=1.3
  is_zombie <- function(dbh) {
    any(diff(is.na(dbh)) == -1)
  }
  
  #Calculates growth rate as a function of past size
  calculate_growth_rate <- function(x, t, f=function(y) y){
    dt = diff(t)/365.25
    if(any(dt < 0, na.rm=TRUE)){
      stop("time must be sorted")
    }
    c(NA, diff(f(x))/dt)
  }
  
  # Function to identify bad data. Adapted from function in CTFS R package
  CTFS_sanity_check <- function(dbh, dbh_increment, dbasal_diam_dt) {
    
    slope <- 0.006214
    intercept <- 0.9036 /1000   #convert from mm to m
    error_limit <- 4
    max_growth <- 75 / 1000     #convert from mm to m
    
    accept <- rep(TRUE, length(dbh))
    # Remove records based on max growth rate
    accept[dbasal_diam_dt > max_growth] <- FALSE
    # Remove records based on min growth rate, estimated from allowbale error
    allowable.decrease <- -error_limit * (slope * dbh + intercept)
    accept[dbh_increment < allowable.decrease] <- FALSE
    accept
  }
  
  mortality_in_next_census <- function(status){
    if(length(status) > 1){
      i <- 1:(length(status)-1)} # if more than 1 obs
    else{
      i <- 0
    }
    as.numeric(c(status[i] == 'alive' & status[i+1] == 'dead', NA))
  }
  
  names(BCI_50haplot) <- tolower(names(BCI_50haplot)) # lower case for all column names
  
  data <- BCI_50haplot %>%
    arrange(sp, treeid, exactdate) %>%
    select(sp, treeid, nostems, censusid, exactdate, dfstatus, pom, dbh) %>%
    mutate(
      #census id for period 7 was entered incorrectly
      censusid = ifelse(censusid==171, 7,censusid), 
      species = lookup_latin(sp, BCI_nomenclature),
      family = lookup_family(sp, BCI_nomenclature),
      #Converts dbh from mm to m
      dbh=dbh/1000) %>%
    # Remove stems from earlier census, measured with course resolution
    # First measurement in 1990 ='1990-02-06'
    filter(censusid >= 3) %>%
    # Remove families that don't exhibit dbh growth e.g. palms
    filter(!family %in% c('Arecaceae', 'Cyatheaceae', 'Dicksoniaceae', 'Metaxyaceae', 
                          'Cibotiaceae', 'Loxomataceae', 'Culcitaceae', 'Plagiogyriaceae', 
                          'Thyrsopteridaceae')) %>%
    # Remove observations without a species code
    filter(!is.na(sp)) %>%
    # For each individual..
    group_by(treeid) %>%
    # Filter plants with multiple stems
    filter(max(nostems)==1) %>%
    # Remove zombies - individuals that are recorded as dead but reappear at later date
    filter(!is_zombie(dbh)) %>%
    # Remove any measurement that was not recorded at 1.3m or that was recorded as 'dead'.
    # Second argument is because dead individuals always have pom = NA. This needs
    # to occur after zombies function
    filter(pom == '1.3' | dfstatus=='dead') %>%
    # Remove individuals that are not alive for at least 2 censuses
    mutate(
      ncensus = length(unique(exactdate[dfstatus=='alive']))) %>%
    filter(ncensus >1) %>% 
    mutate(
      # First measurement in 1990 ='1990-02-06'
      julian = as.vector(julian(as.Date(exactdate,"%Y-%m-%d"), as.Date("1990-02-06", "%Y-%m-%d"))),
      census_interval = c(NA, diff(julian/365.25)),
      dbh_increment = c(NA, diff(dbh)),
      dbh_dt = calculate_growth_rate(dbh, julian),
      dbh_dt_rel = calculate_growth_rate(dbh, julian, log),
      basal_area = 0.25*pi*dbh^2,
      basal_area_dt = calculate_growth_rate(basal_area, julian),
      basal_area_dt_rel = calculate_growth_rate(basal_area, julian, log),
      dead_next_census = mortality_in_next_census(dfstatus)) %>%
    
    # Only keep alive stems
    filter(dfstatus=="alive") %>%
    filter(CTFS_sanity_check(dbh, dbh_increment, dbh_dt)) %>%
    select(sp,treeid,censusid,exactdate,julian,census_interval,pom,nostems,
           dbh,dbh_dt,dbh_dt_rel,basal_area,basal_area_dt,
           basal_area_dt_rel,dead_next_census)
}
#Final counts between 1990 and 2010 censuses:
#No. Obs = 775871
#No. Inds = 203277
#No. Sp = 286
#No. Deaths = 200968 (~26% from 1990 to 2010) 
#No. CTFS observed errors = 35385 observations
#No. Zombies = 7330 individuals
#No. Multistems = 54614 individuals
#No. non dbh growth families = 17067 observations
#No. NA spcodes = 0. New dataset fixed this issue
#No. Inidividuals only recorded once = 103023

