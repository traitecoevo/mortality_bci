#List of functions used to manipulate and summarise data from BCI.

subset_BCI_data_by_census <- function(data, census=4) {
  filter(data, censusid==census)
}

merge_BCI_data <- function(BCI_demography, BCI_traits) {
  merge(BCI_demography,BCI_traits[,c('sp','sg100c_avg')],by = 'sp') %>% #only uses species trait data exists for.
  filter(!is.na(sg100c_avg) & !is.na(dbh_dt) & !is.na(dead_next_census))
}

#Download BCI data
# download function from package downloader provides wrapper
# to download file so that works for https and across platforms
BCI_download_50ha_plot_full<- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.full.Rdata31Aug2012.zip"
  download(url, dest, mode="wb")
}

#Load 50ha census data
BCI_load_50ha_plot <- function(path_to_zip) {

  tmp <- tempfile()
  unzip(path_to_zip, exdir=tmp)
  on.exit(unlink(tmp, recursive=TRUE))

  files <- list.files(tmp, pattern=".rdata", full.names=TRUE)
  data <- tbl_df(lapply(list.files(tmp, pattern=".rdata", full.names=TRUE), function(x) load_rdata(x)) %>% rbind_all)
  names(data) <- tolower(names(data)) # lower case for all column names
  data
}

#loads an RData file, and returns it
load_rdata <- function(file) {
  v <- load(file)
  get(v)
}

BCI_download_species_table <- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.spptable.rdata"
  download(url, dest, mode="wb")
}

BCI_load_nomenclature <- function(file){
  data <- load_rdata(file)
  names(data) <- tolower(names(data))
  data
}

#Look up family
lookup_family <- function(tag, spp_table){
  i <- match(tag, tolower(spp_table[['sp']]))
  spp_table$family[i]
}

#Look up species code
lookup_latin <- function(tag, spp_table){
  spp_table$latin <- paste(spp_table$genus, spp_table$species)
  i <- match(tag, tolower(spp_table[['sp']]))
  spp_table[['latin']][i]
}

load_trait_data <- function(file) {
  data <- read.csv(file, stringsAsFactors=FALSE)
  names(data) <- tolower(names(data)) # lowers trait column names for merging
  data$sp <- tolower(data$sp) # lowers species code names for merging
  data
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

BCI_calculate_individual_growth <- function(BCI_data, spp_table) {

  data <- BCI_data %>%
    arrange(sp, treeid, exactdate) %>%
    select(sp, treeid, nostems, censusid, exactdate, dfstatus, pom, dbh) %>%
    mutate(
      #census id for period 7 was entered incorrectly
      censusid = ifelse(censusid==171, 7,censusid),
      species = lookup_latin(sp, spp_table),
      family = lookup_family(sp, spp_table),
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

