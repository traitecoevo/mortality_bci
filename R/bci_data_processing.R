# This file contains functions to download, load and process BCI datasets.


# DOWNLOADING FUNCTIONS

# Download BCI data
BCI_download_50ha_plot_full<- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.full.Rdata31Aug2012.zip"
  download(url, dest, mode="wb")
}

# Download canopy strata data
BCI_download_canopy_data_full<- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/21929/Canopy_Ascii.zip"
  download(url, dest, mode="wb")
}

# Downloads species table
BCI_download_species_table <- function(dest) {
  url <-"https://repository.si.edu/bitstream/handle/10088/20925/bci.spptable.rdata"
  download(url, dest, mode="wb")
}

# REMAKE EXPORT FUNCTION
## Required for remake (1 function -> n file outputs)
export_data <- function(data, filename) {
  filename_fmt <- sub("\\.rds$", "_%s.rds", filename)
  filename_sub <- sprintf(filename_fmt, seq_along(data))
  for (i in seq_along(data)) {
    saveRDS(data[[i]], filename_sub[[i]])
  }
}

# GENERIC LOADING FUNCTION
# loads an RData file, and returns it
load_rdata <- function(file) {
  v <- load(file)
  get(v)
}

# SPECIFIC LOADING FUNCTIONS
# Load 50ha census data
BCI_load_50ha_plot <- function(path_to_zip) {
  
  tmp <- tempfile()
  unzip(path_to_zip, exdir=tmp)
  on.exit(unlink(tmp, recursive=TRUE))
  
  files <- list.files(tmp, pattern=".rdata", full.names=TRUE)
  data <- tbl_df(lapply(list.files(tmp, pattern=".rdata", full.names=TRUE), function(x) load_rdata(x)) %>% bind_rows)
  names(data) <- tolower(names(data)) # lower case for all column names
  data
}
# Loads wood density data 
load_trait_data <- function(file) {
  data <- read.csv(file, stringsAsFactors=FALSE)
  names(data) <- tolower(names(data)) # lowers trait column names for merging
  data$sp <- tolower(data$sp) # lowers species code names for merging
  
  mutate(data, rho = sg100c_avg) %>% # in g/cm3
    select(-sg100c_avg)
}

# Load BCI measurement error data
BCI_load_dbh_error_data <- function(file) {
  data <-  load_rdata(file)
  data$dbh1 <- data$dbh1/10 # convert mm to cm
  data$dbh2 <- data$dbh2/10 # convert mm to cm 
  data$discrep <- data$dbh1 - data$dbh2
  data
}

# Load BCI canopy data
BCI_load_canopy <- function(path_to_zip) {
  
  tmp <- tempfile()
  unzip(path_to_zip, exdir=tmp)
  on.exit(unlink(tmp, recursive=TRUE))
  
  files <- list.files(tmp, pattern=".csv", full.names=TRUE)
  tbl_df(lapply(list.files(tmp, pattern=".csv", full.names=TRUE), function(x)
    read_tsv(x, col_types=cols_only(
      x = "d",
      y = "d",
      ht0_2 = "d",
      ht0_1 = "d",
      ht1_2 = "d",
      ht2_5 = "d",
      ht5_10 = "d",
      ht10_20 = "d",
      ht20_30 = "d",
      ht30_ = "d")) %>% 
      mutate_(year = gsub('^.*_|\\D', '', basename(x))) %>%
      mutate_each(funs(replace(.,.==100,1)), -x,-y,-year)) %>%
      bind_rows) %>%
    select(x,y,year,ht0_1,ht0_2,ht1_2,ht2_5,ht5_10,ht10_20,ht20_30,ht30_)
}

# Load species table
BCI_load_nomenclature <- function(file){
  data <- load_rdata(file)
  names(data) <- tolower(names(data))
  data
}

# DATA PROCESSING FUNCTION

BCI_clean <- function(BCI_data, spp_table) {
  # NESTED FUNCTIONS
  # Drop last observation
  drop_last <- function(x) {
    if(length(x) > 0)
      x[seq_len(length(x)-1)]
    else
      NULL
  }
  
  # SUB FUNCTIONS USED TO PROCESS DATA
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
  
  # Identifies individuals that return from the dead or are supposably refound
  # i.e. Individuals given dbh=NA and then later given numeric value
  # Note this function must be used prior to subsetting only observations with pom=1.3
  is_zombie <- function(dbh) {
    any(diff(is.na(dbh)) == -1)
  }
  
  # Calculates growth rate as a function of past size
  calculate_growth_rate <- function(x, t, f=function(y) y){
    dt = diff(t)/365.25
    if(any(dt < 0, na.rm=TRUE)){
      stop("time must be sorted")
    }
    c(NA, diff(f(x))/dt)
  }
  
  # Check if individual died next census
  mortality_in_next_census <- function(status){
    if(length(status) > 1){
      i <- 1:(length(status)-1)} # if more than 1 obs
    else{
      i <- 0
    }
    as.numeric(c(status[i] == 'alive' & status[i+1] == 'dead', NA))
  }
  
  #Add taxonomic information
  BCI_data$species = lookup_latin(BCI_data$sp, spp_table)
  BCI_data$family = lookup_family(BCI_data$sp, spp_table)
  
  #census id for period 7 was entered incorrectly
  BCI_data$census <-   BCI_data$censusid
  BCI_data$census[ BCI_data$census==171] <- 7
  
  #Converts dbh from mm to cm
  BCI_data$dbh <- BCI_data$dbh/10
  
  data <- BCI_data %>%
    arrange(sp, treeid, exactdate) %>%
    select(gx,gy,sp, species, family, treeid, nostems, census, exactdate, dfstatus, pom, dbh) %>%
    filter(
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
    mutate(species = replace(species, sp == "swars2", "Swartzia simplex2")) %>%
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
      dead_next_census = mortality_in_next_census(dfstatus)) %>%
    filter(
      # Only keep alive stems
      dfstatus=="alive" &
        #Removes data from most recent survey because survival unknown
        !is.na(dead_next_census)
    ) %>%
    mutate(
      n_census = length(census),
      # First measurement in 1990 ='1990-02-06'
      julian = as.vector(julian(as.Date(exactdate,"%Y-%m-%d"), as.Date("1990-02-06", "%Y-%m-%d"))),
      census_interval = c(NA, diff(julian/365.25)),
      dbh_dt = calculate_growth_rate(dbh, julian),
      dbh_prev = c(NA, drop_last(dbh))
    ) %>%
    filter(
      # Some individuals
      n_census > 1 &
        # Some individuals skipped a census and therefore have interval much more than 5 years
        census_interval < 8  &
        # Remove extreme growth rates
        dbh_dt < 5 &
        dbh_dt/(dbh) > -0.25 &
        # Remove anything where don't have adequate growth from previous period
        !is.na(census_interval*dbh_dt)
    ) %>%
    ungroup() %>%
    group_by(sp) %>%
    mutate(n_ind = length(unique(treeid))) %>%
    # ensures at least 1 individual is in the heldout dataset
    filter(n_ind >=10) %>%
    ungroup() %>%
    select(gx,gy,species,sp,n_ind,treeid,census,exactdate,julian,census_interval,pom,nostems,
           dbh_prev,dbh,dead_next_census)
  
}

# MERGING FUNCTIONS

# Merge BCI individual data with trait database
merge_BCI_data <- function(BCI_demography, BCI_traits) {
  merge(BCI_demography,BCI_traits[,c('sp','rho')],by = 'sp') %>% #only uses species trait data exists for
    filter(!is.na(rho)) %>%
    mutate(sp_id = as.numeric(factor(sp)),
           censusid = as.numeric(factor(census)))
}

# Add true growth rate estimates to data
add_true_growth <- function(data, true_dbh_mod) {
  true <- extract_true_dbh_estimates(true_dbh_mod)
  data$true_dbh1 <- true$true_dbh1
  data$true_dbh2 <- true$true_dbh2
  data$obs_dbh_dt <- (data$dbh - data$dbh_prev)/data$census_interval
  data$true_dbh_dt <- (data$true_dbh2 - data$true_dbh1)/data$census_interval
  data$true_dbh_dt_rel <- (log(data$true_dbh2) - log(data$true_dbh1))/data$census_interval
  data$obs_basal_area_dt <- ((0.25 * pi * data$dbh^2) -  (0.25 * pi * data$dbh_prev^2))/data$census_interval
  data$true_basal_area_dt <- ((0.25 * pi * data$true_dbh2^2) -  (0.25 * pi * data$true_dbh1^2))/data$census_interval
  data$true_basal_area_dt_rel <- (log(0.25 * pi * data$true_dbh2^2) -  log(0.25 * pi * data$true_dbh1^2))/data$census_interval
  data
}

# CROSS VALIDATION FUNCTIONS

# Split into k equally sized datasets
split_into_kfolds <- function(data, k=10) {
  # make dataset an even multiple of 10
  data <- data[seq_len(floor(nrow(data) / k) * k), ]
  # execute the split
  # use an ordered vector so that all species distributed
  # approx. equally across groups
  fold <- rep(seq_len(k), nrow(data)/k)
  split(data, fold)
}

# Break up into train and heldout samples
extract_trainheldout_set <- function(data, k=NA) {
  # by default train on whole dataset
  i_train <- seq_len(length(data))
  if (is.na(k)) {
    i_heldout <- NA
    res <- rbind_all(data[i_train])
  } else {
    i_train <- setdiff(i_train, k)
    i_heldout <- k
    
    res <- list(
      train = rbind_all(data[i_train]),
      heldout  = rbind_all(data[i_heldout]))
  }
  return(res)
}

# Make multiple data folds
make_trainheldout <- function(data) {
  lapply(seq_along(data), function(i)
    extract_trainheldout_set(data, i))
}


# CANOPY GAP FUNCTIONS

# Summarize canopy data by census such 
# presence per stratum above 2m is summed and the minimum is taken by census
# doing so will capture the possiblity of a census occuring in the middle of a census

gap_data <- function(canopy_data) {
  canopy_data %>%
    dplyr::select(-ht0_1) %>% # Not used in earlier years
    filter(year >=1985 & year <= 1995) %>% # 2003 onwwards uses voxels which is not comparable to earlier years
    group_by(x,y,year) %>%
    mutate(sum_canopy_above2m = sum(ht2_5,ht5_10,ht10_20,ht20_30,ht30_, na.rm=TRUE)) %>% 
    # sums presences above 2m and converts it to a scale between 0 and 1.
    #where 1 = gap (no veg above 2 m) & 0 = non gap (veg in all strata above 2 m)
    ungroup %>%
    mutate(censusid = findInterval(year,c(1990))+1) %>% # 1= past light environment for 1985 recruits etc..
    group_by(x,y,censusid) %>%
    summarise(gap_index = (5 - min(sum_canopy_above2m))/5) %>% # Find minumum canopy stratum above 2m per census.
    ungroup() %>%
    dplyr::select(x,y,censusid,gap_index)
}

# get dataframe of recruits in years 1985 to 1995
# where recruit is a presence in a later census that wasn't there prior to 1985.
recruits_8595 <- function(raw_BCI_data) {
  
  raw_BCI_data %>%
    filter(censusid >1 & censusid < 5) %>% # Only using recruit data from 1985 to 1995 as this uses only vertical line approach
    arrange(sp, treeid, censusid) %>%
    dplyr::select(gx,gy,sp, treeid, censusid, dfstatus, dbh) %>%
    filter(dfstatus=="alive" & !is.na(dbh) & !is.na(gx) & !is.na(gy)) %>% 
    # above removes dead observations & NA dbh or coordinates.
    group_by(treeid) %>%
    slice(1) %>% # Takes first observation per treeid
    ungroup() %>%
    filter(censusid!=2 & # removes first census as unsure whether these are recruits or not
             dbh < 250) %>% # removes individuals with a dbh > 25 cm - unlikely to be recruits
    mutate(censusid = as.numeric(factor(censusid))) %>% # rescales censusid such that 1 = 85. 
    dplyr::select(x=gx,y=gy,sp,treeid,censusid)
}

# Make a raster of gap index
get_gap_index_raster <- function(canopy_data,weight_matrix = matrix(c(1, 1, 1, 1, 8, 1, 1, 1, 1), 3, 3)) {
  
  gap_data(canopy_data) %>%
    as.data.frame %>% # converts to df for sp package
    {coordinates(.) <- ~x+y; .} %>%
    raster::shift(x=2.5, y=2.5) %>% # centers coordinates on cell mid point
    split(.$censusid) %>% # splits by census
    lapply(function(x) { # Converts to raster
      raster::rasterFromXYZ(as.data.frame(x)[c('x','y','gap_index')], res = c(5,5)) 
    }) %>% # calculates mean gap index value using weights
    lapply(raster::focal, weight_matrix, mean) %>% # calculates mean gap index value using weights
    lapply(setNames, 'mean_gap_index') %>% # Names the gap index column
    lapply(function(x) x/raster::maxValue(x)) %>% # Converts to binary scale 0 = no gap 1 = full gap.
    {names(.) <- c("1985 to 1990", "1990 to 1995"); .}
}

# Extract gap conditions for all recruits
get_recruit_gap_conditions <- function(recruits, gap_index_raster) {
  # get light environment for recruits
  recruits  %>% 
    as.data.frame %>% # converts to df for sp package
    {coordinates(.) <- ~x+y; .} %>% 
    raster::crop(raster::extent(.) - 20*2) %>%  # removes edge effects out to 20 m
    split(.$censusid) %>% # splits by census
    mapply(function(raster, points) raster::extract(raster, points, sp=TRUE), gap_index_raster, .)
}

# Summarises recruit gap environment by species
# This can be used as a measure of shade tolerance.
# The idea being that species less shade intolerant should have
# more recruits in gaps relative to a shade tolerant species, and hence
# a higher gap_index.
get_mean_spp_gap_index <- function(recruit_gap_conditions) {
  as.data.frame(do.call(rbind, recruit_gap_conditions))%>%
    group_by(sp) %>%
    summarise(mean_gap_index = mean(mean_gap_index))
}

# Find the 95 percentile dbh observed for each species.
get_spp_dbh95 <- function(raw_BCI_data) {
  raw_BCI_data %>%
    arrange(sp, treeid, exactdate) %>%
    select(sp, treeid, nostems, pom, dbh) %>%
    filter(!is.na(sp)) %>%
    mutate(dbh = dbh/10) %>%
    group_by(treeid) %>%
    filter(max(nostems)==1 & !is.na(dbh)) %>%
    filter(pom == '1.3') %>%
    ungroup() %>%
    group_by(sp) %>%
    summarise(dbh_95 = quantile(dbh, 0.95, na.rm=TRUE)) %>%
    ungroup() %>%
    select(sp,dbh_95)
}