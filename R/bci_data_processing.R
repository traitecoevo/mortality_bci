#List of functions used to manipulate and summarise data from BCI.

subset_BCI_data_by_census <- function(data, census=4) {
  filter(data, censusid==census)
}

merge_BCI_data <- function(BCI_demography, BCI_traits) {
  merge(BCI_demography,BCI_traits[,c('sp','rho')],by = 'sp') %>% #only uses species trait data exists for
  filter(!is.na(rho)) %>%
    mutate(sp_id = as.numeric(factor(sp)),
           censusid = as.numeric(factor(census)))
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

BCI_load_dbh_error_data <- function(file) {
data <-  load_rdata(file)
data$dbh1 <- data$dbh1/10 # convert mm to cm
data$dbh2 <- data$dbh2/10 # convert mm to cm 
data$discrep <- data$dbh1 - data$dbh2
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

  mutate(data, rho = sg100c_avg) %>% # in g/cm3
  select(-sg100c_avg)
}

# Identifies individuals that return from the dead or are supposably refound
# i.e. Individuals given dbh=NA and then later given numeric value
# Note this function must be used prior to subsetting only observations with pom=1.3
is_zombie <- function(dbh) {
  any(diff(is.na(dbh)) == -1)
}

drop_last <- function(x) {
  if(length(x) > 0)
    x[seq_len(length(x)-1)]
  else
    NULL
}

mortality_in_next_census <- function(status){
  if(length(status) > 1){
    i <- 1:(length(status)-1)} # if more than 1 obs
  else{
    i <- 0
  }
  as.numeric(c(status[i] == 'alive' & status[i+1] == 'dead', NA))
}

BCI_clean <- function(BCI_data, spp_table) {

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
    select(sp, species, family, treeid, nostems, census, exactdate, dfstatus, pom, dbh) %>%
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
    select(sp,n_ind,treeid,census,exactdate,julian,census_interval,pom,nostems,
           dbh_prev,dbh,dead_next_census)

  }

reduce_to_single_ind_obs <- function(data) {
  # set seed so that same subsetting is implemented on all machines
  set.seed(523)

  # returns vector of same length which is all FALSE except
  # for a single, randomly placed TRUE
  sample_one <- function(x) {
    seq_along(x) == sample(length(x), 1)
  }

  ret <-
    data %>%
      group_by(treeid) %>%
      mutate(keep = sample_one(treeid)) %>%
      filter(keep) %>%
      select(-keep) %>%
      ungroup() %>%
    select(sp, sp_id, censusid, dead_next_census,
           census_interval, rho, dbh_prev, dbh)
}

# split into k equally sized datasets
split_into_kfolds <- function(data, k=10) {
  # make dataset an even multiple of 10
  data <- data[seq_len(floor(nrow(data) / k) * k), ]
  # execute the split
  # use an ordered vector so that all species distributed
  # approx. equally across groups
  fold <- rep(seq_len(k), nrow(data)/k)
  split(data, fold)
}

extract_trainheldout_set <- function(data, k=NA) {
  # by default train on whole dataset
  i_train <- seq_len(length(data))
  if (is.na(k)) {
    i_heldout <- NA
  } else {
    i_train <- setdiff(i_train, k)
    i_heldout <- k
  }

  list(
    train = rbind_all(data[i_train]),
    heldout  = rbind_all(data[i_heldout]))
}

make_trainheldout <- function(data) {
  lapply(seq_along(data), function(i)
         extract_trainheldout_set(data, i))
}

## Really ugly working around something I've not worked out how to do
## in remake (1 function -> n file outputs)
export_data <- function(data, filename) {
  saveRDS(data, filename)
  filename_fmt <- sub("\\.rds$", "_%s.rds", filename)
  filename_sub <- sprintf(filename_fmt, seq_along(data))
  for (i in seq_along(data)) {
    saveRDS(data[[i]], filename_sub[[i]])
  }
}

# Calculates growth rate as a function of past size
calculate_growth_rate <- function(x, t, f=function(y) y){
  dt = diff(t)/365.25
  if(any(dt < 0, na.rm=TRUE)){
    stop("time must be sorted")
  }
  c(NA, diff(f(x))/dt)
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
