library('dplyr')
library('R2jags')
load('../data_BCI/output/bci.mainstem.Rdata')
bci.traits <- read.csv('../data_BCI/rawdata/traits/BCI_traits_20101220.csv')
names(bci.traits) <- tolower(names(bci.traits)) # lowers trait column names for merging
bci.traits$sp <- tolower(bci.traits$sp) # lowers species code names for merging

bci.mainstem <- merge(bci.mainstem,bci.traits[,c('sp','sg100c_avg')],by = 'sp') #only uses species trait data exists for.
rm(bci.traits) # no longer needed

bci.mainstem <- bci.mainstem %.%
  group_by('sp') %.% 
  mutate(rel.dbh = dbh/max(dbh, na.rm=T),
         max.dbh = max(dbh, na.rm=T),
         bins = bins.greedy(rel.dbh, target.bins = 100, max.breaks = 200)$binct)

test<- bci.mainstem %.%
  mutate(
         bins = bins.greedy(rel.dbh, nbins = 10)$binct

bci.mainstem <-subset(bci.mainstem, bci.mainstem$year==2000 & bci.mainstem$sp=='hybapr' & !is.na(bci.mainstem$sg100c_avg) & !is.na(bci.mainstem$census.interval))

data <- list(
  n.obs = nrow(bci.mainstem),
  n.spp = length(unique(bci.mainstem$sp)),
  SPP = as.numeric(factor(bci.mainstem$sp), as.character(unique(bci.mainstem$sp))),
  wd = scale(unique(bci.mainstem[c('sp', 'sg100c_avg')])[,'sg100c_avg'])[,1]/2,
  dbh = scale(log(bci.mainstem$rel.dbh))[,1]/2,
  ln.census = scale(log(bci.mainstem$census.interval))[,1]/2,
  died = bci.mainstem$dead.next.census)

# Note this runs multiple models with each loop iteration changing the covariates used

  model <- function(x) {
    for(it in 1:n.obs) {
      
      died[it] ~ dbern(p[it])
      cloglog(p[it]) <- alpha[SPP[it]] + b.2 * dbh[it] + b.3 * dbh[it] * dbh[it] + b.4 * dbh[it] * wd[SPP[it]]
      }
    
    for (j in 1:n.spp) {
      alpha[j] <- GM +  b.1 * wd[j]
      }
    
    #Priors
    
    GM ~ dnorm(0, 0.0001) # Intercept
    b.1 ~ dnorm(0, 0.0001) # Wood Density effect
    b.2 ~ dnorm(0, 0.0001) # Growth/size effect
    b.3 ~ dnorm(0, 0.0001) # Wood density x growth/size effect
    b.4 ~ dnorm(0, 0.0001) # Wood density x growth/size effect
  
    }
  
         test <- jags.parallel(model.file = model,
                       n.iter = 4000,
                       data= names(data), 
                       envir=list2env(data),
                       inits = NULL,
                       parameters.to.save = c('GM','b.1','b.2','b.3', 'b.4'))
