## Simplified version of exp1.R that drops reference to remake
##
## Before running this file, make sure you've run
##   remake::make("data")
## to create the required dataset.
##
## There are odd choices made for how this is structured; these are to
## meet part way with rrqueue.
packages <- c("rstan", "plyr", "parallel")
sources <- c("R/model_all.R",
             "R/model_constant.R",
             "R/model_species.R",
             "R/model_trait.R",
             "R/model_trait_species.R",
             "R/model_all.R",
             "R/stan_models.R",
             "R/utils.R",
             "R/experiment1.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}

# Launching
pars <- exp1_pars()
pars_list <- df_to_list(pars)
create_dirs(pars_list)

## Try on a test data set.
# res <- train_model(pars_list[[i]])

## Rerun jobs if needed
# rerun <- c(21,1,25,39,73,74,75,98,51,80,31, 43,44)
# rerun <- which(!file.exists(pars$filename))
# tmp <- mclapply(df_to_list(pars[rerun,]), train_model, mc.cores=8, mc.preschedule = FALSE)

## Analysis
split_ids <- c("model", "effect", "growth_measure", "data")
complete <- file.exists(pars$filename)
results <- pars[complete,]

summarise_stanfits <- function(x) {

  sx <- summary(x)$summary

  data.frame(lp=mean(extract(x, 'lp__')$lp__),
                          n_eff_min=min(sx[, 'n_eff']),
                          rhat_max =max(sx[, 'Rhat']),
                          r_bad_n =sum(sx[, 'Rhat'] > 1.1))
}

load_stan_chains <- function(files) {
  n_chains <- length(files)
  if(n_chains == 0) {
    NULL
  } else if(n_chains == 1) {
    readRDS(files[1])
  } else if(n_chains > 1) {
    sflist2stanfit(lapply(files, readRDS))
  }
}

load_summarise_stan <- function(data) {
   chains <- load_stan_chains(data$filename)
   data.frame(n_chains = length(data$filename), summarise_stanfits(chains))
}

x <- ddply(results, split_ids, load_summarise_stan)
y <- ddply(results, c(split_ids, "chain"), load_summarise_stan)

y2 <- merge(y, pars, by = c(split_ids, "chain"))
y2[y2$r_bad_n > 0,]$jobid
