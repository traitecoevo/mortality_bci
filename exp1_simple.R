## Simplified version of exp1.R that drops reference to remake
##
## Before running this file, make sure you've run
##   remake::make("data")
## to create the required dataset.
##
## There are odd choices made for how this is structured; these are to
## meet part way with rrqueue.
packages <- "rstan"
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

pars <- exp1_pars(iter=10)
pars_list <- df_to_list(pars)
for (d in unique(dirname(pars$filename))) {
  dir.create(d, FALSE, TRUE)
}

## Try on a test data set.
i <- which(pars$effect == "trait_species")[[1]]
p <- pars_list[[i]]
res <- exp1_run_model(p)
