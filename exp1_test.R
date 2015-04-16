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
pars_list <- df_to_list(exp1_pars_test(name="test"))
create_dirs(pars_list)
res <- mclapply(pars_list, train_model)
