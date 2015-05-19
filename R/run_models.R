## Before running this file, make sure you've run
##   remake::make("data")
## to create the required dataset.
##
## There are odd choices made for how this is structured; these are to
## meet part way with rrqueue.
#remake::make("data")

packages <- c("rstan", "plyr", "parallel")
sources <- c("R/model_constant.R",
             "R/model_census_err.R",
             "R/model_no_spp_err.R",
             "R/model_no_trait.R",
             "R/model_full.R",
             "R/model_full_priors.R",
             "R/model_full_rho_combs.R",
             "R/task_compiler.R",
             "R/get_all_model_chunks.R",
             "R/stan_functions.R",
             "R/utils.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}


# Prior test
functional_parsimony_pars <- pars_functional_parsimony(iter = 20)
functional_parsimony_pars <- subset(functional_parsimony_pars, fold_data=="export/bci_data_1.rds" & model==3)
create_dirs(unique(dirname(functional_parsimony_pars$filename)))
ret <- mclapply(df_to_list(functional_parsimony_pars), model_data)

# Will make below into functions when we move away from mclapply
# Test to make sure all model forms work
test_pars <- pars_test(iter = 2000)
create_dirs(unique(dirname(test_pars$filename)))
ret <- mclapply(df_to_list(test_pars), model_data)

# Launching growth comparison analysis
growth_pars <- pars_growth(iter = 2000)
create_dirs(unique(dirname(growth_pars$filename)))
ret <- mclapply(df_to_list(growth_pars), model_data)

# Launching rho combination analysis (only run once best growth rate has been determined)

rho_combs_pars <- pars_rho_combs(iter = 2000)
create_dirs(unique(dirname(rho_combs_pars$filename)))
ret <- mclapply(df_to_list(rho_combs_pars), model_data)

# Launching rho combination analysis (only run once best growth rate has been determined)
functional_parsimony_pars <- pars_functional_parsimony(iter = 2000)
create_dirs(unique(dirname(functional_parsimony_pars$filename)))
ret <- mclapply(df_to_list(functional_parsimony_pars), model_data)