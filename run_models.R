## Before running this file, make sure you've run
##   remake::make("data")
## to create the required dataset.
##
#remake::make("data")

packages <- c("rstan","plyr","parallel","remake")
sources <- c("R/dbh_error_model.r",
             "R/true_dbh_model.r",
             "R/model2.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}

# The following code is to run each set of models.
# The models must be run consecutively as later models 
# either depend on estimate of previous models (e.g. true dbh estimates)
# or won't run the full suite of models as some are run in previous runs.
# Also because these models use mclapply they cannot be run via R or Rstuido's
# gui reliably. Run these models in R via the terminal.


# Estimate dbh measurement error (time: few seconds)
run_dbh_error_model()

# Estimate true dbh (time: ~ 7 hours)
run_true_dbh_model()

# NOTE the following models run 100's of chains
# each taking X hours. These models are best run on a cluster.

# Launching growth comparison analysis
run_growth_comparison(iter = 2000)

# Launching rho combination analysis (only run once best growth rate has been determined)
run_rho_combination(iter = 10, growth_measure = 'dbh_dt')


# Remove this code once testing is complete
# Test on subset - growth combinations
tasks <- tasks_growth(iter = 2000,name = 'test')
tasks <- tasks[c(1,2,3),]
create_dirs(unique(dirname(tasks$filename)))
ret <- mclapply(df_to_list(tasks), model_compiler)
# Test on subset - rho combinations
tasks <- tasks_rho_combos(iter = 10,growth_measure = 'dbh_dt')
tasks <- tasks[c(1,4,215,232),]
create_dirs(unique(dirname(tasks$filename)))
ret <- mclapply(df_to_list(tasks), model_compiler)

xx <- sflist2stanfit(c(readRDS('results/test_growth_nc/1.rds'),readRDS('results/test_growth_nc/2.rds'),readRDS('results/test_growth_nc/3.rds')))
