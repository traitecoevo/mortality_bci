## Before running this file, make sure you've run
##   remake::make("data")
## to create the required dataset.
##
## There are odd choices made for how this is structured; these are to
## meet part way with rrqueue.
#remake::make("data")

packages <- c("rstan","plyr","parallel")
sources <- c("R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}


# Launching growth comparison analysis
run_growth_comparison(iter = 2000)

# Launching rho combination analysis (only run once best growth rate has been determined)
run_rho_combination(iter = 10, growth_measure = 'dbh_dt')


# Test on subset - growth combinations
tasks <- tasks_growth(iter = 10)
tasks <- tasks[c(1,4,120),]
create_dirs(unique(dirname(tasks$filename)))
ret <- mclapply(df_to_list(tasks), model_compiler)
# Test on subset - rho combinations
tasks <- tasks_rho_combos(iter = 10,growth_measure = 'dbh_dt')
tasks <- tasks[c(1,4,215,232),]
create_dirs(unique(dirname(tasks$filename)))
ret <- mclapply(df_to_list(tasks), model_compiler)
