packages <- c("plyr","parallel","remake","rstan")
sources <- c("R/dbh_error_model.r",
             "R/true_dbh_model.r",
             "R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}


# FIRST run make stage 1:
#make() not functional yet

# NOTE the following models run 100's of chains
# each taking X hours. These models are best run on a cluster.

# 3) Launching growth comparison analysis
run_growth_comparison(iter = 1000)

# 4) Launching rho combination analysis (only run once best growth rate has been determined)
run_rho_combination(iter = 1000, growth_measure = 'dbh_dt')


# Remove this code once testing is complete
# Test on subset - growth combinations
tasks <- tasks_growth(iter = 2000,name = 'final_test')
tasks <- tasks[c(1:12),]
create_dirs(unique(dirname(tasks$filename)))
ret <- mclapply(df_to_list(tasks), model_compiler, mc.cores=12)
# Test on subset - rho combinations
tasks <- tasks_rho_combos(iter = 10,growth_measure = 'dbh_dt')
tasks <- tasks[c(1,4,215,232),]
create_dirs(unique(dirname(tasks$filename)))
ret <- mclapply(df_to_list(tasks), model_compiler)
