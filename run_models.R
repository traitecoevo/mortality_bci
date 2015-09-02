# To run the mortality analysis it first requires the data to be downloaded, cleaned and processed for use with stan.
# To do this run remake::('export_1')
# Note this will take approximately 10 hours as it needs to estimate true growth for every observation in the dataset.

packages <- c("plyr","parallel","remake","rstan")
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


# Launching growth comparison analysis - need to replace mclapply
tasks <- tasks_growth(iter = 1000, name="growth_comparison")
ret <- mclapply(df_to_list(tasks), model_compiler, mc.cores = getOption("mc.cores", 2L))
