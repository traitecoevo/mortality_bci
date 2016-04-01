# To run the mortality analysis it first requires the data to be downloaded, cleaned and processed for use with stan.
# To do this run remake::('export_1')
# Note this will take approximately 10 hours as it needs to estimate true growth for every observation in the dataset.

packages <- c("rstan","dplyr","tidyr")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}

#precompile_all()
# Launching growth comparison analysis - need to replace mclapply
tasks <- tasks_2_run(comparison = 'function_growth_comparison',iter = 20)
tasks <- tasks[c(6),]
#tasks <- tasks[c(31),]
ret <- parallel::mclapply(df_to_list(tasks), model_compiler, mc.cores = getOption("mc.cores", 6L))

my_list <- df_to_list(tasks)
model_compiler(mylist[[1]])

docker run --rm --it  -v ${PWD}:/home/data -t traitecoevo/mortality_bci:latest 

