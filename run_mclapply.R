packages <- c("dplyr","parallel","remake","rstan","pbmcapply",'remake')
sources <- c("R/bci_data_processing.R", "R/model.R","R/stan_functions.R")

for (p in packages) {
  library(p, character.only=TRUE, quietly=TRUE)
}
for (s in sources) {
  source(s)
}

# What tasks to you want to run?
tasks <- remake::make('function_growth_tasks')
tasks <- null_model_tasks[91:180,]

# Run tasks
mclapply(df_to_list(tasks), model_compiler, mc.cores = 35)
