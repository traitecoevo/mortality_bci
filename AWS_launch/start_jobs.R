library(rrqueue)
packages <- c("rstan","dplyr")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")

# RUN DOCKER CONTAINER LOCALLY
#obj <- queue("rrq", redis_host="redis", packages=packages, sources=sources)

# RUN DOCKER CONTAINER ON CLUSTER
obj <- queue("rrq", redis_host="localhost", packages=packages, sources=sources,  redis_port = 31379)

# PHASE 1: BEST GROWTH & FUNCTIONAL FORM
func_growth_tasks <- tasks_2_run(comparison = 'function_growth_comparison',iter = 4000, 
                     path="/home/data")
res <- enqueue_bulk(func_growth_tasks, model_compiler, obj, progress_bar = TRUE)

# PHASE 2: BEST GROWTH & FUNCTIONAL FORM
species_re_tasks <- tasks_2_run(comparison = 'species_random_effects',iter = 4000, 
                     path="/home/data")
res <- enqueue_bulk(species_re_tasks, model_compiler, obj, progress_bar = TRUE)

# PHASE 3:  BEST RHO COMBINATION
rho_combos <- tasks_2_run(comparison = 'rho_combinations',iter = 4000, 
                     path="/home/data")
res <- enqueue_bulk(rho_combos, model_compiler, obj, progress_bar = TRUE)



