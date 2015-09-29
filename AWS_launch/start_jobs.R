library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")

# FIRST RUN GROWTH COMPARISON ANALYSIS ON CLUSTER
obj <- queue("rrq", redis_host="localhost", packages=packages, sources=sources)
# on cluster redis queue is called redis.marathon.mesos, but from local machine called localhost -- via exposed tunnel
growth_tasks <- tasks_2_run(analysis = 'no_gamma_model',iter = 1000, 
                     growth_measure = c('true_dbh_dt','true_basal_area_dt'),
                     path="/home/data")
# queue the jobs
res <- enqueue_bulk(growth_tasks, model_compiler, obj, progress_bar = FALSE)

# NEXT FIND THE MOST PARIMONIOUS COMBINATION OF RHOs
# true_dbh_dt has a higher log likelihood and thus is used for subseqent analysis.
rho_tasks <- tasks_2_run(analysis = 'rho_combinations',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(rho_tasks, model_compiler, obj, progress_bar = FALSE)

# NULL MODEL
# What about a model with just a constant hazard?
null_tasks <- tasks_2_run(analysis = 'null_model',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(null_tasks, model_compiler, obj, progress_bar = FALSE)

# NULL MODEL VARIES BY SPECIES ID
null_re_tasks <- tasks_2_run(analysis = 'null_model_random_effects',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(null_re_tasks, model_compiler, obj, progress_bar = FALSE)

# TRADITIONAL NEGATIVE EXPONENTIAL WITHOUT GAMMA

no_gamma_tasks <- tasks_2_run(analysis = 'no_gamma_model',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(no_gamma_tasks, model_compiler, obj, progress_bar = FALSE)

# TRADITIONAL NEGATIVE EXPONENTIAL WITHOUT GAMMA BUT WITH RANDOM EFFECTS

no_gamma_re_tasks <- tasks_2_run(analysis = 'no_gamma_model_random_effects',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(no_gamma_re_tasks, model_compiler, obj, progress_bar = FALSE)



