library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")

# FIRST RUN GROWTH COMPARISON ANALYSIS ON CLUSTER
obj <- queue("rrq", redis_host="localhost", packages=packages, sources=sources)
# on cluster redis queue is called redis.marathon.mesos, but from local machine called localhost -- via exposed tunnel
tasks <- tasks_growth(iter = 1000, path="/home/data")
# queue the jobs
res <- enqueue_bulk(tasks, model_compiler, obj, progress_bar = FALSE)


# true_dbh_dt has a higher log likelihood and thus is used for subseqent analysis.
# Can we just add these jobs or do we need to create another redis queue? 
tasks <- tasks_rho_combos(iter = 1000, growth_measure = 'true_dbh_dt')
res <- enqueue_bulk(tasks, model_compiler, obj, progress_bar = FALSE)