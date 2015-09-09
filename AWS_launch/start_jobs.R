library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")

obj <- queue("rrq", redis_host="localhost", packages=packages, sources=sources)
# on cluster redis queu is called redis.marathon.mesos, but from local machine called localhost -- via exposed tunnel

tasks <- tasks_growth(iter = 10, path="/home/data") # Set to 10 for testing, set to 1000 for actual deployment

create_dirs(unique(dirname(tasks$filename)))

# precompile all the models (takes a little while)
precompile_tasks(tasks)

# queue the jobs
res <- enqueue_bulk(tasks, model_compiler, obj)
