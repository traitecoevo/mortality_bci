# This script launches jobs on the NCI raijin cluster
# It is written to be run from the project root. 

source("scripts/pbs.R")

launch_task_i_raijin <- function(...) {
	launch_task_i(..., pbs.whisker = "scripts/pbs_raijin.whisker")
}

# test
launch_task_i_raijin(1, "null_model", iter =100)
launch_task_i_raijin(10*1:10, "function_growth_comparison", iter=10)
launch_task_i_raijin(10*1:10, "rho_combinations", iter=10)
